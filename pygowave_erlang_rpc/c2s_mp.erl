
%%
%% PyGoWave Server - The Python Google Wave Server
%% Copyright 2009 Patrick Schneider <patrick.p2k.schneider@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(c2s_mp).
-export([start/1]).
-export([process/3]).
-export([purge_connections/1]).

%% Records
-include("models.hrl").
-include("operation.hrl").

%% Defines
-include("settings.hrl").

%% Logging
log(Module, Line, Level, Format, Arguments) ->
	LogOk = case ?LOG_LEVEL of
		debug ->
			true;
		normal ->
			case Level of
				debug -> false;
				_ -> true
			end;
		error ->
			case Level of
				debug -> false;
				normal -> false;
				_ -> true
			end
	end,
	case LogOk of
		true -> io:format("~w:~b: " ++ Format ++ "~n", [Module, Line] ++ Arguments);
		_ -> ok
	end.

-define(Log(Level, Format, Arguments),
	log(?MODULE, ?LINE, Level, Format, Arguments)).

%% -- Erlang port of the PyGoWave Client-to-Server Message Processor --

start({ready, DBPoolId}) ->
	receive
		terminate ->
			?Log(normal, "Terminating...", []);
		purge ->
			?MODULE:purge_connections(DBPoolId),
			?MODULE:start({ready, DBPoolId});
		{process, Sender, RKey, Data} ->
			Sender ! ?MODULE:process(RKey, Data, DBPoolId),
			?MODULE:start({ready, DBPoolId})
	after 600000 -> % Auto-restart after 10 minutes to allow code swapping
		?MODULE:start({ready, DBPoolId})
	end;

start(DBPoolId) ->
	?Log(normal, "Starting...", []),
	init_random_seed(),
	start({ready, DBPoolId}).

purge_connections(DBPoolId) ->
	?Log(debug, "Purging connections...", []),
	lists:foreach(fun (PConn=#pconn{id=PConnId, participant_id=PId}) ->
			models:delete_pconn({pconn, PConn}, DBPoolId),
			?Log(normal, "[~s/~p] Connection to server closed (by timeout)", [PId, PConnId])
		end,
		models:get_timeout_pconns(?ACCESS_KEY_TIMEOUT_MINUTES, DBPoolId)).

%% --------------------------------------------------------------------

init_random_seed() ->
	{A1,A2,A3} = erlang:now(),
	random:seed(A1,A2,A3).

%%% General message handlers %%%

%% Process one or more messages and return a list of tuples {Destination, Messages}
process(RKey, Data, DBPoolId) ->
	case string:tokens(RKey, ".") of
		[PConnKey, WaveletId, "clientop"] ->
			%% Get participant connection
			PConn = case models:get_pconn({tx_key, PConnKey}, DBPoolId) of
				undefined -> #pconn{rx_key=PConnKey, tx_key=PConnKey};
				GotPConn -> GotPConn
			end,
			try
				process(PConn, WaveletId, Data, DBPoolId)
			catch
				throw:Term ->
					?Log(error, "{~s} Exception throw '~w' - Stacktrace:~n~p", [PConn#pconn.tx_key, Term, erlang:get_stacktrace()]),
					[{mk_target(PConn, WaveletId), [mk_error(<<"EXCEPTION_IN_HANDLER">>, <<"The message handler has thrown an exeption">>)]}];
				exit:Reason ->
					?Log(error, "{~s} Exception exit '~w' - Stacktrace:~n~p", [PConn#pconn.tx_key, Reason, erlang:get_stacktrace()]),
					[{mk_target(PConn, WaveletId), [mk_error(<<"EXCEPTION_IN_HANDLER">>, <<"The message handler has thrown an exeption">>)]}];
				error:Reason ->
					?Log(error, "{~s} Exception error '~w' - Stacktrace:~n~p", [PConn#pconn.tx_key, Reason, erlang:get_stacktrace()]),
					[{mk_target(PConn, WaveletId), [mk_error(<<"EXCEPTION_IN_HANDLER">>, <<"The message handler has thrown an exeption">>)]}]
			end;
		_ ->
			[] % Bad routing key -> ignore
	end.

process(_, _, [], _) ->
	[]; % All done
process(PConn, WaveletId, [Data|Rest], DBPoolId) ->
	Results = case Data of
		{obj, [{"type", Type}]} -> process_message(PConn, WaveletId, Type, undefined, DBPoolId);
		{obj, [{"type", Type}, {"property", Prop}]} -> process_message(PConn, WaveletId, Type, Prop, DBPoolId);
		{obj, [{"property", Prop}, {"type", Type}]} -> process_message(PConn, WaveletId, Type, Prop, DBPoolId);
		_ ->
			?Log(error, "Unknown message from ~s.~s", [PConn#pconn.tx_key, WaveletId]),
			[{mk_target(PConn, WaveletId), [mk_error(<<"UNKNOWN_MESSAGE">>, <<"Message lacks 'type' field">>)]}]
	end,
	case Results of
		disconnected ->
			[]; % Discard all following messages
		_ ->
			combine_results(Results, process(PConn, WaveletId, Rest, DBPoolId)) % Process next message
	end;
process(PConn, WaveletId, One, DBPoolId) ->
	process(PConn, WaveletId, [One], DBPoolId).

process_message(PConn, "login", Type, Prop, DBPoolId) ->
	process_login_message(PConn, Type, Prop, DBPoolId);

process_message(PConn, WaveletId, Type, Prop, DBPoolId) ->
	case models:get_participant({pconn, PConn}, DBPoolId) of
		undefined ->
			[{mk_target(PConn, WaveletId), [mk_error(<<"NO_CONNECTION">>, <<"Not logged in or disconnected by server">>)]}];
		Participant ->
			% Update the PConn first
			models:update_pconn({pconn, PConn}, DBPoolId),
			case WaveletId of
				"manager" ->
					process_management_message(PConn, Participant, Type, Prop, DBPoolId);
				_ ->
					case models:get_wavelet({participant, Participant, wavelet_id, WaveletId}, DBPoolId) of
						undefined ->
							?Log(error, "[~s/~p@~s] Wavelet not found (or not participating)", [Participant#participant.name, PConn#pconn.id, WaveletId]),
							[{mk_target(PConn, WaveletId), [mk_error(<<"WAVELET_NOT_AVAILABLE">>, <<"Wavelet not found or not participating">>)]}];
						Wavelet ->
							process_wavelet_message(PConn, Participant, Wavelet, Type, Prop, DBPoolId)
					end
			end
	end.

%%% Specific message handlers %%%

%% Login messages

process_login_message(PConn, <<"LOGIN">>, Prop, DBPoolId) ->
	case Prop of
		{obj, Props} ->
			Username = getattr(Props, "username", <<>>),
			Password = getattr(Props, "password", <<>>),
			Msg = case models:user_authenticate({Username, Password}, DBPoolId) of
				error ->
					?Log(normal, "[~s@login] Login failed", [PConn#pconn.tx_key]),
					mk_error(<<"LOGIN_FAILED">>, <<"Unknown username or password">>);
				{ok, UserId} ->
					Participant = models:get_or_create_participant({user_id, UserId}, DBPoolId),
					NewPConn = models:create_new_pconn(Participant, DBPoolId),
					?Log(normal, "[~s@login] Participant logged in via key '~s'", [Participant#participant.name, NewPConn#pconn.rx_key]),
					mk_login(Participant, NewPConn)
			end,
			[{mk_target(PConn, "login"), [Msg]}];
		_ ->
			[{PConn#pconn.rx_key, mk_error(<<"MALFORMED_MESSAGE">>, <<"Message could not be processed">>)}]
	end;
process_login_message(PConn, Type, _, _) ->
	?Log(error, "[~s@login] Unknown message type: ~s", [PConn#pconn.tx_key, Type]),
	[{mk_target(PConn, "login"), [mk_error(<<"UNKNOWN_MESSAGE">>, io_lib:format("Type '~s' not recognised", [Type]))]}].

%% Manager messages

process_management_message(PConn, _, <<"PING">>, Prop, _) ->
	[{mk_target(PConn, "manager"), [mk_message(<<"PONG">>, Prop)]}];

process_management_message(PConn, Participant, <<"DISCONNECT">>, _, DBPoolId) ->
	?Log(normal, "[~s/~p@manager] Connection to server closed (by client)", [Participant#participant.name, PConn#pconn.id]),
	models:delete_pconn({pconn, PConn}, DBPoolId),
	disconnected;

process_management_message(PConn, Participant, <<"PARTICIPANT_INFO">>, Prop, DBPoolId) ->
	?Log(normal, "[~s/~p@manager] Sending participant information", [Participant#participant.name, PConn#pconn.id]),
	PInfo = lists:map(
		fun (PId) ->
			case models:get_participant({participant_id, PId}, DBPoolId) of
				undefined -> {PId, null};
				P -> {PId, models:serialize_participant(P)}
			end
		end, Prop),
	[{mk_target(PConn, "manager"), [mk_message(<<"PARTICIPANT_INFO">>, {obj, PInfo})]}];

process_management_message(PConn, Participant, <<"PARTICIPANT_SEARCH">>, Prop, DBPoolId) ->
	UnicodeQuery = unicode:characters_to_list(Prop),
	Msg = if
		length(UnicodeQuery) < ?PARTICIPANT_SEARCH_LENGTH ->
			?Log(debug, "[~s/~p@manager] Participant search query too short", [Participant#participant.name, PConn#pconn.id]),
			mk_message(<<"PARTICIPANT_SEARCH">>, {obj, [{"result", <<"TOO_SHORT">>}, {"data", ?PARTICIPANT_SEARCH_LENGTH}]});
		true ->
			?Log(normal, "[~s/~p@manager] Performing participant search", [Participant#participant.name, PConn#pconn.id]),
			ParticipantId = Participant#participant.id,
			Participants = lists:filter(fun (Id) -> Id /= ParticipantId end, models:find_participant_ids({name_icontains, Prop}, DBPoolId)),
			mk_message(<<"PARTICIPANT_SEARCH">>, {obj, [{"result", <<"OK">>}, {"data", Participants}]})
	end,
	[{mk_target(PConn, "manager"), [Msg]}];

process_management_message(PConn, Participant, <<"GADGET_LIST">>, _, DBPoolId) ->
	AllGadgets = models:all_gadgets(DBPoolId),
	ParticipantNamesMap = lists:foldl(
		fun (Gadget, Acc) ->
			case lists:keymember(Gadget#gadget.by_user_id, 1, Acc) of
				true ->
					Acc;
				false ->
					case models:get_participant({user_id, Gadget#gadget.by_user_id}, DBPoolId) of
						undefined -> [{Gadget#gadget.by_user_id, <<"?">>}|Acc];
						P -> [{Gadget#gadget.by_user_id, P#participant.name}|Acc]
					end
			end
		end,
		[{Participant#participant.user_id, Participant#participant.name}], AllGadgets),
	GadgetList = lists:map(fun (#gadget{id=Id, by_user_id=ByUserId, title=Title, description=Description, url=Url}) ->
		{obj, [{"id", Id}, {"uploaded_by", element(2, lists:keyfind(ByUserId, 1, ParticipantNamesMap))}, {"name", Title}, {"descr", Description}, {"url", Url}]} end, AllGadgets),
	?Log(normal, "[~s/~p@manager] Sending Gadget list", [Participant#participant.name, PConn#pconn.id]),
	[{mk_target(PConn, "manager"), [mk_message(<<"GADGET_LIST">>, GadgetList)]}];

process_management_message(PConn, Participant, <<"WAVE_LIST">>, _, DBPoolId) ->
	?Log(normal, "[~s/~p@manager] Sending Wave list", [Participant#participant.name, PConn#pconn.id]),
	WaveList = lists:foldl(
		fun (Wavelet, Acc) ->
			WaveId = Wavelet#wavelet.wave_id,
			SerialWavelet = models:serialize_wavelet(Wavelet, DBPoolId),
			case lists:keyfind(WaveId, 1, Acc) of
				{_, {obj, Wavelets}} -> lists:keyreplace(WaveId, 1, Acc, {WaveId, {obj, [{Wavelet#wavelet.id, SerialWavelet}|Wavelets]}});
				false -> [{WaveId, {obj, [{Wavelet#wavelet.id, SerialWavelet}]}}|Acc]
			end
		end,
		[], models:get_participant_wavelets({participant, Participant}, DBPoolId)),
	[{mk_target(PConn, "manager"), [mk_message(<<"WAVE_LIST">>, {obj, WaveList})]}];

process_management_message(PConn, Participant, <<"WAVELET_LIST">>, Prop, DBPoolId) ->
	WaveId = getattr(Prop, "waveId", <<>>),
	Msg = case models:wave_exists({wave_id, WaveId}, DBPoolId) of
		false ->
			?Log(error, "[~s/~p@manager] Cannot list Wavelets; Wave '~s' not found", [Participant#participant.name, PConn#pconn.id, WaveId]),
			mk_error(<<"WAVE_NOT_FOUND">>, io_lib:format("A Wave with id '~s' does not exist", [WaveId]));
		true ->
			?Log(normal, "[~s/~p@manager] Sending Wavelet list for Wave '~s'", [Participant#participant.name, PConn#pconn.id, WaveId]),
			WaveletList = lists:map(
				fun (Wavelet) ->
					{Wavelet#wavelet.id, models:serialize_wavelet(Wavelet, DBPoolId)}
				end, models:get_wavelets({participant, Participant, wave_id, WaveId}, DBPoolId)),
			mk_message(<<"WAVELET_LIST">>, {obj, [{"waveId", WaveId}, {"wavelets", {obj, WaveletList}}]})
	end,
	[{mk_target(PConn, "manager"), [Msg]}];

process_management_message(PConn, Participant, <<"WAVELET_CREATE">>, Prop, DBPoolId) ->
	WaveId = getattr(Prop, "waveId", <<>>),
	Wavelet = models:create_wavelet(WaveId, Participant, getattr(Prop, "title", <<>>), DBPoolId),
	if
		WaveId == <<>> -> ?Log(normal, "[~s/~p@manager] Created Wave '~s'", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.wave_id]);
		true -> ok
	end,
	Msg = case Wavelet of
		{error, wave_not_found} ->
			?Log(error, "[~s/~p@manager] Cannot create Wavelet; Wave '~s' not found", [Participant#participant.name, PConn#pconn.id, WaveId]),
			mk_error(<<"WAVE_NOT_FOUND">>, io_lib:format("A Wave with id '~s' does not exist", [WaveId]));
		#wavelet{id=WaveletId, wave_id=CreatedWaveId} ->
			?Log(normal, "[~s/~p@manager] Created Wavelet '~s'", [Participant#participant.name, PConn#pconn.id, WaveletId]),
			mk_message(<<"WAVELET_CREATED">>, {obj, [{"waveId", CreatedWaveId}, {"waveletId", WaveletId}]})
	end,
	[{mk_target(PConn, "manager"), [Msg]}];

process_management_message(PConn, Participant, Type, _, _) ->
	?Log(error, "[~s/~p@manager] Unknown message type: ~s", [Participant#participant.name, PConn#pconn.id, Type]),
	[{mk_target(PConn, "manager"), [mk_error(<<"UNKNOWN_MESSAGE">>, io_lib:format("Type '~s' not recognised", [Type]))]}].

%% Wavelet messages

process_wavelet_message(PConn, Participant, Wavelet, <<"WAVELET_OPEN">>, _, DBPoolId) ->
	?Log(error, "[~s/~p@~s] Opening wavelet", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.wave_id]),
	models:add_wavelet_to_pconn({wavelet, Wavelet, pconn, PConn}, DBPoolId),
	[{mk_target(PConn, Wavelet#wavelet.id), [mk_message(<<"WAVELET_OPEN">>, {obj, [{"wavelet", models:serialize_wavelet(Wavelet, DBPoolId)}, {"blips", models:serialize_wavelet_blips(Wavelet, DBPoolId)}]})]}];

process_wavelet_message(PConn, Participant, Wavelet, <<"WAVELET_CLOSE">>, _, DBPoolId) ->
	models:remove_wavelet_from_pconn({wavelet, Wavelet, pconn, PConn}, DBPoolId),
	?Log(error, "[~s/~p@~s] Connection to wavelet closed (by client)", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.wave_id]),
	[]; % No reply

process_wavelet_message(PConn, Participant, Wavelet, <<"OPERATION_MESSAGE_BUNDLE">>, Prop, DBPoolId) ->
	Version = getattr(Prop, "version", 0),
	% Unserialize operations
	Ops = lists:map(fun operations:op_from_json/1, getattr(Prop, "operations", [])),
	% Get relevant deltas
	Deltas = models:get_wavelet_deltas({wavelet, Wavelet, version, Version}, DBPoolId),
	% Transform
	TrOps = lists:foldl(
		fun (#delta{ops=DeltaOps}, AccOps) ->
			element(2, operations:transform(DeltaOps, AccOps))
		end,
		Ops, Deltas),
	% Check for no-go's
	TrOpsChecked = lists:filter(
		fun (Op) ->
			if
				(Op#operation.type == wavelet_remove_participant) and (Op#operation.property /= Participant#participant.id) ->
					?Log(error, "[~s/~p@~s] Participant tried to remove '~s'", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.wave_id, Op#operation.property]),
					false;
				true -> true
			end
		end,
		TrOps),
	% Process participant add/remove
	{Added, MessagesForRemoved} = lists:foldl(
		fun (#operation{type=OpType, property=PId}, {AccAdded, AccMessagesForRemoved}) ->
			if
				OpType == wavelet_remove_participant ->
					Msgs = broadcast_manager_message(Wavelet, <<"WAVELET_REMOVE_PARTICIPANT">>, {obj, [{"id", PId}]}, PConn, false, DBPoolId),
					models:remove_wavelet_from_pconn({wavelet, Wavelet, pconn, PConn}, DBPoolId),
					{AccAdded, AccMessagesForRemoved ++ Msgs};
				OpType == wavelet_add_participant ->
					{[PId|AccAdded], AccMessagesForRemoved};
				true ->
					{AccAdded, AccMessagesForRemoved}
			end
		end,
		{[], []}, TrOpsChecked),
	% Apply and raise version
	{NewBlips, TrOpsUpdated} = models:wavelet_apply_operations(Wavelet, TrOpsChecked, Participant, DBPoolId),
	% Create manager messages for added participants
	MessagesForAdded = lists:foldl(fun (PId, Acc) -> Acc ++ broadcast_manager_message(Wavelet, <<"WAVELET_ADD_PARTICIPANT">>, {obj, [{"id", PId}]}, PConn, true, DBPoolId) end, [], Added),
	% Store delta
	Delta = models:create_delta(TrOpsUpdated, Wavelet#wavelet.id, Wavelet#wavelet.version+1, DBPoolId),
	% Create tentative checksums
	Blipsums = models:get_wavelet_blipsums({wavelet, Wavelet}, DBPoolId),
	% Respond
	MessagesForBundle = broadcast_message(
		Wavelet,
		<<"OPERATION_MESSAGE_BUNDLE">>,
		{obj, [
			{"version", Wavelet#wavelet.version+1},
			{"operations", lists:map(fun operations:op_to_json/1, TrOpsUpdated)},
			{"blipsums", {obj, Blipsums}},
			{"timestamp", models:datetime_to_ms(Delta#delta.timestamp)},
			{"contributor", Participant#participant.id}
		]},
		PConn,
		DBPoolId
	),
	AckMessage = {mk_target(PConn, Wavelet#wavelet.id), [mk_message(
		<<"OPERATION_MESSAGE_BUNDLE_ACK">>,
		{obj, [
			{"version", Wavelet#wavelet.version+1},
			{"blipsums", {obj, Blipsums}},
			{"timestamp", models:datetime_to_ms(Delta#delta.timestamp)},
			{"newblips", {obj, NewBlips}}
		]}
	)]},
	?Log(normal, "[~s/~p@~s] Processed delta #~b -> v~b", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.wave_id, Version, Wavelet#wavelet.version+1]),
	case models:get_wavelet_participant_count({wavelet, Wavelet}, DBPoolId) of
		0 -> % Wavelet has no participants -> kill it
			if
				Wavelet#wavelet.is_root == 1 -> % Oh my god, you killed the root Wavelet! You bastard!
					?Log(normal, "[~s/~p@~s] Wave got killed", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.wave_id]),
					models:delete_wave({wave_id, Wavelet#wavelet.wave_id}, DBPoolId);
				true -> % Ok, another one bites the dust
					?Log(normal, "[~s/~p@~s] Wavelet got killed", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.id]),
					models:delete_wavelet({wavelet, Wavelet}, DBPoolId)
			end;
		_ ->
			ok
	end,
	[AckMessage|MessagesForRemoved ++ MessagesForAdded ++ MessagesForBundle];

process_wavelet_message(PConn, Participant, Wavelet, Type, _, _) ->
	?Log(error, "[~s/~p@~s] Unknown message type: ~s", [Participant#participant.name, PConn#pconn.id, Wavelet#wavelet.id, Type]),
	[{mk_target(PConn, Wavelet#wavelet.id), [mk_error(<<"UNKNOWN_MESSAGE">>, io_lib:format("Type '~s' not recognised", [Type]))]}].

%% Message synthesis

broadcast_message(Wavelet, Type, Property, PConn, DBPoolId) ->
	Targets = models:get_broadcast_targets({wavelet, Wavelet, except_pconn, PConn, target_spec, connected}, DBPoolId),
	Msg = mk_message(Type, Property),
	lists:map(fun (Target) -> {mk_target(Target, Wavelet#wavelet.id), [Msg]} end, Targets).

broadcast_manager_message(Wavelet, Type, Property, PConn, OnlyUnconnected, DBPoolId) ->
	TargetSpec = case OnlyUnconnected of true -> unconnected; false -> both end,
	Targets = models:get_broadcast_targets({wavelet, Wavelet, except_pconn, PConn, target_spec, TargetSpec}, DBPoolId),
	Msg = case Property of
		{obj, PropList} -> mk_message(Type, {obj, [{"waveId", Wavelet#wavelet.wave_id},{"waveletId", list_to_binary(Wavelet#wavelet.id)}|PropList]});
		null -> mk_message(Type, {obj, [{"waveId", Wavelet#wavelet.wave_id},{"waveletId", list_to_binary(Wavelet#wavelet.id)}]})
	end,
	lists:map(fun (Target) -> {mk_target(Target, "manager"), [Msg]} end, Targets).

mk_target(PConn=#pconn{}, WaveletId) ->
	mk_target(PConn#pconn.rx_key, WaveletId);
mk_target(RxKey, WaveletId) when is_list(RxKey) ->
	RxKey ++ "." ++ WaveletId ++ ".waveop".

mk_message(Type, Property) ->
	{obj, [
		{"type", Type},
		{"property", Property}
	]}.

mk_error(Tag, Desc) when is_list(Desc) ->
	mk_error(Tag, list_to_binary(Desc));
mk_error(Tag, Desc) when is_binary(Desc) ->
	mk_message(
		<<"ERROR">>,
		{obj, [
			{"tag", Tag},
			{"desc", Desc}
		]}
	).

mk_login(Participant=#participant{}, PConn=#pconn{}) ->
	mk_message(
		<<"LOGIN">>,
		{obj, [
			{"rx_key", list_to_binary(PConn#pconn.rx_key)},
			{"tx_key", list_to_binary(PConn#pconn.tx_key)},
			{"viewer_id", Participant#participant.id}
		]}
	).

%% Helper functions

combine_results(L, []) ->
	L;
combine_results(L, [{Key, ValueList}|R]) ->
	case lists:keyfind(Key, 1, L) of
		{_, OtherValueList} ->
			combine_results(lists:keyreplace(Key, 1, L, OtherValueList ++ ValueList), R);
		false ->
			combine_results(L ++ [{Key, ValueList}], R)
	end.

getattr({obj, TupleList}, Name, DefaultValue) ->
	getattr(TupleList, Name, DefaultValue);

getattr(TupleList, Name, DefaultValue) ->
	case lists:keyfind(Name, 1, TupleList) of
		{_, Value} -> Value;
		false -> DefaultValue
	end.
