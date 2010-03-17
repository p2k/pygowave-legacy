
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

-module(stomp_client).
-export([
	start/0,
	msg_sender/1,
	msg_receiver/3,
	run_purge/1
	]).

%% Defines
-include("settings.hrl").

%% Logging
log(Module, Line, Level, Format, Arguments) ->
	if
		(Module == mysql) or (Module == mysql_conn) or (Module == mysql_auth) ->
			ok;
		true ->
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
			end
	end.

-define(Log(Level, Format, Arguments),
	log(?MODULE, ?LINE, Level, Format, Arguments)).

log(Module, Line, Level, FormatFun) ->
	{Format, Arguments} = FormatFun(),
	log(Module, Line, Level, Format, Arguments).

start() ->
	%% Connect to database
	case mysql:start_link(pygo_sql, "localhost", undefined, "pygowave", "pygowave", "pygowave", fun log/4, utf8) of
		{ok, _} -> ok;
		{error, {already_started, _}} -> ok
	end,
	%% Connect to message queue
	Conn = stomp:connect("localhost", 61613, "pygowave_server", "pygowave_server"),
	%% Subscribe to queue
	stomp:subscribe("wavelet_rpc_singlethread", Conn, [{"exchange", "wavelet.topic"}, {"routing_key", "#.#.clientop"}, {"exchange_type", "topic"}]),
	%% Spawn Message Processor
	MP = spawn(c2s_mp, start, [pygo_sql]),
	%% Spawn Message Sender
	S = spawn(?MODULE, msg_sender, [Conn]),
	%% Initial purge & setup purge timer (every 10 minutes)
	MP ! purge,
	{ok, _PurgeTimer} = timer:apply_interval(600000, ?MODULE, run_purge, [MP]),
	%% Enter message loop
	stomp:on_message(fun (Frame) -> msg_receiver(MP, S, Frame) end, Conn).

run_purge(MP) ->
	MP ! purge.

%% Receiver
msg_receiver(MP, S, Frame) ->
	case read_frame(Frame) of
		{ok, RKey, Data} ->
			?Log(debug, "Received from ~s: ~s", [RKey, rfc4627:encode(Data)]),
			MP ! {process, S, RKey, Data}, ok;
		{error, _} -> error
	end.

%% Sender
msg_sender(Conn) ->
	receive
		Msgs -> msg_sender(Conn, Msgs)
	end,
	msg_sender(Conn).

msg_sender(_, []) ->
	ok;
msg_sender(Conn, [{RKey, Msgs}|T]) ->
	EncMsgs = rfc4627:encode(Msgs),
	?Log(debug, "Sending to   ~s: ~s", [RKey, EncMsgs]),
	stomp:send(
		Conn,
		RKey,
		[{"exchange","wavelet.direct"},{"content-type","application/json"},{"content-encoding","utf-8"}],
		EncMsgs
	),
	msg_sender(Conn, T).

%% Read and process a STOMP frame
read_frame([{type, Type}, {headers, Headers}, {body, Body}]) ->
	read_frame(Type, Headers, Body);
read_frame(_) ->
	?Log(error, "Got bogus data from STOMP library!", []),
	{error, bogus_data}.

read_frame("ESSAGE", Headers, Body) ->
	read_frame("MESSAGE", Headers, Body);

read_frame("MESSAGE", Headers, Body) ->
	case conv_headers(Headers) of
		[{destination, Dest},{message_id, _},{content_type, "application/json"},{content_encoding, "utf-8"}] ->
			parse_message(Dest, Body);
		[{destination, _},{message_id, _},{content_type, CType},{content_encoding, CEnc}] ->
			?Log(error, "Unknown content-type \"~s\" and/or encoding \"~s\", skipping.", [CType, CEnc]),
			{error, wrong_type_or_encoding}
	end;
read_frame(Type, _, _) ->
	?Log(error, "Unknown STOMP frame type \"~s\", skipping.", [Type]),
	{error, unknown_stomp_frame}.

%% Parse the headers
conv_headers(Headers) ->
	conv_headers(Headers, [{destination, ""},{message_id, ""},{content_type, ""},{content_encoding, ""}]).

conv_headers([], CvdHeaders) ->
	CvdHeaders;
conv_headers([H|T], [{destination, Dest},{message_id, MsgId},{content_type, CType},{content_encoding, CEnc}]) ->
	case H of
		{"destination", Val} -> conv_headers(T, [{destination, Val},{message_id, MsgId},{content_type, CType},{content_encoding, CEnc}]);
		{"message-id", Val} -> conv_headers(T, [{destination, Dest},{message_id, Val},{content_type, CType},{content_encoding, CEnc}]);
		{"content-type", Val} -> conv_headers(T, [{destination, Dest},{message_id, MsgId},{content_type, Val},{content_encoding, CEnc}]);
		{"content-encoding", Val} -> conv_headers(T, [{destination, Dest},{message_id, MsgId},{content_type, CType},{content_encoding, Val}]);
		_ -> conv_headers(T, [{destination, Dest},{message_id, MsgId},{content_type, CType},{content_encoding, CEnc}])
	end.

%% Parse a message
parse_message(Dest, Body) ->
	case rfc4627:decode(Body) of
		{ok, Data, []} -> {ok, Dest, Data};
		{ok, _, _} -> {error, json_has_remainder};
		{error, Reason} -> {error, {json_error, Reason}}
	end.
