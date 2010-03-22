
%%
%% PyGoWave Server - The Python Google Wave Server
%% Copyright 2009-2010 Patrick Schneider <patrick.p2k.schneider@gmail.com>
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

%% Parts of this file are derived from Ian Brown's erlang stomp client
%% spam@hccp.org; http://www.hccp.org/erlang-stomp-client.html

%% -----------------------------------------------------------------------
%% Exports/Module specification
%% -----------------------------------------------------------------------

-module(stomp_client).

-export([
	start_link/5,
	start_link/6,
	start_link/7,
	init/8
]).

-export([
	send/1,
	send/2
]).

%% -----------------------------------------------------------------------
%% Imports
%% -----------------------------------------------------------------------

-import(lists, [filter/2, foldl/3, map/2, foreach/2]).

%% -----------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------

-record(config, {
	log_fun,
	mp_fun
}).

-include("stomp.hrl").

-record(state, {
	config,
	stomp_state
}).

%% -----------------------------------------------------------------------
%% Defines/Macros
%% -----------------------------------------------------------------------

-define(SERVER, ?MODULE).

-define(Log(LogFun, Level, Format, Arguments),
	LogFun(?MODULE, ?LINE, Level, Format, Arguments)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -----------------------------------------------------------------------
%% Lifecycle
%% -----------------------------------------------------------------------

start_link(StompHost, StompPort, StompUser, StompPassword, MPFun) ->
	start_link(undefined, StompHost, StompPort, StompUser, StompPassword, MPFun, undefined).

start_link(StompHost, StompPort, StompUser, StompPassword, MPFun, LogFun) ->
	start_link(undefined, StompHost, StompPort, StompUser, StompPassword, MPFun, LogFun).

start_link(MaybeServerName, StompHost, StompPort, StompUser, StompPassword, MPFun, MaybeLogFun) ->
	ServerName = case MaybeServerName of
		undefined -> ?SERVER;
		_ -> MaybeServerName
	end,
	LogFun = case MaybeLogFun of
		undefined -> fun generic_logger/5;
		_ -> MaybeLogFun
	end,
	case whereis(ServerName) of
		undefined ->
			proc_lib:start_link(?MODULE, init, [self(), ServerName, StompHost, StompPort, StompUser, StompPassword, MPFun, LogFun]);
		Pid ->
			{error, {already_started, Pid}}
	end.

init(Starter, ServerName, StompHost, StompPort, StompUser, StompPassword, MPFun, LogFun) ->
	try register(ServerName, self()) of
		true ->
			case stomp:tcp_connect(StompHost, StompPort, StompUser, StompPassword, [{active, false}]) of
				{ok, StompStateInitial} ->
					case stomp:receive_frames(StompStateInitial, 5000) of
						{error, Reason} ->
							?Log(LogFun, error, "stomp:receive_frames returned an error (~p)!", [Reason]),
							proc_lib:init_ack(Starter, {error, Reason});
						{StompState, Frames} ->
							case Frames of
								[#stomp_frame{command="CONNECTED"}] ->
									ok = gen_tcp:controlling_process(StompState#stomp_state.socket, self()),
									inet:setopts(StompState#stomp_state.socket, [{active, true}]),
									stomp:subscribe(StompState, "wavelet_rpc_singlethread", [{"exchange", "wavelet.topic"}, {"routing_key", "#.#.clientop"}, {"exchange_type", "topic"}]),
									proc_lib:init_ack(Starter, {ok, self()}),
									[{registered_name, RegName}] = erlang:process_info(self(), [registered_name]),
									?Log(LogFun, normal, "Connected; registered as \"~p\".", [RegName]),
									loop(#state{stomp_state=StompState, config=#config{log_fun=LogFun, mp_fun=MPFun}});
								_ ->
									?Log(LogFun, error, "Server response incorrect or timed out!", []),
									proc_lib:init_ack(Starter, {error, server_response_incorrect_or_timed_out})
							end
					end;
				{error, Reason} ->
					?Log(LogFun, error, "Could not connect to server (~p)!", [Reason]),
					proc_lib:init_ack(Starter, {error, Reason})
			end
	catch
		error:_ ->
			proc_lib:init_ack(Starter, {error, {already_started, whereis(ServerName)}})
	end.

%% -----------------------------------------------------------------------
%% Convenience functions which post messages to the worker process
%% -----------------------------------------------------------------------

send(Messages) ->
	send(undefined, Messages).

send(MaybeServerName, Messages) ->
	ServerName = case MaybeServerName of
		undefined -> ?SERVER;
		_ -> MaybeServerName
	end,
	ServerName ! {send, Messages}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generic_logger(Module, Line, _Level, Format, Arguments) ->
	io:format("~w:~b: " ++ Format ++ "~n", [Module, Line] ++ Arguments).


loop(State=#state{config=Config, stomp_state=StompState}) ->
	#config{log_fun=LogFun, mp_fun=MPFun} = Config,
	receive
		{send, Messages} ->
			foreach(fun ({RKey, Msgs}) -> send_messages(StompState, RKey, Msgs, LogFun) end, Messages),
			loop(State);
		{tcp, _, Data} ->
			{NewStompState, Frames} = stomp:get_frames(StompState, Data),
			foreach(
				fun (Frame) ->
					case read_frame(Frame, LogFun) of
						{ok, RKey, DecData} ->
							?Log(LogFun, debug, "Received from ~s: ~s", [RKey, rfc4627:encode(DecData)]),
							MPFun(RKey, DecData);
						_ ->
							skipped
					end
				end,
				Frames
			),
			loop(State#state{stomp_state=NewStompState});
		terminate ->
			ok
	end.

send_messages(StompState, RKey, Msgs, LogFun) ->
	EncMsgs = rfc4627:encode(Msgs),
	?Log(LogFun, debug, "Sending to    ~s: ~s", [RKey, EncMsgs]),
	stomp:send(StompState, RKey, EncMsgs, [{"exchange", "wavelet.direct"}, {"content-type", "application/json"}, {"content-encoding", "utf-8"}]).

read_frame(#stomp_frame{command="MESSAGE", header=Header, body=Body}, LogFun) ->
	Dest = proplists:get_value("destination", Header),
	ContentType = proplists:get_value("content-type", Header),
	ContentEncoding = proplists:get_value("content-encoding", Header),
	if
		ContentEncoding /= "utf-8" ->
			?Log(LogFun, error, "Unknown content-encoding \"~s\", skipping.", [ContentEncoding]),
			{error, wrong_type_or_encoding};
		ContentType /= "application/json" ->
			?Log(LogFun, error, "Unknown content-type \"~s\", skipping.", [ContentType]),
			{error, wrong_type_or_encoding};
		true ->
			case rfc4627:decode(Body) of
				{ok, Data, []} -> {ok, Dest, Data};
				{ok, _, _} -> {error, json_has_remainder};
				{error, Reason} -> {error, {json_error, Reason}}
			end
	end;

read_frame(#stomp_frame{command=Command}, LogFun) ->
	?Log(LogFun, error, "Unknown STOMP frame type \"~s\", skipping.", [Command]),
	{error, unknown_stomp_frame}.
