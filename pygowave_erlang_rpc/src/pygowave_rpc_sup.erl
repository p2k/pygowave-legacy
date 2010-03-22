
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

-module(pygowave_rpc_sup).

-behaviour(supervisor).

-export([
	start_link/13,
	start_child/1,
	start_child/2,
	init/1
]).

-export([
	dummy_logger/4,
	dummy_logger/5
]).

-define(SERVER, ?MODULE).
-define(DB_POOL_ID, pygo_sql).

start_link(MySQLHost, MySQLPort, MySQLUser, MySQLPassword, MySQLDatabase,
		StompHost, StompPort, StompUser, StompPassword,
		AccessKeyTimeoutMinutes, ParticipantSearchLength, WaveDomain,
		LogLevel) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [
		MySQLHost, MySQLPort, MySQLUser, MySQLPassword, MySQLDatabase,
		StompHost, StompPort, StompUser, StompPassword,
		AccessKeyTimeoutMinutes, ParticipantSearchLength, WaveDomain,
		LogLevel
	]).

start_child(Mod) ->
	start_child(Mod, []).

start_child(Mod, Args) ->
	{ok, _} = supervisor:start_child(?SERVER,
		{Mod, {Mod, start_link, Args},
		transient, 100, worker, [Mod]}),
	ok.

dummy_logger(_Module, _Line, _Level, _FormatFun) ->
	ok.

dummy_logger(_Module, _Line, _Level, _Format, _Arguments) ->
	ok.

log_level_to_int(Level) ->
	case Level of
		debug -> 0;
		normal -> 1;
		error -> 2;
		_ -> 3
	end.

init([MySQLHost, MySQLPort, MySQLUser, MySQLPassword, MySQLDatabase,
		StompHost, StompPort, StompUser, StompPassword,
		AccessKeyTimeoutMinutes, ParticipantSearchLength, WaveDomain,
		LogLevel]) ->
	
	LogLevelInt = log_level_to_int(LogLevel),
	MyLogger = fun (Module, Line, Level, Format, Arguments) ->
		LevelInt = log_level_to_int(Level),
		if LevelInt >= LogLevelInt ->
			PrintMe = io_lib:format("~w:~b: " ++ Format ++ "~n", [Module, Line] ++ Arguments),
			case Level of
				error -> error_logger:error_msg(PrintMe);
				normal -> error_logger:info_msg(PrintMe);
				debug -> io:format(PrintMe)
			end;
		true -> ok
		end
	end,
	
	{ok, {{one_for_one, 10, 10}, [
		{mysql, {mysql, start_link, [?DB_POOL_ID, MySQLHost, MySQLPort, MySQLUser, MySQLPassword, MySQLDatabase, fun dummy_logger/4, utf8]}, transient, 100, worker, [mysql]},
		{c2s_mp, {c2s_mp, start_link, [?DB_POOL_ID, AccessKeyTimeoutMinutes, ParticipantSearchLength, WaveDomain, fun stomp_client:send/1, MyLogger]}, transient, 100, worker, [c2s_mp]},
		{stomp_client, {stomp_client, start_link, [StompHost, StompPort, StompUser, StompPassword, fun c2s_mp:process/2, MyLogger]}, transient, 100, worker, [stomp_client]}
	]}}.
