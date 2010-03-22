
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

-module(pygowave_rpc).

-behavior(application).

-export([
	start/2,
	shutdown/0,
	stop/1
]).

start(normal, []) ->
	{ok, Product} = application:get_key(id),
	{ok, Version} = application:get_key(vsn),
	
	% Print banner
	io:format("=> ~s v~s starting <=~n", [Product, Version]),
	
	% Write a pidfile if requested
	PidFile = case init:get_argument(pidfile) of
		{ok, [[FileName]]} ->
			ok = file:write_file(FileName, os:getpid()),
			FileName;
		_ ->
			undefined
	end,
	
	% Get configuration
	{ok, MySQLHost} = application:get_env(mysql_hostname),
	{ok, MySQLPort} = application:get_env(mysql_port),
	{ok, MySQLUser} = application:get_env(mysql_user),
	{ok, MySQLPassword} = application:get_env(mysql_password),
	{ok, MySQLDatabase} = application:get_env(mysql_database),
	
	{ok, StompHost} = application:get_env(stomp_hostname),
	{ok, StompPort} = application:get_env(stomp_port),
	{ok, StompUser} = application:get_env(stomp_user),
	{ok, StompPassword} = application:get_env(stomp_password),
	
	{ok, AccessKeyTimeoutMinutes} = application:get_env(access_key_timeout_minutes),
	{ok, ParticipantSearchLengt} = application:get_env(participant_search_length),
	{ok, WaveDomain} = application:get_env(wave_domain),
	{ok, LogLevel} = application:get_env(log_level),
	
	% Start supervisor
	{ok, SupPid} = pygowave_rpc_sup:start_link(
		MySQLHost, MySQLPort, MySQLUser, MySQLPassword, MySQLDatabase,
		StompHost, StompPort, StompUser, StompPassword,
		AccessKeyTimeoutMinutes, ParticipantSearchLengt, WaveDomain,
		LogLevel
	),
	
	% Setup connection purging
	c2s_mp:purge_connections(),
	{ok, PurgeTimer} = timer:apply_interval(AccessKeyTimeoutMinutes * 30000, c2s_mp, purge_connections, []),
	
	{ok, SupPid, [PurgeTimer, PidFile]}.

shutdown() ->
	application:stop(pygowave_rpc).

stop([PurgeTimer, PidFile]) ->
	io:format("=> Stopping <=~n"),
	timer:cancel(PurgeTimer),
	case PidFile of
		undefined -> ok;
		_ -> file:delete(PidFile)
	end.
