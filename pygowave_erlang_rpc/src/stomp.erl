
%%
%% Erlang Stomp Module
%% Copyright 2010 Patrick Schneider <patrick.p2k.schneider@gmail.com>
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

%%
%% Parts of this file are derived from Ricky Iacovou's StompBuffer, an
%% optional utility class accompanying Stomper, distributed under the
%% Apache License, Version 2.0; see http://code.google.com/p/stomper/
%%

%% -----------------------------------------------------------------------
%% Exports/Module specification
%% -----------------------------------------------------------------------

-module(stomp).

-export([
	tcp_connect/4, tcp_connect/5, tcp_connect/6,
	
	connect/3, connect/4,
	disconnect/1, disconnect/2,
	send/3, send/4,
	subscribe/2, subscribe/3,
	unsubscribe/2, unsubscribe/3,
	ack/2, ack/3,
	begin_transaction/2, begin_transaction/3,
	abort_transaction/2, abort_transaction/3,
	commit_transaction/2, commit_transaction/3,
	send_frame/2,
	
	receive_frames/1, receive_frames/2,
	on_frame/2,
	
	get_frames/1, get_frames/2
]).

%% -----------------------------------------------------------------------
%% Imports
%% -----------------------------------------------------------------------

-import(lists, [filter/2, foldl/3, map/2, foreach/2]).
-import(erlang, [max/2]).

%% -----------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------

-include("stomp.hrl").

%% -----------------------------------------------------------------------
%% Defines
%% -----------------------------------------------------------------------

-define(VALID_COMMANDS, [<<"ABORT">>, <<"ACK">>, <<"BEGIN">>,
	<<"COMMIT">>, <<"CONNECT">>, <<"DISCONNECT">>, <<"CONNECTED">>,
	<<"MESSAGE">>, <<"SEND">>, <<"SUBSCRIBE">>, <<"UNSUBSCRIBE">>,
	<<"RECEIPT">>, <<"ERROR">>]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -----------------------------------------------------------------------
%% Connecting
%% -----------------------------------------------------------------------

%% Connect to a Stomp host via TCP/IP.
%% Sends a CONNECT frame on successful connection.
%% Returns either {ok, StompState} or {error, Reason}
tcp_connect(StompHost, StompPort, StompUser, StompPassword) ->
	tcp_connect(StompHost, StompPort, StompUser, StompPassword, undefined, []).

tcp_connect(StompHost, StompPort, StompUser, StompPassword, TcpOptions) ->
	tcp_connect(StompHost, StompPort, StompUser, StompPassword, TcpOptions, []).

tcp_connect(StompHost, undefined, StompUser, StompPassword, TcpOptions, HeaderList) ->
	tcp_connect(StompHost, 61613, StompUser, StompPassword, TcpOptions, HeaderList);

tcp_connect(StompHost, StompPort, StompUser, StompPassword, undefined, HeaderList) ->
	tcp_connect(StompHost, StompPort, StompUser, StompPassword, [{active, false}], HeaderList);

tcp_connect(StompHost, StompPort, StompUser, StompPassword, TcpOptions, HeaderList) ->
	case gen_tcp:connect(StompHost, StompPort, TcpOptions) of
		{ok, Sock} ->
			InitialState=#stomp_state{socket = Sock, buffer = <<>>},
			connect(InitialState, StompUser, StompPassword, HeaderList),
			{ok, InitialState};
		{error, Reason} ->
			{error, Reason}
	end.

%% -----------------------------------------------------------------------
%% Frame synthesis and sending
%% -----------------------------------------------------------------------

%% Sends a CONNECT frame
connect(StompState, StompUser, StompPassword) ->
	connect(StompState, StompUser, StompPassword, []).

connect(StompState, StompUser, StompPassword, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="CONNECT", header=setattr(setattr(HeaderList, "login", StompUser), "passcode", StompPassword)}).

%% Sends a DISCONNECT frame; also disconnects the TCP/IP socket
disconnect(StompState) ->
	disconnect(StompState, []).

disconnect(StompState=#stomp_state{socket=Sock}, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="DISCONNECT", header=HeaderList}),
	gen_tcp:close(Sock).

%% Sends a SEND frame
send(StompState, Destination, Body) ->
	send(StompState, Destination, Body).

send(StompState, Destination, Body, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="SEND", header=setattr(HeaderList, "destination", Destination), body=Body}).

%% Sends a SUBSCRIBE frame
subscribe(StompState, Destination) ->
	subscribe(StompState, Destination, []).

subscribe(StompState, Destination, HeaderList) ->
	HeaderListChecked = case proplists:is_defined("ack", HeaderList) of
		true -> HeaderList;
		false -> HeaderList ++ [{"ack", "auto"}]
	end,
	send_frame(StompState, #stomp_frame{command="SUBSCRIBE", header=setattr(HeaderListChecked, "destination", Destination)}).

%% Sends an UNSUBSCRIBE frame
unsubscribe(StompState, Destination) ->
	unsubscribe(StompState, Destination, []).

unsubscribe(StompState, Destination, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="UNSUBSCRIBE", header=setattr(HeaderList, "destination", Destination)}).

%% Sends an ACK frame
ack(StompState, MessageId) ->
	ack(StompState, MessageId, []).

ack(StompState, MessageId, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="ACK", header=setattr(HeaderList, "message-id", MessageId)}).

%% Sends a BEGIN frame
begin_transaction(StompState, TransactionId) ->
	begin_transaction(StompState, TransactionId, []).

begin_transaction(StompState, TransactionId, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="BEGIN", header=setattr(HeaderList, "transaction", TransactionId)}).

%% Sends a ABORT frame
abort_transaction(StompState, TransactionId) ->
	begin_transaction(StompState, TransactionId, []).

abort_transaction(StompState, TransactionId, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="ABORT", header=setattr(HeaderList, "transaction", TransactionId)}).

%% Sends a COMMIT frame
commit_transaction(StompState, TransactionId) ->
	begin_transaction(StompState, TransactionId, []).

commit_transaction(StompState, TransactionId, HeaderList) ->
	send_frame(StompState, #stomp_frame{command="COMMIT", header=setattr(HeaderList, "transaction", TransactionId)}).


%% Sends an arbitrary frame
send_frame(#stomp_state{socket=Sock}, Frame=#stomp_frame{}) ->
	gen_tcp:send(Sock, encode_frame(Frame)).

%% -----------------------------------------------------------------------
%% Frame analysis and retrieval
%% -----------------------------------------------------------------------

%% Wait actively until frames arrive on the TCP connection
%% and retrieve all frames. Returns {NewState, FramesList} or
%% {error, Reason}.
receive_frames(State) ->
	receive_frames(State, infinity).

receive_frames(State=#stomp_state{socket=Sock}, Timeout) ->
	case gen_tcp:recv(Sock, 0, Timeout) of
		{ok, Data} when is_list(Data) -> get_frames(State, list_to_binary(Data));
		{error, timeout} -> {State, []};
		{error, Reason} -> {error, Reason}
	end.


on_frame(State, F) ->
	{NewState, FramesList} = receive_frames(State),
	foreach(F, FramesList),
	on_frame(NewState, F).


%% Tries to get as many frames as possible off the internal buffer.
%% Returns {NewState, FramesList}.
get_frames(State=#stomp_state{buffer=Buffer}) ->
	{NewBuffer, FramesList} = get_frames_recursive(Buffer, []),
	{State#stomp_state{buffer=NewBuffer}, FramesList}.

%% Appends Data to the internal buffer and then tries to get
%% as many frames as possible.
%% Returns {NewState, FramesList}.
get_frames(State, Data) when is_list(Data) ->
	get_frames(State, list_to_binary(Data));
get_frames(State=#stomp_state{buffer=OldBuffer}, Data) ->
	get_frames(State#stomp_state{buffer = <<OldBuffer/bytes, Data/bytes>>}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tail recursive function to get as many frames as possible off the buffer
get_frames_recursive(Buffer, Frames) ->
	case get_frame(Buffer) of
		{NewBuffer, none} ->
			{NewBuffer, Frames};
		{NewBuffer, Frame} ->
			get_frames_recursive(NewBuffer, Frames ++ [Frame])
	end.

get_frame(<<>>) ->
	{<<>>, none};

%% -----------------------------------------------------------------------
%% Main function
%% -----------------------------------------------------------------------

%% Tries to get one frame off the buffer. Does sanity checks and tries to
%% fix the buffer on corruption. Also honours a possible content-length
%% specification in the frame. Frame delimiters can be \0\n or just \0.
get_frame(Buffer) ->
	% Buffer sanity check
	IsSane = case re:run(Buffer, <<"^([^\n]*)\n">>, [{capture, all, binary}]) of
		{match, [_, Command]} -> lists:any(fun (X) -> X == Command end, ?VALID_COMMANDS);
		_ -> false
	end,
	case IsSane of
		true ->
			% Look for content-length
			HeaderEnd = case re:run(Buffer, <<"\n\n">>) of
				{match, [{HeaderEndIndex, _}]} -> HeaderEndIndex;
				_ -> undefined
			end,
			ContentLength = case HeaderEnd of
				undefined -> undefined;
				_ ->
					case re:run(Buffer, <<"\ncontent-length\\s*:\\s*(\\d+)\\s*\n">>) of
						{match, [_, {ContentLengthIndex, Length}]} when ContentLengthIndex < HeaderEnd ->
							<<_:ContentLengthIndex/bytes, ContentLengthBin:Length/bytes, _/bytes>> = Buffer,
							case catch list_to_integer(binary_to_list(ContentLengthBin)) of
								{'EXIT', _} ->
									undefined;
								CL -> CL
							end;
						_ -> undefined
					end
			end,
			FrameWithCl = case ContentLength of
				undefined -> undefined;
				_ when byte_size(Buffer) >= (ContentLength+HeaderEnd+2) ->
					FrameSizeWithCl = ContentLength+HeaderEnd+2,
					<<RawFrameWithCl:FrameSizeWithCl/bytes, RestWithDelim/bytes>> = Buffer,
					RestWithCl = case RestWithDelim of
						<<"\0\n", X/bytes>> -> X;
						<<"\0", X/bytes>> -> X;
						X -> X
					end,
					{RestWithCl, decode_frame(RawFrameWithCl, HeaderEnd)};
				_ ->
					{Buffer, none} % Not enough data, wait for more
			end,
			case FrameWithCl of
				undefined when (HeaderEnd /= undefined) -> % No content-length, look for \0\n
					case re:run(Buffer, <<"\\0\n">>) of
						{match, [{DelimIndex, _}]} ->
							<<RawFrame:DelimIndex/bytes, "\0\n", Rest/bytes>> = Buffer,
							{Rest, decode_frame(RawFrame, HeaderEnd)};
						_ -> % Look for alternative delimiter \0
							case re:run(Buffer, <<"\\0">>) of
								{match, [{DelimIndex, _}]} ->
									<<RawFrame:DelimIndex/bytes, "\0", Rest/bytes>> = Buffer,
									{Rest, decode_frame(RawFrame, HeaderEnd)};
								_ ->
									{Buffer, none} % Not enough data, wait for more
							end
					end;
				undefined ->
					{Buffer, none};
				_ ->
					FrameWithCl
			end;
		
		false -> % Try to repair by first looking for a frame delimiter; otherwise trashes the buffer
			case re:run(Buffer, <<"\\0\n">>) of
				{match, [{SyncPos, _}]} ->
					SyncPosWithDelim = SyncPos+2,
					<<_:SyncPosWithDelim/bytes, NewBuffer/bytes>> = Buffer,
					get_frame(NewBuffer);
				_ ->
					case re:run(Buffer, <<"\\0">>) of
						{match, [{SyncPos, _}]} ->
							SyncPosWithDelim = SyncPos+1,
							<<_:SyncPosWithDelim/bytes, NewBuffer/bytes>> = Buffer,
							get_frame(NewBuffer);
						_ ->
							{<<>>, none}
					end
			end
	end.

%% -----------------------------------------------------------------------
%% Functions for decoding/encoding of frames
%% -----------------------------------------------------------------------

decode_frame(Data, HeaderEnd) ->
	<<HeaderWithCommand:HeaderEnd/bytes, "\n\n", Body/bytes>> = Data,
	[Command|Header] = re:split(HeaderWithCommand, <<"\n">>),
	DecodedHeader = filter(fun (X) -> X /= undefined end, map(fun decode_header_line/1, Header)),
	#stomp_frame{command=binary_to_list(Command), header=DecodedHeader, body=Body}.


decode_header_line(Line) ->
	case re:run(Line, <<"([^:]*):(.*)">>, [{capture, all, binary}]) of
		{match, [_, Name, Value]} ->
			case string:strip(binary_to_list(Name)) of
				"passcode" ->
					{"passcode", Value};
				"login" ->
					{"login", Value};
				StrName ->
					{StrName, string:strip(binary_to_list(Value))}
			end;
		_ -> undefined
	end.


encode_frame(Frame=#stomp_frame{body=undefined}) ->
	encode_frame(Frame#stomp_frame{body = <<>>});

encode_frame(Frame=#stomp_frame{body=Body}) when is_list(Body) ->
	encode_frame(Frame#stomp_frame{body=list_to_binary(Body)});

encode_frame(#stomp_frame{command=Command, header=OrigHeader, body=Body}) ->
	BinCommand = list_to_binary(Command),
	% Check for null-bytes in Body or presence of content-length
	Header = case re:run(Body, <<"\\0">>) of
		{match, _} ->
			setattr(OrigHeader, "content-length", integer_to_list(byte_size(Body)));
		_ ->
			case proplists:is_defined("content-length", OrigHeader) of
				false -> OrigHeader;
				true -> setattr(OrigHeader, "content-length", integer_to_list(byte_size(Body)))
			end
	end,
	EncodedHeader = foldl(
		fun (HeaderLine, Acc) ->
			EncodedHeaderLine = encode_header_line(HeaderLine),
			<<Acc/bytes, "\n", EncodedHeaderLine/bytes>>
		end,
		<<>>,
		Header
	),
	<<BinCommand/bytes, EncodedHeader/bytes, "\n\n", Body/bytes, "\0\n">>.


encode_header_line({Name, Value}) when is_list(Name) and is_list(Value) ->
	encode_header_line({list_to_binary(Name), list_to_binary(Value)});

encode_header_line({Name, Value}) when is_binary(Name) and is_binary(Value) ->
	<<Name/bytes, ": ", Value/bytes>>;

encode_header_line({Name, Value}) when is_atom(Name) ->
	encode_header_line({list_to_binary(atom_to_list(Name)), Value});

encode_header_line({Name, Value}) when is_list(Name) ->
	encode_header_line({list_to_binary(Name), Value});

encode_header_line({Name, Value}) when is_atom(Value) ->
	encode_header_line({Name, list_to_binary(atom_to_list(Value))});

encode_header_line({Name, Value}) when is_list(Value) ->
	encode_header_line({Name, list_to_binary(Value)});

encode_header_line({<<"passcode">>, Value}) ->
	<<"passcode:", Value/bytes>>;

encode_header_line({<<"login">>, Value}) ->
	<<"login:", Value/bytes>>.

%% -----------------------------------------------------------------------
%% Helper function
%% -----------------------------------------------------------------------

setattr(TupleList, Name, Value) ->
	lists:keystore(Name, 1, TupleList, {Name, Value}).
