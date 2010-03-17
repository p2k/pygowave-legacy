%% Module for client support of the STOMP messaging protocol (http://stomp.codehaus.org/Protocol). 
%% Version 0.1
%% Authored by Ian Brown (spam@hccp.org)
%% Documentation can be found at http://www.hccp.org/erlang-stomp-client.html
%% This was an experiment to get a feel for the Erlang language, and provide simple client access to STOMP suppurting message brokers.  
%% Please feel free to use and re-distribute as you see fit. Comments, improvements and questions welcome. 

-module (stomp).
-export ([connect/4]). %% "You sunk my scrabbleship!"
-export ([disconnect/1]).
-export ([subscribe/2]).
-export ([subscribe/3]).
-export ([unsubscribe/2]).
-export ([get_messages/1]).
-export ([get_message_id/1]).
-export ([ack/2]).
-export ([ack/3]).
-export ([send/4]).
-export ([begin_transaction/2]).
-export ([commit_transaction/2]).
-export ([abort_transaction/2]).
-export ([on_message/2]).


%% Example:	Conn = stomp:connect("localhost", 61613, "", "").

connect (Host, PortNo, Login, Passcode)  ->
	Message=lists:append(["CONNECT", "\nlogin: ", Login, "\npasscode: ", Passcode, "\n\n", [0]]),
	{ok,Sock}=gen_tcp:connect(Host,PortNo,[{active, false}]),
	gen_tcp:send(Sock,Message),
	{ok, Response}=gen_tcp:recv(Sock, 0),
	[{type, Type}, _, _, _]=get_message(Response), %%UGLY!
	case (Type) of 
		"CONNECTED" -> Sock;
		_-> throw("Error occured during connection attempt.")
	end,
	Sock.


%% Example: stomp:subscribe("/queue/foobar", Conn).

subscribe (Destination, Connection) ->
	subscribe (Destination, Connection, [{"ack","auto"}]),
	ok.

%%  Example: stomp:subscribe("/queue/foobar", Conn, [{"ack", "client"}]).
%%  Example: stomp:subscribe("/queue/foobar", Conn, [{"ack", "client"}, {"activemq.prefetchSize", 1}]).


subscribe (Destination, Connection, Options) ->
	Message=lists:append(["SUBSCRIBE", "\ndestination: ", Destination, concatenate_options(Options),"\n\n", [0]]),
	gen_tcp:send(Connection,Message),
	ok.
	

%% Example: stomp:unsubscribe("/queue/foobar", Conn).
	
unsubscribe (Destination, Connection) ->
	Message=lists:append(["UNSUBSCRIBE", "\ndestination: ", Destination, "\n\n", [0]]),
	gen_tcp:send(Connection,Message),
	ok.	
	
%% Example: stomp:disconnect(Conn).
	
disconnect (Connection) ->
	Message=lists:append(["DISCONNECT", "\n\n", [0]]),
	gen_tcp:send(Connection,Message),
	gen_tcp:close(Connection),
	ok.	

%% Example: stomp:get_message_id(Message).

get_message_id ([_, {headers, Headers}, _]) ->
	get_message_id (Headers);
get_message_id ([H|T]) ->
	case (H) of
		{"message-id", MessageId}->MessageId;
		_ -> get_message_id(T)
	end;
get_message_id ([])	->
	throw("No header with name of 'message-id' was found.").
	
	
	
	
	
%% Example: stomp:ack(Conn, Message).
%% Example: stomp:ack(Conn, stomp:get_message_id(Message)).
%% Example: stomp:ack(Conn, "ID:phosphorus-63844-1247442885553-3:1:1:1:1").

ack (Connection, [Type, Headers, Body]) ->
	MessageId=get_message_id([Type, Headers, Body]),
	ack(Connection, MessageId);
ack (Connection, MessageId)	->
	AckMessage=lists:append(["ACK", "\nmessage-id: ", MessageId, "\n\n", [0]]),
	gen_tcp:send(Connection,AckMessage),
	ok.



%% Example: stomp:ack(Conn, Message, TransactionId).
%% Example: stomp:ack(Conn, stomp:get_message_id(Message), TransactionId).
%% Example: stomp:ack(Conn, "ID:phosphorus-63844-1247442885553-3:1:1:1:1", TransactionId).

ack (Connection, [Type, Headers, Body], TransactionId) ->
	MessageId=get_message_id([Type, Headers, Body]),
	ack(Connection, MessageId, TransactionId);
ack (Connection, MessageId, TransactionId)	->
	AckMessage=lists:append(["ACK", "\nmessage-id: ", MessageId, "\ntransaction: ", TransactionId, "\n\n", [0]]),
	gen_tcp:send(Connection,AckMessage),
	ok.

%% Example: stomp:send(Conn, "/queue/foobar", [], "hello world").
%% Example: stomp:send(Conn, "/queue/foobar", [{"priority","15"}], "high priority hello world").
	
send (Connection, Destination, Headers, MessageBody) ->
	Message=lists:append(["SEND", "\ndestination: ", Destination, concatenate_options(Headers), "\n\n", MessageBody, [0]]),
	gen_tcp:send(Connection,Message),
	ok.
		
		
	
	

%% Example: stomp:get_messages(Conn).

get_messages (Connection) ->
	get_messages (Connection, []).
	
get_messages (Connection, Messages) ->
	{ok, Response}=gen_tcp:recv(Connection, 0),
	get_messages(Connection, Messages, Response).
		
get_messages (_, Messages, []) ->
	Messages;
get_messages (Connection, Messages, Response) ->
			[{type, Type}, {headers, Headers}, {body, MessageBody}, TheRest]=get_message(Response),
			T = fun ([]) -> []; ([_|T]) -> T end (TheRest), %% U.G.L.Y. . . .  you ain't got no alibi.
			NewMessages=lists:append(Messages, [[{type, Type}, {headers, Headers}, {body, MessageBody}]]),
			get_messages (Connection, NewMessages, T).


%% Example: MyFunction=fun([_, _, {_, X}]) -> io:fwrite("message ~s ~n", [X]) end, stomp:on_message(MyFunction, Conn).

on_message (F, Conn) ->
	Messages=get_messages(Conn),
	apply_function_to_messages(F, Messages),
	on_message(F, Conn).


%% Example: stomp:begin_transaction(Conn, "MyUniqueTransactionIdBlahBlahBlah1234567890").

begin_transaction (Connection, TransactionId) ->
	Message=lists:append(["BEGIN", "\ntransaction: ", TransactionId, "\n\n", [0]]),
	gen_tcp:send(Connection,Message),
	ok.


%% Example: stomp:commit_transaction(Conn, "MyUniqueTransactionIdBlahBlahBlah1234567890").
	
commit_transaction (Connection, TransactionId) ->
	Message=lists:append(["COMMIT", "\ntransaction: ", TransactionId, "\n\n", [0]]),
	gen_tcp:send(Connection,Message),
	ok.
		

%% Example: stomp:abort_transaction(Conn, "MyUniqueTransactionIdBlahBlahBlah1234567890").

abort_transaction (Connection, TransactionId) ->
	Message=lists:append(["ABORT", "\ntransaction: ", TransactionId, "\n\n", [0]]),
	gen_tcp:send(Connection,Message),
	ok.			

%% PRIVATE METHODS . . .	
concatenate_options ([]) ->
	[];
concatenate_options ([H|T]) ->
	{Name, Value}=H,
	lists:append(["\n", Name, ": ", Value, concatenate_options(T)]).

apply_function_to_messages(_, []) ->
	ok;
apply_function_to_messages(F, [H|T]) ->
	F(H),
	apply_function_to_messages(F, T).

% MESSAGE PARSING  . . . get's a little ugly in here . . . would help if I truly grokked Erlang, I suspect.
% 7/12/09 - yeah, ugly indeed, i need to make this use the same pattern as get_headers_from_raw_src . . . currently scanning header block multiple times and making unnecessary copies
get_message(Message) ->
 	[Type, {Headers, MessageBody}, TheRest]=get_type(Message), %% Ugly . . .
	{ParsedHeaders, _}=get_headers_from_raw_src([], Headers),
	[{type, Type}, {headers, ParsedHeaders}, {body, MessageBody}, TheRest].




	
%% extract message body
get_message_body ([H|T]) ->
		get_message_body ([H|T], []).

get_message_body ([H|T], MessageBody) ->
	case(H) of
		0 -> {MessageBody, T};
		_ -> {MyMessageBody, TheRest}=get_message_body(T, MessageBody), {lists:append([MessageBody, [H], MyMessageBody]), TheRest}
	end.	


%% extract headers as a blob of chars, after having iterated over . . .

get_headers (Message) ->
	get_headers (Message, []).

get_headers (Message, Headers) ->
	get_headers (Message, Headers, -1).
get_headers ([H|T], Headers, LastChar) ->
	case ({H, LastChar}) of
		{10, 10} -> {MessageBody, TheRest}=get_message_body(T),[{Headers, MessageBody}, TheRest];
		{_, _} -> get_headers(T, lists:append([Headers, [H]]), H)
	end.
	
	
	
%% extract type ("MESSAGE", "CONNECT", etc.) from message string . . .	

get_type(Message) ->
	get_type (Message, []).

get_type ([], Type) ->
	Type;
get_type ([H|T], Type) ->	
	case (H) of
		10 -> [{Headers, MessageBody}, TheRest]=get_headers(T), [Type, {Headers, MessageBody}, TheRest];
		_ -> get_type(T, lists:append([Type, [H]]))	
	end.	


%% parse header clob into list of tuples . . .
get_headers_from_raw_src (Headers, []) ->
	{Headers, []};
get_headers_from_raw_src(Headers, RawSrc) ->
	{Header, RestOfList}=get_header(RawSrc),
	get_headers_from_raw_src(lists:append([Headers, [Header]]), RestOfList).

get_header (RawSrc) ->
	{HeaderName, RestOfListAfterHeaderExtraction}=get_header_name([], RawSrc),
	{HeaderValue, RestOfListAfterValueExtraction}=get_header_value([], RestOfListAfterHeaderExtraction),
	{{HeaderName, HeaderValue}, RestOfListAfterValueExtraction}.
	
	
get_header_name (HeaderName, [H|T]) ->
		case (H) of 
			58 ->  {HeaderName, T};
			_ -> get_header_name(lists:append([HeaderName, [H]]), T)
		end.
	
get_header_value (HeaderValue, [H|T]) ->
		case (H) of 
			10 -> {HeaderValue, T};
			_ -> get_header_value(lists:append([HeaderValue, [H]]), T)
			end.	



