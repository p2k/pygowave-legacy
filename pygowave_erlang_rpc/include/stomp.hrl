
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

-record(stomp_state, {
	socket,
	buffer
}).

%% Notes on the stomp_frame record:
%% command
%%   A string specifying the Stomp command. Required.
%%   Example: "SEND"
%% header
%%   A key-value list of strings which represents the header in the Stomp frame.
%%   May be left undefined. Keys and values can also be atoms or binaries which
%%   will be converted before sending.
%%   Example: [{"content-encoding", "utf-8"}, {"content-type", "application/json"}]
%% body
%%   A binary or string which represents the payload of the frame. May be left
%%   undefined. If a null-byte is found within the body, then "content-length"
%%   will be set automatically before sending. Also, if "content-length" is
%%   specified in the header with any value, it will be set to the actual size
%%   of the body.

-record(stomp_frame, {
	command,
	header,
	body
}).
