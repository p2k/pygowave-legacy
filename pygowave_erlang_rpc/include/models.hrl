
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

%% Note: Load from the erlang shell with rr("models.hrl").

-record(pconn, {
	id,
	participant_id,
	created,
	last_contact,
	rx_key,
	tx_key
	}).

-record(participant, {
	id = <<>>,
	user_id,
	is_bot = 0,
	last_contact,
	name,
	avatar = <<>>,
	profile = <<>>
	}).

-record(wavelet, {
	id,
	wave_id,
	creator_id,
	is_root,
	root_blip_id,
	created,
	last_modified,
	title,
	version
	}).

-record(gadget, {
	id,
	by_user_id,
	title,
	description,
	url,
	hosted_filename,
	devel
	}).

-record(blip, {
	id,
	wavelet_id,
	parent_id,
	created,
	creator_id,
	version,
	last_modified,
	submitted,
	text
	}).

-record(element, {
	id,
	blip_id,
	position,
	type,
	properties
	}).

-record(annotation, {
	id,
	blip_id,
	name,
	start_index,
	end_index,
	value
	}).

-record(delta, {
	id,
	timestamp,
	version,
	wavelet_id,
	ops
	}).
