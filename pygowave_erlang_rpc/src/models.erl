
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

%% -- Thin wrapper around the PyGoWave database structure

-module(models).
-export([
	get_pconn/2,
	update_pconn/2,
	delete_pconn/2,
	create_new_pconn/2,
	get_timeout_pconns/2,
	get_or_create_participant/3,
	get_participant/2,
	save_participant/2,
	serialize_participant/1,
	find_participant_ids/2,
	get_participant_wavelets/2,
	get_wavelet/2,
	get_wavelets/2,
	get_wavelet_participant_ids/2,
	serialize_wavelet/2,
	serialize_wavelet_blips/2,
	create_wavelet/4,
	add_wavelet_to_pconn/2,
	remove_wavelet_from_pconn/2,
	get_wavelet_deltas/2,
	wavelet_apply_operations/4,
	get_wavelet_blipsums/2,
	get_wavelet_participant_count/2,
	delete_wavelet/2,
	get_broadcast_targets/2,
	delete_wave/2,
	wave_exists/2,
	create_delta/4,
	all_gadgets/1,
	get_blip/2,
	user_authenticate/2,
	datetime_to_ms/1,
	ms_to_datetime/1,
	gen_random_id/1
]).

%% Records
-include("models.hrl").
-include("operation.hrl").

%% Defines
-define(RANDOM_ID_BASE, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
-define(RANDOM_ID_BASE_COUNT, 62).
-define(ROOT_WAVELET_ID_SUFFIX, "!conv+root").

%% Imports
-import(lists, [nth/2, map/2, filter/2, flatten/1, foldl/3, keyfind/3, keymerge/3, keysort/2, keystore/4]).

%% Exported Functions

get_pconn({tx_key, TxKey}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_pconn_sql, <<"SELECT id, participant_id, created, last_contact, rx_key FROM pygowave_server_participantconn WHERE tx_key = ?">>),
		[TxKey]),
	case mysql:get_result_rows(Result) of
		[[Id, ParticipantId, Created, LastContact, RxKey]] ->
			#pconn{id=Id, participant_id=ParticipantId, created=Created, last_contact=LastContact, rx_key=binary_to_list(RxKey), tx_key=TxKey};
		_ ->
			undefined
	end.

%% Note: Only sets the last_contact to NOW()
update_pconn({pconn, #pconn{id=PConnId}}, DBPoolId) ->
	mysql:execute(DBPoolId,
		prepare_once(update_pconn_sql, <<"UPDATE pygowave_server_participantconn SET last_contact = NOW() WHERE id = ?">>),
		[PConnId]),
	ok.

delete_pconn({pconn, #pconn{id=PConnId}}, DBPoolId) ->
	delete_pconn({pconn_id, PConnId}, DBPoolId);

delete_pconn({pconn_id, PConnId}, DBPoolId) ->
	prepare_once(delete_pconn_sql1, <<"DELETE FROM pygowave_server_wavelet_participant_conns WHERE participantconn_id = ?">>),
	prepare_once(delete_pconn_sql2, <<"DELETE FROM pygowave_server_participantconn WHERE id = ?">>),
	mysql:transaction(DBPoolId,
		fun () ->
			mysql:execute(delete_pconn_sql1, [PConnId]),
			mysql:execute(delete_pconn_sql2, [PConnId])
		end
	).

%% Note: Parameters are binary strings
%% Todo: Write last connection date
user_authenticate({Username, Password}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(user_authenticate_sql1, <<"SELECT id, password, is_active FROM auth_user WHERE username = ?">>),
		[Username]),
	case mysql:get_result_rows(Result) of
		[[UserId, EncPassword, 1]] ->
			case check_pwd(binary_to_list(Password), binary_to_list(EncPassword)) of
				true ->
					{ok, UserId};
				false ->
					error
			end;
		_ ->
			error
	end.

get_or_create_participant({user_id, UserId}, DBPoolId, WaveDomain) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_or_create_participant_sql1, <<"SELECT id, is_bot, last_contact, name, avatar, profile FROM pygowave_server_participant WHERE user_id = ?">>),
		[UserId]),
	case mysql:get_result_rows(Result) of
		[[Id, IsBot, LastContact, Name, Avatar, Profile]] ->
			#participant{id=Id, user_id=UserId, is_bot=IsBot, last_contact=LastContact, name=Name, avatar=Avatar, profile=Profile};
		_ ->
			{data, UResult} = mysql:execute(DBPoolId,
				prepare_once(get_or_create_participant_sql2, <<"SELECT username FROM auth_user WHERE id = ?">>),
				[UserId]),
			case mysql:get_result_rows(UResult) of
				[[Username]] ->
					NewId = list_to_binary(binary_to_list(Username) ++ "@" ++ WaveDomain),
					mysql:execute(DBPoolId,
						prepare_once(get_or_create_participant_sql3, <<"INSERT INTO pygowave_server_participant (id, user_id, is_bot, last_contact, name, avatar, profile) VALUES (?, ?, 0, NOW(), ?, '', '')">>),
						[NewId, UserId, Username]),
					#participant{id=NewId, user_id=UserId, name=Username};
				_ -> undefined
			end
	end.

get_participant({pconn, undefined}, _) ->
	undefined;

get_participant({pconn, #pconn{participant_id=ParticipantId}}, DBPoolId) ->
	get_participant({participant_id, ParticipantId}, DBPoolId);

get_participant({participant_id, ParticipantId}, DBPoolId) when is_binary(ParticipantId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_participant_sql1, <<"SELECT user_id, is_bot, last_contact, name, avatar, profile FROM pygowave_server_participant WHERE id = ?">>),
		[ParticipantId]),
	case mysql:get_result_rows(Result) of
		[[UserId, IsBot, LastContact, Name, Avatar, Profile]] ->
			#participant{id=ParticipantId, user_id=UserId, is_bot=IsBot, last_contact=LastContact, name=Name, avatar=Avatar, profile=Profile};
		_ ->
			undefined
	end;

get_participant({user_id, UserId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_participant_sql2, <<"SELECT id, is_bot, last_contact, name, avatar, profile FROM pygowave_server_participant WHERE user_id = ?">>),
		[UserId]),
	case mysql:get_result_rows(Result) of
		[[Id, IsBot, LastContact, Name, Avatar, Profile]] ->
			#participant{id=Id, user_id=UserId, is_bot=IsBot, last_contact=LastContact, name=Name, avatar=Avatar, profile=Profile};
		_ ->
			undefined
	end.

save_participant(Participant=#participant{}, DBPoolId) ->
	mysql:execute(DBPoolId,
		prepare_once(save_participant_sql, <<"INSERT INTO pygowave_server_participant (id, user_id, is_bot, last_contact, name, avatar, profile) VALUES (?, ?, ?, ?, ?, ?, ?) "
			"ON DUPLICATE KEY UPDATE user_id = VALUES(user_id), is_bot = VALUES(is_bot), last_contact = VALUES(last_contact), name = VALUES(name), avatar = VALUES(avatar), profile = VALUES(profile)">>),
		[Participant#participant.id, Participant#participant.user_id, Participant#participant.is_bot, Participant#participant.last_contact, Participant#participant.name, Participant#participant.avatar, Participant#participant.profile]),
	ok.

serialize_participant(Participant=#participant{}) ->
	{obj, [
		{"id", Participant#participant.id},
		{"displayName", Participant#participant.name},
		{"thumbnailUrl", Participant#participant.avatar},
		{"profileUrl", Participant#participant.profile},
		{"isBot", case Participant#participant.is_bot of 0 -> false; 1 -> true end}
	]}.

find_participant_ids({name_icontains, Substring}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(find_participant_ids_sql, <<"SELECT id FROM pygowave_server_participant WHERE name LIKE CONCAT(\\'%\\', ?, \\'%\\')">>),
		[Substring]),
	flatten(mysql:get_result_rows(Result)).

get_participant_wavelets({participant, #participant{id=ParticipantId}}, DBPoolId) ->
	get_participant_wavelets({participant_id, ParticipantId}, DBPoolId);

get_participant_wavelets({participant_id, ParticipantId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_participant_wavelets_sql, <<"SELECT w.id, wave_id, creator_id, is_root, root_blip_id, created, last_modified, title, version "
			"FROM pygowave_server_wavelet AS w INNER JOIN pygowave_server_wavelet_participants AS wp ON w.id = wp.wavelet_id WHERE wp.participant_id = ?">>),
		[ParticipantId]),
	map(fun ([WaveletId, WaveId, CreatorId, IsRoot, RootBlipId, Created, LastModified, Title, Version]) ->
		#wavelet{id=binary_to_list(WaveletId), wave_id=WaveId, creator_id=CreatorId, is_root=IsRoot, root_blip_id=RootBlipId, created=Created, last_modified=LastModified, title=Title, version=Version} end, mysql:get_result_rows(Result)).

create_new_pconn(#participant{id=ParticipantId}, DBPoolId) ->
	RxKey = uuid:to_string(uuid:v4()),
	TxKey = uuid:to_string(uuid:v4()),
	mysql:execute(DBPoolId,
		prepare_once(create_new_pconn_sql, <<"INSERT INTO pygowave_server_participantconn (participant_id, created, last_contact, rx_key, tx_key) VALUES (?, NOW(), NOW(), ?, ?)">>),
		[ParticipantId, RxKey, TxKey]),
	get_pconn({tx_key, TxKey}, DBPoolId).

get_timeout_pconns(TimeoutMinutes, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_timeout_pconns_sql, <<"SELECT id, participant_id, created, last_contact, tx_key, rx_key FROM pygowave_server_participantconn WHERE NOW() > ADDTIME(last_contact, ?)">>),
		[{time, {0, TimeoutMinutes, 0}}]),
	map(fun ([Id, ParticipantId, Created, LastContact, TxKey, RxKey]) ->
		#pconn{id=Id, participant_id=ParticipantId, created=Created, last_contact=LastContact, rx_key=binary_to_list(RxKey), tx_key=TxKey} end, mysql:get_result_rows(Result)).

get_wavelet({participant, #participant{id=ParticipantId}, wavelet_id, WaveletId}, DBPoolId) ->
	get_wavelet({participant_id, ParticipantId, wavelet_id, WaveletId}, DBPoolId);

get_wavelet({participant_id, ParticipantId, wavelet_id, WaveletId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_sql, <<"SELECT wave_id, creator_id, is_root, root_blip_id, created, last_modified, title, version "
			"FROM pygowave_server_wavelet AS w INNER JOIN pygowave_server_wavelet_participants AS wp ON w.id = wp.wavelet_id WHERE wp.participant_id = ? AND w.id = ?">>),
		[ParticipantId, WaveletId]),
	case mysql:get_result_rows(Result) of
		[[WaveId, CreatorId, IsRoot, RootBlipId, Created, LastModified, Title, Version]] ->
			#wavelet{id=WaveletId, wave_id=WaveId, creator_id=CreatorId, is_root=IsRoot, root_blip_id=RootBlipId, created=Created, last_modified=LastModified, title=Title, version=Version};
		_ ->
			undefined
	end.

get_wavelets({participant, #participant{id=ParticipantId}, wave_id, WaveId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelets_sql1, <<"SELECT w.id, creator_id, is_root, root_blip_id, created, last_modified, title, version "
			"FROM pygowave_server_wavelet AS w INNER JOIN pygowave_server_wavelet_participants AS wp ON w.id = wp.wavelet_id WHERE wp.participant_id = ? AND w.wave_id = ?">>),
		[ParticipantId, WaveId]),
	map(fun ([WaveletId, CreatorId, IsRoot, RootBlipId, Created, LastModified, Title, Version]) ->
		#wavelet{id=binary_to_list(WaveletId), wave_id=WaveId, creator_id=CreatorId, is_root=IsRoot, root_blip_id=RootBlipId, created=Created, last_modified=LastModified, title=Title, version=Version} end, mysql:get_result_rows(Result));

get_wavelets({wave_id, WaveId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelets_sql2, <<"SELECT id, creator_id, is_root, root_blip_id, created, last_modified, title, version FROM pygowave_server_wavelet WHERE wave_id = ?">>),
		[WaveId]),
	map(fun ([WaveletId, CreatorId, IsRoot, RootBlipId, Created, LastModified, Title, Version]) ->
		#wavelet{id=binary_to_list(WaveletId), wave_id=WaveId, creator_id=CreatorId, is_root=IsRoot, root_blip_id=RootBlipId, created=Created, last_modified=LastModified, title=Title, version=Version} end, mysql:get_result_rows(Result)).

get_wavelet_participant_ids({wavelet, #wavelet{id=WaveletId}}, DBPoolId) ->
	get_wavelet_participant_ids({wavelet_id, WaveletId}, DBPoolId);

get_wavelet_participant_ids({wavelet_id, WaveletId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_participant_ids_sql, <<"SELECT participant_id FROM pygowave_server_wavelet_participants WHERE wavelet_id = ?">>),
		[WaveletId]),
	flatten(mysql:get_result_rows(Result)).

serialize_wavelet(Wavelet=#wavelet{}, DBPoolId) ->
	{obj, [
		{"rootBlipId", Wavelet#wavelet.root_blip_id},
		{"title", Wavelet#wavelet.title},
		{"creator", Wavelet#wavelet.creator_id},
		{"creationTime", datetime_to_ms(Wavelet#wavelet.created)},
		{"dataDocuments", null},
		{"waveletId", if is_binary(Wavelet#wavelet.id) -> Wavelet#wavelet.id; is_list(Wavelet#wavelet.id) -> list_to_binary(Wavelet#wavelet.id) end},
		{"participants", get_wavelet_participant_ids({wavelet, Wavelet}, DBPoolId)},
		{"version", Wavelet#wavelet.version},
		{"lastModifiedTime", datetime_to_ms(Wavelet#wavelet.last_modified)},
		{"waveId", Wavelet#wavelet.wave_id},
		{"isRoot", Wavelet#wavelet.is_root == 1}
	]}.

serialize_wavelet_blips(Wavelet=#wavelet{}, DBPoolId) ->
	{obj, map(fun (Blip) -> {Blip#blip.id, serialize_blip(Blip, Wavelet, DBPoolId)} end, get_wavelet_blips({wavelet, Wavelet}, DBPoolId))}.

create_wavelet(WaveId, Creator=#participant{}, Title, DBPoolId) ->
	create_wavelet(WaveId, Creator#participant.id, Title, DBPoolId);

% No WaveId -> create Wave first
create_wavelet(<<>>, CreatorId, Title, DBPoolId) ->
	WaveId = list_to_binary(gen_random_id(10)),
	case models:wave_exists({wave_id, WaveId}, DBPoolId) of % Check if random generated id exists
		true ->
			create_wavelet(<<>>, CreatorId, Title, DBPoolId); % Retry
		false ->
			mysql:execute(DBPoolId,
				prepare_once(create_wavelet_sql1, <<"INSERT INTO pygowave_server_wave (id) VALUES (?)">>),
				[WaveId]),
			create_wavelet(WaveId, CreatorId, Title, 1, DBPoolId) % Ok, create a root wavelet
	end;

% WaveId available -> check if Wave exists
create_wavelet(WaveId, CreatorId, Title, DBPoolId) ->
	case models:wave_exists({wave_id, WaveId}, DBPoolId) of
		true ->
			create_wavelet(WaveId, CreatorId, Title, 0, DBPoolId); % Ok, create a non-root wavelet
		false ->
			{error, wave_not_found}
	end.

add_wavelet_to_pconn({wavelet, Wavelet=#wavelet{}, pconn, PConn=#pconn{}}, DBPoolId) ->
	add_wavelet_to_pconn({wavelet_id, Wavelet#wavelet.id, pconn, PConn}, DBPoolId);

add_wavelet_to_pconn({wavelet_id, WaveletId, pconn, #pconn{id=PConnId}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(add_wavelet_to_pconn_sql1, <<"SELECT COUNT(id) FROM pygowave_server_wavelet_participant_conns WHERE wavelet_id = ? AND participantconn_id = ?">>),
		[WaveletId, PConnId]),
	case mysql:get_result_rows(Result) of
		[[0]] ->
			mysql:execute(DBPoolId,
				prepare_once(add_wavelet_to_pconn_sql2, <<"INSERT INTO pygowave_server_wavelet_participant_conns (wavelet_id, participantconn_id) VALUES (?, ?)">>),
				[WaveletId, PConnId]),
			ok;
		_ ->
			ok
	end.

remove_wavelet_from_pconn({wavelet, #wavelet{id=WaveletId}, pconn, PConn=#pconn{}}, DBPoolId) ->
	remove_wavelet_from_pconn({wavelet_id, WaveletId, pconn, PConn}, DBPoolId);

remove_wavelet_from_pconn({wavelet_id, WaveletId, pconn, #pconn{id=PConnId}}, DBPoolId) ->
	mysql:execute(DBPoolId,
		prepare_once(remove_wavelet_from_pconn_sql, <<"DELETE FROM pygowave_server_wavelet_participant_conns WHERE wavelet_id = ? AND participantconn_id = ?">>),
		[WaveletId, PConnId]),
	ok.

get_wavelet_deltas({wavelet, #wavelet{id=WaveletId}, version, Version}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_deltas_sql, <<"SELECT id, timestamp, version, operations FROM pygowave_server_delta WHERE wavelet_id = ? AND version > ?">>),
		[WaveletId, Version]),
	map(
		fun ([Id, Timestamp, Version2, JsonOpsString]) ->
			{ok, JsonOps, []} = rfc4627:decode(JsonOpsString),
			Ops = map(fun operations:op_from_json/1, JsonOps),
			#delta{id=Id, timestamp=Timestamp, version=Version2, wavelet_id=WaveletId, ops=Ops}
		end,
		mysql:get_result_rows(Result)
	).

wavelet_apply_operations(#wavelet{id=WaveletId}, Ops, #participant{id=ParticipantId}, DBPoolId) ->
	Result = foldl(fun (Op, Acc) -> wavelet_apply_operation(WaveletId, Op, ParticipantId, Acc, DBPoolId) end, {[], []}, Ops),
	mysql:execute(DBPoolId, prepare_once(wavelet_apply_operations_sql, <<"UPDATE pygowave_server_wavelet SET version = version + 1 WHERE id = ?">>), [WaveletId]),
	Result.

get_wavelet_blipsums({wavelet, Wavelet=#wavelet{}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_blipsums_sql, <<"SELECT id, SHA1(text) FROM pygowave_server_blip WHERE wavelet_id = ?">>),
		[Wavelet#wavelet.id]),
	map(fun ([Id, Sha1]) -> {Id, Sha1} end, mysql:get_result_rows(Result)).

get_wavelet_participant_count({wavelet, Wavelet=#wavelet{}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_participant_count_sql, <<"SELECT COUNT(id) FROM pygowave_server_wavelet_participants WHERE wavelet_id = ?">>),
		[Wavelet#wavelet.id]),
	[[Count]] = mysql:get_result_rows(Result),
	Count.

delete_wavelet({wavelet, Wavelet=#wavelet{}}, DBPoolId) ->
	% Delete Blips (and Elements, Annotations, Blip contributors, etc.)
	map(fun (BlipId) -> delete_blip({blip_id, BlipId}, DBPoolId) end, get_wavelet_blip_ids({wavelet, Wavelet}, DBPoolId)),
	Identifiers = [
		% Delete Data Documents
		prepare_once(delete_wavelet_sql1, <<"DELETE FROM pygowave_server_datadocument WHERE wavelet_id = ?">>),
		% Delete Deltas
		prepare_once(delete_wavelet_sql2, <<"DELETE FROM pygowave_server_delta WHERE wavelet_id = ?">>),
		% Delete Participants
		prepare_once(delete_wavelet_sql3, <<"DELETE FROM pygowave_server_wavelet_participants WHERE wavelet_id = ?">>),
		% Delete Connections
		prepare_once(delete_wavelet_sql4, <<"DELETE FROM pygowave_server_wavelet_participant_conns WHERE wavelet_id = ?">>),
		% Finally delete the Wavelet
		prepare_once(delete_wavelet_sql5, <<"DELETE FROM pygowave_server_wavelet WHERE id = ?">>)
	],
	mysql:transaction(DBPoolId, fun () -> map(fun (Identifier) -> mysql:execute(Identifier, [Wavelet#wavelet.id]) end, Identifiers) end),
	ok.

get_broadcast_targets({wavelet, Wavelet=#wavelet{}, except_pconn, PConn=#pconn{}, target_spec, TargetSpec}, DBPoolId) ->
	Criteria = case TargetSpec of
		connected -> 1;
		unconnected -> 0;
		both -> 2
	end,
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(delete_wavelet_sql, <<"SELECT pconn.rx_key FROM pygowave_server_participantconn AS pconn "
			"INNER JOIN pygowave_server_wavelet_participants AS wp ON wp.participant_id = pconn.participant_id "
			"LEFT JOIN pygowave_server_wavelet_participant_conns AS wpc ON wpc.participantconn_id = pconn.id AND wpc.wavelet_id = wp.wavelet_id "
			"WHERE wp.wavelet_id = ? AND pconn.id != ? AND ISNULL(wpc.id) != ?">>),
		[Wavelet#wavelet.id, PConn#pconn.id, Criteria]),
	map(fun ([RxKey]) -> binary_to_list(RxKey) end, mysql:get_result_rows(Result)).

delete_wave({wave_id, WaveId}, DBPoolId) ->
	map(fun (Wavelet) -> delete_wavelet({wavelet, Wavelet}, DBPoolId) end, get_wavelets({wave_id, WaveId}, DBPoolId)),
	mysql:execute(DBPoolId,
		prepare_once(delete_wave_sql, <<"DELETE FROM pygowave_server_wave WHERE id = ?">>),
		[WaveId]),
	ok.

wave_exists({wave_id, WaveId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(wave_exists_sql, <<"SELECT COUNT(id) FROM pygowave_server_wave WHERE id = ?">>),
		[WaveId]),
	case mysql:get_result_rows(Result) of
		[[0]] -> false;
		[[1]] -> true
	end.

create_delta(Ops, WaveletId, Version, DBPoolId) ->
	{updated, _} = mysql:execute(DBPoolId,
		prepare_once(create_delta_sql1, <<"INSERT INTO pygowave_server_delta (timestamp, version, wavelet_id, operations) VALUES (NOW(), ?, ?, ?)">>),
		[Version, WaveletId, rfc4627:encode(map(fun operations:op_to_json/1, Ops))]),
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(create_delta_sql2, <<"SELECT id, timestamp FROM pygowave_server_delta WHERE id = LAST_INSERT_ID()">>),
		[]),
	[[Id, Timestamp]] = mysql:get_result_rows(Result),
	#delta{id=Id, timestamp=Timestamp, version=Version, wavelet_id=WaveletId, ops=Ops}.

all_gadgets(DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(all_gadgets_sql, <<"SELECT id, by_user_id, title, description, url, hosted_filename, devel FROM pygowave_server_gadget">>), []),
	map(fun ([Id, ByUserId, Title, Description, Url, HostedFilename, Devel]) ->
		#gadget{id=Id, by_user_id=ByUserId, title=Title, description=Description, url=Url, hosted_filename=HostedFilename, devel=Devel} end, mysql:get_result_rows(Result)).

%% Internal functions

create_wavelet(WaveId, CreatorId, Title, IsRoot, DBPoolId) ->
	WaveletId = case IsRoot of
		0 -> binary_to_list(WaveId) ++ "!" ++ gen_random_id(10);
		1 -> binary_to_list(WaveId) ++ ?ROOT_WAVELET_ID_SUFFIX
	end,
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(create_wavelet_sql2, <<"SELECT COUNT(id) FROM pygowave_server_wavelet WHERE id = ?">>),
		[WaveletId]),
	case mysql:get_result_rows(Result) of % Check if random generated id exists
		[[1]] ->
			case IsRoot of
				0 -> create_wavelet(WaveId, CreatorId, Title, IsRoot, DBPoolId); % Retry
				1 -> {error, root_wavelet_exists}
			end;
		[[0]] ->
			mysql:execute(DBPoolId,
				prepare_once(create_wavelet_sql3, <<"INSERT INTO pygowave_server_wavelet (id, wave_id, creator_id, is_root, created, last_modified, title, version) VALUES (?, ?, ?, ?, NOW(), NOW(), ?, 0)">>),
				[WaveletId, WaveId, CreatorId, IsRoot, Title]),
			add_participant_to_wavelet({participant_id, CreatorId, wavelet_id, WaveletId}, DBPoolId),
			Blip = create_blip(WaveletId, CreatorId, DBPoolId),
			set_root_blip({wavelet_id, WaveletId, blip_id, Blip#blip.id}, DBPoolId),
			get_wavelet({participant_id, CreatorId, wavelet_id, WaveletId}, DBPoolId)
	end.

set_root_blip({wavelet_id, WaveletId, blip_id, BlipId}, DBPoolId) ->
	{updated, Result} = mysql:execute(DBPoolId,
		prepare_once(set_root_blip_sql, <<"UPDATE pygowave_server_wavelet SET root_blip_id = ? WHERE id = ?">>),
		[BlipId, WaveletId]),
	case mysql:get_result_affected_rows(Result) of
		1 ->
			ok;
		0 ->
			error
	end.

create_blip(WaveletId, CreatorId, DBPoolId) ->
	create_blip(WaveletId, undefined, CreatorId, DBPoolId).

create_blip(WaveletId, ParentId, CreatorId, DBPoolId) ->
	BlipId = list_to_binary(gen_random_id(10)),
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(create_blip_sql1, <<"SELECT COUNT(id) FROM pygowave_server_blip WHERE id = ?">>),
		[BlipId]),
	case mysql:get_result_rows(Result) of % Check if random generated id exists
		[[1]] ->
			create_blip(WaveletId, ParentId, CreatorId, DBPoolId); % Retry
		[[0]] ->
			mysql:execute(DBPoolId,
				prepare_once(create_blip_sql2, <<"INSERT INTO pygowave_server_blip (id, wavelet_id, parent_id, created, creator_id, version, last_modified, submitted, text) "
					"VALUES (?, ?, ?, NOW(), ?, 0, NOW(), 0, \\'\\')">>),
				[BlipId, WaveletId, ParentId, CreatorId]),
			get_blip({wavelet_id, WaveletId, blip_id, BlipId}, DBPoolId)
	end.

get_blip({wavelet_id, WaveletId, blip_id, BlipId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_blip_sql, <<"SELECT parent_id, created, creator_id, version, last_modified, submitted, text FROM pygowave_server_blip WHERE wavelet_id = ? AND id = ?">>),
		[WaveletId, BlipId]),
	case mysql:get_result_rows(Result) of
		[[ParentId, Created, CreatorId, Version, LastModified, Submitted, Text]] ->
			#blip{id=BlipId, wavelet_id=WaveletId, parent_id=ParentId, created=Created, creator_id=CreatorId, version=Version, last_modified=LastModified, submitted=Submitted, text=Text};
		_ ->
			undefined
	end.

serialize_blip(Blip=#blip{}, Wavelet=#wavelet{}, DBPoolId) ->
	{obj, [
		{"blipId", Blip#blip.id},
		{"content", Blip#blip.text},
		{"elements", map(fun (Element) -> serialize_element(Element) end, get_blip_elements({blip, Blip}, DBPoolId))},
		{"contributors", get_blip_contributor_ids({blip, Blip}, DBPoolId)},
		{"creator", Blip#blip.creator_id},
		{"creationTime", datetime_to_ms(Blip#blip.created)},
		{"parentBlipId", case Blip#blip.parent_id of undefined -> null; ParentId -> ParentId end},
		{"annotations", map(fun (Annotation) -> serialize_annotation(Annotation) end, get_blip_annotations({blip, Blip}, DBPoolId))},
		{"waveletId", if is_binary(Blip#blip.wavelet_id) -> Blip#blip.wavelet_id; is_list(Blip#blip.wavelet_id) -> list_to_binary(Blip#blip.wavelet_id) end},
		{"version", Blip#blip.version},
		{"lastModifiedTime", datetime_to_ms(Blip#blip.last_modified)},
		{"childBlipIds", get_blip_child_ids({blip, Blip}, DBPoolId)},
		{"waveId", Wavelet#wavelet.wave_id},
		{"submitted", Blip#blip.submitted == 1},
		{"checksum", list_to_binary(sha1_hexstring(Blip#blip.text))} % Note: This is tentative and subject to change
	]}.

get_blip_elements({blip, Blip=#blip{}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_blip_elements_sql, <<"SELECT id, position, type, properties FROM pygowave_server_element WHERE blip_id = ?">>),
		[Blip#blip.id]),
	map(
		fun ([Id, Position, Type, Properties]) ->
			{ok, PropertiesDec, []} = rfc4627:decode(Properties),
			#element{id=Id, blip_id=Blip#blip.id, position=Position, type=Type, properties=PropertiesDec}
		end,
		mysql:get_result_rows(Result)
	).

get_blip_annotations({blip, Blip=#blip{}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_blip_annotations_sql, <<"SELECT id, name, start, end, value FROM pygowave_server_annotation WHERE blip_id = ?">>),
		[Blip#blip.id]),
	map(
		fun ([Id, Name, Start, End, Value]) ->
			#annotation{id=Id, blip_id=Blip#blip.id, name=Name, start_index=Start, end_index=End, value=Value}
		end,
		mysql:get_result_rows(Result)
	).

get_blip_contributor_ids({blip, Blip=#blip{}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_blip_contributor_ids_sql, <<"SELECT participant_id FROM pygowave_server_blip_contributors WHERE blip_id = ?">>),
		[Blip#blip.id]),
	flatten(mysql:get_result_rows(Result)).

get_blip_child_ids({blip, Blip=#blip{}}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_blip_child_ids_sql, <<"SELECT id FROM pygowave_server_blip WHERE parent_id = ?">>),
		[Blip#blip.id]),
	flatten(mysql:get_result_rows(Result)).

delete_blip({blip_id, BlipId}, DBPoolId) ->
	Identifiers = [
		% Delete elements
		prepare_once(delete_blip_sql1, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_gadgetelement AS t2 ON t1.id = t2.element_ptr_id WHERE t1.blip_id = ?">>),
		prepare_once(delete_blip_sql2, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_formelement AS t2 ON t1.id = t2.element_ptr_id WHERE t1.blip_id = ?">>),
		prepare_once(delete_blip_sql3, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_image AS t2 ON t1.id = t2.element_ptr_id WHERE t1.blip_id = ?">>),
		prepare_once(delete_blip_sql4, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_inlineblip AS t2 ON t1.id = t2.element_ptr_id WHERE t1.blip_id = ?">>),
		prepare_once(delete_blip_sql5, <<"DELETE FROM pygowave_server_element WHERE blip_id = ?">>),
		% Delete annotations
		prepare_once(delete_blip_sql6, <<"DELETE FROM pygowave_server_annotation WHERE blip_id = ?">>),
		% Delete contributors
		prepare_once(delete_blip_sql7, <<"DELETE FROM pygowave_server_blip_contributors WHERE blip_id = ?">>),
		% Finally delete the blip
		prepare_once(delete_blip_sql8, <<"DELETE FROM pygowave_server_blip WHERE id = ?">>)
	],
	mysql:transaction(DBPoolId, fun () -> map(fun (Identifier) -> mysql:execute(Identifier, [BlipId]) end, Identifiers) end),
	ok.

update_blip(#blip{id=BlipId,text=Text}, ContributorId, text, DBPoolId) ->
	update_blip(
		BlipId,
		ContributorId,
		prepare_once(update_blip_sql1, <<"UPDATE pygowave_server_blip SET text = ?, last_modified = NOW() WHERE id = ?">>),
		[Text, BlipId],
		DBPoolId
	).

update_blip(#blip{id=BlipId}, ContributorId, DBPoolId) ->
	update_blip(
		BlipId,
		ContributorId,
		prepare_once(update_blip_sql2, <<"UPDATE pygowave_server_blip SET last_modified = NOW() WHERE id = ?">>),
		[BlipId],
		DBPoolId
	).

update_blip(BlipId, ContributorId, QueryId, Params, DBPoolId) ->
	mysql:execute(DBPoolId, QueryId, Params),
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(update_blip_sql99, <<"SELECT COUNT(id) FROM pygowave_server_blip_contributors WHERE blip_id = ? AND participant_id = ?">>),
		[BlipId, ContributorId]),
	case mysql:get_result_rows(Result) of
		[[0]] ->
			mysql:execute(DBPoolId,
				prepare_once(update_blip_sql100, <<"INSERT INTO pygowave_server_blip_contributors (blip_id, participant_id) VALUES (?, ?)">>),
				[BlipId, ContributorId]),
			ok;
		_ ->
			ok
	end.

add_participant_to_wavelet({participant_id, ParticipantId, wavelet_id, WaveletId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(add_participant_to_wavelet_sql1, <<"SELECT COUNT(id) FROM pygowave_server_wavelet_participants WHERE wavelet_id = ? AND participant_id = ?">>),
		[WaveletId, ParticipantId]),
	case mysql:get_result_rows(Result) of
		[[0]] ->
			mysql:execute(DBPoolId,
				prepare_once(add_participant_to_wavelet_sql2, <<"INSERT INTO pygowave_server_wavelet_participants (wavelet_id, participant_id) VALUES (?, ?)">>),
				[WaveletId, ParticipantId]),
			ok;
		_ ->
			already
	end.

remove_participant_from_wavelet({participant_id, ParticipantId, wavelet_id, WaveletId}, DBPoolId) ->
	{updated, Result} = mysql:execute(DBPoolId,
		prepare_once(remove_participant_from_wavelet_sql, <<"DELETE FROM pygowave_server_wavelet_participants WHERE wavelet_id = ? AND participant_id = ?">>),
		[WaveletId, ParticipantId]),
	case mysql:get_result_affected_rows(Result) of
		1 -> ok;
		0 -> error
	end.

get_wavelet_blips({wavelet, #wavelet{id=WaveletId}}, DBPoolId) ->
	get_wavelet_blips({wavelet_id, WaveletId}, DBPoolId);

get_wavelet_blips({wavelet_id, WaveletId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_blips_sql, <<"SELECT id, parent_id, created, creator_id, version, last_modified, submitted, text FROM pygowave_server_blip WHERE wavelet_id = ?">>),
		[WaveletId]),
	map(
		fun ([Id, ParentId, Created, CreatorId, Version, LastModified, Submitted, Text]) ->
			#blip{id=Id, wavelet_id=WaveletId, parent_id=ParentId, created=Created, creator_id=CreatorId, version=Version, last_modified=LastModified, submitted=Submitted, text=Text}
		end,
		mysql:get_result_rows(Result)
	).

get_wavelet_blip_ids({wavelet, #wavelet{id=WaveletId}}, DBPoolId) ->
	get_wavelet_blip_ids({wavelet_id, WaveletId}, DBPoolId);

get_wavelet_blip_ids({wavelet_id, WaveletId}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_wavelet_blip_ids_sql, <<"SELECT id FROM pygowave_server_blip WHERE wavelet_id = ?">>),
		[WaveletId]),
	flatten(mysql:get_result_rows(Result)).

serialize_annotation(Annotation=#annotation{}) ->
	{obj, [
		{"range", {obj, [
			{"start", Annotation#annotation.start_index},
			{"end", Annotation#annotation.end_index}
		]}},
		{"name", Annotation#annotation.name},
		{"value", Annotation#annotation.value}
	]}.

serialize_element(Element=#element{}) ->
	{obj, [
		{"id", Element#element.id},
		{"index", Element#element.position},
		{"type", Element#element.type},
		{"properties", Element#element.properties}
	]}.

get_element({blip_id, BlipId, index, Index}, DBPoolId) ->
	{data, Result} = mysql:execute(DBPoolId,
		prepare_once(get_element_sql, <<"SELECT id, type, properties FROM pygowave_server_element WHERE blip_id = ? AND position = ?">>),
		[BlipId, Index]),
	case mysql:get_result_rows(Result) of
		[[Id, Type, JSProperties]] ->
			{ok, Properties, []} = rfc4627:decode(JSProperties),
			#element{id=Id, blip_id=BlipId, position=Index, type=Type, properties=Properties};
		_ ->
			undefined
	end.

delete_element({blip_id, BlipId, index, Index}, DBPoolId) ->
	Identifiers = [
		prepare_once(delete_element_sql1, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_gadgetelement AS t2 ON t1.id = t2.element_ptr_id WHERE t1.blip_id = ? AND t1.position = ?">>),
		prepare_once(delete_element_sql2, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_formelement AS t2 ON t1.id = t2.element_ptr_id AND t1.blip_id = ? AND t1.position = ?">>),
		prepare_once(delete_element_sql3, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_image AS t2 ON t1.id = t2.element_ptr_id AND t1.blip_id = ? AND t1.position = ?">>),
		prepare_once(delete_element_sql4, <<"DELETE FROM t2 USING pygowave_server_element AS t1 INNER JOIN pygowave_server_inlineblip AS t2 ON t1.id = t2.element_ptr_id AND t1.blip_id = ? AND t1.position = ?">>),
		prepare_once(delete_element_sql5, <<"DELETE FROM pygowave_server_element WHERE blip_id = ? AND position = ?">>)
	],
	mysql:transaction(DBPoolId, fun () -> map(fun (Identifier) -> mysql:execute(Identifier, [BlipId, Index]) end, Identifiers) end),
	ok.

create_element(BlipId, Index, Type, Properties, DBPoolId) ->
	mysql:execute(DBPoolId,
		prepare_once(create_element_sql1, <<"INSERT INTO pygowave_server_element (blip_id, position, type, properties) VALUES (?, ?, ?, ?)">>),
		[BlipId, Index, Type, rfc4627:encode(Properties)]),
	{data, Result} = mysql:execute(DBPoolId, prepare_once(create_element_sql2, <<"SELECT LAST_INSERT_ID()">>), []),
	[[Id]] = mysql:get_result_rows(Result),
	if
		Type == 2 ->
			mysql:execute(DBPoolId, prepare_once(create_element_sql3, <<"INSERT INTO pygowave_server_gadgetelement (element_ptr_id) VALUES (?)">>), [Id]);
		true -> ok
	end,
	#element{id=Id, blip_id=BlipId, position=Index, type=Type, properties=Properties}.

update_element_properties(#element{id=Id, properties=Properties}, DBPoolId) ->
	{updated, Result} = mysql:execute(DBPoolId,
		prepare_once(update_element_properties_sql, <<"UPDATE pygowave_server_element SET properties = ? WHERE id = ?">>),
		[rfc4627:encode(Properties), Id]),
	case mysql:get_result_affected_rows(Result) of
		0 -> error;
		1 -> ok
	end.

element_apply_delta(BlipId, Index, Delta, DBPoolId) ->
	case get_element({blip_id, BlipId, index, Index}, DBPoolId) of
		Element = #element{type=2, properties=Properties} ->
			{obj, Fields} = getattr(Properties, "fields", {obj, []}),
			% Key-sort first
			SortedFields = keysort(1, Fields),
			SortedDelta = case Delta of
				{obj, DeltaList} -> keysort(1, DeltaList);
				_ -> []
			end,
			% Merge
			MergedFields = keymerge(1, SortedDelta, SortedFields),
			% Filter out nulls
			FilteredFields = filter(fun ({_, Value}) -> (Value /= null) and (Value /= undefined) end, MergedFields),
			% Store and update
			NewProperties = setattr(Properties, "fields", {obj, FilteredFields}),
			update_element_properties(Element#element{properties=NewProperties}, DBPoolId);
		_ ->
			error
	end.

element_apply_setpref(BlipId, Index, Key, Value, DBPoolId) when is_binary(Key) ->
	element_apply_setpref(BlipId, Index, binary_to_list(Key), Value, DBPoolId);

element_apply_setpref(BlipId, Index, Key, Value, DBPoolId) ->
	case get_element({blip_id, BlipId, index, Index}, DBPoolId) of
		Element=#element{type=2, properties=Properties} ->
			Prefs = getattr(Properties, "userprefs", {obj, []}),
			NewPrefs = setattr(Prefs, Key, Value),
			NewProperties = setattr(Properties, "userprefs", NewPrefs),
			update_element_properties(Element#element{properties=NewProperties}, DBPoolId);
		_ ->
			error
	end.

move_blip_elts_and_annos(#blip{id=BlipId}, Index, Amount, DBPoolId) ->
	mysql:execute(DBPoolId,
		prepare_once(move_blip_elts_and_annos_sql1, <<"UPDATE pygowave_server_element SET position = position + ? WHERE blip_id = ? AND position >= ?">>),
		[Amount, BlipId, Index]),
	mysql:execute(DBPoolId,
		prepare_once(move_blip_elts_and_annos_sql2, <<"UPDATE pygowave_server_annotation SET start = start + ? WHERE blip_id = ? AND start >= ?">>),
		[Amount, BlipId, Index]),
	mysql:execute(DBPoolId,
		prepare_once(move_blip_elts_and_annos_sql3, <<"UPDATE pygowave_server_annotation SET end = end + ? WHERE blip_id = ? AND end >= ?">>),
		[Amount, BlipId, Index]),
	ok.


wavelet_apply_operation(WaveletId, Op=#operation{blip_id = <<>>, type=wavelet_add_participant, property=PIdToAdd}, _, {NewBlips, UpdatedOps}, DBPoolId) ->
	case get_participant({participant_id, PIdToAdd}, DBPoolId) of
		undefined ->
			{NewBlips, UpdatedOps}; % Deletes the op
		_ ->
			case add_participant_to_wavelet({participant_id, PIdToAdd, wavelet_id, WaveletId}, DBPoolId) of
				ok -> {NewBlips, UpdatedOps ++ [Op]};
				already -> {NewBlips, UpdatedOps} % Deletes the op
			end
	end;

wavelet_apply_operation(WaveletId, Op=#operation{blip_id = <<>>, type=wavelet_remove_participant, property=PIdToRemove}, _, {NewBlips, UpdatedOps}, DBPoolId) ->
	case remove_participant_from_wavelet({participant_id, PIdToRemove, wavelet_id, WaveletId}, DBPoolId) of
		ok -> {NewBlips, UpdatedOps ++ [Op]};
		error -> {NewBlips, UpdatedOps}
	end;

wavelet_apply_operation(WaveletId, Op=#operation{blip_id = <<>>, type=wavelet_append_blip, property=Prop}, ContributorId, {NewBlips, UpdatedOps}, DBPoolId) ->
	Blip = create_blip(WaveletId, ContributorId, DBPoolId),
	{[{getattr(Prop, "blipId", <<"(MISSING)">>), Blip#blip.id}|NewBlips], UpdatedOps ++ [Op#operation{property=setattr(Prop, "blipId", Blip#blip.id)}]};

wavelet_apply_operation(_, #operation{blip_id = <<>>}, _, {NewBlips, UpdatedOps}, _) -> % Delete unknown
	{NewBlips, UpdatedOps};

wavelet_apply_operation(WaveletId, Op=#operation{blip_id=BlipId, type=Type, index=Index, property=Prop}, ContributorId, {NewBlips, UpdatedOps}, DBPoolId) ->
	Blip = get_blip({wavelet_id, WaveletId, blip_id, BlipId}, DBPoolId),
	case Blip of
		undefined ->
			{NewBlips, UpdatedOps};
		_ ->
			case wavelet_apply_blip_operation(Blip, Type, Index, Prop, ContributorId, DBPoolId) of
				error -> {NewBlips, UpdatedOps};
				ok -> {NewBlips, UpdatedOps ++ [Op]};
				NewBlipEntry -> {[NewBlipEntry|NewBlips], UpdatedOps ++ [Op]}
			end
	end.


wavelet_apply_blip_operation(Blip, document_delete, Index, Prop, ContributorId, DBPoolId) ->
	UnicodeText = unicode:characters_to_list(Blip#blip.text),
	UpdatedUnicodeText = string:sub_string(UnicodeText, 1, Index) ++ string:sub_string(UnicodeText, Index+Prop+1),
	update_blip(Blip#blip{text=unicode:characters_to_binary(UpdatedUnicodeText)}, ContributorId, text, DBPoolId),
	move_blip_elts_and_annos(Blip, Index, -Prop, DBPoolId);

wavelet_apply_blip_operation(Blip, document_insert, Index, Prop, ContributorId, DBPoolId) ->
	UnicodeText = unicode:characters_to_list(Blip#blip.text),
	UpdatedUnicodeText = string:sub_string(UnicodeText, 1, Index) ++ Prop ++ string:sub_string(UnicodeText, Index+1), % Prop is already decoded
	update_blip(Blip#blip{text=unicode:characters_to_binary(UpdatedUnicodeText)}, ContributorId, text, DBPoolId),
	move_blip_elts_and_annos(Blip, Index, length(Prop), DBPoolId);

wavelet_apply_blip_operation(Blip, document_element_delete, Index, _, ContributorId, DBPoolId) ->
	delete_element({blip_id, Blip#blip.id, index, Index}, DBPoolId),
	UnicodeText = unicode:characters_to_list(Blip#blip.text),
	UpdatedUnicodeText = string:sub_string(UnicodeText, 1, Index) ++ string:sub_string(UnicodeText, Index+2),
	update_blip(Blip#blip{text=unicode:characters_to_binary(UpdatedUnicodeText)}, ContributorId, text, DBPoolId),
	move_blip_elts_and_annos(Blip, Index, -1, DBPoolId);

wavelet_apply_blip_operation(Blip, document_element_insert, Index, Prop, ContributorId, DBPoolId) ->
	move_blip_elts_and_annos(Blip, Index, 1, DBPoolId),
	create_element(Blip#blip.id, Index, getattr(Prop, "type", 0), getattr(Prop, "properties", {obj, []}), DBPoolId),
	UnicodeText = unicode:characters_to_list(Blip#blip.text),
	UpdatedUnicodeText = string:sub_string(UnicodeText, 1, Index) ++ "\n" ++ string:sub_string(UnicodeText, Index+1),
	update_blip(Blip#blip{text=unicode:characters_to_binary(UpdatedUnicodeText)}, ContributorId, text, DBPoolId);

wavelet_apply_blip_operation(Blip, document_element_delta, Index, Prop, ContributorId, DBPoolId) ->
	element_apply_delta(Blip#blip.id, Index, Prop, DBPoolId),
	update_blip(Blip, ContributorId, DBPoolId);

wavelet_apply_blip_operation(Blip, document_element_setpref, Index, Prop, ContributorId, DBPoolId) ->
	element_apply_setpref(Blip#blip.id, Index, getattr(Prop, "key", <<>>), getattr(Prop, "value", <<>>), DBPoolId),
	update_blip(Blip, ContributorId, DBPoolId);

wavelet_apply_blip_operation(#blip{id=BlipId}, blip_delete, _, _, _, DBPoolId) ->
	delete_blip({blip_id, BlipId}, DBPoolId),
	ok;

wavelet_apply_blip_operation(#blip{id=BlipId, wavelet_id=WaveletId}, blip_create_child, _, Prop, ContributorId, DBPoolId) ->
	BlipChild = create_blip(WaveletId, BlipId, ContributorId, DBPoolId),
	{getattr(Prop, "blipId", <<"(MISSING)">>), BlipChild#blip.id};

wavelet_apply_blip_operation(_, _, _, _, _, _) ->
	error.


%% Utility functions

prepare_once(Name, Query) when is_atom(Name) and is_binary(Query) ->
	case mysql:get_prepared(Name) of
		{ok, {Query, _}} ->
			Name;
		{ok, _} ->
			mysql:prepare(Name, Query),
			Name;
		{error, _} ->
			mysql:prepare(Name, Query),
			Name
	end.

check_pwd(Plain, Enc) ->
	["sha1", Seed, Hash] = string:tokens(Enc, "$"),
	Hash == sha1_hexstring(Seed ++ Plain).

datetime_to_ms({datetime, DateTime}) ->
	(calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200) * 1000.

ms_to_datetime(Ms) ->
	{datetime, calendar:gregorian_seconds_to_datetime((Ms div 1000) + 62167219200)}.

gen_random_id(0) ->
	[];
gen_random_id(Length) ->
	[nth(random:uniform(?RANDOM_ID_BASE_COUNT), ?RANDOM_ID_BASE)|gen_random_id(Length-1)].

getattr({obj, TupleList}, Name, DefaultValue) ->
	getattr(TupleList, Name, DefaultValue);

getattr(TupleList, Name, DefaultValue) ->
	proplists:get_value(Name, TupleList, DefaultValue).

setattr({obj, TupleList}, Name, Value) ->
	{obj, setattr(TupleList, Name, Value)};

setattr(TupleList, Name, Value) ->
	keystore(Name, 1, TupleList, {Name, Value}).

bin2hex(<<>>) -> "";
bin2hex(<<I:8, Rest/bytes>>) -> lists:flatten(io_lib:format("~2.16.0b", [I])) ++ bin2hex(Rest).

sha1_hexstring(String) ->
	bin2hex(crypto:sha(String)).
