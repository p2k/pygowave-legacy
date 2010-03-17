 
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

-module(operations).

-export([op_is_null/1, op_is_compatible_to/2, op_is_insert/1, op_is_delete/1, op_is_change/1]).
-export([op_length/1, op_resize/2, op_insert_string/3, op_delete_string/3]).

-export([mk_document_insert/5, mk_document_delete/5, mk_document_element_insert/6, mk_document_element_delete/4]).
-export([mk_document_element_delta/5, mk_document_element_setpref/6, mk_wavelet_add_participant/3, mk_wavelet_remove_participant/3]).
-export([mk_wavelet_append_blip/3, mk_blip_delete/3, mk_blip_create_child/4]).

-export([op_to_json/1, op_from_json/1]).

-export([transform/2]).

%% -- Erlang port of the OT Algorithm --

%% Records
-include("operation.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "Methods" on operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Return weather this operation is a null operation i.e. it does not
%% change anything.
op_is_null(#operation{type=document_insert,property=Property}) ->
	length(Property) == 0;
op_is_null(#operation{type=document_delete,property=Property}) ->
	Property == 0;
op_is_null(#operation{}) ->
	false.

%% Check if the operation can be influenced by `other_op` and vice-versa.
%% Currently all supported operations are compatible to each other (if on the same blip)
op_is_compatible_to(#operation{wave_id=WaveId,wavelet_id=WaveletId,blip_id=BlipId},
	#operation{wave_id=WaveId,wavelet_id=WaveletId,blip_id=BlipId}) ->
	true;
op_is_compatible_to(#operation{}, #operation{}) ->
	false.

%% Returns true, if this op is an insertion operation.
op_is_insert(#operation{type=document_insert}) -> true;
op_is_insert(#operation{type=document_element_insert}) -> true;
op_is_insert(#operation{}) -> false.

%% Returns true, if this op is a deletion operation.
op_is_delete(#operation{type=document_delete}) -> true;
op_is_delete(#operation{type=document_element_delete}) -> true;
op_is_delete(#operation{}) -> false.

%% Returns true, if this op is an (attribute) change operation.
op_is_change(#operation{type=document_element_delta}) -> true;
op_is_change(#operation{type=document_element_setpref}) -> true;
op_is_change(#operation{}) -> false.

%% Returns the length of this operation.
%% This can be interpreted as the distance a concurrent operation's index
%% must be moved to include the effects of this operation.
op_length(#operation{type=document_insert,property=Property}) -> length(Property);
op_length(#operation{type=document_delete,property=Property}) -> Property;
op_length(#operation{type=document_element_insert}) -> 1;
op_length(#operation{type=document_element_delete}) -> 1;
op_length(#operation{}) -> 0.

%% Delete operations: Sets the amount of deleted characters/elements to
%% `value`.
%% 
%% Other operations: No effect.
op_resize(Op, Value) when Op#operation.type == document_delete ->
	if
		Value > 0 -> Op#operation{property=Value};
		true -> Op#operation{property=0}
	end;
op_resize(Op, _) -> Op.

%% document_insert: Inserts the string into the property.
%%
%% Other operations: No effect.
op_insert_string(Op, Pos, S) when Op#operation.type == document_insert ->
	Property=Op#operation.property,
	Op#operation{property=string:concat(string:substr(Property, 1, Pos), string:concat(S, string:substr(Property, Pos+1)))};
op_insert_string(Op, _, _) -> Op.

%% document_insert: Deletes a substring from the property.
%%
%% Other operations: No effect.
op_delete_string(Op, Pos, Length) when Op#operation.type == document_insert ->
	Property=Op#operation.property,
	Op#operation{property=string:concat(string:substr(Property, 1, Pos), string:substr(Property, Pos+Length+1))};
op_delete_string(Op, _, _) -> Op.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operation composition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Requests to insert content into a document at a specific location.
mk_document_insert(WaveId, WaveletId, BlipId, Index, Content) ->
	#operation{
		type=document_insert,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		index=Index,
		property=Content
	}.

%% Requests to insert content into a document at a specific location.
mk_document_delete(WaveId, WaveletId, BlipId, Start, End) ->
	#operation{
		type=document_delete,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		index=Start,
		property=End-Start % = length
	}.

%% Requests to insert an element at the given position.
mk_document_element_insert(WaveId, WaveletId, BlipId, Index, Type, Properties) ->
	#operation{
		type=document_element_insert,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		index=Index,
		property={obj,[{"type", list_to_binary(Type)}, {"properties", Properties}]}
	}.

%% Requests to delete an element from the given position.
mk_document_element_delete(WaveId, WaveletId, BlipId, Index) ->
	#operation{
		type=document_element_delete,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		index=Index
	}.

%% Requests to apply a delta to the element at the given position.
mk_document_element_delta(WaveId, WaveletId, BlipId, Index, Delta) ->
	#operation{
		type=document_element_delta,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		index=Index,
		property=Delta
	}.

%% Requests to set a UserPref of the element at the given position.
mk_document_element_setpref(WaveId, WaveletId, BlipId, Index, Key, Value) ->
	#operation{
		type=document_element_insert,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		index=Index,
		property={obj,[
			{"key", list_to_binary(rfc4627:unicode_encode({'utf-8', Key}))},
			{"value", list_to_binary(rfc4627:unicode_encode({'utf-8', Value}))}
		]}
	}.

%% Requests to add a Participant to the Wavelet.
mk_wavelet_add_participant(WaveId, WaveletId, ParticipantId) ->
	#operation{
		type=wavelet_add_participant,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		property=ParticipantId
	}.

%% Requests to remove a Participant from the Wavelet.
mk_wavelet_remove_participant(WaveId, WaveletId, ParticipantId) ->
	#operation{
		type=wavelet_remove_participant,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		property=ParticipantId
	}.

%% Requests to append a new Blip to the Wavelet.
mk_wavelet_append_blip(WaveId, WaveletId, TempId) ->
	#operation{
		type=wavelet_append_blip,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		property={obj, [
			{"waveId", list_to_binary(WaveId)},
			{"waveletId", list_to_binary(WaveletId)},
			{"blipId", list_to_binary(TempId)}
		]}
	}.

%% Requests to delete a Blip.
mk_blip_delete(WaveId, WaveletId, BlipId) ->
	#operation{
		type=blip_delete,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId
	}.

%% Requests to create a clild Blip.
mk_blip_create_child(WaveId, WaveletId, BlipId, TempId) ->
	#operation{
		type=blip_create_child,
		wave_id=WaveId,
		wavelet_id=WaveletId,
		blip_id=BlipId,
		property={obj, [
			{"waveId", list_to_binary(WaveId)},
			{"waveletId", list_to_binary(WaveletId)},
			{"blipId", list_to_binary(TempId)}
		]}
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialization/Unserialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Serialize an OP record to a JSON structure, ready to be encoded
op_to_json(#operation{type=Type,wave_id=WaveId,wavelet_id=WaveletId,blip_id=BlipId,index=Index,property=Property}) ->
	{
		obj,
		[
			{"type", list_to_binary(string:to_upper(atom_to_list(Type)))},
			{"waveId", WaveId},
			{"waveletId", list_to_binary(WaveletId)},
			{"blipId", BlipId},
			{"index", Index},
			{"property", op_property_to_json(Type, Property)}
		]
	}.

%% Helper functions
op_property_to_json(document_insert, P) -> list_to_binary(rfc4627:unicode_encode({'utf-8', P})); % must be encoded
op_property_to_json(document_delete, P) -> P;			% already a number
op_property_to_json(document_element_insert, P) -> P;	% already a JSON-style object
op_property_to_json(document_element_delete, _) -> null;% always null
op_property_to_json(document_element_delta, P) -> P;	% already a JSON-style object
op_property_to_json(document_element_setpref, P) -> P;	% already a JSON-style object
op_property_to_json(wavelet_add_participant, P) -> P;	% don't encode an id
op_property_to_json(wavelet_remove_participant, P) -> P;% don't encode an id
op_property_to_json(wavelet_append_blip, P) -> P;		% already a JSON-style object
op_property_to_json(blip_delete, _) -> null;			% always null
op_property_to_json(blip_create_child, P) -> P.			% already a JSON-style object

%% Unserialize a JSON structure to an OP record
op_from_json({obj, Data}) ->
	Op=op_from_json(Data, #operation{}),
	Op#operation{property=op_property_from_json(Op#operation.type, Op#operation.property)}.

op_from_json([], Op) ->
	Op;
op_from_json([H|T], Op) ->
	case H of
		{"type", Atom} -> op_from_json(T, Op#operation{type=list_to_atom(string:to_lower(binary_to_list(Atom)))});
		{"waveId", WaveId} -> op_from_json(T, Op#operation{wave_id=WaveId});
		{"waveletId", WaveletId} -> op_from_json(T, Op#operation{wavelet_id=binary_to_list(WaveletId)});
		{"blipId", BlipId} -> op_from_json(T, Op#operation{blip_id=BlipId});
		{"index", Index} -> op_from_json(T, Op#operation{index=Index});
		{"property", Property} -> op_from_json(T, Op#operation{property=Property});
		_ -> op_from_json(T, Op)
	end.

%% Helper functions
op_property_from_json(document_insert, P) -> {'utf-8', Data} = rfc4627:unicode_decode(P), Data; % must be decoded
op_property_from_json(document_delete, P) -> P;				% stays a number
op_property_from_json(document_element_insert, P) -> P;		% stays a JSON-style object
op_property_from_json(document_element_delete, _) -> null;	% always null
op_property_from_json(document_element_delta, P) -> P;		% stays a JSON-style object
op_property_from_json(document_element_setpref, P) -> P;	% stays a JSON-style object
op_property_from_json(wavelet_add_participant, P) -> P;		% don't decode an id
op_property_from_json(wavelet_remove_participant, P) -> P;	% don't decode an id
op_property_from_json(wavelet_append_blip, P) -> P;			% stays a JSON-style object
op_property_from_json(blip_delete, _) -> null;				% always null
op_property_from_json(blip_create_child, P) -> P.			% stays a JSON-style object


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OT Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform([], MyOps) ->
	{[], MyOps};
transform(InOps, []) ->
	{InOps, []};
transform(InOps, MyOps) ->
	transform(InOps, [], MyOps, []).

transform(InOps, InOpsDone, [], MyOpsDone) ->
	{InOpsDone ++ InOps, MyOpsDone};
transform([], InOpsDone, [MyOp|MyOpsRest], MyOpsDone) ->
	transform(InOpsDone, [], MyOpsRest, MyOpsDone ++ [MyOp]);
transform([InOp|InOpsRest], InOpsDone, [MyOp|MyOpsRest], MyOpsDone) ->
	case transform_op(InOp, MyOp) of
		{null, null} -> transform(InOpsRest, InOpsDone, MyOpsRest, MyOpsDone);
		{null, MyOpTr} -> transform(InOpsRest, InOpsDone, [MyOpTr|MyOpsRest], MyOpsDone);
		{InOpTr, null} -> transform(InOpsRest, InOpsDone ++ [InOpTr], MyOpsRest, MyOpsDone);
		{{InOpTr, InOpNew}, MyOpTr} -> transform([InOpNew|InOpsRest], InOpsDone ++ [InOpTr], [MyOpTr|MyOpsRest], MyOpsDone);
		{InOpTr, {MyOpTr, MyOpNew}} -> transform(InOpsRest, InOpsDone ++ [InOpTr], [MyOpTr,MyOpNew|MyOpsRest], MyOpsDone);
		{InOpTr, MyOpTr} -> transform(InOpsRest, InOpsDone ++ [InOpTr], [MyOpTr|MyOpsRest], MyOpsDone)
	end.

transform_op(InOp, MyOp) ->
	AreCompatible=op_is_compatible_to(InOp, MyOp),
	if
		not(AreCompatible) ->
			{InOp, MyOp};
		true ->
			InOpIsDelete=op_is_delete(InOp),
			MyOpIsDelete=op_is_delete(MyOp),
			InOpIsInsert=op_is_insert(InOp),
			MyOpIsInsert=op_is_insert(MyOp),
			InOpIsChange=op_is_change(InOp),
			MyOpIsChange=op_is_change(MyOp),
			InOpLength=op_length(InOp),
			MyOpLength=op_length(MyOp),
			if
				InOpIsDelete and MyOpIsDelete ->
					if
						InOp#operation.index < MyOp#operation.index ->
							End = InOp#operation.index + InOpLength,
							if
								End =< MyOp#operation.index ->
									{InOp, MyOp#operation{index = MyOp#operation.index - InOpLength}};
								End < MyOp#operation.index + MyOpLength ->
									TrMyOp=op_resize(MyOp, MyOpLength - (End - MyOp#operation.index)),
									{op_resize(InOp, MyOp#operation.index - InOp#operation.index), TrMyOp#operation{index=InOp#operation.index}};
								true ->
									{op_resize(InOp, InOpLength - MyOpLength), null}
							end;
						true ->
							End = MyOp#operation.index + MyOpLength,
							if
								InOp#operation.index >= End ->
									{InOp#operation{index = InOp#operation.index - MyOpLength}, MyOp};
								InOp#operation.index + InOpLength =< End ->
									TrMyOp=op_resize(MyOp, MyOpLength - InOpLength),
									case op_is_null(TrMyOp) of
										true ->
											{null, null};
										false ->
											{null, TrMyOp}
									end;
								true ->
									TrInOp=op_resize(InOp, InOpLength - (End - InOp#operation.index)),
									TrMyOp=op_resize(MyOp, MyOpLength - (End - InOp#operation.index)),
									case op_is_null(TrMyOp) of
										true ->
											{TrInOp#operation{index=MyOp#operation.index}, null};
										false ->
											{TrInOp#operation{index=MyOp#operation.index}, TrMyOp}
									end
							end
					end;
				InOpIsDelete and MyOpIsInsert ->
					if
						InOp#operation.index < MyOp#operation.index ->
							if
								InOp#operation.index + InOpLength =< MyOp#operation.index ->
									{InOp, MyOp#operation{index = MyOp#operation.index - InOpLength}};
								true ->
									TrInOp=op_resize(InOp, MyOp#operation.index - InOp#operation.index),
									{{TrInOp, op_resize(InOp, InOpLength - op_length(TrInOp))}, MyOp#operation{index = MyOp#operation.index - op_length(TrInOp)}}
							end;
						true ->
							{InOp#operation{index = InOp#operation.index + MyOpLength}, MyOp}
					end;
				InOpIsInsert and MyOpIsDelete ->
					if
						InOp#operation.index =< MyOp#operation.index ->
							{InOp, MyOp#operation{index = MyOp#operation.index + InOpLength}};
						InOp#operation.index >= MyOp#operation.index + MyOpLength ->
							{InOp#operation{index = InOp#operation.index - MyOpLength}, MyOp};
						true ->
							TrMyOp=op_resize(MyOp, InOp#operation.index - MyOp#operation.index),
							{InOp#operation{index=MyOp#operation.index}, {TrMyOp, op_resize(MyOp, MyOpLength - op_length(TrMyOp))}}
					end;
				InOpIsInsert and MyOpIsInsert ->
					if
						InOp#operation.index =< MyOp#operation.index ->
							{InOp, MyOp#operation{index = MyOp#operation.index + InOpLength}};
						true ->
							{InOp#operation{index = InOp#operation.index + MyOpLength}, MyOp}
					end;
				InOpIsChange and MyOpIsDelete ->
					if
						InOp#operation.index > MyOp#operation.index ->
							if
								InOp#operation.index =< MyOp#operation.index + MyOpLength ->
									{InOp#operation{index=MyOp#operation.index}, MyOp};
								true ->
									{InOp#operation{index = InOp#operation.index - MyOpLength}, MyOp}
							end;
						true ->
							{InOp, MyOp}
					end;
				InOpIsChange and MyOpIsInsert ->
					if
						InOp#operation.index >= MyOp#operation.index ->
							{InOp#operation{index = InOp#operation.index + MyOpLength}, MyOp};
						true ->
							{InOp, MyOp}
					end;
				InOpIsDelete and MyOpIsChange ->
					if
						InOp#operation.index < MyOp#operation.index ->
							if
								MyOp#operation.index =< InOp#operation.index + InOpLength ->
									{InOp, MyOp#operation{index=InOp#operation.index}};
								true ->
									{InOp, MyOp#operation{index = MyOp#operation.index - InOpLength}}
							end;
						true ->
							{InOp, MyOp}
					end;
				InOpIsInsert and MyOpIsChange ->
					if
						InOp#operation.index =< MyOp#operation.index ->
							{InOp, MyOp#operation{index = MyOp#operation.index + InOpLength}};
						true ->
							{InOp, MyOp}
					end;
				((InOp#operation.type == wavelet_add_participant) and (MyOp#operation.type == wavelet_add_participant))
				or ((InOp#operation.type == wavelet_remove_participant) and (MyOp#operation.type == wavelet_remove_participant)) ->
					if
						InOp#operation.property == MyOp#operation.property -> {InOp, null};
						true -> {InOp, MyOp}
					end;
				(InOp#operation.type == blip_delete) and not(InOp#operation.blip_id == "") and not(MyOp#operation.blip_id == "") ->
					{InOp, null};
				true ->
					{InOp, MyOp}
			end
	end.
