/*
 * PyGoWave Client Script a.k.a. Microwave
 * Copyright (C) 2009 by p2k and avital
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/*
	Note to our valued source code readers and potential hackers:
	The ID numbers used in this script are completely random and are generated
	each time a user visits a Wave. If the user leaves the Wave, the IDs will
	be rendered useless. Suspicious traffic sent from a client will be detected
	and results in instant disconnection from the server.
*/

/*
	Note to our developers:
	Please consequently wrap up every text with the appropriate gettext() call.
	
	Sidenote: JavaScript > Java
*/

$(document).ready(function() {
	
	var logger = Orbited.getLogger();
	
	// --- User Interface Setup ---
	
	// Add participant / searchbox
	var okcancel = {};
	var searchBoxSelected = null;
	okcancel[gettext("Cancel")] = function () {
		$(this).dialog("close");
		$("#p_searchresult").html("");
		$("#p_searchbox").val("");
	};
	okcancel[gettext("OK")] = function() {
		if (searchBoxSelected == null) {
			alert(gettext("Please search and select a participant from the list."));
			return;
		}
		$(this).dialog("close");
		addParticipant($(searchBoxSelected).children(".participant_id").text());
		$("#p_searchresult").html("");
		$("#p_searchbox").val("");
	}
	var setSearchboxHeight = function() {
		$("#p_searchresult").css("height",
								 $("#add_participant").parent().height()
								 - $("#p_searchresult").position().top
								 - $("#add_participant").nextAll(".ui-dialog-buttonpane").height()
								 - 25);
	};
	$('<div id="add_participant" title="'+gettext("Add participant")+'">\n'+gettext("Search")+': <input id="p_searchbox" type="text" /><div id="p_searchresult"></div></div>')
	.appendTo(document.body)
	.dialog({
		autoOpen: false,
		width: 210,
		minWidth: 210,
		height: 410,
		buttons: okcancel,
		resize: setSearchboxHeight
	});
	
	// Show dialog
	$("#add_participant_div > img").click(function (event) {
		var p = $(this).offset();
		$("#add_participant").dialog("option", "position", [p.top, p.left]);
		$("#add_participant").dialog("open");
		setSearchboxHeight();
	});
	
	// Typing in the searchbox	
	$("#p_searchbox").keyup(function () {
		if ($(this).val() == "")
			$("#p_searchresult").html("");
		else
			$("#p_searchresult").load(ParticipantSearchURL + encodeURIComponent($(this).val()), function() {
				$(".searchresult_item")
				.mouseenter(function(){$(this).addClass("ui-state-hover");})
				.mouseleave(function(){$(this).removeClass("ui-state-hover");})
				.click(function() {
					if (searchBoxSelected != null)
						$(searchBoxSelected).removeClass("ui-state-active");
					if (this == searchBoxSelected) { // Deselect
						searchBoxSelected = null;
						return;
					}
					$(this).addClass("ui-state-active");
					searchBoxSelected = this;
				});
			});
	});
	
	// Leave Wave
	$("#leave_button")
	.mouseenter(function(){$(this).addClass("ui-state-hover");})
	.mouseleave(function(){$(this).removeClass("ui-state-hover");})
	.click(function (event) {
		if (confirm(gettext("Do you really want to leave the Wave?\n\nWarning: If you do so, you cannot come back\nto this wave unless someone adds you again.")))
			leaveWave();
	});
	
	// Set Gadget
	$("#gadget_selector")
	.change(function () {
		if ($(this).val() == "")
			return; // TODO: delete gadget
		setGadget($(this).val());
	});
	
	// Gadget frame
	$("#gadget_container")
	.css("height", "400px")
	.css("width", "600px");
	
	// --- Message Queue Connection ---
	var stomp = new STOMPClient();
	
	$.extend(stomp, {
		onclose: function(c) {
			alert('Lost Connection, Code: ' + c);
		},
		onerror: function(error) {
			alert("Error: " + error);
		},
		onconnectedframe: function() {
			this.subscribe("", {id: WaveAccessKeyRx+"-queue", exchange: "wavelet.topic", routing_key: WaveAccessKeyRx});
			this.send_json({"pygowave": "hi"}); // Say "hi" to the server
		},
		onmessageframe: function(frame) {
			obj = $.evalJSON(frame.body);
			processMessages(obj);
		},
		send_json: function (obj) {
			this.send($.toJSON(obj), WaveAccessKeyTx, {exchange: "wavelet.topic"});
		}
	});
	
	stomp.connect("localhost", 61613, "pygowave", "pygowave");
	
	// --- PyGoWave Wire Protocol Implementation ---
	var participants = {};
	var newParticipantId = 0;
	var myOffset = 0;
	var gadgetData = {};
	var getParticipantsForGadget = function() {
		return {
			myId: myOffset,
			authorId: 0,
			participants: participants
		};
	}
	var doAddParticipant = function (participant) {
		var i = newParticipantId;
		participants[newParticipantId] = participant;
		newParticipantId += 1;
		
		var html = '<div class="participant">';
		if (participant.profileUrl != "")
			html += '<a href="' + participant.profileUrl + '">';
		if (participant.thumbnailUrl == "")
			participant.thumbnailUrl = AvatarURL + "default.png";
		html += '<img id="participant-' + i + '" src="' + participant.thumbnailUrl;
		html += '" width="32" height="32" alt="' + participant.displayName;
		html += '" title="' + participant.displayName + '" />';
		if (participant.profileUrl != "")
			html += "</a>";
		html += '<br/><label for="participant-' + i + '">' + participant.displayName;
		html += '</label></div>';
		
		$(html)
		.insertBefore($("#add_participant_div"))
		.hide()
		.show("drop", {direction: "up"}, 500);
	};
	
	var processMessages = function (messages) {
		for (var i = 0; i < messages.length; i++) {
			var msg = messages[i];
			
			switch(msg.type) {
				case "WAVELET_ADD_PARTICIPANT":
					if (msg.property.id == MyID)
						myOffset = newParticipantId;
					doAddParticipant(msg.property);
					invokeRPCCallbacks("wave_participants", getParticipantsForGadget());
					break;
				case "WAVELET_REMOVE_PARTICIPANT":
					if (msg.property == MyID)
						document.location = "../"; // I killed myself :)
					else {
						var i; var found = false;
						for (i in participants) {
							if (participants[i].id == msg.property) {
								found = true;
								break;
							}
						}
						if (found) {
							delete participants[i];
							invokeRPCCallbacks("wave_participants", getParticipantsForGadget());
							$("#participant-" + i).hide("drop", {direction: "up"}, 500, function () {
								$(this).remove();
							});
						}
					}
					break;
				case "DOCUMENT_ELEMENT_REPLACE":
					gadgetData = msg.property.data;
					$("#gadget_frame").attr("src", GadgetLoaderURL + "url=" + encodeURIComponent(msg.property.url) + "&gadget_id=" + msg.property.id);
					$("#gadget_frame").ready(function () {
						// Dummy
					});
					break;
				case "DOCUMENT_ELEMENT_DELTA":
					$.extend(gadgetData, msg.property.delta); // Apply delta
					invokeRPCCallbacks("wave_gadget_state", gadgetData); // Callback
					break;
			}
		}
	};
	
	gadgetFrameOnLoad = function () {
		invokeOnLoadCallbacks();
		invokeRPCCallbacks("wave_participants", getParticipantsForGadget());
		invokeRPCCallbacks("wave_gadget_state", gadgetData);
	}
	
	// --- User actions ---
	var addParticipant = function (id) {
		stomp.send_json({
			type: "WAVELET_ADD_PARTICIPANT",
			property: id
		});
	};
	var leaveWave = function () {
		stomp.send_json({
			type: "WAVELET_REMOVE_SELF",
			property: null
		});
	};
	var setGadget = function (id) {
		stomp.send_json({
			type: "DOCUMENT_ELEMENT_REPLACE",
			property: id
		});
	};
	
	// --- Gadget callback routines ---
	var rpc_callbacks = Array();
	var rpc_load_callbacks = Array();
	window.gadget_rpc = {
		call: function (gadgetId, targetId, serviceName, callback, var_args) {
			if (targetId == null) {
				switch (serviceName) {
					case "wave_gadget_state":
						// TODO
						break;
					case "wave_log":
						logger.info(var_args);
						break;
					case "wave_enable":
						// TODO
						break;
				}
			}
		},
		register: function (gadgetId, serviceName, handler) {
			rpc_callbacks.push({gadgetId: gadgetId, serviceName: serviceName, handler: handler});
		},
		registerOnLoadHandler: function (gadgetId, handler) {
			rpc_callbacks.push({gadgetId: gadgetId, handler: handler});
		}
	}
	
	var invokeRPCCallbacks = function (serviceName, var_args) {
		for (var i = 0; i < rpc_callbacks.length; i++) {
			if (rpc_callbacks[i].serviceName == serviceName)
				rpc_callbacks[i].handler(var_args);
		}
	};
	var invokeOnLoadCallbacks = function () {
		for (var i = 0; i < rpc_load_callbacks.length; i++)
			rpc_load_callbacks[i].handler();
	};
});
