/*
 * PyGoWave Client Script a.k.a. Microwave
 * Copyright (C) 2009 by p2k
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

/**
 * <p>This collection of JavaScript libraries represent the PyGoWave Client
 * interface for web browsers. It communicates seamlessly with any PyGoWave
 * Server instance.</p>
 * <p>PyGoWave Client is designed with the Model-View-Controller design
 * pattern.</p>
 * <p>To set up PyGoWave Client for proper operation, you must include
 * Orbited.js and stomp.js before any of these scripts. Then include all three
 * libraries in arbitrary order. (TODO...)</p>
 * 
 * @project PyGoWave Client
 * @author p2k - patrick.p2k.schneider@gmail.com
 * @version 0.1.0
 * @description PyGoWave JavaScript Client a.k.a. Microwave
 */

/**
 * Root object of the namespace.
 *
 * @namespace pygowave
 */
window.pygowave = $defined(window.pygowave) ? window.pygowave : {};

/**@scope pygowave*/

/**
 * Controller module.
 * @module pygowave.controller
 */
pygowave.controller = function () {
	// -- Private classes --
	
	// -- Public classes --
	
	/**
	 * Controller class; handles all user input and server-communication.
	 * 
	 * @class {public} pygowave.controller.PyGoWaveClient
	 */
	var PyGoWaveClient = new Class({
		Implements: [Options, Events],
		options: {
			stompServer: "localhost",
			stompPort: 61613,
			stompUsername: "pygowave_client",
			stompPassword: "pygowave_client",
			
			waveAccessKeyRx: "",
			waveAccessKeyTx: "",
			initialWave: "",
			initialWavelet: "",
			viewerId: ""
		},
		
		// --- Event documentation ---
		/**
		 * Fired on wavelet opening.
		 * @event onWaveletOpened
		 * @param {String} wave_id ID of the wave
		 * @param {String} wavelet_id ID of the wavelet
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {pygowave.model.WaveModel} model Initial wave model
		 * @param {pygowave.view.WaveView} view Initial wave view
		 * @param {Object} options Client configuration options
		 */
		initialize: function(model, view, options) {
			this.setOptions(options);
			
			this._iview = view;
			this._iview.addEvent('textInserted', this._onTextInserted.bind(this));
			this._iview.addEvent('textDeleted', this._onTextDeleted.bind(this));
			
			this.waves = new Hash();
			this.waves.set(model.id(), model);
			
			this.wavelets = new Hash();
			this.participants = new Hash();
			
			// The connection object must be stored in this.conn and must have a sendJson and subscribeWavelet method (defined below).
			this.conn = new STOMPClient(); // STOMP is used as communication protocol
			var self = this;
			$extend(this.conn, {
				onclose: function(c) {self.onConnClose(c);},
				onerror: function(e) {self.onConnError(e);},
				onconnectedframe: function() {
					self.openWavelet(self.options.initialWave, self.options.initialWavelet);
				},
				onmessageframe: function(frame) {
					self.onConnReceive(frame.headers.destination.split(".")[1], JSON.decode(frame.body));
				},
				sendJson: function (wavelet_id, obj) {
					this.send(
						JSON.encode(obj),
						self.options.waveAccessKeyTx + "." + wavelet_id + ".clientop",
						{
							exchange: "wavelet.topic",
							"content-type": "application/json"
						}
					);
				},
				subscribeWavelet: function (wavelet_id) {
					this.subscribe(
						self.options.waveAccessKeyRx + "." + wavelet_id + ".waveop",
						{
							routing_key: self.options.waveAccessKeyRx + "." + wavelet_id + ".waveop",
							exchange: "wavelet.direct",
							exclusive: true
						}
					);
					this.sendJson(wavelet_id, {"type": "WAVELET_OPEN"});
				}
			});
		},
		
		/**
		 * Callback for server connection socket.
		 * Handles connection close.
		 * 
		 * @function {private} onConnClose
		 * @param {int} code Error code provided by socket API.
		 */
		onConnClose: function (code) {
			alert('Lost Connection, Code: ' + code);
		},
		
		/**
		 * Callback for server connection socket.
		 * Handles connection errors.
		 * 
		 * @function {private} onConnError
		 * @param {int} code Error code provided by socket API.
		 */
		onConnError: function (code) {
			alert("Error: " + code);
		},
		
		/**
		 * Callback for server connection socket.
		 * Dispatches incoming server messages.
		 * 
		 * @function {private} onConnReceive
		 * @param {object} obj JSON-decoded message object for processing.
		 */
		onConnReceive: function (wavelet_id, obj) {
			for (var it = new _Iterator(obj);it.hasNext();) {
				obj = it.next();
				switch (obj.type) {
					case "WAVELET_OPEN":
						this._collateParticipants(obj.property.wavelet.participants);
						
						var wave_id = obj.property.wavelet.waveId;
						var wave_model = this.waves[wave_id];
						wave_model.loadFromSnapshot(obj.property, this.participants);
						this.wavelets[wavelet_id] = {
							model: wave_model,
							pending: false
						};
						this._setupOpManagers(wave_id, wavelet_id);
						this.fireEvent("waveletOpened", {"wave_id": wave_id, "wavelet_id": wavelet_id});
						break;
				}
			}
		},
		
		/**
		 * Connect to the server and load the initial wavelet
		 * @function {public} connect
		 */
		connect: function () {
			this.conn.connect(
				this.options.stompServer,
				this.options.stompPort,
				this.options.stompUsername,
				this.options.stompPassword
			);
		},
		
		/**
		 * Open a wavelet.
		 * @function {public} openWavelet
		 * @param {String} wave_id ID of the wave of the wavelet
		 * @param {String} wavelet_id ID of the wavelet to open
		 */
		openWavelet: function (wave_id, wavelet_id) {
			if (!this.waves.has(wave_id))
				this.waves[wave_id] = new pygowave.model.WaveModel(wave_id, this.options.viewerId);
			
			this.conn.subscribeWavelet(wavelet_id);
		},
		
		/**
		 * Returns true if this wavelet has pending operations.
		 * @function {public Boolean} hasPendingOperations
		 * @param {String} wavelet_id ID of the wavelet
		 */
		hasPendingOperations: function (wavelet_id) {
			return this.wavelets[wavelet_id].pending;
		},
		
		/**
		 * Collate the internal participant "database" with the given ID list.
		 * @function {private} _collateParticipants
		 * @param {String[]} id_list List of participant IDs
		 */
		_collateParticipants: function (id_list) {
			for (var it = new _Iterator(id_list);it.hasNext();) {
				var id = it.next();
				if (!this.participants.has(id))
					this.participants.set(id, new pygowave.model.Participant(id));
			}
		},
		
		/**
		 * Setup two new OpManagers, one for pending and one for cached operations.
		 * Connect them to the controller using closures.
		 *
		 * @function {private} _setupOpManagers
		 * @param {String} wave_id ID of the Wave
		 * @param {String} wavelet_id ID of the Wavelet
		 */
		_setupOpManagers: function (wave_id, wavelet_id) {
			this.wavelets[wavelet_id].mpending = new pygowave.operations.OpManager(wave_id, wavelet_id);
			this.wavelets[wavelet_id].mcached = new pygowave.operations.OpManager(wave_id, wavelet_id);
			var self = this;
			this.wavelets[wavelet_id]._mcached_wrapper = function (start, end) {
				if (!self.hasPendingOperations(wavelet_id))
					self._transferOperations(wavelet_id);
			};
			this.wavelets[wavelet_id].mcached.addEvent('afterOperationsInserted', this.wavelets[wavelet_id]._mcached_wrapper);
		},
		/**
		 * Send ready made operations to the server.
		 *
		 * @function {private} _transferOperations
		 * @param {String} wavelet_id ID of the Wavelet
		 */
		_transferOperations: function (wavelet_id) {
			var mpending = this.wavelets[wavelet_id].mpending;
			var mcached = this.wavelets[wavelet_id].mcached;
			mpending.put(mcached.fetch());
			this.wavelets[wavelet_id].pending = true;
			//self.processOperations.emit(self.__version, simplejson.dumps(self.opsPending.serialize(False)))
		},
		
		/**
		 * Callback from view on text insertion.
		 *
		 * @function {private} _onTextInserted
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} index Start index of insertion
		 * @param {String} content Inserted content
		 */
		_onTextInserted: function (waveletId, blipId, index, content) {
			this.wavelets[waveletId].mcached.documentInsert(blipId, index, content);
		},
		/**
		 * Callback from view on text deletion.
		 *
		 * @event {private} _onTextDeleted
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} start Start index of deletion
		 * @param {int} end End index of deletion
		 */
		_onTextDeleted: function (waveletId, blipId, start, end) {
			this.wavelets[waveletId].mcached.documentDelete(blipId, start, end);
		}
	});
	
	return {
		PyGoWaveClient: PyGoWaveClient
	};
	
}();