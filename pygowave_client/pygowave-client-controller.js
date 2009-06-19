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
 * <p>This collection of JavaScript libraries represents the PyGoWave Client
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
 * Controller module.
 * @module pygowave.controller
 */

/**
 * Controller class; handles all user input and server-communication.
 * 
 * @class {public} pygowave.controller.PyGoWaveClient
 */
PyGoWaveClient = new Class({
	Implements: [Options, Events],
	options: {
		stompServer: "localhost",
		stompPort: 61613,
		stompUsername: "pygowave",
		stompPassword: "pygowave",
		
		waveAccessKeyRx: "",
		waveAccessKeyTx: ""
	},
	initialize: function(options) {
		this.setOptions(options);
		
		// The connection object must be stored in this.conn and must have a sendJson method.
		this.conn = new STOMPClient(); // STOMP is used as communication protocol
		var self = this;
		$extend(this.conn, {
			onclose: function(c) {self.onConnClose(c);},
			onerror: function(e) {self.onConnError(e);},
			onconnectedframe: function() {this.subscribe(self.options.waveAccessKeyRx, {exchange: "wavelet.topic"}); this.sendJson({"pygowave": "hi"});},
			onmessageframe: function(frame) {self.onConnReceive(JSON.decode(frame.body));},
			sendJson: function (obj) {this.send(JSON.encode(obj), self.options.waveAccessKeyTx, {exchange: "wavelet.topic"});}
		});
	},
	
	/**
	 * Callback for server connection socket.
	 * Handles connection close.
	 * 
	 * @function onConnClose
	 * @param {int} code Error code provided by socket API.
	 */
	onConnClose: function (code) {
		alert('Lost Connection, Code: ' + c);
	},
	/**
	 * Callback for server connection socket.
	 * Handles connection errors.
	 * 
	 * @function onConnError
	 * @param {int} code Error code provided by socket API.
	 */
	onConnError: function (code) {
		alert("Error: " + code);
	},
	/**
	 * Callback for server connection socket.
	 * Dispatches incoming server messages.
	 * 
	 * @function onConnReceive
	 * @param {object} obj JSON-decoded message object for processing.
	 */
	onConnReceive: function (obj) {
		
	}
});
