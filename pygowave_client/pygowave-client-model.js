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
 * Models module.
 * @module pygowave.model
 */

/**
 * Wave model class. Top level model which sends all events.
 * 
 * @class {public} pygowave.model.WaveModel
 */
WaveModel = new Class({
	Implements: [Options, Events],
	options: {
		id: "",
		rootWavelet: null,
		onParticipantsChanged: $empty
	},
	/**
	 * 
	 * @constructor {public} initialize
	 * @param {object} options Hand over initial settings and objects. Possible
	 *     values: id - WaveId; rootWavelet - wavelet object from JSON wire
	 *     protocol; onParticipantsChanged - function to call on participant change
	 */
	initialize: function(options) {
		this.setOptions(options);
		this.wavelets = {};
		
		if ($defined(this.options.rootWavelet))
			this.wavelets[this.options.rootWavelet.id] = this.options.rootWavelet;
	},
	
	loadFromJSON: function(waveobj) {
		
	},
	createWavelet: function (options) {
		
	},
	wavelet: function (waveletId) {
		return this.wavelets[waveletId];
	},
	rootWavelet: function () {
		return this.options.rootWavelet;
	}
});

Participant = new Class({
	Implements: Options,
	options: {
		id: "",
		name: "",
		thumbnailURL: "",
		profileURL: "",
		isBot: false
	},
	
	initialize: function(options) {
		this.setOptions(options);
	},
	
	id: function () {return this.options.id;},
	name: function () {return this.options.name;},
	thumbnailURL: function () {return this.options.thumbnailURL;},
	profileURL: function () {return this.options.profileURL;}
});

Wavelet = new Class({
	Implements: Options,
	options: {
		id: "",
		wave: null,
		creator: null,
		is_root: false,
		root_blip: null,
		created: null,
		last_modified: null,
		title: "",
		version: 0
	},
	
	initialize: function(options) {
		this.setOptions(options);
		this.participants = {};
		this.blips = new Array();
	},
	
	addParticipant: function (data) {
		var p = new Participant(data);
		this.participants[p.id] = p;
		
		this.options.wave.fireEvent('participantsChanged', this.options.id);
	},
	participant: function (id) {
		return this.participants[id];
	},
	allParticipants: function () {
		return this.participants;
	}
});

