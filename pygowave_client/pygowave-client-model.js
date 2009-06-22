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

/**@scope pygowave*/
window.pygowave = $defined(window.pygowave) ? window.pygowave : {};

/**
 * Models module.
 * @module pygowave.model
 */
pygowave.model = function () {
	// -- Private classes --
	
	/**
	 * Models a participant to a Wavelet. Note that the implementation (i.e.
	 * the controller) should only create one participant object per participant
	 * because view objects are connected to only one participant instance.
	 * If the state of a participant changes they can be updated as appropriate.
	 * @class {public} pygowave.model.Participant
	 */
	var Participant = new Class({
		Implements: [Options, Events],
		options: {
			displayName: "",
			thumbnailUrl: "",
			profileUrl: "",
			isOnline: false,
			isBot: false
		},
		
		// --- Event documentation ---
		/**
		 * Fired if the participant's online state changes.
		 * @event onOnlineStateChanged
		 * @param {Boolean} online True, if the participant is now online.
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {String} id ID (address) of the participant
		 * @param {Object} options Additional information:
		 * @... {String} displayName Display name
		 * @... {String} thumbnailUrl URL of the participant's avatar
		 * @... {String} profileUrl A link to the participant's profile
		 * @... {Boolean} isOnline True, if the participant is online
		 * @... {Boolean} isBot True, if the participant is a robot
		 */
		initialize: function(id, options) {
			this.setOptions(options);
			this._id = id;
			if (this.options.name == "")
				this.options.name = id;
		},
		
		/**
		 * Returns the ID (address) of the participant.
		 * @function {public String} id
		 */
		id: function () {return this._id;},
		
		/**
		 * Returns the participant's display name.
		 * @function {public String} displayName
		 */
		displayName: function () {return this.options.displayName;},
		
		/**
		 * Returns the URL of the participant's avatar.
		 * @function {public String} thumbnailUrl
		 */
		thumbnailUrl: function () {return this.options.thumbnailUrl;},
		
		/**
		 * Returns the URL to the participant's profile.
		 * @function {public String} profileUrl
		 */
		profileUrl: function () {return this.options.profileUrl;},
		
		/**
		 * Returns weather the participant is a bot.
		 * @function {public Boolean} isBot
		 */
		isBot: function () {return this.options.isBot},
		
		/**
		 * Returns weather the participant is a online.
		 * @function {public Boolean} isBot
		 */
		isOnline: function () {return this.options.isOnline},
		
		/**
		 * Set the participant's online state.<br/>
		 * Fires onOnlineStateChanged if changed.
		 * @function {public} setOnline
		 * @param {Boolean} online True, if the participant is now online.
		 */
		setOnline: function (online) {
			if (this.options.isOnline != online) {
				this.options.isOnline = online;
				this.fireEvent('onlineStateChanged', online);
			}
		},
		
		/**
		 * Convenience function to serialize a participant object into the
		 * Wave Gadget API format.
		 * @function {public Object} toGadgetFormat
		 */
		toGadgetFormat: function () {
			return {
				id: this._id,
				displayName: this.options.displayName,
				thumbnailUrl: this.options.thumbnailUrl
			};
		}
	});
	
	/**
	 * Models a Wavelet on a Wave.<br/>
	 * This is a private class and cannot be instanitated directly. Please
	 * use {@link pygowave.model.WaveModel.createWavelet WaveModel.createWavelet}.
	 * @class {private} pygowave.model.Wavelet
	 */
	var Wavelet = new Class({
		Implements: Options,
		options: {
			creator: null,
			is_root: false,
			root_blip: null,
			created: null,
			last_modified: null,
			title: "",
			version: 0
		},
		
		/**
		 * Called on instantiation. Documented for internal purposes.
		 * @constructor {public} initialize
		 * @param {WaveModel} wave Parent WaveModel object
		 * @param {String} id Wavelet ID
		 * @param {Object} options Information about the Wavelet. Possible values:
		 * @... {Participant} creator Creator of this Wavelet
		 * @... {Boolean} is_root True if this is the root Wavelet; if this value
		 *      is set and the parent WaveModel does not have a root Wavelet yet,
		 *      this Wavelet is set as the Wave's root Wavelet
		 * @... {Date} created Date of creation
		 * @... {Date} last_modified Date of last modification
		 * @... {String} title Title of the Wavelet
		 * @... {int} version Version of the Wavelet
		 */
		initialize: function(wave, id, options) {
			this.setOptions(options);
			this._wave = wave;
			if (this.options.is_root) {
				if (this._wave.rootWavelet() == null)
					this._wave._setRootWavelet(this);
				else
					this.options.is_root = false;
			}
			this._id = id;
			this._participants = new Hash();
			this._blips = new Array();
		},
		
		/**
		 * Returns true, if this Wavelet is the Wave's root Wavelet.
		 * @function {public Boolean} isRoot
		 */
		isRoot: function () {
			return this.options.is_root;
		},
		
		/**
		 * Returns the ID of this Wavelet.
		 * @function {public String} id
		 */
		id: function () {return this._id;},
		
		/**
		 * Add a participant to this Wavelet.<br/>
		 * Note: Triggers {@link pygowave.model.WaveModel.onParticipantsChanged onParticipantsChanged}
		 * of the WaveModel.
		 * @function {public} addParticipant
		 * @param {Participant} participant Participant to be added
		 */
		addParticipant: function (participant) {
			this._participants.set(participant.id(), participant);
			this._wave.fireEvent('participantsChanged', this._id);
		},
		
		/**
		 * Returns the Participant object with the given id, if the participant
		 * resides on this Wavelet. Returns null otherwise.
		 * @function {Participant} participant
		 * @param {String} id ID of the 
		 */
		participant: function (id) {
			return this._participants.get(id);
		},
		
		/**
		 * Returns a list of all participants on this Wavelet.
		 * @function {public Participant[]} allParticipants
		 */
		allParticipants: function () {
			return this._participants.getValues();
		},
		
		/**
		 * Returns a list of all IDs of the participants on this Wavelet.
		 * @function {public String[]} allParticipantIDs
		 */
		allParticipantIDs: function () {
			return this._participants.getKeys();
		},
		
		/**
		 * Convenience function to serialize all participants into the Wave
		 * Gadget API format.
		 * @function {public Object} allParticipantsForGadget
		 */
		allParticipantsForGadget: function () {
			var ret = {};
			this._participants.each(function (participant, id) {
				ret[id] = participant.toGadgetFormat();
			});
			return ret;
		}
	})
	
	// -- Public classes --
	
	/**
	 * Wave model class. Top level model which sends all events.
	 * 
	 * @class {public} pygowave.model.WaveModel
	 */
	var WaveModel = new Class({
		Implements: Events,
		
		// --- Event documentation ---
		/**
		 * Fired if a participant joins or leaves.
		 * @event onParticipantsChanged
		 * @param {String} waveletId ID of the Wavelet whose participants changed
		 */
		
		/**
		 * Fired if a Wavelet has been added.
		 * @event onWaveletAdded
		 * @param {String} waveletId ID of the Wavelet that has been added
		 * @param {Boolean} isRoot True if this is the (new) root Wavelet
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {String} waveId ID of the Wave
		 * @param {String} viewerId ID of the viewer
		 */
		initialize: function (waveId, viewerId) {
			this._rootWavelet = null;
			this._waveId = waveId;
			this._viewerId = viewerId;
			this._wavelets = new Hash();
		},
		
		/**
		 * Create a Wavelet and add it to this Wave. For options see the
		 * {@link pygowave.model.Wavelet.initialize Wavelet constructor}.<br/>
		 * Hint: Emits {@link pygowave.model.WaveModel.onWaveletAdded onWaveletAdded}
		 * @function {public Wavelet} createWavelet
		 */
		createWavelet: function (id, options) {
			var w = new Wavelet(this, id, options);
			this._wavelets.set(id, w);
			this.fireEvent('waveletAdded', [id, w.isRoot()]);
			return w;
		},
		
		/**
		 * Return a Wavelet of this Wave by its ID.
		 * @function {public Wavelet} wavelet
		 * @param {String} waveletId ID of the Wavelet
		 */
		wavelet: function (waveletId) {
			return this._wavelets.get(waveletId);
		},
		
		/**
		 * Returns the root Wavelet of this Wave
		 * @function {public Wavelet} rootWavelet
		 */
		rootWavelet: function () {
			return this._rootWavelet;
		},
		
		/**
		 * Internal method to set the root Wavelet. Not intended to be called
		 * outside of this implementation.
		 * @function {private} _setRootWavelet
		 * @param {Wavelet} wavelet Wavelet to be set as root Wavelet
		 */
		_setRootWavelet: function (wavelet) {
			this._rootWavelet = wavelet;
		}
	});
	
	return {
		WaveModel: WaveModel,
		Participant: Participant
	};

}();
