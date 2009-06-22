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

// Dummy functions for gettext support
if (!$defined(window.gettext)) {
	function gettext(m){return m;}
	function ngettext(s,p,c){return(c==1)?s:p;}
	function gettext_noop(m){return m;}
	function interpolate(f,o,n){if(n){return f.replace(/%\(\w+\)s/g,function(m){return String(o[m.slice(2,-2)])});}else{return f.replace(/%s/g,function(m){return String(o.shift())});}}
}

/**
 * View module.
 * @module pygowave.view
 */
pygowave.view = function () {
	
	// -- Private classes --
	
	/**
	 * Base class for all view Widgets.
	 * 
	 * @class {private} pygowave.view.Widget
	 */
	var Widget = new Class({
		Implements: Events,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {Element} contentElement DOM element to be inserted into the parent element
		 */
		initialize: function(parentElement, contentElement) {
			this.contentElement = contentElement;
			this.inject(parentElement);
		},
		
		/**
		 * Removes the widget from the DOM. Sets the parentElement to null.
		 * @function {public Widget} dispose
		 * @return Returns a reference to this widget.
		 */
		dispose: function () {
			this.contentElement.dispose();
			this.parentElement = null;
			return this;
		},
		
		/**
		 * Inject this widget somewhere else. Sets the parent element.
		 * @function {public Widget} inject
		 * @param {Element} parentElement New parent element
		 * @return Returns a reference to this widget.
		 */
		inject: function (parentElement) {
			this.parentElement = parentElement;
			this.contentElement.inject(this.parentElement);
		}
	});
	
	/**
	 * A widget to display a participant i.e. his/her avatar and a online
	 * indicator.
	 *
	 * @class {private} pygowave.view.ParticipantWidget
	 * @extends pygowave.view.Widget
	 */
	var ParticipantWidget = new Class({
		Extends: Widget,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {pygowave.model.Participant} participant Participant object
		 * @param {Boolean} online True if the participant is online; renders a online indicator
		 */
		initialize: function (view, parentElement, participant, online) {
			this._view = view;
			var contentElement = new Element('div', {'class': 'wavelet_participant'});
			var tn = participant.thumbnailUrl();
			if (tn == "")
				tn = view.defaultThumbnailUrl();
			this._pImage = new Element('img', {
				'src': tn,
				'class': 'thumbnail',
				'alt': participant.displayName(),
				'title': participant.displayName()
			}).inject(contentElement, 'bottom');
			this.parent(parentElement, contentElement);
			participant.addEvent('onlineStateChanged', this._onOnlineStateChanged.bind(this));
			this._onOnlineStateChanged(online);
		},
		
		/**
		 * Callback for {@link pygowave.model.Participant.onOnlineStateChanged onOnlineStateChanged}.
		 * Shows/hides the online indicator.<br/>
		 * Note: The behaviour depends on your stylesheet. This method toggles
		 * between the 'offline' and 'online' classes of the element.
		 * @function {public} _onOnlineStateChanged
		 * @param {Boolean} online True if the participant is online
		 */
		_onOnlineStateChanged: function (online) {
			this._online = online;
			if (!$defined(this._oImage))
				this._oImage = new Element('div', {'class': 'indicator'}).inject(this.contentElement, 'bottom');
			
			this._oImage.addClass(online ? 'online' : 'offline').removeClass(online ? 'offline' : 'online');
		},
		
		/**
		 * Return if the participant is online.
		 * @function {public Boolean} isOnline
		 */
		isOnline: function () {
			return this._online;
		}
	});
	
	/**
	 * This Widget renders a standard Wavelet view. The participants are listed
	 * at top and the "Add participant" button is placed there as well.
	 * Below the participants the actual Wavelet content is rendered.
	 * 
	 * @class {private} pygowave.view.WaveletWidget
	 * @extends pygowave.view.Widget
	 */
	var WaveletWidget = new Class({
		Extends: Widget,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {pygowave.model.Wavelet} wavelet Wavelet to be rendered
		 */
		initialize: function (view, parentElement, wavelet) {
			this._view = view;
			var contentElement = new Element('div', {'class': 'wavelet_widget'});
			this._participantsDiv = new Element('div', {'class': 'wavelet_participants_div'}).inject(contentElement);
			this.parent(parentElement, contentElement);
			this._wavelet = wavelet;
			this._participantWidgets = new Hash();
			this.updateParticipants();
		},
		
		/**
		 * Update the participants list. Adds and removes participant widgets
		 * as appropriate.
		 * 
		 * @function {public} updateParticipants
		 */
		updateParticipants: function () {
			var ids = this._wavelet.allParticipantIDs();
			ids.each(function (id) {
				if (!this._participantWidgets.has(id)) {
					var wgt = new ParticipantWidget(this._view, this._participantsDiv, this._wavelet.participant(id), false);
					this._participantWidgets.set(id, wgt);
				}
			}, this);
			this._participantWidgets.getKeys().each(function (id) {
				if (!ids.contains(id)) {
					this._participantWidgets.get(id).dispose();
					this._participantWidgets.erase(id);
				}
			}, this);
		}
	});
	
	// -- Public classes --
	
	/**
	 * Main Wave view class. Renders changes of the given model to HTML.
	 *
	 * @class {public} pygowave.view.WaveView
	 */
	var WaveView = new Class({
		Implements: [Options, Events],
		options: {
			participantSearchUrl: "about:blank",
			gadgetLoaderUrl: "about:blank",
			defaultThumbnailUrl: "about:blank"
		},
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {pygowave.model.WaveModel} model Model to connect to
		 * @param {Element} container Container DOM element to render the view on
		 * @param {Object} options Various settings to correctly render the view
		 * @... {String} participantSearchUrl URL to the participant serach view;
		 *      prepared to have the query appended *subject to change*
		 * @... {String} gadgetLoaderUrl URL to the gadget loader view; prepared
		 *      to have the gadget URL appended
		 * @... {String} defaultThumbnailUrl URL to a thumbnail image to be used
		 *      if the participant has not uploaded his own.
		 */
		initialize: function (model, container, options) {
			this.setOptions(options);
			this.model = model;
			this.container = container;
			this.waveletWidgets = new Hash();
			
			var rwv = this.model.rootWavelet();
			if (rwv != null) {
				this.rootWaveletWidget = new WaveletWidget(this, this.container);
				this.waveletWidgets.set(rwv.id(), this.rootWaveletWidget);
			}
			else
				this.rootWaveletWidget = null;
			
			// Connect event listeners
			this.model.addEvent('participantsChanged', this._onModelParticipantsChanged.bind(this));
			this.model.addEvent('waveletAdded', this._onModelWaveletAdded.bind(this));
		},
		
		/**
		 * Callback for {@link pygowave.model.WaveModel.onParticipantsChanged onParticipantsChanged}.
		 * @function {private} _onModelParticipantsChanged
		 * @param {String} waveletId ID of the Wavelet whose participants changed
		 */
		_onModelParticipantsChanged: function (waveletId) {
			if (this.waveletWidgets.has(waveletId))
				this.waveletWidgets.get(waveletId).updateParticipants(this.model.wavelet(waveletId));
		},
		
		/**
		 * Callback for {@link pygowave.model.WaveModel.onWaveletAdded onWaveletAdded}.
		 * This only handles the root Wavelet for now.
		 * @param {String} waveletId ID of the Wavelet that has been added
		 * @param {Boolean} isRoot True if this is the (new) root Wavelet
		 */
		_onModelWaveletAdded: function (waveletId, isRoot) {
			if (!isRoot)
				return;
			
			var rwv = this.model.wavelet(waveletId);
			this.rootWaveletWidget = new WaveletWidget(this, this.container, rwv);
			this.waveletWidgets.set(rwv.id(), this.rootWaveletWidget);
		},
		
		/**
		 * Returns the default thumbnail URL for participants without thumbnail.
		 * @function {public String} defaultThumbnailUrl
		 */
		defaultThumbnailUrl: function () {
			return this.options.defaultThumbnailUrl;
		}
	});
	
	return {
		WaveView: WaveView
	};

}();
