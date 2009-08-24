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
window.pygowave = $defined(window.pygowave) ? window.pygowave : new Hash(); 

pygowave.view = $defined(pygowave.view) ? pygowave.view : new Hash();

/**
 * Main module. Contains Widgets and Classes for displaying waves.
 * 
 * @module pygowave.view.view
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	var AddParticipantWidget = pygowave.view.AddParticipantWidget;
	var ParticipantWidget = pygowave.view.ParticipantWidget;
	var BlipEditorWidget = pygowave.view.BlipEditorWidget;
	var WaveletStatusBar = pygowave.view.WaveletStatusBar;
	
	/**
	 * A widget for rendering multiple BlipWidgets in a wavelet.
	 * @class {private} pygowave.view.BlipContainerWidget
	 * @extends pygowave.view.Widget
	 */
	var BlipContainerWidget = new Class({
		Extends: Widget,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 */
		initialize: function (view, parentElement) {
			this._view = view;
			var contentElement = new Element('div', {'class': 'blip_container_widget'});
			this.parent(parentElement, contentElement);
			this._blips = [];
			this._rootBlip = null;
		},
		
		/**
		 * Insert a new BlipEditorWidget at the specified index. Returns the
		 * created blip editor widget.
		 * 
		 * @function {public BlipEditorWidget} insertBlip
		 * @param {int} index Index to insert the BlipEditorWidget
		 * @param {pygowave.model.Blip} blip Blip object to connect the new
		 *        BlipEditorWidget to
		 */
		insertBlip: function (index, blip) {
			var where = 'bottom';
			var parentElement = this.contentElement;
			if (index != this._blips.length) {
				where = 'before';
				parentElement = this._blips[index];
			}
			
			var blipwgt = new BlipEditorWidget(this._view, blip, parentElement, where);
			this._blips.insert(index, blipwgt);
			
			if (blip.isRoot())
				this._rootBlip = blipwgt;
			
			return blipwgt;
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
			this._addParticipantWidget = new AddParticipantWidget(this._view, this._participantsDiv);
			this._blipContainerWidget = new BlipContainerWidget(this._view, contentElement);
			this._statusBar = new WaveletStatusBar(wavelet, contentElement);
			this.updateParticipants();
			
			// Connect event listeners
			wavelet.addEvent('participantsChanged', this.updateParticipants.bind(this));
			wavelet.addEvent('blipInserted', this._onBlipInserted.bind(this));
		},
		
		/**
		 * Returns a reference to the AddParticipantWindow of the
		 * AddParticipantWidget, if it is opened; null otherwise.
		 * 
		 * @function {public AddParticipantWindow} addParticipantWindow
		 */
		addParticipantWindow: function () {
			return this._addParticipantWidget.addParticipantWindow();
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
					var wgt = new ParticipantWidget(this._view,
						this._addParticipantWidget.contentElement,
						this._wavelet.participant(id),
						false,
						'before');
					this._participantWidgets.set(id, wgt);
				}
			}, this);
			this._participantWidgets.getKeys().each(function (id) {
				if (!ids.contains(id)) {
					this._participantWidgets.get(id).dispose();
					this._participantWidgets.erase(id);
				}
			}, this);
		},
		
		/**
		 * Callback on Blip insertion
		 * @function {private} _onBlipInserted
		 * @param {int} index index Index of the inserted Blip
		 * @param {String} blip_id ID of the inserted Blip
		 */
		_onBlipInserted: function (index, blip_id) {
			var blipwgt = this._blipContainerWidget.insertBlip(index, this._wavelet.blipByIndex(index));
			this._statusBar.connectBlipEditor(blipwgt);
		},
		
		/**
		 * Fit the wavlet subwidgets to the available space.
		 * 
		 * @function {private} fitWidget
		 */
		fitWidget: function () {
			var height = this.contentElement.getCoordinates().height;
			height -= this._statusBar.contentElement.getCoordinates().height + 2;
			height -= this._participantsDiv.getCoordinates().height;
			this._blipContainerWidget.contentElement.setStyle("height", height);
		}
	});
	
	/**
	 * Main Wave view class. Renders changes of the given model to HTML.
	 *
	 * @class {public} pygowave.view.WaveView
	 */
	var WaveView = new Class({
		Implements: [Options, Events],
		options: {
			gadgetLoaderUrl: "about:blank",
			defaultThumbnailUrl: "about:blank",
			rtl: false
		},
		
		// --- Event documentation ---
		/**
		 * Fired if the user enters something into the participant search box.
		 * <br/>Note: This event is fired by a AddParticipantWindow instance.
		 * 
		 * @event onSearchForParticipant
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} text Entered search query
		 */
		
		/**
		 * Fired if the user has searched for a participant and now wants to
		 * add him/her to the wave.
		 * <br/>Note: This event is fired by a AddParticipantWindow instance.
		 * 
		 * @event onAddParticipant
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} participantId ID of the Participant to add
		 */
		
		/**
		 * Fired on text insertion.
		 * <br/>Note: This event is fired by a BlipEditor instance.
		 * 
		 * @event onTextInserted
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} index Start index of insertion
		 * @param {String} content Inserted content
		 */
		
		/**
		 * Fired on text deletion.
		 * <br/>Note: This event is fired by a BlipEditor instance.
		 * 
		 * @event onTextDeleted
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} start Start index of deletion
		 * @param {int} end End index of deletion
		 */
		
		/**
		 * Fired if the user wants to leave a wavelet.
		 *
		 * @event onLeaveWavelet
		 * @param {String} waveletId ID of the Wave
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {pygowave.model.WaveModel} model Model to connect to
		 * @param {Element} container Container DOM element to render the view on
		 * @param {Object} options Various settings to correctly render the view
		 * @... {String} gadgetLoaderUrl URL to the gadget loader view; prepared
		 *      to have the gadget URL appended
		 * @... {String} defaultThumbnailUrl URL to a thumbnail image to be used
		 *      if the participant has not uploaded his own.
		 * @... {Boolean} rtl Set to true, if you want to enable rendering
		 *      for right-to-left languages.
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
			
			// Connect event listener(s)
			this.model.addEvent('waveletAdded', this._onModelWaveletAdded.bind(this));
			
			window.addEvent('resize', this._onWindowResize.bind(this));
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
		 * Update the search result on participant addition. Forwards to the
		 * opened AddParticipantWindow.
		 * 
		 * @function {public} updateSearchResults
		 * @param {pygowave.model.Participant[]} results Participant objects to
		 *        display as search result.
		 */
		updateSearchResults: function (results) {
			if (this.rootWaveletWidget == null)
				return;
			var apw = this.rootWaveletWidget.addParticipantWindow();
			if (apw != null)
				apw.updateSearchResults(results);
		},
		
		/**
		 * Remove search results and display the number of characters that must
		 * be entered for a valid search query. Forwards to the opened
		 * AddParticipantWindow.
		 * 
		 * @function {public} invalidSearch
		 * @param {Integer} chars Number of characters for a valid search
		 */
		invalidSearch: function (chars) {
			if (this.rootWaveletWidget == null)
				return;
			var apw = this.rootWaveletWidget.addParticipantWindow();
			if (apw != null)
				apw.invalidSearch(chars);
		},
		
		/**
		 * Returns the default thumbnail URL for participants without thumbnail.
		 * @function {public String} defaultThumbnailUrl
		 */
		defaultThumbnailUrl: function () {
			return this.options.defaultThumbnailUrl;
		},
		
		/**
		 * Show an error message from the controller
		 *
		 * @function {public} showControllerError
		 * @param {String} message The errormessage to show
		 */
		showControllerError: function (message) {
			this.showMessage(message, gettext("Shit happens..."), gettext("Oh well"));
		},
		
		/**
		 * Show a confirmation message to let the user leave the wave.<br/>
		 * Note: Emits
		 *
		 * @function {public} confirmLeaveWave
		 */
		confirmLeaveWave: function () {
			var self = this;
			this.showQuestion(
				gettext("Do you really want to leave the Wave?<br/>\n<br/>\nWarning: If you do so, you cannot come back to this wave unless someone adds you again."),
				gettext("Please confirm"),
				function () {
					MochaUI.closeWindow(this);
					self.fireEvent('leaveWavelet', self.model.rootWavelet().id());
				}
			);
		},
		
		/**
		 * Show a general purpose modal message box
		 *
		 * @function {public} showMessage
		 * @param {String} message The message to show
		 * @param {String} title Title of the message box
		 * @param {optional String} buttonLabel Label of the button, defaults
		 *        to "OK"
		 */
		showMessage: function (message, title, buttonLabel) {
			var buttons = {};
			if (!$defined(buttonLabel))
				buttonLabel = gettext("OK");
			buttons[buttonLabel] = function () {MochaUI.closeWindow(this);};
			this._showDialog(title, message, buttons);
		},
		
		/**
		 * Show a general purpose modal question box with two buttons.
		 *
		 * @function {public} showQuestion
		 * @param {String} message The message to show
		 * @param {String} title Title of the message box
		 * @param {Function} yesCallback Function to be called by clicking "Yes"
		 * @param {optional Function} noCallback Function to be called by
		 *        clicking "No". Defaults to close window.
		 * @param {optional String} yesLabel Alternative label of the "Yes" button
		 * @param {optional String} noLabel Alternative label of the "No" button
		 */
		showQuestion: function (message, title, yesCallback, noCallback, yesLabel, noLabel) {
			var buttons = {};
			if (!$defined(noCallback))
				noCallback = function () {MochaUI.closeWindow(this);};
			if (!$defined(yesLabel))
				yesLabel = gettext("Yes");
			if (!$defined(noLabel))
				noLabel = gettext("No");
			buttons[noLabel] = noCallback;
			buttons[yesLabel] = yesCallback;
			this._showDialog(title, message, buttons);
		},
		
		/**
		 * Common function for showMessage and showQuestion.
		 *
		 * @function {private MochaUI.Window} _showDialog
		 * @param {String} title Dialog box title
		 * @param {String} content Dialog box content
		 * @param {Object} buttons Buttons/callback dictionary
		 */
		_showDialog: function (title, content, buttons) {
			return new MochaUI.Window({
				title: title,
				type: "modal",
				content: content,
				width: 300,
				height: 126,
				headerStartColor: [95, 163, 237],
				headerStopColor: [85, 144, 210],
				bodyBgColor: [201, 226, 252],
				closeBgColor: [66, 114, 166],
				closeColor: [255, 255, 255],
				cornerRadius: 4,
				resizable: false,
				footerHeight: 34,
				rtl: this.options.rtl,
				buttons: buttons
			});
		},
		
		/**
		 * Callback on window resize
		 * @function {private} _onWindowResize
		 * @param {Object} e Event object
		 */
		_onWindowResize: function (e) {
			var viewport = window.getSize();
			var doc = window.getScrollSize();
			var coords = this.container.getCoordinates();
			if (viewport.y < coords.top + 150)
				viewport.y = coords.top + 150;
			this.container.setStyle("height", viewport.y - coords.top - 25);
			this.rootWaveletWidget.fitWidget();
		}
	});
	
	pygowave.view.extend({
		WaveView: WaveView
	});
})();
