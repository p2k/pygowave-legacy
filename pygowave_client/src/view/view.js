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
	var ToolbarWidget = pygowave.view.ToolbarWidget;
	var ToolbarButton = pygowave.view.ToolbarButton;
	var AddGadgetWindow = pygowave.view.AddGadgetWindow;
	
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
		 * @param {pygowave.model.Wavelet} wavelet Wavelet to be rendered
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 */
		initialize: function (view, wavelet, parentElement) {
			this._view = view;
			var contentElement = new Element('div', {'class': 'wavelet_widget'});
			this._participantsDiv = new Element('div', {'class': 'wavelet_participants_div'}).inject(contentElement);
			this.parent(parentElement, contentElement);
			this._wavelet = wavelet;
			this._participantWidgets = new Hash();
			this._addParticipantWidget = new AddParticipantWidget(this._view, wavelet.id(), this._participantsDiv);
			this._toolBar = new ToolbarWidget(contentElement, 'wavelet_toolbar');
			this._blipContainerWidget = new BlipContainerWidget(this._view, contentElement);
			this._activeBlipEditor = null;
			this._statusBar = new WaveletStatusBar(wavelet, contentElement);
			this.updateParticipants();
			
			// Connect event listeners
			wavelet.addEvent('participantsChanged', this.updateParticipants.bind(this));
			wavelet.addEvent('blipInserted', this._onBlipInserted.bind(this));
			
			// Build toolbar
			this._addGadgetWindow = null;
			this._buildToolbar();
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
		 * Returns a reference to the AddGadgetWindow, if it is opened; null
		 * otherwise.
		 * 
		 * @function {public AddGadgetWindow} addGadgetWindow
		 */
		addGadgetWindow: function () {
			return this._addGadgetWindow;
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
			this._activeBlipEditor = blipwgt; //TODO change this if multiple blips supported
			this._statusBar.connectBlipEditor(blipwgt);
		},
		
		/**
		 * Fit the wavlet subwidgets to the available space.
		 * 
		 * @function {private} fitWidget
		 */
		fitWidget: function () {
			var height = this.contentElement.getCoordinates().height;
			height -= this._participantsDiv.getCoordinates().height;
			height -= this._toolBar.contentElement.getCoordinates().height;
			height -= this._statusBar.contentElement.getCoordinates().height + 2;
			this._blipContainerWidget.contentElement.setStyle("height", height);
		},
		
		/**
		 * Set up the wavelet toolbar.
		 *
		 * @function {private} _buildToolbar
		 */
		_buildToolbar: function () {
			this._toolBar.addButton(new ToolbarButton("add_gadget", {
				icon_class: "wavelet_toolbar_add_gadget",
				label: gettext("Add Gadget"),
				tiptext: gettext("Add a gadget to the blip"),
				onClick: function () {
					if (this._addGadgetWindow == null) {
						this._addGadgetWindow = new AddGadgetWindow(this._view, this._wavelet.id());
						this._addGadgetWindow.initLoadGadgetList();
						this._addGadgetWindow.addEvent('closeComplete', function () {this._addGadgetWindow = null;}.bind(this));
					}
					else
						MochaUI.focusWindow(this._addGadgetWindow.windowEl);
				}.bind(this)
			}));
		},
		
		/**
		 * Returns the currently active BlipWidget or null if no BlipWidget is active.
		 *
		 * @function {public BlipEditorWidget} activeBlipEditorWidget
		 */
		activeBlipEditorWidget: function () {
			return this._activeBlipEditor;
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
		 * Fired on element insertion.
		 * 
		 * @event onElementInsert
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} index Position of the new element
		 * @param {String} type Element type
		 * @param {Object} properties Element properties
		 */
		
		/**
		 * Fired on element deletion.
		 * 
		 * @event onElementDelete
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} index Position of the element to delete
		 */
		
		/**
		 * Fired if an element submits a delta.
		 * <br/>Note: This event is fired by a GadgetElementWidget instance.
		 *
		 * @event onElementDeltaSubmitted
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} index Position of the element
		 * @param {Object} delta Delta to apply to the element
		 */
		
		/**
		 * Fired if an element sets a UserPref.
		 * <br/>Note: This event is fired by a GadgetElementWidget instance.
		 *
		 * @event onElementSetUserpref
		 * @param {String} waveletId ID of the Wavelet
		 * @param {String} blipId ID of the Blip
		 * @param {int} index Position of the element
		 * @param {Object} key Name of the UserPref
		 * @param {Object} value Value of the UserPref
		 */
		
		/**
		 * Fired if the user wants to leave a wavelet.
		 *
		 * @event onLeaveWavelet
		 * @param {String} waveletId ID of the Wave
		 */
		
		/**
		 * Fired if the user wants to add a gadget or clicked the refresh button.
		 * <br/>Note: This event is fired by a AddGadgetWindow instance.
		 * 
		 * @event onRefreshGadgetList
		 * @param {String} waveletId ID of the Wavelet
		 * @param {Boolean} forced True if the user explicitly clicked refresh
		 */
		
		/**
		 * Fired if the view was busy and now is ready to receive modification.
		 * @event onReady
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
			this._busyCounter = 0;
			
			var rwv = this.model.rootWavelet();
			if (rwv != null) {
				this.rootWaveletWidget = new WaveletWidget(this, rwv, this.container);
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
			this.rootWaveletWidget = new WaveletWidget(this, rwv, this.container);
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
			var buttons = {};
			buttons[gettext("Dismiss")] = function () {MochaUI.closeWindow(this);};
			buttons[gettext("Reload")] = function () {window.location.href = window.location.href;};
			this._showDialog(
				gettext("Shit happens..."),
				message,
				buttons
			);
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
		},
		
		/**
		 * Update the gadget list. Forwards to the
		 * opened AddGadgetWindow.
		 * 
		 * @function {public} updateGadgetList
		 * @param {Object[]} gadgets List of gadgets with the following fields:
		 * @... {int} id ID of the gadget
		 * @... {String} name Display name of the gadget
		 * @... {String} descr Description of the gadget
		 */
		updateGadgetList: function (gadgets) {
			if (this.rootWaveletWidget == null)
				return;
			var agw = this.rootWaveletWidget.addGadgetWindow();
			if (agw != null)
				agw.updateGadgetList(gadgets);
		},
		
		/**
		 * Insert a Gadget at the current cursor position. Returns false on
		 * error, true on success.<br/>
		 * Note: This is called by a AddGadgetWindow instance.
		 * 
		 * @function {public} insertGadgetAtCursor
		 * @param {String} waveletId ID of the Wavelet to insert the Gadget into
		 * @param {String} url URL of the Gadget to insert
		 */
		insertGadgetAtCursor: function (waveletId, url) {
			var waveletWidget = this.waveletWidgets.get(waveletId);
			if (!$defined(waveletWidget)) {
				this.showMessage(
					gettext("The WaveletWidget for Wavelet '%s' was not found.").sprintf(waveletId),
					gettext("Sorry, but..."),
					gettext("I see")
				);
				return false;
			}
			var editor = waveletWidget.activeBlipEditorWidget();
			if (!$defined(editor)) {
				this.showMessage(
					gettext("You must be inside of a Blip to add a Gadget."),
					gettext("Sorry, but..."),
					gettext("I see")
				);
				return false;
			}
			var textrange = editor.lastValidTextRange();
			if (!$defined(textrange)) {
				this.showMessage(
					gettext("You must place your cursor inside the text to mark the Gadget's insertion point."),
					gettext("Sorry, but..."),
					gettext("I see")
				);
				return false;
			}
			
			var index = textrange[0];
			
			if (!editor.checkOrAddNewline(index))
				index++;
			
			this.fireEvent('elementInsert', [
				waveletId,
				editor.blip().id(),
				index,
				pygowave.model.ELEMENT_TYPE.GADGET,
				{"url": url}
			]);
			return true;
		},
		
		/**
		 * Returns weather the view is busy processing one or more user
		 * interactions.
		 *
		 * @function {public Boolean} isBusy
		 */
		isBusy: function () {
			return (this._busyCounter != 0);
		},
		/**
		 * Set the view busy. Called by interactive widgets to block controller
		 * events.
		 * @function {public} setBusy
		 */
		setBusy: function () {
			this._busyCounter++;
		},
		/**
		 * Unset the view busy. Called by interactive widgets to free controller
		 * events. May fire a ready event.
		 * @function {public} unsetBusy
		 */
		unsetBusy: function () {
			this._busyCounter--;
			if (this._busyCounter < 0)
				this._busyCounter = 0;
			if (this._busyCounter == 0)
				this.fireEvent('ready');
		}
	});
	
	pygowave.view.extend({
		WaveView: WaveView
	});
})();
