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
		 * @param {Element} contentElement DOM element to be inserted into the
		 *        parent element
		 * @param {String} where Where to inject the contentElement relative to
		 *        the parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function(parentElement, contentElement, where) {
			this.contentElement = contentElement;
			this.inject(parentElement, where);
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
		 * @param {String} where Where to inject the contentElement relative to
		 *        the parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 * @return Returns a reference to this widget.
		 */
		inject: function (parentElement, where) {
			this.parentElement = parentElement;
			this.contentElement.inject(this.parentElement, where);
		}
	});
	
	/**
	 * Renders a fancy search box.
	 *
	 * @class {public} pygowave.view.SearchWidget
	 * @extends pygowave.view.Widget
	 */
	var SearchWidget = new Class({
		Extends: Widget,
		
		// --- Event documentation ---
		/**
		 * Fired when the content of the search field changes.
		 * @event onChange
		 * @param {String} text The current (and recently changed) text of the
		 *        search field
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {String} where Where to inject the widget relative to the
		 *        parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function (parentElement, where) {
			var contentElement = new Element('div', {'class': 'search_widget'});
			this._lcorner = new Element('div', {'class': 'left_corner inactive'}).inject(contentElement);
			this._iwrapper = new Element('div', {'class': 'input_wrapper'}).inject(contentElement);
			this._input = new Element('input', {'type': 'text', 'class': 'inactive'}).inject(this._iwrapper);
			this._input.addEvent('focus', this._onFocus.bind(this));
			this._input.addEvent('blur', this._onBlur.bind(this));
			this._rcorner = new Element('div', {'class': 'right_corner inactive'}).inject(contentElement);
			this.parent(parentElement, contentElement, where);
		},
		
		/**
		 * Called when the input box receives focus. Toggles
		 * active/inactive style classes.
		 * @function {private} _onFocus
		 */
		_onFocus: function () {
			this._lcorner.addClass('active').removeClass('inactive');
			this._input.addClass('active').removeClass('inactive');
			this._rcorner.addClass('active').removeClass('inactive');
		},
		
		/**
		 * Called when the input box looses focus. Toggles
		 * active/inactive style classes.
		 * @function {private} _onBlur
		 */
		_onBlur: function () {
			this._lcorner.addClass('inactive').removeClass('active');
			this._input.addClass('inactive').removeClass('active');
			this._rcorner.addClass('inactive').removeClass('active');
		},
		
		/**
		 * Forwards the onChange event.
		 * @function {private} _onChange
		 */
		_onChange: function () {
			this.fireEvent('change', this.text());
		},
		
		/**
		 * Set focus to the input box.
		 * @function {public} setFocus
		 */
		setFocus: function () {
			this._input.focus();
		},
		
		/**
		 * Returns the text of the search field.
		 * @function {public String} text
		 */
		text: function () {
			return this._input.get('value');
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
		 * @param {Boolean} small True to render a small version of the widget
		 * @param {String} where Where to inject the widget relative to the
		 *        parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function (view, parentElement, participant, small, where) {
			this._view = view;
			var contentElement = new Element('div', {'class': 'wavelet_participant'});
			if (small) contentElement.addClass('small');
			var tn = participant.thumbnailUrl();
			if (tn == "")
				tn = view.defaultThumbnailUrl();
			this._pImage = new Element('img', {
				'src': tn,
				'class': 'thumbnail',
				'alt': participant.displayName(),
				'title': participant.displayName()
			}).inject(contentElement);
			if (small) this._pImage.addClass('small');
			this.parent(parentElement, contentElement, where);
			participant.addEvent('onlineStateChanged', this._onOnlineStateChanged.bind(this));
			this._onOnlineStateChanged(participant.isOnline());
		},
		
		/**
		 * Callback for {@link pygowave.model.Participant.onOnlineStateChanged
		 * onOnlineStateChanged}. Shows/hides the online indicator.<br/>
		 * Note: The behaviour depends on your stylesheet. This method toggles
		 * between the 'offline' and 'online' classes of the element.
		 * @function {private} _onOnlineStateChanged
		 * @param {Boolean} online True if the participant is online
		 */
		_onOnlineStateChanged: function (online) {
			if (!$defined(this._oImage))
				this._oImage = new Element('div', {'class': 'indicator'}).inject(this.contentElement, 'bottom');
			
			this._oImage.addClass(online ? 'online' : 'offline').removeClass(online ? 'offline' : 'online');
		}
	});
	
	/**
	 * A window that displays two tables of outgoing operations (pending and
	 * cached). This is for debugging purposes.
	 * Instances of this class automatically connect to events of the provided
	 * operation managers.
	 *
	 * @class {public} pygowave.view.OperationsViewer
	 * @extends MochaUI.Window
	 */
	var OperationsViewer = new Class({
		Extends: MochaUI.Window,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {OpManager} pending A reference to the pending operations manager
		 * @param {OpManager} cached A reference to the cached operations manager
		 */
		initialize: function (pending, cached) {
			this._mpending = pending;
			this._mcached = cached;
			
			this._content = new Element('div', {'class': 'debug_window'});
			
			new Element('div', {'class': 'debug_window_bar', 'text': 'Pending Operations'}).inject(
				new Element('div', {'class': 'debug_window_row'}).inject(this._content)
			);
			this._ptable = new Element('table', {'class': 'debug_window_table'}).inject(
				new Element('div', {'class': 'debug_window_table_container'}).inject(
					new Element('div', {'class': 'debug_window_row'}).inject(this._content)
				)
			);
			new Element('div', {'class': 'debug_window_bar', 'text': 'Cached Operations'}).inject(
				new Element('div', {'class': 'debug_window_row'}).inject(this._content)
			);
			this._ctable = this._ptable = new Element('table', {'class': 'debug_window_table'}).inject(
				new Element('div', {'class': 'debug_window_table_container'}).inject(
					new Element('div', {'class': 'debug_window_row'}).inject(this._content)
				)
			);
			
			this.parent({
				title: gettext("Operations Viewer (Debug)"),
				content: this._content,
				width: 450,
				height: 300,
				x: $(window).getSize().x - 490,
				y: 20,
				headerStartColor: [95, 163, 237],
				headerStopColor: [85, 144, 210],
				bodyBgColor: [201, 226, 252],
				closeBgColor: [66, 114, 166],
				closeColor: [255, 255, 255],
				cornerRadius: 4,
				resizable: true,
				footerHeight: 34,
				padding: {top: 0, right: 0, bottom: 0, left: 0},
				onClose: this._onClose.bind(this)
			});
			this._content.getParent().setStyle("height", "100%");
		},
		
		/**
		 * Called if the window is closed. Disconnects OpManagers.
		 * @function {private} _onClose
		 */
		_onClose: function () {
			
		}
	});
	
	/**
	 * Add participant window.
	 *
	 * @class {private} pygowave.view.AddParticipantWindow
	 * @extends MochaUI.Window
	 */
	var AddParticipantWindow = new Class({
		Extends: MochaUI.Window,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 */
		initialize: function (view) {
			this._view = view;
			var buttons = {};
			buttons[gettext("Cancel")] = this._onCancel.bind(this);
			buttons[gettext("OK")] = this._onOK.bind(this);
			this._content = new Element('div');
			this._topBar = new Element('div', {'class': 'wavelet_add_participant_searchbar'}).inject(this._content);
			this._searchbox = new SearchWidget(this._topBar);
			this.parent({
				title: gettext("Add participant"),
				content: this._content,
				width: 240,
				height: 410,
				headerStartColor: [95, 163, 237],
				headerStopColor: [85, 144, 210],
				bodyBgColor: [201, 226, 252],
				closeBgColor: [66, 114, 166],
				closeColor: [255, 255, 255],
				cornerRadius: 4,
				resizable: true,
				footerHeight: 34,
				rtl: this._view.options.rtl,
				buttons: buttons,
				padding: {top: 0, right: 0, bottom: 0, left: 0}
			});
			this._searchbox.setFocus.delay(350, this._searchbox);
			this._searchbox.addEvent('change', this._onQueryChange.bind(this));
		},
		
		/**
		 * Called if the users presses cancel.
		 * @function {private} _onCancel
		 */
		_onCancel: function () {
			MochaUI.closeWindow(this.windowEl);
		},
		
		/**
		 * Called if the user presses OK.
		 * @function {private} _onOK
		 */
		_onOK: function () {
			
		},
		
		/**
		 * Called if the user enters a query. Forwards event to view.
		 * @function {private} _onQueryChange
		 * @param {String} text Entered query text
		 */
		_onQueryChange: function (text) {
			this._view.fireEvent('searchForParticipant', text);
		}
	});
	
	/**
	 * Represents the "Add participant" button.
	 *
	 * @class {private} pygowave.view.AddParticipantWidget
	 * @extends pygowave.view.Widget
	 */
	var AddParticipantWidget = new Class({
		Extends: Widget,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 */
		initialize: function (view, parentElement) {
			this._view = view;
			var contentElement = new Element('div', {
				'class': 'wavelet_add_participant_widget',
				'title': gettext("Add participant")
			});
			this.parent(parentElement, contentElement);
			contentElement.addEvent('click', this._onClick.bind(this));
			this._addParticipantWindow = null;
		},
		
		/**
		 * Callback on click.
		 * @function {private} _onClick
		 */
		_onClick: function () {
			if (this._addParticipantWindow == null) {
				this._addParticipantWindow = new AddParticipantWindow(this._view);
				this._addParticipantWindow.addEvent('closeComplete', this._onClose.bind(this));
			}
			else
				MochaUI.focusWindow(this._addParticipantWindow.windowEl);
		},
		
		/**
		 * Callback if the search window is closed.
		 * @function {private} _onClose
		 */
		_onClose: function () {
			this._addParticipantWindow = null;
		}
	});
	
	/**
	 * A widget for rendering blips.
	 * @class {private} pygowave.view.BlipWidget
	 * @extends pygowave.view.Widget
	 */
	var BlipWidget = new Class({
		Extends: Widget,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 */
		initialize: function (view, parentElement) {
			var contentElement = new Element('div', {'class': 'blip_widget'});
			contentElement.contentEditable = 'true';
			this.parent(parentElement, contentElement);
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
			this._rootBlipWidget = new BlipWidget(this._view, contentElement);
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
			defaultThumbnailUrl: "about:blank",
			rtl: false
		},
		
		// --- Event documentation ---
		/**
		 * Fired if the user enters something into the participant search box.
		 * <br/>Note: This event is fired by a AddParticipantWindow instance.
		 * @event onSearchForParticipant
		 * @param {String} text Entered search query
		 */
		// ---------------------------
		
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
		WaveView: WaveView,
		SearchWidget: SearchWidget,
		OperationsViewer: OperationsViewer
	};

}();
