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
 * Widgets for displaying and adding participants.
 * 
 * @module pygowave.view.participants
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	var SearchWidget = pygowave.view.SearchWidget;
	
	/**
	 * A widget to display a participant. This includes his/her avatar and a
	 * online indicator; may also display the participants nickname if rendered
	 * as searchbox widget.
	 *
	 * @class {private} pygowave.view.ParticipantWidget
	 * @extends pygowave.view.Widget
	 */
	var ParticipantWidget = new Class({
		Extends: Widget,
		
		// --- Event documentation ---
		/**
		 * Fired if clicked.
		 * @event onClick
		 * @param {ParticipantWidget} widget The clicked widget (for convenience)
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {pygowave.model.Participant} participant Participant object
		 * @param {String} display_style Set the display style of the widget.
		 *        Possible values: 'normal', 'small', 'searchbox'.
		 * @param {String} where Where to inject the widget relative to the
		 *        parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function (view, parentElement, participant, display_style, where) {
			this._view = view;
			this._participant = participant;
			var contentElement = new Element('div', {'class': 'wavelet_participant'});
			this._pImage = new Element('img', {
				'class': 'thumbnail',
				'src': '',
				'alt': '',
				'title': ''
			}).inject(contentElement);
			if (display_style == 'small') {
				contentElement.addClass('small');
				this._pNick = null;
			}
			else if (display_style == 'searchbox') {
				contentElement.addClass('searchbox');
				this._pNick = new Element('div', {
					'class': 'nicktext'
				}).inject(contentElement);
			}
			contentElement.addEvent('click', this._onClick.bind(this));
			this._onOnlineStateChanged = this._onOnlineStateChanged.bind(this);
			this._onDataChanged = this._onDataChanged.bind(this);
			this.parent(parentElement, contentElement, where);
		},
		
		/**
		 * Returns the displayed participant object.
		 * @function {public pygowave.model.Participant} participant
		 */
		participant: function () {
			return this._participant;
		},
		
		/**
		 * Overridden from {@link pygowave.view.Widget.dispose Widget.dispose}.<br/>
		 * Removes the widget from the DOM, sets the parentElement to null
		 * and removes event handlers.
		 * @function {public Widget} dispose
		 */
		dispose: function () {
			this.parent();
			this._participant.removeEvent('onlineStateChanged', this._onOnlineStateChanged);
			this._participant.removeEvent('dataChanged', this._onDataChanged);
			return this;
		},
		
		/**
		 * Overridden from {@link pygowave.view.Widget.inject Widget.inject}.<br/>
		 * Inject this widget somewhere else. Sets the parent element and adds
		 * event listeners to the (new) participant object.
		 * 
		 * @function {public Widget} inject
		 * @param {Element} parentElement New parent element
		 * @param {optional String} where Where to inject the contentElement
		 *        relative to the parent element. Can be 'top', 'bottom',
		 *        'after', or 'before'.
		 * @param {optional pygowave.model.Participant} participant New
		 *        Participant object to bind this element to.
		 * @return Returns a reference to this widget.
		 */
		inject: function (parentElement, where, participant) {
			this.parent(parentElement, where);
			
			if ($defined(participant))
				this._participant = participant;
			else
				participant = this._participant;
				
			participant.addEvent('onlineStateChanged', this._onOnlineStateChanged);
			participant.addEvent('dataChanged', this._onDataChanged);
			
			var display_name = participant.displayName();
			if (display_name == "") display_name = "...";
			
			var tn = participant.thumbnailUrl();
			if (tn == "") tn = this._view.defaultThumbnailUrl();
			
			this._pImage.set({
				'src': tn,
				'alt': display_name,
				'title': display_name
			});
			
			if (this._pNick != null)
				this._pNick.set('text', display_name);
			this._onOnlineStateChanged(participant.isOnline());
			return this;
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
		},
		
		/**
		 * Callback for {@link pygowave.model.Participant.onDataChanged
		 * onDataChanged}. Updates the representation of the participant.
		 * @function {private} _onDataChanged
		 */
		_onDataChanged: function () {
			var pImage = this._pImage;
			var display_name = this._participant.displayName();
			if (display_name == "") display_name = "...";
			pImage.set({
				"alt": display_name,
				"title": display_name
			});
			if (this._pNick != null)
				this._pNick.set('text', display_name);
			
			var tn = this._participant.thumbnailUrl();
			if (tn == "") tn = this._view.defaultThumbnailUrl();
			
			if (pImage.get("src") != tn) { // Exchange thumbnail
				new Fx.Tween(pImage, {duration:200}).start("opacity", 1, 0).chain(function () {
					pImage.set("src", "");
					pImage.set.delay(100, pImage, ["src", tn]);
					this.start.delay(100, this ,["opacity", 0, 1]);
				});
			}
		},
		
		/**
		 * Callback on click. Forwards event to listeners.
		 * @function _onClick
		 */
		_onClick: function () {
			this.fireEvent('click', this);
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
		 * @param {String} waveletId ID of the Wavelet
		 */
		initialize: function (view, waveletId) {
			this._view = view;
			this._waveletId = waveletId;
			var buttons = {};
			buttons[gettext("Cancel")] = this._onCancel.bind(this);
			buttons[gettext("OK")] = this._onOK.bind(this);
			this._content = new Element('div');
			this._topBar = new Element('div', {'class': 'wavelet_add_participant_searchbar'}).inject(this._content);
			this._searchbox = new SearchWidget(this._topBar);
			this._results = new Element('div', {'class': 'wavelet_add_participant_results'}).inject(this._content);
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
			this.addEvent('closeComplete', this._onClose.bind(this));
			this._searchbox.setFocus.delay(350, this._searchbox);
			this._searchbox.addEvent('change', this._onQueryChange.bind(this));
			this._found = null;
			this._onSelect = this._onSelect.bind(this);
			this._selected = null;
		},
		
		/**
		 * Overrides drawWindow of mochaUI. Resizes result box to available
		 * space.
		 * @function {private} drawWindow
		 * @param {Element} windowEl Window element
		 * @param {Boolean} shadows False will draw a window without shadows
		 */
		drawWindow: function (windowEl, shadows) {
			this.parent(windowEl, shadows);
			this._results.setStyle("height", this.contentWrapperEl.getHeight()-this._topBar.getHeight());
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
			if (this._selected == null)
				this._view.showMessage(gettext("You must select a participant from your search results first."), gettext("Notice"));
			else {
				this._view.fireEvent('addParticipant', [this._waveletId, this._selected.participant().id()]);
				MochaUI.closeWindow(this.windowEl);
			}
		},
		
		/**
		 * Called if the window is closed. Disconnects events.
		 * @function {private} _onClose
		 */
		_onClose: function () {
			if (this._found != null) {
				for (var i = 0; i < this._found.length; i++)
					this._cleanupParticipantWidget(this._found[i]);
				this._found = null;
			}
		},
		
		/**
		 * Called if the user enters a query. Forwards event to view.
		 * @function {private} _onQueryChange
		 * @param {String} text Entered query text
		 */
		_onQueryChange: function (text) {
			this._view.fireEvent('searchForParticipant', [this._waveletId, text]);
		},
		
		/**
		 * Called if a participant was selected.
		 * @function {private} _onSelect
		 * @param {ParticipantWidget} widget The selected widget
		 */
		_onSelect: function (widget) {
			if (this._selected != null)
				this._selected.contentElement.removeClass("selected");
			widget.contentElement.addClass("selected");
			this._selected = widget;
		},
		
		/**
		 * Helper function to remove event handlers etc. from used
		 * ParticipantWidgets. Destroys the widget afterwards.
		 * @function {private} _cleanupParticipantWidget
		 * @param {ParticipantWidget} widget ParticipantWidget to be cleaned
		 */
		_cleanupParticipantWidget: function (widget) {
			widget.removeEvent('click', this._onSelect);
			if (this._selected == widget)
				this._selected = null;
			widget.dispose();
			widget.contentElement.destroy();
		},
		
		/**
		 * Update the search result on participant addition. Forwarded from the
		 * view.
		 * 
		 * @function {public} updateSearchResults
		 * @param {pygowave.model.Participant[]} results Participant objects to
		 *        display as search result.
		 */
		updateSearchResults: function (results) {
			if (this._found == null) {
				this._found = new Array();
				this._results.set("html", "");
			}
			var i, j, deleted, id, newwgt;
			for (i = 0; i < this._found.length; i++) {
				if (i >= results.length) {
					this._cleanupParticipantWidget(this._found.pop(i));
					i--;
					continue;
				}
				
				id = this._found[i].participant().id();
				if (id != results[i].id()) {
					deleted = true;
					for (j = i+1; j < results.length; j++) {
						if (id == results[j].id())
							deleted = false;
					}
					if (deleted) {
						this._cleanupParticipantWidget(this._found.pop(i));
						i--;
					}
					else {
						newwgt = new ParticipantWidget(this._view, this._results, results[i], 'searchbox');
						newwgt.addEvent('click', this._onSelect);
						this._found.insert(i, newwgt);
					}
				}
			}
			for (; i < results.length; i++) {
				newwgt = new ParticipantWidget(this._view, this._results, results[i], 'searchbox');
				newwgt.addEvent('click', this._onSelect);
				this._found.append(newwgt);
			}
		},
		
		/**
		 * Remove search results and display the number of characters that must
		 * be entered for a valid search query. Forwarded from the view.
		 * 
		 * @function {public} invalidSearch
		 * @param {Integer} chars Number of characters for a valid search
		 */
		invalidSearch: function (chars) {
			if (this._found != null) {
				for (var i = 0; i < this._found.length; i++)
					this._cleanupParticipantWidget(this._found[i])
			}
			this._found = null;
			this._results.set("html", "<div class=\"results_invalid\">" + gettext("Please enter at least %d letters.").sprintf(chars) + "</div>");
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
		 * @param {String} waveletId ID of the Wavelet
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 */
		initialize: function (view, waveletId, parentElement) {
			this._view = view;
			this._waveletId = waveletId;
			var contentElement = new Element('div', {
				'class': 'wavelet_add_participant_widget',
				'title': gettext("Add participant")
			});
			this.parent(parentElement, contentElement);
			contentElement.addEvent('click', this._onClick.bind(this));
			this._addParticipantWindow = null;
		},
		
		/**
		 * Returns a reference to the AddParticipantWindow, if it is opened;
		 * null otherwise.
		 * @function {public AddParticipantWindow} addParticipantWindow
		 */
		addParticipantWindow: function () {
			return this._addParticipantWindow;
		},
		
		/**
		 * Callback on click.
		 * @function {private} _onClick
		 */
		_onClick: function () {
			if (this._addParticipantWindow == null) {
				this._addParticipantWindow = new AddParticipantWindow(this._view, this._waveletId);
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
	
	pygowave.view.extend({
		ParticipantWidget: ParticipantWidget,
		AddParticipantWidget: AddParticipantWidget
	});
})();
