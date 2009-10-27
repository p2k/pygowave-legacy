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
 * Debug windows, bars and functions.
 * 
 * @module pygowave.view.debug_tools
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	
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
		 * @param {pygowave.controller.PyGoWaveClient} controller The controller
		 * @param {String} wavelet_id ID of the wavelet to monitor
		 */
		initialize: function (controller, wavelet_id) {
			this._controller = controller;
			this._wavelet_id = wavelet_id;
			this._mpending = controller.wavelets[wavelet_id].mpending;
			this._mcached = controller.wavelets[wavelet_id].mcached;
			
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
			this._ctable = new Element('table', {'class': 'debug_window_table'}).inject(
				new Element('div', {'class': 'debug_window_table_container'}).inject(
					new Element('div', {'class': 'debug_window_row'}).inject(this._content)
				)
			);
			
			var elt = new Element('tr').inject(this._ptable);
			new Element('td', {'class': 'col1'}).inject(elt);
			new Element('td', {'class': 'col2'}).inject(elt);
			new Element('td', {'class': 'col3'}).inject(elt);
			new Element('td', {'class': 'col4'}).inject(elt);
			elt = new Element('tr').inject(this._ctable);
			new Element('td', {'class': 'col1'}).inject(elt);
			new Element('td', {'class': 'col2'}).inject(elt);
			new Element('td', {'class': 'col3'}).inject(elt);
			new Element('td', {'class': 'col4'}).inject(elt);
			
			// Bind callbacks
			for (var property in this) {
				if (property.startswith("cached_on") || property.startswith("pending_on") || property.startswith("_on"))
					this[property] = this[property].bind(this);
			}
			
			var buttons = {};
			var buttonText = gettext(controller.isBlocked(wavelet_id) ? "Unblock" : "Block");
			buttons[buttonText] = this._onBlock;
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
				buttons: buttons
			});
			this._content.getParent().setStyle("height", "100%");
			this.addEvent('closeComplete', this._onClose.bind(this));
			
			// Add latency text
			this._latencyText = new Element('span', {text: ''}).inject(this.buttonsEl);
			this._controller.addEvent('ping', this._onPing);
			
			// Connect callbacks
			this._mcached.addEvent('beforeOperationsInserted', this.cached_onBeforeOperationsInserted);
			this._mcached.addEvent('afterOperationsInserted', this.cached_onAfterOperationsInserted);
			this._mcached.addEvent('afterOperationsRemoved', this.cached_onAfterOperationsRemoved);
			this._mcached.addEvent('operationChanged', this.cached_onOperationChanged);
			this._mpending.addEvent('beforeOperationsInserted', this.pending_onBeforeOperationsInserted);
			this._mpending.addEvent('afterOperationsInserted', this.pending_onAfterOperationsInserted);
			this._mpending.addEvent('afterOperationsRemoved', this.pending_onAfterOperationsRemoved);
			this._mpending.addEvent('operationChanged', this.pending_onOperationChanged);
			
			// Populate table
			if (!this._mcached.isEmpty()) {
				this.cached_onBeforeOperationsInserted(0, this._mcached.operations.length-1);
				this.cached_onAfterOperationsInserted(0, this._mcached.operations.length-1);
			}
			if (!this._mpending.isEmpty()) {
				this.pending_onBeforeOperationsInserted(0, this._mpending.operations.length-1);
				this.pending_onAfterOperationsInserted(0, this._mpending.operations.length-1);
			}
		},
		
		/**
		 * Called if the window is closed. Disconnects OpManagers.
		 * @function {private} _onClose
		 */
		_onClose: function () {
			// Disconnect callbacks
			this._mcached.removeEvent('beforeOperationsInserted', this.cached_onBeforeOperationsInserted);
			this._mcached.removeEvent('afterOperationsInserted', this.cached_onAfterOperationsInserted);
			this._mcached.removeEvent('afterOperationsRemoved', this.cached_onAfterOperationsRemoved);
			this._mcached.removeEvent('operationChanged', this.cached_onOperationChanged);
			this._mpending.removeEvent('beforeOperationsInserted', this.pending_onBeforeOperationsInserted);
			this._mpending.removeEvent('afterOperationsInserted', this.pending_onAfterOperationsInserted);
			this._mpending.removeEvent('afterOperationsRemoved', this.pending_onAfterOperationsRemoved);
			this._mpending.removeEvent('operationChanged', this.pending_onOperationChanged);
			this._controller.removeEvent('ping', this._onPing);
		},
		/**
		 * Called if the "Block" button was clicked.
		 * @function {private} _onBlock
		 */
		_onBlock: function () {
			if (!this._controller.isBlocked(this._wavelet_id)) {
				this._controller.setBlocked(this._wavelet_id, true);
				this.buttonsEl.childNodes[0].set('text', gettext("Unblock"));
			}
			else {
				this._controller.setBlocked(this._wavelet_id, false);
				this.buttonsEl.childNodes[0].set('text', gettext("Block"));
			}
		},
		
		/**
		 * Generic helper function.
		 * @function {private} generic_addRows
		 * @param {Element} elt Parent table element
		 * @param {int} start Start index of the insertion.
		 * @param {int} end End index of the insertion.
		 */
		generic_addRows: function (elt, start, end) {
			var where = 'before';
			var rows = elt.getChildren();
			var entry = null;
			elt = rows[start];
			
			for (var i = start; i <= end; i++) {
				elt = new Element('tr').inject(elt, where);
				new Element('td').inject(elt);
				new Element('td').inject(elt);
				new Element('td').inject(elt);
				new Element('td').inject(elt);
				where = 'after';
			}
		},
		/**
		 * Generic helper function.
		 * @function {private} generic_removeRows
		 * @param {Element} elt Parent table element
		 * @param {int} start Start index of the removal.
		 * @param {int} end End index of the removal.
		 */
		generic_removeRows: function (elt, start, end) {
			var rows = elt.getChildren();
			for (var i = start; i <= end; i++)
				rows[i].dispose();
		},
		/**
		 * Generic helper function.
		 * @function {private} generic_updateRow
		 * @param {Element} row Table row element
		 * @param {Object} entry Data entry to be put in the row
		 */
		generic_updateRow: function (row, entry) {
			if (!$defined(row) || !$defined(entry))
				return;
			var cols = row.getChildren();
			
			cols[0].set('text', entry.type);
			cols[1].set('text', entry.blip_id);
			cols[2].set('text', entry.index);
			cols[3].set('text', JSON.encode(entry.property).substring(0, 30));
		},
		
		/**
		 * Cached model callback.
		 * @function {private} cached_onBeforeOperationsInserted
		 * @param {int} start Start index of the insertion.
		 * @param {int} end End index of the insertion.
		 */
		cached_onBeforeOperationsInserted: function (start, end) {
			this.generic_addRows(this._ctable, start, end);
		},
		/**
		 * Cached model callback.
		 * @function {private} cached_onAfterOperationsInserted
		 * @param {int} start Start index of the insertion.
		 * @param {int} end End index of the insertion.
		 */
		cached_onAfterOperationsInserted: function (start, end) {
			var rows = this._ctable.getChildren();
			for (var i = start; i <= end; i++)
				this.generic_updateRow(rows[i], this._mcached.operations[start]);
		},
		/**
		 * Cached model callback.
		 * @function {private} cached_onAfterOperationsRemoved
		 * @param {int} start Start index of the removal.
		 * @param {int} end End index of the removal.
		 */
		cached_onAfterOperationsRemoved: function (start, end) {
			this.generic_removeRows(this._ctable, start, end);
		},
		/**
		 * Cached model callback.
		 * @function {private} cached_onOperationChanged
		 * @param index Index of the changed operation
		 */
		cached_onOperationChanged: function (index) {
			this.generic_updateRow(this._ctable.getChildren()[index], this._mcached.operations[index]);
		},
		
		/**
		 * Pending model callback.
		 * @function {private} pending_onBeforeOperationsInserted
		 * @param {int} start Start index of the insertion.
		 * @param {int} end End index of the insertion.
		 */
		pending_onBeforeOperationsInserted: function (start, end) {
			this.generic_addRows(this._ptable, start, end);
		},
		/**
		 * Pending model callback.
		 * @function {private} pending_onAfterOperationsInserted
		 * @param {int} start Start index of the insertion.
		 * @param {int} end End index of the insertion.
		 */
		pending_onAfterOperationsInserted: function (start, end) {
			var rows = this._ptable.getChildren();
			for (var i = start; i <= end; i++)
				this.generic_updateRow(rows[i], this._mpending.operations[start]);
		},
		/**
		 * Pending model callback.
		 * @function {private} pending_onAfterOperationsRemoved
		 * @param {int} start Start index of the removal.
		 * @param {int} end End index of the removal.
		 */
		pending_onAfterOperationsRemoved: function (start, end) {
			this.generic_removeRows(this._ptable, start, end);
		},
		/**
		 * Pending model callback.
		 * @function {private} pending_onOperationChanged
		 * @param index Index of the changed operation
		 */
		pending_onOperationChanged: function (index) {
			this.generic_updateRow(this._ptable.getChildren()[index], this._mpending.operations[index]);
		},
		/**
		 * Ping callback. Displays latency.
		 */
		_onPing: function (latency) {
			this._latencyText.set('text', gettext("Ping: %dms").sprintf(latency));
		}
	});
	
	/**
	 * Displays various status messages.
	 *
	 * @class {private} pygowave.view.WaveletStatusBar
	 * @extends pygowave.view.Widget
	 */
	var WaveletStatusBar = new Class({
		Extends: Widget,
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {pygowave.model.Wavelet} wavelet Wavelet to be monitored
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {String} where Where to inject the widget relative to the
		 *        parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function (wavelet, parentElement, where) {
			this._wavelet = wavelet;
			
			var contentElement = new Element('div', {'class': 'wavelet_status_bar'});
			
			this._documentIcon = new Element('div', {'class': 'document'}).inject(contentElement);
			this._onStatusChange("clean");
			
			this._textRange = new Element('div', {'class': 'message'}).inject(contentElement);
			
			this.parent(parentElement, contentElement, where);
			
			wavelet.addEvent("statusChange", this._onStatusChange.bind(this));
			this._onCurrentTextRangeChanged = this._onCurrentTextRangeChanged.bind(this);
		},
		
		/**
		 * Connect a blip editor to the status bar.<br/>
		 * Currently captures text range changes.
		 * @function {public} connectBlipEditor
		 * @param {pygowave.view.BlipEditorWidget} wgt A blip editor instance
		 */
		connectBlipEditor: function (wgt) {
			wgt.addEvent("currentTextRangeChanged", this._onCurrentTextRangeChanged);
		},
		
		/**
		 * Callback from wavelet on status change.
		 *
		 * @function {private} _onStatusChange
		 * @param {String} status The new status
		 */
		_onStatusChange: function (status) {
			switch (status) {
				case "dirty":
					this._documentIcon.addClass("dirty").removeClass("clean").removeClass("invalid");
					this._documentIcon.set('title', gettext("Status: Waiting for acknowledgement"));
					break;
				case "clean":
					this._documentIcon.addClass("clean").removeClass("dirty").removeClass("invalid");
					this._documentIcon.set('title', gettext("Status: Document in sync with server"));
					break;
				case "invalid":
					this._documentIcon.addClass("invalid").removeClass("clean").removeClass("dirty");
					this._documentIcon.set('title', gettext("Status: Document not in sync with server"));
					break;
			}
		},
		
		/**
		 * Callback on text range change in a blip.
		 * @function {private} _onCurrentTextRangeChanged
		 * @param {int} start
		 * @param {int} end
		 */
		_onCurrentTextRangeChanged: function (start, end) {
			if (!$defined(start) || !$defined(end))
				this._textRange.set('text', "");
			else if (start == end)
				this._textRange.set('text', gettext("Position: %d").sprintf(start));
			else
				this._textRange.set('text', gettext("Selected: %d-%d").sprintf(start, end));
		}
	});
	
	pygowave.view.extend({
		OperationsViewer: OperationsViewer,
		WaveletStatusBar: WaveletStatusBar
	});
})();
