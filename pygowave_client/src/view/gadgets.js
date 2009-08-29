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
 * Classes and functions for handling gadgets.
 * 
 * @module pygowave.view.gadgets
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	
	/**
	 * Add gadget window.
	 *
	 * @class {private} pygowave.view.AddGadgetWindow
	 * @extends MochaUI.Window
	 */
	var AddGadgetWindow = new Class({
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
			
			this._content = new Element('div', {'class': 'wavelet_add_gadget'});
			new Element('div', {'text': gettext("Choose a Gadget from the list below:")}).inject(this._content);
			this._selectDiv = new Element('div', {'class': 'select_div'}).inject(this._content);
			this._select = new Element('select', {'disabled': true}).inject(this._selectDiv);
			this._refreshButton = new Element('div', {'class': 'refresh_button'}).inject(this._selectDiv);
			this._descr = new Element('div', {'class': 'description'}).inject(this._content);
			
			this._selection = 0;
			this._select.addEvent('change', this._onSelect.bind(this));
			
			var buttons = {};
			buttons[gettext("Cancel")] = this._onCancel.bind(this);
			buttons[gettext("OK")] = this._onOK.bind(this);
			
			this.parent({
				title: gettext("Add Gadget"),
				content: this._content,
				width: 310,
				height: 150,
				headerStartColor: [95, 163, 237],
				headerStopColor: [85, 144, 210],
				bodyBgColor: [201, 226, 252],
				closeBgColor: [66, 114, 166],
				closeColor: [255, 255, 255],
				cornerRadius: 4,
				resizable: false,
				footerHeight: 34,
				rtl: this._view.options.rtl,
				buttons: buttons
			});
			
			this._gadgetInfo = new Hash();
			this._refreshButton.addEvent('click', this._onRefreshClicked.bind(this));
			
			this._select.grab(new Element('option', {'text': gettext("Loading..."), 'value': 0}));
			this.showSpinner(this.spinnerEl);
		},
		
		/**
		 * Fire an event to initially load the Gadgets list.
		 *
		 * @function {public} initLoadGadgetList
		 */
		initLoadGadgetList: function () {
			this._view.fireEvent('refreshGadgetList', [this._waveletId, false]);
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
			if (this._selection == 0)
				this._view.showMessage(gettext("You must choose a gadget from the list first."), gettext("Notice"));
			else {
				var info = this._gadgetInfo[this._selection];
				if (this._view.insertGadgetAtCursor(this._waveletId, info[2]))
					MochaUI.closeWindow(this.windowEl);
			}
		},
		
		/**
		 * Updates the gadget list and descriptions. Forwarded from the view.
		 *
		 * @function {public} updateGadgetList
		 * @param {Object[]} gadgets List of gadgets with the following fields:
		 * @... {int} id ID of the gadget
		 * @... {String} name Display name of the gadget
		 * @... {String} descr Description of the gadget
		 */
		updateGadgetList: function (gadgets) {
			var found = false;
			this._select.empty();
			this._select.grab(new Element('option', {'text': gettext("(Please choose...)"), 'value': 0}));
			this._gadgetInfo.empty();
			for (var i = 0; i < gadgets.length; i++) {
				var gadget = gadgets[i];
				this._gadgetInfo.set(gadget.id, [gadget.uploaded_by, gadget.descr, gadget.url]);
				var opt = new Element('option', {'text': gadget.name, 'value': gadget.id}).inject(this._select);
				if (gadget.id == this._selection)
					found = true;
			}
			
			if (found)
				this._select.set('value', this._selection);
			this._onSelect();
			
			this._select.set('disabled', false);
			this.hideSpinner(this.spinnerEl);
		},
		
		/**
		 * Callback from select; displays description.
		 *
		 * @function {private} _onSelect
		 */
		_onSelect: function () {
			var id = parseInt(this._select.get('value'));
			this._selection = id;
			if (id == 0) {
				this._descr.set('html', "<p>" + gettext("Description") + ":<br/>-</p>");
			}
			else {
				var info = this._gadgetInfo[id];
				this._descr.set('html', "<p>" + gettext("Description") + ":<br/>" + info[1] + "</p><p>" + gettext("Uploaded by %s").sprintf(info[0]) + "</p>");
			}
		},
		
		/**
		 * Callback from refresh button. Fires event.
		 *
		 * @function {private} _onRefreshClicked
		 */
		_onRefreshClicked: function () {
			this._select.empty();
			this._select.set('disabled', true);
			this._select.grab(new Element('option', {'text': gettext("Loading..."), 'value': 0}));
			this.showSpinner(this.spinnerEl);
			this._view.fireEvent('refreshGadgetList', [this._waveletId, true]);
		}
	});
	
	/**
	 * Widget for managing a GadgetElement. This also is a RPC handler.
	 *
	 * @class {private} pygowave.view.GadgetElementWidget
	 * @extends pygowave.view.Widget
	 */
	var GadgetElementWidget = new Class({
		Extends: Widget,
		
		/**
		 * Fired when the user clicks on the delete button.
		 * @event onDeleteClicked
		 * @param {int} index Index of the element
		 */
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {pygowave.model.GadgetElement} gadgetElement GadgetElement to be rendered
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {String} where Where to inject the widget relative to the
		 *        parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function (view, gadgetElement, parentElement, where) {
			this._onParticipantsChanged = this._onParticipantsChanged.bind(this);
			this._onStateChange = this._onStateChange.bind(this);
			this._onSetUserPref = this._onSetUserPref.bind(this);
			
			this._view = view;
			this._gadgetElement = gadgetElement;
			var blip = gadgetElement.blip();
			this._wavelet = blip.wavelet();
			this._blipId = blip.id();
			this._viewerId = this._wavelet.waveModel().viewerId();
			
			this._callbacksOnLoad = new Array();
			this._callbacksRegistered = new Hash();
			
			window.gadget_rpc.registerModuleHandler(gadgetElement.id(), this);
			this._wavelet.addEvent('participantsChanged', this._onParticipantsChanged);
			this._gadgetElement.addEvent('stateChange', this._onStateChange);
			this._gadgetElement.addEvent('setUserPref', this._onSetUserPref);
			
			var contentElement = new Element('div', {'class': 'gadget_element'});
			contentElement.contentEditable = "false";
			
			this._deleteBox = new Element('div', {
				'class': 'delete_box',
				'title': gettext("Delete Gadget")
			}).inject(contentElement);
			this._deleteBox.addEvent('click', function () {
				this.fireEvent('deleteClicked', this._gadgetElement.position());
			}.bind(this));
			
			this._gadgetFrame = new Element('iframe', {
				'src': "%surl=%s&gadget_id=%d".sprintf(view.options.gadgetLoaderUrl, encodeURIComponent(gadgetElement.url()), gadgetElement.id())
			}).inject(contentElement);
			this._gadgetFrame.addEvent('load', this._onGadgetLoaded.bind(this));
			
			this.parent(parentElement, contentElement, where);
		},
		
		dispose: function () {
			this.parent();
			this._wavelet.removeEvent('participantsChanged', this._onParticipantsChanged);
			window.gadget_rpc.unregisterModuleHandler(this._gadgetElement.id());
		},
		
		element: function () {
			return this._gadgetElement;
		},
		position: function () {
			return this._gadgetElement.position();
		},
		
		_onGadgetLoaded: function () {
			for (var it = new _Iterator(this._callbacksOnLoad); it.hasNext(); ) {
				var callback = it.next();
				callback();
			}
			this._callbacksOnLoad.empty();
			this._onParticipantsChanged();
			this._onStateChange();
			this.adjustHeight();
		},
		_onParticipantsChanged: function () {
			this.invokeCallbacks(
				"wave_participants",
				{
					myId: this._viewerId,
					authorId: 0,
					participants: this._wavelet.allParticipantsForGadget()
				}
			);
		},
		_onStateChange: function () {
			this.invokeCallbacks("wave_gadget_state", this._gadgetElement.fields());
		},
		_onSetUserPref: function (key, value) {
			this.invokeCallbacks("set_pref", ["unknown", key, value]);
		},
		invokeCallbacks: function (serviceName, var_args) {
			if (!this._callbacksRegistered.has(serviceName))
				return;
			for (var it = new _Iterator(this._callbacksRegistered.get(serviceName)); it.hasNext(); ) {
				var callback = it.next();
				var takethis = {a: var_args};
				callback.call(takethis, var_args);
			}
		},
		
		call: function (targetId, serviceName, callback, var_args) {
			if (targetId == null) {
				switch (serviceName) {
					case "wave_gadget_state":
						this._view.fireEvent('elementDeltaSubmitted', [this._wavelet.id(), this._blipId, this._gadgetElement.position(), var_args]);
						break;
					case "wave_log":
						dbgprint(gettext("Gadget #%d at index %d says:").sprintf(this._gadgetElement.id(), this._gadgetElement.position()), var_args);
						break;
					case "wave_enable":
						//TODO?
						break;
				}
			}
		},
		register: function (serviceName, handler) {
			if (!this._callbacksRegistered.has(serviceName))
				this._callbacksRegistered.set(serviceName, new Array());
			this._callbacksRegistered.get(serviceName).push(handler);
		},
		registerOnLoadHandler: function (callback) {
			this._callbacksOnLoad.push(callback);
		},
		adjustHeight: function (opt_height) {
			if ($defined(opt_height))
				this._gadgetFrame.setStyle('height', opt_height);
			else {
				try { // On unexpected errors in the gadget loader, this may fail
					var doc = this._gadgetFrame.contentDocument;
					this._gadgetFrame.setStyle('height', doc.documentElement.scrollHeight);
				}catch(e){}
			}
		},
		set_pref: function (key, value) {
			this._view.fireEvent('elementSetUserpref', [this._wavelet.id(), this._blipId, this._gadgetElement.position(), key, value]);
		}
	});
	
	/**
	 * Singleton class for handling Gadget RPC calls.
	 * Acts as a proxy between the actual gadget in the iframe and the
	 * associated GadgetElementWidget.
	 *
	 * @class {private} GadgetRPCHandler
	 */
	var GadgetRPCHandler = new Class({
		Implements: Events,
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 */
		initialize: function () {
			this._modules = new Hash();
		},
		
		registerModuleHandler: function (moduleId, handler) {
			this._modules.set(moduleId, handler);
		},
		
		unregisterModuleHandler: function (moduleId) {
			this._modules.erase(moduleId);
		},
		
		
		call: function (moduleId, targetId, serviceName, callback, var_args) {
			if (this._modules.has(moduleId))
				this._modules[moduleId].call(targetId, serviceName, callback, var_args);
		},
		
		register: function (moduleId, serviceName, handler) {
			if (this._modules.has(moduleId))
				this._modules[moduleId].register(serviceName, handler);
		},
		
		registerOnLoadHandler: function (moduleId, callback) {
			if (this._modules.has(moduleId))
				this._modules[moduleId].registerOnLoadHandler(callback);
		},
		
		adjustHeight: function (moduleId, opt_height) {
			if (this._modules.has(moduleId))
				this._modules[moduleId].adjustHeight(opt_height);
		},
		
		set_pref: function (moduleId, key, value) {
			if (this._modules.has(moduleId))
				this._modules[moduleId].set_pref(key, value);
		}
	});
	
	window.gadget_rpc = new GadgetRPCHandler();
	
	pygowave.view.extend({
		AddGadgetWindow: AddGadgetWindow,
		GadgetElementWidget: GadgetElementWidget
	});
})();
