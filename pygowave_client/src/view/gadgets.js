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
			
			this._content = new Element('div', {'class': 'wavelet_add_gadget_div'});
			new Element('div', {'text': gettext("Choose a Gadget from the list below:")}).inject(this._content);
			this._selectDiv = new Element('div').inject(this._content);
			this._select = new Element('select', {'disabled': true}).inject(this._selectDiv);
			
			var buttons = {};
			buttons[gettext("Cancel")] = this._onCancel.bind(this);
			buttons[gettext("OK")] = this._onOK.bind(this);
			
			this.parent({
				title: gettext("Add Gadget"),
				content: this._content,
				width: 310,
				height: 75,
				headerStartColor: [95, 163, 237],
				headerStopColor: [85, 144, 210],
				bodyBgColor: [201, 226, 252],
				closeBgColor: [66, 114, 166],
				closeColor: [255, 255, 255],
				cornerRadius: 4,
				resizable: false,
				footerHeight: 34,
				rtl: this._view.options.rtl,
				buttons: buttons,
				padding: {top: 0, right: 0, bottom: 0, left: 0}
			});
			
			this._select.grab(new Element('option', {'text': gettext("Loading..."), 'value': 0}));
			this.showSpinner(this.spinnerEl);
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
			
		}
	});
	
	/**
	 * Singleton class for handling Gadget RPC calls.
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
		
		
		
		call: function (moduleId, targetId, serviceName, callback, var_args) {
			//if (this._modules.has(moduleId))
		},
		
		register: function (moduleId, serviceName, handler) {
			
		},
		
		registerOnLoadHandler: function (moduleId, callback) {
			
		},
		
		adjustHeight: function (moduleId, opt_height) {
			
		},
		
		set_pref: function (moduleId, key, val) {
			
		}
	});
	
	window.gadget_rpc = new GadgetRPCHandler();
	
	pygowave.view.extend({
		AddGadgetWindow: AddGadgetWindow
	});
})();
