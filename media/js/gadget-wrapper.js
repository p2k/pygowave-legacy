/*
 * Gadget Wrapper Library
 * Copyright (C) 2009 by p2k and avital
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
 * This script tries to mimic the behaviour of a subset of the Google Gadget API
 * in order to make gadgets work on PyGoWave Server.
 *
 */

// Cut to minimum to allow cross-subdomain access
try {
	document.domain = document.domain.split('.').slice(1).join('.');
}
catch (e){
	document.domain = document.domain;
}

gadgets = {
	_urlParameters: null,
	_prefs: {},
	_country: "us",
	_lang: "en"
};

window.__MODULE_ID__ = null;

gadgets.rpc = {
	call: function (targetId, serviceName, callback, var_args) {
		if (window.__MODULE_ID__ != null)
			window.parent.gadget_rpc.call(window.__MODULE_ID__, targetId, serviceName, callback, var_args);
	},
	register: function (serviceName, handler) {
		if (window.__MODULE_ID__ != null)
			window.parent.gadget_rpc.register(window.__MODULE_ID__, serviceName, handler);
	}
};

gadgets.util = {
	getUrlParameters: function () {
		return gadgets._urlParameters;
	},
	registerOnLoadHandler: function(callback) {
		window.parent.gadget_rpc.registerOnLoadHandler(window.__MODULE_ID__, callback);
	}
};

gadgets.window = {
	adjustHeight: function (opt_height) {
		window.parent.gadget_rpc.adjustHeight(window.__MODULE_ID__, opt_height);
	},
	getViewportDimensions: function () {
		var w = 0, h = 0;
		try {
			w = window.innerWidth;
			h = window.innerHeight;
		}
		catch (e) {
			w = document.documentElement.clientWidth;
			h = document.documentElement.clientHeight;
		}
		return {width: w, height: h};
	},
	setTitle: function (title) {
		// Ignored in Waves
	}
};

function _IG_Prefs(opt_moduleId) {
	if (opt_moduleId == undefined || opt_moduleId == window.__MODULE_ID__) {
		this._moduleId = window.__MODULE_ID__;
		this._prefs = gadgets._prefs;
	}
	else {
		this._moduleId = opt_moduleId;
		this._prefs = {};
	}
	
	this.getArray = function (key) {
		if (key in this._prefs) {
			var pref = this._prefs[key];
			if (pref.datatype == "list") {
				if ("value" in pref)
					return pref.value;
				else if ("default_value" in pref)
					return pref.default_value;
				else
					return [];
			}
			else {
				if ("value" in pref)
					return [pref.value];
				else if ("default_value" in pref)
					return [pref.default_value];
				else
					return [];
			}
		}
		else
			return [];
	};
	
	this.getBool = function (key) {
		if (key in this._prefs) {
			var pref = this._prefs[key];
			if ("value" in pref)
				return !!pref.value;
			else if ("default_value" in pref)
				return pref.default_value;
			else
				return false;
		}
		else
			return false;
	};
	
	this.getCountry = function () {
		return gadgets._country;
	};
	
	this.getLang = function () {
		return gadgets._lang;
	};
	
	this.getFloat = function (key) {
		if (key in this._prefs) {
			var pref = this._prefs[key];
			if ("value" in pref)
				return parseFloat(pref.value);
			else if ("default_value" in pref)
				return parseFloat(pref.default_value);
			else
				return 0;
		}
		else
			return 0;
	};
	
	this.getInt = function (key) {
		if (key in this._prefs) {
			var pref = this._prefs[key];
			if ("value" in pref)
				return parseInt(pref.value);
			else if ("default_value" in pref)
				return parseInt(pref.default_value);
			else
				return 0;
		}
		else
			return 0;
	};
	
	this.getModuleId = function () {
		return window.__MODULE_ID__;
	};
	
	this.getMsg = function (key) {
		return "";
	};
	
	this.getString = function (key) {
		if (key in this._prefs) {
			var pref = this._prefs[key];
			if ("value" in pref)
				return String(pref.value);
			else if ("default_value" in pref)
				return pref.default_value;
			else
				return "";
		}
		else
			return "";
	};
	
	// --- setprefs ---
	this.set = function (key, val) {
		if (key in this._prefs)
			this._prefs[key].value = val;
		else
			this._prefs[key] = {value: val};
		
		window.parent.gadget_rpc.set_pref(this._moduleId, key, val);
	};
	
	this.setArray = this.set;
}

gadgets.Prefs = _IG_Prefs;
