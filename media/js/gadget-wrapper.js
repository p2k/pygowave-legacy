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

gadgets = {};

// Cut to minimum to allow cross-subdomain access
try {
	document.domain = document.domain.split('.').slice(1).join('.');
}
catch (e){
	document.domain = document.domain;
}

jQuery.extend(gadgets, {
	rpc: {
		call: function (targetId, serviceName, callback, var_args) {
			if (gadgets._gadgetID != null)
				window.parent.gadget_rpc.call(gadgets._gadgetID, targetId, serviceName, callback, var_args);
		},
		register: function (serviceName, handler) {
			if (gadgets._gadgetID != null)
				window.parent.gadget_rpc.register(gadgets._gadgetID, serviceName, handler);
		}
	},

	util: {
		getUrlParameters: function () {
			return gadgets._urlParameters;
		},
		registerOnLoadHandler: function(callback) {
			window.parent.gadget_rpc.registerOnLoadHandler(gadgets._gadgetID, callback);
		}
	},
	
	_urlParameters: null,
	_gadgetID: null
});
