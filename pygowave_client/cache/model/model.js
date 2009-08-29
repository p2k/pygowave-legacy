/* This file was generated with PyCow - the Python to JavaScript translator */

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

window.pygowave = $defined(window.pygowave) ? window.pygowave : {};

/**
 * Models module.
 * @module pygowave.model
 */
pygowave.model = (function() {
	/* from pycow.decorators import Class, Implements */;

	/* from pycow.utils import Events, Options, Hash */;

	/* from hashlib import sha1 as sha_constructor */;

	/**
	 * Models a participant to a Wavelet. Note that the implementation (i.e.
	 * the controller) should only create one participant object per participant
	 * because view objects are connected to only one participant instance.
	 * If the state of a participant changes they can be updated as appropriate.
	 * @class {public} pygowave.model.Participant
	 */
	var Participant = new Class({
		Implements: [Options, Events],
		options: {
			displayName: "",
			thumbnailUrl: "",
			profileUrl: "",
			isOnline: false,
			isBot: false
		}
		/**
		 * Fired if the participant's online state changes.
		 * @event onOnlineStateChanged
		 * @param {Boolean} online True, if the participant is now online.
		 */
		
		/**
		 * Fired if the participant's data changes.
		 * @event onDataChanged
		 */
		,

		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {String} id ID (address) of the participant
		 * @param {optional Object} options Additional information:
		 * @... {String} displayName Display name
		 * @... {String} thumbnailUrl URL of the participant's avatar
		 * @... {String} profileUrl A link to the participant's profile
		 * @... {Boolean} isOnline True, if the participant is online
		 * @... {Boolean} isBot True, if the participant is a robot
		 */
		initialize: function (id, options) {
			if (!$defined(options)) options = null;
			this.setOptions(options);
			this._id = id;
			if (this.options.name == "")
				this.options.name = id;
		},

		/**
		 * Returns the ID (address) of the participant.
		 * @function {public String} id
		 */
		id: function () {
			return this._id;
		},

		/**
		 * Returns the participant's display name.
		 * @function {public String} displayName
		 */
		displayName: function () {
			return this.options.displayName;
		},

		/**
		 * Returns the URL of the participant's avatar.
		 * @function {public String} thumbnailUrl
		 */
		thumbnailUrl: function () {
			return this.options.thumbnailUrl;
		},

		/**
		 * Returns the URL to the participant's profile.
		 * @function {public String} profileUrl
		 */
		profileUrl: function () {
			return this.options.profileUrl;
		},

		/**
		 * Returns weather the participant is a bot.
		 * @function {public Boolean} isBot
		 */
		isBot: function () {
			return this.options.isBot;
		},

		/**
		 * Returns weather the participant is a online.
		 * @function {public Boolean} isOnline
		 */
		isOnline: function () {
			return this.options.isOnline;
		},

		/**
		 * Set the participant's online state.<br/>
		 * Fires onOnlineStateChanged if changed.
		 * @function {public} setOnline
		 * @param {Boolean} online True, if the participant is now online.
		 */
		setOnline: function (online) {
			if (this.options.isOnline != online) {
				this.options.isOnline = online;
				this.fireEvent("onlineStateChanged", online);
			}
		},

		/**
		 * Updates participant data from a JSON-serialized map/dict.<br/>
		 * Fires onDataChanged.
		 *
		 * @function {public} updateData
		 * @param {Object} obj JSON-serialized participant data
		 */
		updateData: function (obj) {
			this.options.displayName = obj.displayName;
			this.options.thumbnailUrl = obj.thumbnailUrl;
			this.options.profileUrl = obj.profileUrl;
			this.options.isBot = obj.isBot;
			this.fireEvent("dataChanged");
		},

		/**
		 * Convenience function to serialize a participant object into the
		 * Wave Gadget API format.
		 * @function {public Object} toGadgetFormat
		 */
		toGadgetFormat: function () {
			return {
				id: this._id,
				displayName: this.options.displayName,
				thumbnailUrl: this.options.thumbnailUrl
			};
		}
	});

	/**
	 * An annotation is metadata that augments a range of text in a Blip's text.
	 * Example uses of annotations include styling text, supplying spelling
	 * corrections, and links to refer that area of text to another Blip or
	 * web site. The size of an annotation range must be positive and non-zero.
	 *
	 * @class {private} pygowave.model.Annotation
	 */
	var Annotation = new Class({

		/**
		 * Called on instantiation. Documented for internal purposes.
		 * @constructor {private} initialize
		 * @param {Blip} blip The annotation's parent Blip
		 * @param {String} name The annotation's name
		 * @param {int} start Start index
		 * @param {int} end End index
		 * @param {String} value The annotation's value
		 */
		initialize: function (blip, name, start, end, value) {
			this._blip = blip;
			this._name = name;
			this._start = start;
			this._end = end;
			this._value = value;
		},

		/**
		 * Returns the Blip on which this element resides.
		 *
		 * @function {public Blip} blip
		 */
		blip: function () {
			return this._blip;
		},

		/**
		 * Returns the name of this annotation.
		 *
		 * @function {public String} name
		 */
		name: function () {
			return this._name;
		},

		/**
		 * Returns the start index of this annotation.
		 *
		 * @function {public int} start
		 */
		start: function () {
			return this._start;
		},

		/**
		 * Sets the start index of this annotation.
		 *
		 * @function {public} setStart
		 * @param {int} index The new start index
		 */
		setStart: function (index) {
			this._start = index;
		},

		/**
		 * Returns the end index of this annotation.
		 *
		 * @function {public int} end
		 */
		end: function () {
			return this._end;
		},

		/**
		 * Sets the end index of this annotation.
		 *
		 * @function {public} setEnd
		 * @param {int} index The new end index
		 */
		setEnd: function (index) {
			this._end = index;
		}
	});

	var ELEMENT_TYPE = {
		NOTHING: 0,
		INLINE_BLIP: 1,
		GADGET: 2,
		INPUT: 3,
		CHECK: 4,
		LABEL: 5,
		BUTTON: 6,
		RADIO_BUTTON: 7,
		RADIO_BUTTON_GROUP: 8,
		PASSWORD: 9,
		IMAGE: 10
	};

	/**
	 * Element-objects are all the non-text elements in a Blip.
	 * An element has no physical presence in the text of a Blip, but it maintains
	 * an implicit protected newline character to keep positions distinct.
	 *
	 * Only special Wave Client elements are treated here.
	 * There are no HTML elements in any Blip. All markup is handled by Annotations.
	 *
	 * @class {private} pygowave.model.Element
	 */
	var Element = new Class({
		Implements: Events,

		/**
		 * Called on instantiation. Documented for internal purposes.
		 * @constructor {private} initialize
		 * @param {Blip} blip The element's parent Blip
		 * @param {int} id ID of the element, setting this to null will assign
		 * a new temporaty ID
		 * @param {int} position Index where this element resides
		 * @param {int} type Type of the element
		 * @param {Object} properties The element's properties
		 */
		initialize: function (blip, id, position, type, properties) {
			this._blip = blip;
			if (id == null)
				this._id = Element.newTempId();
			else
				this._id = id;
			this._pos = position;
			this._type = type;
			if (properties == null)
				this._properties = new Hash();
			else
				this._properties = new Hash(properties);
		},

		/**
		 * Returns the Blip on which this element resides.
		 *
		 * @function {public Blip} blip
		 */
		blip: function () {
			return this._blip;
		},

		/**
		 * Set the parent Blip to the given Blip.
		 *
		 * @function {public} setBlip
		 */
		setBlip: function (blip) {
			this._blip = blip;
		},

		/**
		 * Return the ID of the element. A negative ID represents a temporary ID,
		 * that is only valid for this session and will be replaced by a real ID
		 * on reload.
		 *
		 * @function {public int} id
		 */
		id: function () {
			return this._id;
		},

		/**
		 * Returns the type of the element.
		 *
		 * @function {public int} type
		 */
		type: function () {
			return this._type;
		},

		/**
		 * Returns the index where this element resides
		 *
		 * @function {public int} position
		 */
		position: function () {
			return this._pos;
		},

		/**
		 * Sets the index where this element resides
		 *
		 * @function {public} setPosition
		 * @param {int} pos New position
		 */
		setPosition: function (pos) {
			this._pos = pos;
		}
	});
	Element.newTempId = function () {
		Element.lastTempId--;
		return Element.lastTempId;
	};

	Element.lastTempId = 0;

	/**
	 * A gadget element.
	 *
	 * @class pygowave.model.GadgetElement
	 * @extends pygowave.model.Element
	 */
	var GadgetElement = new Class({
		Extends: Element,
		/**
		 * Fired on gadget state change.
		 *
		 * @event onStateChange
		 */
		/**
		 * Fired on UserPref setting.
		 *
		 * @event onSetUserPref
		 * @param {String} key
		 * @param {Object} value
		 */

		/**
		 * Called on instantiation. Documented for internal purposes.
		 * @constructor {private} initialize
		 * @param {Blip} blip The gadget's parent Blip
		 * @param {int} id ID of the element, setting this to null will assign
		 * a new temporaty ID
		 * @param {int} position Index where this gadget resides
		 * @param {Object} properties The gadget element's properties
		 * @param {int} id
		 */
		initialize: function (blip, id, position, properties) {
			this.parent(blip, id, position, ELEMENT_TYPE.GADGET, properties);
			if (this._properties.has("fields"))
				this._properties.set("fields", new Hash(this._properties.get("fields")));
			if (this._properties.has("userprefs"))
				this._properties.set("userprefs", new Hash(this._properties.get("userprefs")));
		},

		/**
		 * Return the gadget's state object i.e. the field map.
		 *
		 * @function {public Object} fields
		 */
		fields: function () {
			if (this._properties.has("fields"))
				return this._properties.get("fields").getClean();
			else
				return {};
		},

		/**
		 * Return all UserPrefs as hash.
		 *
		 * @function {public Hash} userPrefs
		 */
		userPrefs: function () {
			if (this._properties.has("userprefs"))
				return this._properties.get("userprefs");
			else
				return {};
		},

		/**
		 * Returns the gadget xml URL.
		 *
		 * @function {public String} url
		 */
		url: function () {
			if (this._properties.has("url"))
				return this._properties.get("url");
			else
				return "";
		},

		/**
		 * Apply a delta to this gadget's state.
		 *
		 * @function {public} applyDelta
		 * @param {Object} delta An object whose fields will be merged into the
		 * gadget state.
		 */
		applyDelta: function (delta) {
			if (!this._properties.has("fields"))
				this._properties.set("fields", new Hash());
			var fields = this._properties.get("fields");
			fields.update(delta);
			for (var __iter0_ = new _Iterator(fields); __iter0_.hasNext();) {
				var value = __iter0_.next();
				var key = __iter0_.key();
				if (value == null)
					delete fields[key];
			}
			delete __iter0_;
			this.fireEvent("stateChange");
		},

		/**
		 * Set a UserPref.
		 *
		 * @function {public} setUserPref
		 * @param {String} key
		 * @param {Object} value
		 */
		setUserPref: function (key, value, noevent) {
			if (!$defined(noevent)) noevent = false;
			if (!this._properties.has("userprefs"))
				this._properties.set("userprefs", new Hash());
			this._properties.get("userprefs").set(key, value);
			if (!noevent)
				this.fireEvent("setUserPref", [key, value]);
		}
	});

	/**
	 * Models a Blip in a Wavelet.<br/>
	 * This is a private class and cannot be instanitated directly. Please
	 * use {@link pygowave.model.Wavelet.addBlip Wavelet.addBlip}.
	 * @class {private} pygowave.model.Blip
	 */
	var Blip = new Class({
		Implements: [Options, Events],
		options: {
			creator: null,
			is_root: false,
			last_modified: null,
			version: 0,
			submitted: false
		}
		/**
		 * Fired on text insertion.
		 *
		 * @event onInsertText
		 * @param {int} index Offset where the text is inserted
		 * @param {String} text The text to be inserted
		 */
		
		/**
		 * Fired on text deletion.
		 *
		 * @event onDeleteText
		 * @param {int} index Offset where the text is deleted
		 * @param {int} length Number of characters to delete
		 */
		
		/**
		 * Fired on element insertion.
		 *
		 * @event onInsertElement
		 * @param {int} index Offset where the element is inserted
		 */
		
		/**
		 * Fired on element deletion.
		 *
		 * @event onDeleteElement
		 * @param {int} index Offset where the element is deleted
		 */
		
		/**
		 * Fired if the Blip has gone out of sync with the server.
		 *
		 * @event onOutOfSync
		 */
		,

		/**
		 * Called on instantiation. Documented for internal purposes.
		 * @constructor {private} initialize
		 * @param {Wavelet} wavelet Parent Wavelet object
		 * @param {String} id ID of this Blip
		 * @param {Object} options Information about the Blip. Possible values:
		 * @... {Participant} creator Creator of this Blip
		 * @... {Boolean} is_root True if this is the root Blip; if this value
		 * is set and the parent Wavelet does not have a root Blip yet,
		 * this Blip is set as the Wavelet's root Blip
		 * @... {Date} last_modified Date of last modification
		 * @... {int} version Version of the Blip
		 * @... {Boolean} submitted True if this Blip is submitted
		 * @param {options String} content Content of the Blip
		 * @param {optional Element[]} elements Element objects which initially
		 * reside in this Blip
		 * @param {optional Blip} parent Parent Blip if this is a nested Blip
		 */
		initialize: function (wavelet, id, options, content, elements, parent) {
			if (!$defined(content)) content = "";
			if (!$defined(elements)) elements = [];
			if (!$defined(parent)) parent = null;
			this.setOptions(options);
			this._wavelet = wavelet;
			this._id = id;
			this._parent = parent;
			this._content = content;
			this._elements = elements;
			for (var __iter0_ = new _Iterator(this._elements); __iter0_.hasNext();) {
				var element = __iter0_.next();
				element.setBlip(this);
			}
			delete __iter0_;
			this._annotations = [];
			this._outofsync = false;
		},

		/**
		 * Returns the ID of this Blip.
		 *
		 * @function {public String} id
		 */
		id: function () {
			return this._id;
		},

		/**
		 * Returns the parent Wavelet of this Blip.
		 *
		 * @function {public Wavelet} wavelet
		 */
		wavelet: function () {
			return this._wavelet;
		},

		/**
		 * Returns true, if this Blip is the Wavelet's root Blip.
		 * @function {public Boolean} isRoot
		 */
		isRoot: function () {
			return this.options.is_root;
		},

		/**
		 * Returns the Element object at the given position or null.
		 *
		 * @function {public Element} elementAt
		 * @param {int} index Index of the element to retrieve
		 */
		elementAt: function (index) {
			for (var __iter0_ = new XRange(len(this._elements)); __iter0_.hasNext();) {
				var i = __iter0_.next();
				var elt = this._elements[i];
				if (elt.position() == index)
					return elt;
			}
			delete __iter0_;
			return null;
		},

		/**
		 * Returns the Elements between the start and end index.
		 *
		 * @function {public Element[]} elementsWithin
		 * @param {int} start Start index
		 * @param {int} end End index
		 */
		elementsWithin: function (start, end) {
			var lst = [];
			for (var __iter0_ = new XRange(len(this._elements)); __iter0_.hasNext();) {
				var i = __iter0_.next();
				var elt = this._elements[i];
				if (elt.position() >= start && elt.position() < end)
					lst.append(elt);
			}
			delete __iter0_;
			return lst;
		},

		/**
		 * Returns all Elements of this Blip.
		 *
		 * @function {public Element[]} allElements
		 */
		allElements: function () {
			return this._elements;
		},

		/**
		 * Insert a text at the specified index. This moves annotations and
		 * elements as appropriate.<br/>
		 * Note: This sets the wavelet status to 'dirty'.
		 *
		 * @function {public} insertText
		 * @param {int} index Index of insertion
		 * @param {String} text Text to be inserted
		 * @param {optional Boolean} noevent Set to true if no event should be generated
		 */
		insertText: function (index, text, noevent) {
			if (!$defined(noevent)) noevent = false;
			this._content = this._content.slice(0, index) + text + this._content.slice(index);
			var length = len(text);
			for (var __iter0_ = new _Iterator(this._elements); __iter0_.hasNext();) {
				var elt = __iter0_.next();
				if (elt.position() >= index)
					elt.setPosition(elt.position() + length);
			}
			delete __iter0_;
			for (var __iter0_ = new _Iterator(this._annotations); __iter0_.hasNext();) {
				var anno = __iter0_.next();
				if (anno.start() >= index) {
					anno.setStart(anno.start() + length);
					anno.setEnd(anno.end() + length);
				}
			}
			delete __iter0_;
			this._wavelet._setStatus("dirty");
			if (!noevent)
				this.fireEvent("insertText", [index, text]);
		},

		/**
		 * Delete text at the specified index. This moves annotations and
		 * elements as appropriate.<br/>
		 * Note: This sets the wavelet status to 'dirty'.
		 *
		 * @function {public} deleteText
		 * @param {int} index Index of deletion
		 * @param {int} length Number of characters to delete
		 * @param {optional Boolean} noevent Set to true if no event should be generated
		 */
		deleteText: function (index, length, noevent) {
			if (!$defined(noevent)) noevent = false;
			this._content = this._content.slice(0, index) + this._content.slice(index + length);
			for (var __iter0_ = new _Iterator(this._elements); __iter0_.hasNext();) {
				var elt = __iter0_.next();
				if (elt.position() >= index)
					elt.setPosition(elt.position() - length);
			}
			delete __iter0_;
			for (var __iter0_ = new _Iterator(this._annotations); __iter0_.hasNext();) {
				var anno = __iter0_.next();
				if (anno.start() >= index) {
					anno.setStart(anno.start() - length);
					anno.setEnd(anno.end() - length);
				}
			}
			delete __iter0_;
			this._wavelet._setStatus("dirty");
			if (!noevent)
				this.fireEvent("deleteText", [index, length]);
		},

		/**
		 * Insert an element at the specified index. This implicitly adds a
		 * protected newline character at the index.<br/>
		 * Note: This sets the wavelet status to 'dirty'.
		 *
		 * @function {public} insertElement
		 * @param {int} index Position of the new element
		 * @param {String} type Element type
		 * @param {Object} properties Element properties
		 * @param {optional Boolean} noevent Set to true if no event should be generated
		 */
		insertElement: function (index, type, properties, noevent) {
			if (!$defined(noevent)) noevent = false;
			this.insertText(index, "\n", true);
			if (type == 2)
				var elt = new GadgetElement(this, null, index, properties);
			else
				elt = new Element(this, null, index, type, properties);
			this._elements.append(elt);
			this._wavelet._setStatus("dirty");
			if (!noevent)
				this.fireEvent("insertElement", index);
		},

		/**
		 * Delete an element at the specified index. This implicitly deletes the
		 * protected newline character at the index.<br/>
		 * Note: This sets the wavelet status to 'dirty'.
		 *
		 * @function {public} deleteElement
		 * @param {int} index Position of the element to delete
		 * @param {optional Boolean} noevent Set to true if no event should be generated
		 */
		deleteElement: function (index, noevent) {
			if (!$defined(noevent)) noevent = false;
			for (var __iter0_ = new XRange(len(this._elements)); __iter0_.hasNext();) {
				var i = __iter0_.next();
				var elt = this._elements[i];
				if (elt.position() == index) {
					this._elements.pop(i);
					break;
				}
			}
			delete __iter0_;
			this.deleteText(index, 1, true);
			if (!noevent)
				this.fireEvent("deleteElement", index);
		},

		/**
		 * Apply an element delta. Currently only for gadget elements.<br/>
		 * Note: This action always emits stateChange.
		 *
		 * @function {public} applyElementDelta
		 * @param {int} index Position of the element
		 * @param {Object} delta Delta to apply to the element
		 */
		applyElementDelta: function (index, delta) {
			for (var __iter0_ = new _Iterator(this._elements); __iter0_.hasNext();) {
				var elt = __iter0_.next();
				if (elt.position() == index) {
					elt.applyDelta(delta);
					break;
				}
			}
			delete __iter0_;
		},

		/**
		 * Set an UserPref of an element. Currently only for gadget elements.
		 *
		 * @function {public} setElementUserpref
		 * param {int} index Position of the element
		 * @param {Object} key Name of the UserPref
		 * @param {Object} value Value of the UserPref
		 * @param {optional Boolean} noevent Set to true if no event should be generated
		 */
		setElementUserpref: function (index, key, value, noevent) {
			if (!$defined(noevent)) noevent = false;
			for (var __iter0_ = new _Iterator(this._elements); __iter0_.hasNext();) {
				var elt = __iter0_.next();
				if (elt.position() == index) {
					elt.setUserPref(key, value, noevent);
					break;
				}
			}
			delete __iter0_;
		},

		/**
		 * Returns the text content of this Blip.
		 * @function {public String} content
		 */
		content: function () {
			return this._content;
		},

		/**
		 * Calculate a checksum of this Blip and compare it against the given
		 * checksum. Fires {@link pygowave.model.Blip.onOutOfSync onOutOfSync} if
		 * the checksum is wrong. Returns true if the checksum is ok.
		 *
		 * Note: Currently this only calculates the SHA-1 of the Blip's text. This
		 * is tentative and subject to change
		 *
		 * @function {public Boolean} checkSync
		 * @param {String} sum Input checksum to compare against
		 */
		checkSync: function (sum) {
			if (this._outofsync)
				return false;
			var mysum = sha_constructor(this._content.encode("utf-8")).hexdigest();
			if (mysum != sum) {
				this.fireEvent("outOfSync");
				this._outofsync = true;
				return false;
			}
			else
				return true;
		}
	});

	/**
	 * Models a Wavelet on a Wave.<br/>
	 * This is a private class and cannot be instanitated directly. Please
	 * use {@link pygowave.model.WaveModel.createWavelet WaveModel.createWavelet}.
	 * @class {private} pygowave.model.Wavelet
	 */
	var Wavelet = new Class({
		Implements: [Options, Events],
		options: {
			creator: null,
			is_root: false,
			created: null,
			last_modified: null,
			title: "",
			version: 0,
			status: "clean"
		}
		/**
		 * Fired if a participant joins or leaves.
		 * @event onParticipantsChanged
		 */
		
		/**
		 * Fired if a Blip was inserted.
		 * @event onBlipInserted
		 * @param {int} index Index of the inserted Blip
		 * @param {String} blip_id ID of the inserted Blip
		 */
		
		/**
		 * Fired when the Wavelet's status changed.
		 *
		 * @event onStatusChange
		 * @param {String} status The new status; can be 'clean', 'dirty' or 'invalid'
		 */
		,

		/**
		 * Called on instantiation. Documented for internal purposes.
		 * @constructor {private} initialize
		 * @param {WaveModel} wave Parent WaveModel object
		 * @param {String} id Wavelet ID
		 * @param {Object} options Information about the Wavelet. Possible values:
		 * @... {Participant} creator Creator of this Wavelet
		 * @... {Boolean} is_root True if this is the root Wavelet; if this value
		 * is set and the parent WaveModel does not have a root Wavelet yet,
		 * this Wavelet is set as the Wave's root Wavelet
		 * @... {Date} created Date of creation
		 * @... {Date} last_modified Date of last modification
		 * @... {String} title Title of the Wavelet
		 * @... {int} version Version of the Wavelet
		 * @... {String} status Status of the Wavelet; can be 'clean', 'dirty' or 'invalid'
		 */
		initialize: function (wave, id, options) {
			this.setOptions(options);
			this._wave = wave;
			if (this.options.is_root) {
				if (this._wave.rootWavelet() == null)
					this._wave._setRootWavelet(this);
				else
					this.options.is_root = false;
			}
			this._id = id;
			this._participants = new Hash();
			this._blips = [];
			this._rootBlip = null;
		},

		/**
		 * Returns true, if this Wavelet is the Wave's root Wavelet.
		 * @function {public Boolean} isRoot
		 */
		isRoot: function () {
			return this.options.is_root;
		},

		/**
		 * Returns the ID of this Wavelet.
		 * @function {public String} id
		 */
		id: function () {
			return this._id;
		},

		/**
		 * Returns the ID of this Wavelet's Wave.
		 * @function {public String} waveId
		 */
		waveId: function () {
			return this._wave.id();
		},

		/**
		 * Returns the parent WaveModel object.
		 * @function {public WaveModel} waveModel
		 */
		waveModel: function () {
			return this._wave;
		},

		/**
		 * Add a participant to this Wavelet.<br/>
		 * Note: Fires {@link pygowave.model.Wavelet.onParticipantsChanged onParticipantsChanged}
		 *
		 * @function {public} addParticipant
		 * @param {Participant} participant Participant to be added
		 */
		addParticipant: function (participant) {
			if (!this._participants.has(participant.id())) {
				this._participants.set(participant.id(), participant);
				this.fireEvent("participantsChanged", this._id);
			}
		},

		/**
		 * Removes a participant from this Wavelet.<br/>
		 * Note: Fires {@link pygowave.model.Wavelet.onParticipantsChanged onParticipantsChanged}
		 *
		 * @function {public} removeParticipant
		 * @param {String} participantId ID of the participant to remove
		 */
		removeParticipant: function (participantId) {
			if (this._participants.has(participantId)) {
				delete this._participants[participantId];
				this.fireEvent("participantsChanged", this._id);
			}
		},

		/**
		 * Returns the Participant object with the given id, if the participant
		 * resides on this Wavelet. Returns null otherwise.
		 * @function {Participant} participant
		 * @param {String} id ID of the Participant
		 */
		participant: function (id) {
			return this._participants.get(id);
		},

		/**
		 * Returns a list of all participants on this Wavelet.
		 * @function {public Participant[]} allParticipants
		 */
		allParticipants: function () {
			return this._participants.getValues();
		},

		/**
		 * Returns a list of all IDs of the participants on this Wavelet.
		 * @function {public String[]} allParticipantIDs
		 */
		allParticipantIDs: function () {
			return this._participants.getKeys();
		},

		/**
		 * Convenience function to serialize all participants into the Wave
		 * Gadget API format.
		 * @function {public Object} allParticipantsForGadget
		 */
		allParticipantsForGadget: function () {
			var ret = {};
			for (var __iter0_ = new _Iterator(this._participants); __iter0_.hasNext();) {
				var participant = __iter0_.next();
				var id = __iter0_.key();
				ret[id] = participant.toGadgetFormat();
			}
			delete __iter0_;
			return ret;
		},

		/**
		 * Convenience function for inserting a new Blip at the end.
		 * For options see the {@link pygowave.model.Blip.initialize Blip constructor}.<br/>
		 * Note: Fires {@link pygowave.model.Wavelet.onBlipInserted onBlipInserted}
		 *
		 * @function {public Blip} appendBlip
		 * @param {String} id ID of the new Blip
		 * @param {Object} options Information about the Blip
		 * @param {optional String} content Content of the Blip
		 * @param {optional Element[]} elements Element objects which initially
		 * reside in this Blip
		 */
		appendBlip: function (id, options, content, elements) {
			if (!$defined(content)) content = "";
			if (!$defined(elements)) elements = [];
			return this.insertBlip(len(this._blips), id, options, content, elements);
		},

		/**
		 * Insert a new Blip at the specified index.
		 * For options see the {@link pygowave.model.Blip.initialize Blip constructor}.<br/>
		 * Note: Fires {@link pygowave.model.Wavelet.onBlipInserted onBlipInserted}
		 *
		 * @function {public Blip} insertBlip
		 * @param {int} index Index where to insert the Blip
		 * @param {String} id ID of the new Blip
		 * @param {Object} options Information about the Blip
		 * @param {optional String} content Content of the Blip
		 * @param {optional Element[]} elements Element objects which initially
		 * reside in this Blip
		 */
		insertBlip: function (index, id, options, content, elements) {
			if (!$defined(content)) content = "";
			if (!$defined(elements)) elements = [];
			var blip = new Blip(this, id, options, content, elements);
			this._blips.insert(index, blip);
			this.fireEvent("blipInserted", [index, id]);
			return blip;
		},

		/**
		 * Returns the Blip object at the given index.
		 *
		 * @function {Blip} blipByIndex
		 * @param {int} index Index of the Blip
		 */
		blipByIndex: function (index) {
			return this._blips[index];
		},

		/**
		 * Returns the Blip object with the given id, if the Blip resides on this
		 * Wavelet. Returns null otherwise.
		 *
		 * @function {Blip} blipById
		 * @param {String} id ID of the Blip
		 */
		blipById: function (id) {
			for (var __iter0_ = new _Iterator(this._blips); __iter0_.hasNext();) {
				var blip = __iter0_.next();
				if (blip.id() == id)
					return blip;
			}
			delete __iter0_;
			return null;
		},

		/**
		 * Internal method to set the root Blip. Not intended to be called
		 * outside of this implementation.
		 *
		 * @function {private} _setRootBlip
		 * @param {Blip} blip Blip to be set as root Blip
		 */
		_setRootBlip: function (blip) {
			this._rootBlip = blip;
		},

		/**
		 * Internal method to set the status. Fires
		 * {@link pygowave.model.Wavelet.onStatusChange onStatusChange} if the
		 * status changed.
		 *
		 * @function {private} _setStatus
		 * @param {String} status The status to set
		 */
		_setStatus: function (status) {
			if (this.options.status != status) {
				this.options.status = status;
				this.fireEvent("statusChange", status);
			}
		},

		/**
		 * Calculate and compare checksums of all Blips to the given map.
		 * Fires {@link pygowave.model.Wavelet.onStatusChange onStatusChange} if
		 * the status changes.
		 *
		 * @function {public} checkSync
		 * @param {Object} blipsums Checksums to compare to
		 */
		checkSync: function (blipsums) {
			var valid = true;
			for (var __iter0_ = new _Iterator(blipsums); __iter0_.hasNext();) {
				var checksum = __iter0_.next();
				var blipId = __iter0_.key();
				var blip = this.blipById(blipId);
				if (blip != null) {
					if (!blip.checkSync(checksum))
						valid = false;
				}
			}
			delete __iter0_;
			if (valid)
				this._setStatus("clean");
			else
				this._setStatus("invalid");
		},

		/**
		 * Apply the operations on the wavelet.
		 *
		 * @function {public} applyOperations
		 * @param {pygowave.operations.Operation[]} ops List of operations to apply
		 */
		applyOperations: function (ops) {
			for (var __iter0_ = new _Iterator(ops); __iter0_.hasNext();) {
				var op = __iter0_.next();
				if (op.blipId != "") {
					var blip = this.blipById(op.blipId);
					if (op.type == pygowave.operations.DOCUMENT_DELETE)
						blip.deleteText(op.index, op.property);
					else if (op.type == pygowave.operations.DOCUMENT_INSERT)
						blip.insertText(op.index, op.property);
					else if (op.type == pygowave.operations.DOCUMENT_ELEMENT_DELETE)
						blip.deleteElement(op.index);
					else if (op.type == pygowave.operations.DOCUMENT_ELEMENT_INSERT)
						blip.insertElement(op.index, op.property.type, op.property.properties);
					else if (op.type == pygowave.operations.DOCUMENT_ELEMENT_DELTA)
						blip.applyElementDelta(op.index, op.property);
					else if (op.type == pygowave.operations.DOCUMENT_ELEMENT_SETPREF)
						blip.setElementUserpref(op.index, op.property.key, op.property.value);
				}
			}
			delete __iter0_;
		}
	});

	/**
	 * Wave model class. Top level model which sends all events.
	 *
	 * @class {public} pygowave.model.WaveModel
	 */
	var WaveModel = new Class({
		Implements: Events,
		/**
		 * Fired if a Wavelet has been added.
		 * @event onWaveletAdded
		 * @param {String} waveletId ID of the Wavelet that has been added
		 * @param {Boolean} isRoot True if this is the (new) root Wavelet
		 */

		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {String} waveId ID of the Wave
		 * @param {String} viewerId ID of the viewer
		 */
		initialize: function (waveId, viewerId) {
			this._rootWavelet = null;
			this._waveId = waveId;
			this._viewerId = viewerId;
			this._wavelets = new Hash();
		},

		/**
		 * Returns the ID of this Wave.
		 *
		 * @function {public String} id
		 */
		id: function () {
			return this._waveId;
		},

		/**
		 * Returns the ID of the viewer.
		 *
		 * @function {public String} viewerId
		 */
		viewerId: function () {
			return this._viewerId;
		},

		/**
		 * Load the wave's contents from a JSON-serialized snapshot and a map of
		 * participant objects.
		 *
		 * @function {public} loadFromSnapshot
		 * @param {Object} obj The JSON-serialized snapshot to load
		 * @param {Hash} participants A map of participant objects
		 */
		loadFromSnapshot: function (obj, participants) {
			var rootWavelet = obj.wavelet;
			var wvl_options = {
				creator: participants[rootWavelet.creator],
				is_root: true,
				created: rootWavelet.creationTime,
				last_modified: rootWavelet.lastModifiedTime,
				title: rootWavelet.title,
				version: rootWavelet.version
			};
			var rootWaveletObj = this.createWavelet(rootWavelet.waveletId, wvl_options);
			for (var __iter0_ = new _Iterator(rootWavelet.participants); __iter0_.hasNext();) {
				var part_id = __iter0_.next();
				rootWaveletObj.addParticipant(participants[part_id]);
			}
			delete __iter0_;
			for (var __iter0_ = new _Iterator(obj.blips); __iter0_.hasNext();) {
				var blip = __iter0_.next();
				var blip_id = __iter0_.key();
				var blip_options = {
					creator: participants[blip.creator],
					is_root: blip.blipId == rootWavelet.rootBlipId,
					last_modified: blip.lastModifiedTime,
					version: blip.version,
					submitted: blip.submitted
				};
				var blip_elements = [];
				for (var __iter1_ = new _Iterator(blip.elements); __iter1_.hasNext();) {
					var serialelement = __iter1_.next();
					if (serialelement.type == ELEMENT_TYPE.GADGET)
						blip_elements.append(new GadgetElement(null, serialelement.id, serialelement.index, serialelement.properties));
					else
						blip_elements.append(new Element(null, serialelement.id, serialelement.index, serialelement.type, serialelement.properties));
				}
				delete __iter1_;
				var blipObj = rootWaveletObj.appendBlip(blip_id, blip_options, blip.content, blip_elements);
			}
			delete __iter0_;
		},

		/**
		 * Create a Wavelet and add it to this Wave. For options see the
		 * {@link pygowave.model.Wavelet.initialize Wavelet constructor}.<br/>
		 * Note: Fires {@link pygowave.model.WaveModel.onWaveletAdded onWaveletAdded}
		 *
		 * @function {public Wavelet} createWavelet
		 * @param {String} id Wavelet ID
		 * @param {Object} options Information about the Wavelet.
		 */
		createWavelet: function (id, options) {
			var w = new Wavelet(this, id, options);
			this._wavelets.set(id, w);
			this.fireEvent("waveletAdded", [id, w.isRoot()]);
			return w;
		},

		/**
		 * Return a Wavelet of this Wave by its ID.
		 *
		 * @function {public Wavelet} wavelet
		 * @param {String} waveletId ID of the Wavelet
		 */
		wavelet: function (waveletId) {
			return this._wavelets.get(waveletId);
		},

		/**
		 * Returns the root Wavelet of this Wave.
		 *
		 * @function {public Wavelet} rootWavelet
		 */
		rootWavelet: function () {
			return this._rootWavelet;
		},

		/**
		 * Internal method to set the root Wavelet. Not intended to be called
		 * outside of this implementation.
		 *
		 * @function {private} _setRootWavelet
		 * @param {Wavelet} wavelet Wavelet to be set as root Wavelet
		 */
		_setRootWavelet: function (wavelet) {
			this._rootWavelet = wavelet;
		}
	});

	return {
		WaveModel: WaveModel,
		Participant: Participant,
		ELEMENT_TYPE: ELEMENT_TYPE
	};
})();
