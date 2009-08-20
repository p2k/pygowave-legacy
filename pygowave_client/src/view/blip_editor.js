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
 * PyGoWave's Blip Editor.
 * 
 * @module pygowave.view.blip_editor
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	var Selection = pygowave.view.Selection;
	
	/**
	 * An editable div which generates events for the controller and is able to
	 * process model changes. Inspired by MooEditable.
	 * 
	 * @class {private} pygowave.view.BlipEditorWidget
	 * @extends pygowave.view.Widget
	 */
	var BlipEditorWidget = new Class({
		Extends: Widget,
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {pygowave.model.Blip} blip Blip to be rendered
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {String} where Where to inject the widget relative to the
		 *        parent element. Can be 'top', 'bottom', 'after', or 'before'.
		 */
		initialize: function (view, blip, parentElement, where) {
			this._view = view;
			this._blip = blip;
			this._lastRange = [0, 0];
			this._lastContent = "";
			
			var contentElement = new Element('div', {
				'class': 'blip_editor_widget'
			});
			
			this._onInsertText = this._onInsertText.bind(this);
			this._onDeleteText = this._onDeleteText.bind(this);
			this._onKeyDown = this._onKeyDown.bind(this);
			this._onKeyPress = this._onKeyPress.bind(this);
			this._onKeyUp = this._onKeyUp.bind(this);
			this._onContextMenu = this._onContextMenu.bind(this);
			this._onMouseUp = this._onMouseUp.bind(this);
			
			this.parent(parentElement, contentElement, where);
			
			contentElement.contentEditable = "true";
		},
		/**
		 * Overridden from {@link pygowave.view.Widget.dispose Widget.dispose}.<br/>
		 * Inject this widget somewhere else. Sets the parent element.<br/>
		 * Note: Called by initialize.
		 * @function {public BlipEditorWidget} inject
		 * @param {Element} parentElement New parent element
		 * @param {optional String} where Where to inject the contentElement
		 *        relative to the parent element. Can be 'top', 'bottom',
		 *        'after', or 'before'.
		 * @param {optional pygowave.model.Blip} blip New
		 *        Blip object to bind this element to.
		 * @return Returns a reference to this widget.
		 */
		inject: function (parentElement, where, blip) {
			this.parent(parentElement, where);
			
			if ($defined(blip))
				this._blip = blip;
			else
				blip = this._blip;
			
			this.reloadContent();
			
			blip.addEvents({
				insertText: this._onInsertText,
				deleteText: this._onDeleteText
			});
			this.contentElement.addEvents({
				keydown: this._onKeyDown,
				keypress: this._onKeyPress,
				keyup: this._onKeyUp,
				contextmenu: this._onContextMenu,
				mouseup: this._onMouseUp
			});
			
			return this;
		},
		/**
		 * Overridden from {@link pygowave.view.Widget.dispose Widget.dispose}.<br/>
		 * Removes the widget from the DOM, sets the parentElement to null and
		 * disconnects from the Blip.
		 * 
		 * @function {public Widget} dispose
		 * @return Returns a reference to this widget.
		 */
		dispose: function () {
			this.parent();
			this._lastRange = [0, 0];
			this._lastContent = "";
			this._blip.removeEvents({
				insertText: this._onInsertText,
				deleteText: this._onDeleteText
			});
			this.contentElement.removeEvents({
				keydown: this._onKeyDown,
				keypress: this._onKeyPress,
				keyup: this._onKeyUp,
				contextmenu: this._onContextMenu,
				mouseup: this._onMouseUp
			});
		},
		
		/**
		 * Build the content elements from the Blip's string and insert it
		 * into the widget.
		 * 
		 * @function {private} reloadContent
		 */
		reloadContent: function () {
			// Clean up first
			for (var it = new _Iterator(this.contentElement.getChildren()); it.hasNext(); ) {
				var oldelt = it.next();
				if (oldelt.get('tag') == 'p')
					oldelt.eliminate("isEmpty");
				oldelt.dispose();
			}
			// Build up
			var content = this._blip.content().replace(/ /g, "\u00a0");
			var lines = content.split("\n"), line, pg;
			for (var i = 0; i < lines.length; i++) {
				line = lines[i];
				pg = new Element("p");
				if (line == "") { // Annotate empty nodes
					//pg.store("isEmpty", true);
					//line = "\u00a0";
					Element("div", {'class': 'blip_line_placeholder', 'text': '*'}).inject(pg);
				}
				else {
					//pg.store("isEmpty", false);
					pg.set('text', line);
				}
				pg.inject(this.contentElement);
			}
		},
		
		/**
		 * Retrieve the content as a simple string (converting paragraphs to
		 * newlines).
		 * @function {public String} contentToString
		 */
		contentToString: function () {
			var cleaned = "", first = true;
			for (var it = new _Iterator(this.contentElement.getChildren("p")); it.hasNext(); ) {
				var elt = it.next();
				if (first)
					first = false;
				else
					cleaned += "\n";
					
				if (!elt.retrieve("isEmpty", false))
					cleaned += elt.get('text').replace(/&nbsp;/gi, " ");
			}
			return cleaned;
		},
		/**
		 * Returns the current selected text range as [start, end]. This
		 * ignores all element nodes except iframe (which is treated as 1 character).
		 * @function {public Array} currentTextRange
		 * @param {optional pygowave.view.Selection} sel Selection object
		 *        (will be fetched if not provided).
		 */
		currentTextRange: function (sel) {
			if (!$defined(sel))
				sel = Selection.currentSelection(this.contentElement);
			
			if (!sel.isValid())
				return null;
			
			sel.moveDown();
			var start = this._walkUp(sel.startNode(), sel.startOffset());
			var end = this._walkUp(sel.endNode(), sel.endOffset());
			
			dbgprint(start, end);
			return [start, end];
		},
		/**
		 * Check if an empty node was filled.
		 * @function {private} _checkInsertion
		 * @param {pygowave.view.Selection} sel Selection object
		 */
		_checkEmptyNodes: function (sel) {
			if (!sel.isValid())
				return;
			sel.moveDown(); // Move down, only treat text nodes of p's
			
			var elt = sel.endNode(), parent, txt;
			if ($type(elt) != 'element') {
				parent = elt.parentNode;
				txt = parent.get('text');
				if (parent.retrieve('isEmpty', false) && txt != "\u00a0") {
					txt = elt.data;
					if (txt == "") {
						txt.appendData("\u00a0");
					}
					else if (txt.substring(sel.endOffset()) == "\u00a0") {
						elt.deleteData(txt.length-1, 1);
						parent.eliminate('isEmpty');
					}
					else {
						elt.deleteData(0, 1);
						sel._startOffset -= 1; //HACK
						if (sel.startNode() == sel.endNode())
							sel._endOffset -= 1; //HACK
						parent.eliminate('isEmpty');
					}
				}
				else if (txt == "") {
					txt.appendData("\u00a0");
					parent.store('isEmpty', true);
				}
			}
		},
		
		_onKeyDown: function (e) {
			// Currently not needed
		},
		
		_onKeyPress: function (e) {
			// Currently not needed
		},
		
		_onKeyUp: function(e) {
			var sel = Selection.currentSelection(this.contentElement);
			this._checkEmptyNodes(sel);
			
			var newContent = this.contentToString();
			var newRange = this.currentTextRange(sel);
			
			// This code should be accurate for now. However, the context menu is not handeled properly.
			if (e.key == "backspace" || e.key == "delete") {
				if (this._lastRange[0] != this._lastRange[1])
					this._view.fireEvent(
						'textDeleted',
						[
							this._blip.wavelet().id(),
							this._blip.id(),
							this._lastRange[0],
							this._lastRange[1]
						]
					);
				else if (e.key == "backspace") {
					if (this._lastRange[0] != 0)
						this._view.fireEvent(
							'textDeleted',
							[
								this._blip.wavelet().id(),
								this._blip.id(),
								this._lastRange[0]-1,
								this._lastRange[0]
							]
						);
				}
				else if (e.key == "delete") {
					if (this._lastRange[0] != this._lastContent.length)
						this._view.fireEvent(
							'textDeleted',
							[
								this._blip.wavelet().id(),
								this._blip.id(),
								this._lastRange[0],
								this._lastRange[0]+1
							]
						);
				}
			}
			else if (newRange[0] > this._lastRange[0] && e.code != 35 && e.code != 39 && e.code != 40) {
				if (this._lastRange[0] != this._lastRange[1])
					this._view.fireEvent(
						'textDeleted',
						[
							this._blip.wavelet().id(),
							this._blip.id(),
							this._lastRange[0],
							this._lastRange[1]
						]
					);
				this._view.fireEvent(
					'textInserted',
					[
						this._blip.wavelet().id(),
						this._blip.id(),
						this._lastRange[0],
						newContent.slice(this._lastRange[0], newRange[0])
					]
				);
			}
			
			this._lastContent = newContent;
			this._lastRange = newRange;
		},
		_onMouseUp: function(e) {
			this._lastRange = this.currentTextRange();
		},
		_onContextMenu: function (e) {
			return false; // Context menu is blocked
		},
		
		_onInsertText: function (index, text) {
			text = text.replace(/ /g, "\u00a0"); // Convert spaces to protected spaces
			var ret = this._walkDown(this.contentElement, index);
			var elt = ret[0], offset = ret[1];
			if ($type(elt) != "textnode" && $type(elt) != "whitespace") { // Should be impossible...
				var newtn = document.createTextNode("");
				elt.parentNode.insertBefore(newtn, elt);
				elt = newtn;
				offset = 0;
			}
			var lines = text.split("\n"), rest;
			var parent = elt.parentNode; // Parent paragraph
			for (var i = 0; i < lines.length; i++) {
				if (lines[i].length > 0) {
					if (offset == elt.data.length && Browser.Engine.trident) { // Buggy in IE
						elt.appendData(" ");
						elt.insertData(offset, lines[i]);
						elt.deleteData(elt.data.length-1, 1);
					}
					else
						elt.insertData(offset, lines[i]);
					if (parent.retrieve("isEmpty", false)) {
						// Remove implicit newline
						elt.deleteData(elt.data.length-1, 1);
						parent.eliminate("isEmpty");
					}
				}
				if (i < lines.length-1) { // Handle newlines
					rest = elt.data.substring(offset+lines[i].length);
					if (rest.length > 0)
						elt.deleteData(offset+lines[i].length, rest.length); // Cutoff remainder
					elt = new Element("p");
					if (rest == "") {
						elt.set('text', "\u00a0");
						elt.store("isEmpty", true);
					}
					else
						elt.set('text', rest);
					elt.inject(parent, 'after');
					parent = elt;
					elt = parent.firstChild; // Set to textnode
					offset = 0;
				}
			}
		},
		_onDeleteText: function (index, length) {
			var ret = this._walkDown(this.contentElement, index);
			var elt = ret[0], next = null;
			var offset = ret[1], dellength = 0;
			while (length > 0) {
				next = elt.nextSibling;
				if ($type(elt) == "element") {
					if (elt.get("tag") == "br") {
						elt.parentNode.removeChild(elt);
						length--;
						offset = 0;
					}
				}
				else if ($type(elt) == "textnode") {
					dellength = elt.data.length-offset;
					if (dellength > length)
						dellength = length;
					if (dellength > 0)
						elt.deleteData(offset, dellength);
					length -= dellength;
				}
				elt = next;
				if (!$defined(elt))
					break; // Deleted past end...
			}
		},
		
		/**
		 * Walk up the DOM tree while summing up all text lengths. Elements are
		 * ignored, except p which is the line container and iframe which count
		 * as one character. Returns the absolute offset.
		 * 
		 * @function {private int} _walkUp
		 * @param {Node} node The node to start
		 * @param {int} nodeOffset The node offset to start
		 */
		_walkUp: function (node, nodeOffset) {
			var offset = 0;
			var prev = node;
			
			if ($type(node) == "textnode" || $type(node) == "whitespace")
				offset = nodeOffset;
			else { // Not completely at the bottom
				node = node.firstChild;
				while (node != null && nodeOffset > 0) {
					prev = node;
					node = node.nextSibling;
					nodeOffset--;
				}
				if (node == null)
					node = prev;
				else
					prev = node;
			}
			
			while (node != null && node != this.contentElement) {
				while ((node = node.previousSibling) != null) {
					prev = node;
					if ($type(node) == "element") {
						if (node.get('tag') == "br" || node.get('tag') == "iframe")
							offset++;
						else {
							if (node.get('tag') == "p")
								offset++;
							// descend
							while ((node = node.lastChild) != null) {
								prev = node;
								if ($type(node) == "textnode") {
									offset += node.data.length;
									break;
								}
							}
						}
						node = prev;
					}
					else if ($type(node) == "textnode")
						offset += node.data.length;
				}
				// ascend
				node = prev.parentNode;
				prev = node;
			}
			return offset;
		},
		/**
		 * Walk down the DOM tree. Opposite of {@link pygowave.view.BlipEditor._walkUp _walkUp}.
		 * Returns a list [node, nodeOffset].
		 * 
		 * @function {private Array} _walkDown
		 * @param {Element} body Body element
		 * @param {int} offset Absolute offset to walk down
		 */
		_walkDown: function (body, offset) {
			var elt = body.firstChild;
			var next;
			
			while (offset > 0 && elt != null) {
				next = null;
				if ($type(elt) == "element") {
					if (elt.get('tag') != "iframe" && (elt.get('tag') != "p" || !elt.retrieve("isEmpty", false)))
						next = elt.firstChild; // descend
				}
				
				if (next != null) {
					elt = next;
					continue;
				}
				
				if ($type(elt) == "textnode") {
					if (offset <= elt.data.length)
						return [elt, offset];
					offset -= elt.data.length;
				}
				else if ($type(elt) == "element" && (elt.get('tag') == "iframe" || elt.get('tag') == "p"))
					offset--;
				
				next = elt.nextSibling;
				if (next == null) { // ascend
					elt = elt.parentNode;
					elt = elt.nextSibling;
					if ($type(elt) == "element" && elt.get('tag') == "p") {
						elt = elt.firstChild; // auto-descent
						offset--;
					}
				}
				else
					elt = next;
			}
			
			return [elt, offset];
		}
	});

	pygowave.view.extend({
		BlipEditorWidget: BlipEditorWidget
	});
})();
