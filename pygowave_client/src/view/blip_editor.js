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
 * @module pygowave.blip_editor
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	
	/**
	 * An extended MooEditable which generates events for the controller and
	 * is able to process model changes.
	 * @class {private} pygowave.view.BlipEditor
	 * @extends MooEditable
	 */
	var BlipEditor = new Class({
		Extends: MooEditable,
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {WaveView} view A reference back to the main view
		 * @param {pygowave.model.Blip} blip Blip to be rendered
		 * @param {Element} textarea Textarea to convert into a BlipEditor
		 * @param {Object} options Options for MooEditable
		 */
		initialize: function (view, blip, textarea, options) {
			this.parent(textarea, options);
			this.options.cleanup = false;
			this._view = view;
			this._blip = blip;
			this._lastRange = [0, 0];
			this._lastContent = "";
			
			this.container.setStyle('width', '100%');
			
			this._onInsertText = this._onInsertText.bind(this);
			this._onDeleteText = this._onDeleteText.bind(this);
			
			blip.addEvent('insertText', this._onInsertText);
			blip.addEvent('deleteText', this._onDeleteText);
		},
		editorKeyUp: function(e) {
			var newContent = this.contentToString();
			var newRange = this.currentTextRange();
			
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
			this.parent(e);
		},
		editorContextMenu: function (e) {
			e.stop();
		},
		editorMouseUp: function(e) {
			this._lastRange = this.currentTextRange();
			this.parent(e);
		},
		/**
		 * Convert the content into a simple string (converting &lt;br/&gt; to
		 * newlines).
		 * @function {public String} contentToString
		 */
		contentToString: function () {
			var cleaned = this.getContent().replace(/<br[^>]*>/gi, "\n").replace(/&nbsp;/gi, " ");
			if (cleaned.endswith("\n"))
				cleaned = cleaned.slice(0, cleaned.length-1);
			return cleaned;
		},
		/**
		 * Returns the current selected text range as [start, end]. This
		 * ignores all element nodes except br (which is treated as 1 character).
		 * @function {public Array} currentTextRange
		 */
		currentTextRange: function () {
			var rng = this.selection.getRange();
			
			var start = this._walkUp(rng.startContainer, rng.startOffset);
			var end = this._walkUp(rng.endContainer, rng.endOffset);
			
			return [start, end];
		},
		
		_onInsertText: function (index, text) {
			text = text.replace(/ /g, "\u00a0"); // Convert spaces to protected spaces
			var ret = this._walkDown(this.doc.body, index);
			var elt = ret[0], offset = ret[1];
			if ($type(elt) != "textnode" && $type(elt) != "whitespace") {
				var newtn = document.createTextNode("");
				elt.parentNode.insertBefore(newtn, elt);
				elt = newtn;
				offset = 0;
			}
			var lines = text.split("\n");
			for (var i = 0; i < lines.length; i++) {
				if (lines[i].length > 0)
					elt.insertData(offset, lines[i]);
				if (i < lines.length-1) { // Handle newlines
					elt.splitText(offset+lines[i].length);
					elt = elt.nextSibling;
					elt.parentNode.insertBefore(new Element("br"), elt);
					offset = 0;
				}
			}
		},
		_onDeleteText: function (index, length) {
			var ret = this._walkDown(this.doc.body, index);
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
		 * ignored, except br and iframe which count as one character.
		 * Returns theabsolute offset.
		 * 
		 * @function {private int} _walkUp
		 * @param {Node} node The node to start
		 * @param {int} nodeOffset The node offset to start
		 */
		_walkUp: function (node, nodeOffset) {
			var offset = 0;
			var prev = node;
			
			if ($type(node) == "textnode")
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
			
			while (node != null && ($type(node) != "element" || node.get('tag') != "body")) {
				while ((node = node.previousSibling) != null) {
					prev = node;
					if ($type(node) == "element") {
						if (node.get('tag') == "br" || node.get('tag') == "iframe")
							offset++;
						else {
							// descend
							while ((node = node.lastChild) != null) {
								prev = node;
								if ($type(node) == "textnode") {
									offset += node.textContent.length;
									break;
								}
							}
						}
						node = prev;
					}
					else if ($type(node) == "textnode")
						offset += node.textContent.length;
				}
				// ascend
				node = prev.parentNode;
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
				if ($type(elt) == "element" && !elt.get('tag') == "iframe")
					next = elt.firstChild; // descend
				else
					next = null;
				
				if (next != null) {
					elt = next;
					continue;
				}
				
				if ($type(elt) == "textnode") {
					if (offset <= elt.textContent.length)
						return [elt, offset];
					offset -= elt.textContent.length;
				}
				else if ($type(elt) == "element" && (elt.get('tag') == "br" || elt.get('tag') == "iframe"))
					offset--;
				
				next = elt.nextSibling;
				if (next == null) { // ascend
					elt = elt.parentNode;
					elt = elt.nextSibling;
				}
				else
					elt = next;
			}
			
			return [elt, offset];
		}
	});

	pygowave.view.extend({
		//BlipEditorWidget: BlipEditorWidget
		BlipEditor: BlipEditor
	});
})();
