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
	
	/**
	 * Internal class to handle selections.
	 * @class {private} pygowave.view.Selection
	 */
	var Selection = new Class({
		initialize: function(win){
			this.win = win;
		},
		getSelection: function(){
			return (this.win.getSelection) ? this.win.getSelection() : this.win.document.selection;
		},
		getRange: function(){
			var s = this.getSelection();
			
			if (!s) return null;
			
			try {
				return s.rangeCount > 0 ? s.getRangeAt(0) : (s.createRange ? s.createRange() : null);
			} catch(e) {
				// IE bug when used in frameset
				return this.win.document.body.createTextRange();
			}
		},
		setRange: function(range){
			if (range.select){
				$try(function(){
					range.select();
				});
			} else {
				var s = this.getSelection();
				if (s.addRange){
					s.removeAllRanges();
					s.addRange(range);
				}
			}
		},
		selectNode: function(node, collapse){
			var r = this.getRange();
			var s = this.getSelection();
			
			if (r.moveToElementText){
				$try(function(){
					r.moveToElementText(node);
					r.select();
				});
			} else if (s.addRange){
				collapse ? r.selectNodeContents(node) : r.selectNode(node);
				s.removeAllRanges();
				s.addRange(r);
			} else {
				s.setBaseAndExtent(node, 0, node, 1);
			}
			
			return node;
		},
		isCollapsed: function(){
			var r = this.getRange();
			if (r.item) return false;
			return r.boundingWidth == 0 || this.getSelection().isCollapsed;
		},
		collapse: function(toStart){
			var r = this.getRange();
			var s = this.getSelection();
			
			if (r.select){
				r.collapse(toStart);
				r.select();
			} else {
				toStart ? s.collapseToStart() : s.collapseToEnd();
			}
		},
		getContent: function(){
			var r = this.getRange();
			var body = new Element('body');
			
			if (this.isCollapsed()) return '';
			
			if (r.cloneContents){
				body.appendChild(r.cloneContents());
			} else if ($defined(r.item) || $defined(r.htmlText)){
				body.set('html', r.item ? r.item(0).outerHTML : r.htmlText);
			} else {
				body.set('html', r.toString());
			}
			
			var content = body.get('html');
			return content;
		},
		getText : function(){
			var r = this.getRange();
			var s = this.getSelection();
			
			return this.isCollapsed() ? '' : r.text || (s.toString ? s.toString() : '');
		},
		getNode: function(){
			var r = this.getRange();
			
			if (!Browser.Engine.trident){
				var el = null;
				
				if (r){
					el = r.commonAncestorContainer;
					
					// Handle selection a image or other control like element such as anchors
					if (!r.collapsed)
						if (r.startContainer == r.endContainer)
							if (r.startOffset - r.endOffset < 2)
								if (r.startContainer.hasChildNodes())
									el = r.startContainer.childNodes[r.startOffset];
					
					while ($type(el) != 'element') el = el.parentNode;
				}
				
				return $(el);
			}
			
			return $(r.item ? r.item(0) : r.parentElement());
		},
		insertContent: function(content){
			if (Browser.Engine.trident){
				var r = this.getRange();
				r.pasteHTML(content);
				r.collapse(false);
				r.select();
			} else {
				this.win.document.execCommand('insertHTML', false, content);
			}
		}
	});
	
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
			
			this.selection = new Selection(window);
			
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
			
			var content = blip.content().replace(/\n/g, "<br/>").replace(/ /g, "\u00a0");
			if (content.contains("<br/>") || content == "")
				content += "<br/>"; // Append implicit newline
			this.contentElement.set('html', content);
			
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
		 * Retrieve the html content of this widget.
		 * @function {public String} getContent
		 */
		getContent: function () {
			return this.contentElement.get('html');
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
		
		_onKeyDown: function (e) {
			// Currently not needed
		},
		
		_onKeyPress: function (e) {
			if (e.key == "enter" && !Browser.Engine.gecko) {
				// Only firefox seems to add br's, so override other browsers
				e.stop();
				this.selection.insertContent("<br/>");
			}
		},
		
		_onKeyUp: function(e) {
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
			
			while (node != null && node != this.contentElement) {
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
		BlipEditorWidget: BlipEditorWidget
	});
})();
