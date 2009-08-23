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
			this._errDiv = null;
			
			var contentElement = new Element('div', {
				'class': 'blip_editor_widget',
				'spellcheck': 'false'
			});
			
			this._onInsertText = this._onInsertText.bind(this);
			this._onDeleteText = this._onDeleteText.bind(this);
			this._onKeyDown = this._onKeyDown.bind(this);
			this._onKeyPress = this._onKeyPress.bind(this);
			this._onKeyUp = this._onKeyUp.bind(this);
			this._onContextMenu = this._onContextMenu.bind(this);
			this._onMouseUp = this._onMouseUp.bind(this);
			this._onMouseDown = this._onMouseDown.bind(this);
			this._onWindowResize = this._onWindowResize.bind(this);
			this._onOutOfSync = this._onOutOfSync.bind(this);
			
			this.parent(parentElement, contentElement, where);
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
				deleteText: this._onDeleteText,
				outOfSync: this._onOutOfSync
			});
			
			this.contentElement.addEvents({
				keydown: this._onKeyDown,
				keypress: this._onKeyPress,
				keyup: this._onKeyUp,
				contextmenu: this._onContextMenu,
				mouseup: this._onMouseUp,
				mousedown: this._onMouseDown
			});
			
			this._processing = false;
			window.addEvent('resize', this._onWindowResize);
			this._onSyncCheckTimer = this._onSyncCheck.periodical(2000, this);
			
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
				deleteText: this._onDeleteText,
				outOfSync: this._onOutOfSync
			});
			this.contentElement.removeEvents({
				keydown: this._onKeyDown,
				keypress: this._onKeyPress,
				keyup: this._onKeyUp,
				contextmenu: this._onContextMenu,
				mouseup: this._onMouseUp
			});
			window.removeEvent('resize', this._onWindowResize);
			$clear(this._onSyncCheckTimer);
		},
		
		/**
		 * Build the content elements from the Blip's string and insert it
		 * into the widget.
		 * 
		 * @function {private} reloadContent
		 */
		reloadContent: function () {
			// Clean up first
			for (var it = new _Iterator(this.contentElement.getChildren()); it.hasNext(); )
				it.next().destroy();
			
			this.contentElement.setStyle("opacity", 1);
			this._removeErrorOverlay();
			
			// Build up
			var content = this._blip.content().replace(/ /g, "\u00a0");
			var lines = content.split("\n"), line, pg;
			for (var i = 0; i < lines.length; i++) {
				line = lines[i];
				pg = new Element("p");
				if (line != "")
					pg.set('text', line);
				if (!Browser.Engine.trident) // IE allows empty paragraphs
					new Element("br").inject(pg); // Others use an implicit br
				pg.inject(this.contentElement);
			}
			
			this._lastContent = this.contentToString();
			if (this._blip.content() != this._lastContent)
				this._displayErrorOverlay("render_fail");
			else
				this.contentElement.contentEditable = "true";
		},
		/**
		 * Places an overlay over the editor and displays an error message with
		 * a button.
		 * @function {private} _displayErrorOverlay
		 * @param {String} type Error type. May be 'resync', 'render_fail'.
		 */
		_displayErrorOverlay: function (type) {
			this._removeErrorOverlay();
				
			this._errDiv = new Element("div", {'class': 'error_overlay'}).inject(this.parentElement);
			var msgdiv = new Element("div", {'class': 'msg_div'}).inject(this._errDiv);
			var bg_div = new Element("div", {'class': 'bg_div'}).inject(this._errDiv);
			bg_div.setStyle("opacity", 0.5);
			var coords = this.contentElement.getCoordinates(false);
			
			switch (type) {
				case "resync":
					new Element("div", {'class': 'msg', 'text': gettext("Sorry, but this Blip Editor has gone out of sync with the internal representation. Please click resync.")}).inject(msgdiv);
					new Element("button", {'class': 'mochaButton', 'text': gettext("Resync")})
						.inject(msgdiv).addEvent('click', this.reloadContent.bind(this));
					break;
				
				case "render_fail":
					new Element("div", {'class': 'msg', 'text': gettext("The text content could not be rendered correctly. This may be a bug or you are using an unsupported browser.")}).inject(msgdiv);
					new Element("button", {'class': 'mochaButton', 'text': gettext("Dismiss")})
						.inject(msgdiv).addEvent('click', this._removeErrorOverlay.bind(this));
					$clear(this._onSyncCheckTimer);
					break;
				
				case "checksum_fail":
					new Element("div", {'class': 'msg', 'text': gettext("Unfortunately, this Blip has gone out of sync with the server. You have to reload the page to be able to use it again.")}).inject(msgdiv);
					new Element("button", {'class': 'mochaButton', 'text': gettext("Reload")})
						.inject(msgdiv).addEvent('click', function () {
							window.location.href = window.location.href;
						});
					$clear(this._onSyncCheckTimer);
					break;
			}
			
			this._errDiv.setStyles({
				position: "absolute",
				top: coords.top,
				left: coords.left,
				width: coords.width,
				height: coords.height,
				opacity: 0
			});
			new Fx.Tween(this._errDiv, {duration: 250}).start("opacity", 0, 1);
			new Fx.Tween(this.contentElement, {duration: 250}).start("opacity", 1, 0.5);
			this.contentElement.contentEditable = "false";
		},
		/**
		 * Removes the error overlay displayed by _displayErrorOverlay.
		 * @function {private} _removeErrorOverlay
		 */
		_removeErrorOverlay: function ()  {
			if (!$defined(this._errDiv))
				return;
			this.contentElement.setStyle("opacity", 1);
			var toKill = this._errDiv;
			this._errDiv = null;
			new Fx.Tween(toKill, {duration: 250}).start("opacity", 1, 0).chain(function () {
				toKill.destroy();
				toKill = null;
			});
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
					
				cleaned += elt.get('text').replace(/\u00a0/gi, " ");
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
			
			var dbg = $('debug_cursor_pos');
			if ($defined(dbg))
				dbg.set('text', start + " / " + end);
			return [start, end];
		},
		
		_onKeyDown: function (e) {
			this._processing = true;
		},
		
		_onKeyPress: function (e) {
			this._processing = true;
		},
		
		_onKeyUp: function(e) {
			this._processing = true;
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
			this._processing = false;
		},
		_onMouseDown: function (e) {
			// Currently not needed
		},
		_onMouseUp: function(e) {
			this._lastRange = this.currentTextRange();
		},
		_onContextMenu: function (e) {
			return false; // Context menu is blocked
		},
		
		_onInsertText: function (index, text) {
			//TODO: this function assumes no formatting elements
			this._processing = true;
			
			text = text.replace(/ /g, "\u00a0"); // Convert spaces to protected spaces
			var ret = this._walkDown(this.contentElement, index);
			var elt = ret[0], offset = ret[1], newelt, newtn;
			if ($type(elt) == "element"	&& elt.get('tag') == "p") { // Empty paragraphs
				newtn = document.createTextNode("");
				if (!Browser.Engine.trident)
					elt.insertBefore(newtn, elt.firstChild);
				else
					elt.appendChild(newtn);
				elt = newtn;
				offset = 0;
			}
			var lines = text.split("\n"), rest;
			var parent = elt.parentNode; // Parent paragraph
			for (var i = 0; i < lines.length; i++) {
				if (lines[i].length > 0) {
					if (!$defined(elt.data)) {
						dbgprint("wtf!");
					}
					if (offset == elt.data.length && Browser.Engine.trident) { // Appending is buggy in IE
						elt.appendData(" ");
						elt.insertData(offset, lines[i]);
						elt.deleteData(elt.data.length-1, 1);
					}
					else
						elt.insertData(offset, lines[i]);
				}
				if (i < lines.length-1) { // Handle newlines
					if (offset < elt.data.length && offset+lines[i].length == 0) {
						// Special case: effectively append an empty node before the current node
						newelt = new Element("p").inject(parent, 'before');
						if (!Browser.Engine.trident)
							new Element("br").inject(newelt);
						newelt = elt;
					}
					else {
						newelt = new Element("p").inject(parent, 'after');
						if (offset < elt.data.length) {
							if (offset+lines[i].length > 0)
								elt.splitText(offset+lines[i].length); // Split
						}
						
						// Move siblings to new node
						if (!Browser.Engine.trident) {
							new Element("br").inject(newelt);
							while ($defined(elt.nextSibling) && elt.nextSibling != parent.lastChild)
								newelt.insertBefore(elt.nextSibling, newelt.firstChild);
						}
						else {
							while ($defined(elt.nextSibling)) {
								if ($defined(newelt.firstChild))
									newelt.insertBefore(elt.nextSibling, newelt.firstChild);
								else
									newelt.appendChild(elt.nextSibling);
							}
						}
						
						if (!$defined(newelt.firstChild))
							newelt.appendChild(document.createTextNode(""));
					}
					
					elt = newelt.firstChild;
					offset = 0;
				}
			}
			this._processing = false;
		},
		_onDeleteText: function (index, length) {
			// Safe for formatting elements
			this._processing = true;
			
			var ret = this._walkDown(this.contentElement, index);
			var elt = ret[0], next = null;
			var offset = ret[1], dellength = 0;
			if ($type(elt) == "element" && elt.get('tag') == "br") // Got the implicit br (TODO should be impossible to reach; check again)
				elt = elt.parentElement;
			while (length > 0 && $defined(elt)) {
				next = this._nextTextOrPara(elt);
				if ($type(elt) == "textnode") {
					dellength = elt.data.length-offset;
					if (dellength > length)
						dellength = length;
					if (dellength > 0)
						elt.deleteData(offset, dellength);
					length -= dellength;
					if (length > 0 && $type(next) == "element" && next.get('tag') == "p") {
						// Merge, then next is empty and will be deleted
						if (!Browser.Engine.trident) {
							while ($defined(next.firstChild) && next.firstChild != next.lastChild)
								elt.parentNode.insertBefore(next.firstChild, elt.parentNode.lastChild);
						}
						else {
							while ($defined(next.firstChild))
								elt.parentNode.appendChild(next.firstChild);
						}
					}
				}
				else if ($type(elt) == "element" && elt.get('tag') == "p") { // Empty p
					elt.dispose();
					length--;
				}
				elt = next;
				offset = 0;
			}
			this._processing = false;
		},
		_onOutOfSync: function () {
			this._displayErrorOverlay('checksum_fail');
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
			
			var next = this._nextTextOrPara(elt, true, true); // Try to descend to first text node
			if ($type(next) == "textnode")
				elt = next;
			
			while (offset > 0 && elt != null) {
				next = this._nextTextOrPara(elt, true, true);
				if ($type(elt) == "element") { // p or iframe
					offset--;
					if (offset == 0 && elt.get('tag') == "p" && (next == null || $type(next) == "element")) // Empty p
						break;
				}
				else if ($type(elt) == "textnode") {
					if (offset <= elt.data.length)
						break;
					offset -= elt.data.length;
				}
				elt = next;
			}
			
			if (offset == 0 && $type(elt) == "element" && elt.get('tag') == "p") {
				next = this._nextTextOrPara(elt, true, true); // Try to descend to first text node
				if ($type(next) == "textnode")
					elt = next;
			}
			
			return [elt, offset];
		},
		/**
		 * Find the next TextNode or Paragraph.
		 * 
		 * @function {private Node} _nextTextOrPara
		 * @param {Node} elt Start node
		 * @param {optional Boolean} descend Set to true if you want to descend
		 *        first if possible, rather than moving to the next sibling.
		 * @param {optional Boolean} iframe_stop Set to true if you also want
		 *        iframes to be returned.
		 */
		_nextTextOrPara: function (elt, descend, iframe_stop) {
			if (!$defined(descend))
				descend = false;
			if (!$defined(iframe_stop))
				iframe_stop = false;
			
			var next;
			if (descend
					&& $type(elt) == "element"
					&& elt.get('tag') != "iframe"
					&& $defined(elt.firstChild))
				next = elt.firstChild;
			else
				next = elt.nextSibling;
			
			while (!$defined(next) || $type(next) == "element") {
				if (!$defined(next)) { // ascend
					elt = elt.parentNode;
					if (elt == this.contentElement)
						return null; // The end
					next = elt.nextSibling;
					continue;
				}
				if (next.get('tag') == "p")
					return next; // Done
				else { // descend or move forward
					elt = next;
					if (iframe_stop && elt.get('tag') == "iframe")
						return elt;
					if (elt.get('tag') != "iframe" && $defined(elt.firstChild))
						next = elt.firstChild;
					else
						next = elt.nextSibling;
				}
			}
			
			return next;
		},
		/**
		 * Callback on window resize.
		 *
		 * @function {private} _onWindowResize
		 */
		_onWindowResize: function () {
			if ($defined(this._errDiv)) {
				var coords = this.contentElement.getCoordinates(false);
				this._errDiv.setStyles({
					top: coords.top,
					left: coords.left,
					width: coords.width,
					height: coords.height
				});
			}
		},
		/**
		 * This function is called periodically to check Blip rendering
		 * consistency.
		 * @function {private} _onSyncCheck
		 */
		_onSyncCheck: function () {
			if (this._processing)
				return;
			
			var content = this.contentToString();
			if (content != this._blip.content()) {
				if (!$defined(this._errDiv)) {
					window.console.info("diff: "+this._diff(content, this._blip.content()));
					this._displayErrorOverlay("resync");
				}
			}
			else if ($defined(this._errDiv))
				this._removeErrorOverlay();
		},
		_diff: function (a, b) {
			if (a.length > b.length)
				return "a > b";
			if (a.length < b.length)
				return "a < b";
			for (var i = 0; i < a.length; i++) {
				if (a[i] != b[i])
					return "a[%d] != b[%d] / %d != %d".sprintf(i,i,a.charCodeAt(i),b.charCodeAt(i));
			}
			return "a == b";
		}
	});

	pygowave.view.extend({
		BlipEditorWidget: BlipEditorWidget
	});
})();
