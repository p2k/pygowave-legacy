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
 * Browser independent selection objects.
 * 
 * @module pygowave.view.selection
 */
(function () {
	
	/* Helper function for IE */
	var setRangeToNode = function (rng, elt) {
		var temprng = rng.duplicate();
		var tempelt, templen;
		if ($type(elt) == 'element')
			rng.moveToElementText(elt);
		else {
			// Start
			if ($defined(elt.previousSibling)) {
				templen = 0;
				tempelt = elt.previousSibling;
				while ($defined(tempelt) && $type(tempelt) != 'element') {
					templen += tempelt.data.length;
					tempelt = tempelt.previousSibling;
				}
				if ($defined(tempelt)) {
					rng.moveToElementText(tempelt);
					if (templen > 0)
						rng.moveEnd('character', templen);
					rng.setEndPoint('StartToEnd', rng);
				}
				else {
					rng.moveToElementText(elt.parentNode);
					if (templen > 0)
						rng.moveStart('character', templen);
				}
			}
			else
				rng.moveToElementText(elt.parentNode);
			
			// End
			if ($defined(elt.nextSibling)) {
				templen = 0;
				tempelt = elt.nextSibling;
				while ($defined(tempelt) && $type(tempelt) != 'element') {
					templen += tempelt.data.length;
					tempelt = tempelt.nextSibling;
				}
				if ($defined(tempelt)) {
					temprng.moveToElementText(tempelt);
					if (templen > 0)
						temprng.moveStart('character', -templen);
					temprng.setEndPoint('EndToStart', rng);
				}
				else {
					temprng.moveToElementText(elt.parentNode);
					if (templen > 0)
						temprng.moveEnd('character', -(templen+1));
				}
			}
			else
				temprng.moveToElementText(elt.parentNode);
			rng.setEndPoint('EndToEnd', temprng);
		}
	};
	
	/**
	 * Browser independent selection class which wraps W3C and MS selection
	 * and range objects.<br/>
	 * Note: This is at no rate a complete implementation for selections and
	 * ranges, it is used for special purposes only.
	 * 
	 * @class {public} pygowave.view.Selection
	 */
	var Selection = new Class({
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {optional Node} startNode Start node
		 * @param {optional int} startOffset Start offset
		 * @param {optional Node} endNode End node
		 * @param {optional int} endOffset End offset
		 * @param {optional Document} ownerDocument Owner document for the selection.
		 */
		initialize: function (startNode, startOffset, endNode, endOffset, ownerDocument) {
			if (!$defined(ownerDocument))
				this._doc = window.document;
			else
				this._doc = ownerDocument;
			this._startNode = ($defined(startNode) ? startNode : null);
			this._endNode = ($defined(endNode) ? endNode : null);
			this._startOffset = ($defined(startOffset) ? startOffset : 0);
			this._endOffset = ($defined(endOffset) ? endOffset : 0);
			this._atBottom = false;
		},
		/**
		 * Return the start node.
		 * @function {public Node} startNode
		 */
		startNode: function () {
			return this._startNode;
		},
		/**
		 * Return the end node.
		 * @function {public Node} endNode
		 */
		endNode: function () {
			return this._endNode;
		},
		/**
		 * Return the start offset.
		 * @function {public Node} startOffset
		 */
		startOffset: function () {
			return this._startOffset;
		},
		/**
		 * Return the end offset.
		 * @function {public Node} endOffset
		 */
		endOffset: function () {
			return this._endOffset;
		},
		/**
		 * Set the start node and offset.
		 * @function {public} setStart
		 * @param {Node} node Start node
		 * @param {int} offset Start offset
		 */
		setStart: function (node, offset) {
			this._startNode = node;
			this._startOffset = offset;
			this._atBottom = false;
		},
		/**
		 * Set the end node and offset.
		 * @function {public} setEnd
		 * @param {Node} node End node
		 * @param {int} offset End offset
		 */
		setEnd: function (node, offset) {
			this._endNode = node;
			this._endOffset = offset;
			this._atBottom = false;
		},
		/**
		 * Returns true if the selection is valid.
		 * @function {public Boolean} isValid
		 */
		isValid: function () {
			return (this._startNode != null && this._endNode != null);
		},
		/**
		 * Returns true if the selection is collapsed (i.e. empty).
		 * @function {public Boolean} isCollapsed
		 */
		isCollapsed: function () {
			return (this._startNode == this._endNode && this._startOffset == this._endOffset);
		},
		/**
		 * Move the start and end point to the lowest node (usually a text
		 * node) if possible.
		 * @function {public} moveDown
		 */
		moveDown: function () {
			if (!this.isValid() || this._atBottom)
				return;
			
			var prev;
			while ($type(this._startNode) == "element" && $defined(this._startNode.firstChild)) {
				prev = this._startNode;
				this._startNode = this._startNode.firstChild;
				while ($defined(this._startNode) && this._startOffset > 0) {
					prev = this._startNode;
					this._startNode = this._startNode.nextSibling;
					this._startOffset--;
				}
				if (!$defined(this._startNode))
					this._startNode = prev;
			}
			
			while ($type(this._endNode) == "element" && $defined(this._endNode.firstChild)) {
				prev = this._endNode;
				this._endNode = this._endNode.firstChild;
				while ($defined(this._endNode) && this._endOffset > 0) {
					prev = this._endNode;
					this._endNode = this._endNode.nextSibling;
					this._endOffset--;
				}
				if (!$defined(this._endNode))
					this._endNode = prev;
			}
			
			this._atBottom = true;
		},
		/**
		 * Return a native text range object (either W3C or MS).
		 * @function {public Object} toNativeRange
		 */
		toNativeRange: function () {
			if (!this.isValid)
				return null;
			
			var rng;
			if (Browser.Engine.trident) {
				rng = this._doc.body.createTextRange();
				setRangeToNode(rng, this._startNode);
				rng.move('character', this._startOffset);
				var endrng = this._doc.body.createTextRange();
				setRangeToNode(endrng, this._endNode);
				endrng.move('character', this._endOffset);
				rng.setEndPoint('EndToStart', endrng);
			}
			else {
				rng = this._doc.createRange();
				rng.setStart(this._startNode, this._startOffset);
				rng.setEnd(this._endNode, this._endOffset);
			}
			return rng;
		},
		/**
		 * Set the user's current selection to this selection.
		 * @function {public} select
		 */
		select: function () {
			var range = this.toNativeRange();
			if (range.select){
				$try(function(){
					range.select();
				});
			} else {
				var win = this._doc.window;
				var s = $defined(win.getSelection) ? win.getSelection() : this._doc.selection;
				if (s.addRange) {
					s.removeAllRanges();
					s.addRange(range);
				}
			}
		}
	});
	/**
	 * Workaround for IE; creates a Selection object from a TextRange object.
	 * @function {private Selection} tridentFindRange
	 * @param {Document} ownerDocument Owner document for the selection.
	 *        Defaults to the current window's document.
	 * @param {Element} scope Scope limitation
	 * @param {TextRange} textrange TextRange object
	 */
	Selection.tridentFindRange = function (ownerDocument, scope, textrange) {
		var elt = scope;
		var ret = new Selection(null, 0, null, 0, ownerDocument);
		var todo = 'Start';
		var emptyElement = false;
		var rng = ownerDocument.body.createTextRange();
		while (true) {
			setRangeToNode(rng, elt);
			
			// check start
			var pos = textrange.compareEndPoints(todo + 'ToStart', rng);
			
			if (pos == -1) // left of - miss
				return new Selection();
			else if (pos == 0) { // direct hit
				if (todo == 'Start') {
					ret.setStart(elt, 0);
					todo = 'End';
					continue;
				}
				else {
					ret.setEnd(elt, 0);
					break;
				}
			}
			
			// right of - check end
			pos = textrange.compareEndPoints(todo + 'ToEnd', rng);
			if (pos <= 0) { // left of/hit - descend/found
				if ($type(elt) == "element") {
					if ($defined(elt.firstChild)) {
						elt = elt.firstChild;
						continue;
					}
					else { // Empty element; treat as hit
						emptyElement = true;
						if (todo == 'Start') {
							ret.setStart(elt, 0);
							todo = 'End';
							continue;
						}
						else {
							ret.setEnd(elt, 0);
							break;
						}
					}
				}
				else {
					// In textnode
					
					var sStart = 0, sEnd = elt.data.length;
					var rel = parseInt((sEnd - sStart) / 2);
					while (sStart < sEnd) {
						if (rel == 0 && sStart != sEnd)
							rel = 1;
						rng.moveStart('character', rel);
						pos = textrange.compareEndPoints(todo + 'ToStart', rng);
						if (pos < 0) { // left of
							sEnd = sStart + rel;
							rng.moveStart('character', -rel);
						}
						else { // right of
							sStart += rel;
							if (pos == 0)
								break;
						}
						rel = parseInt((sEnd - sStart) / 2);
					}
					
					if (todo == 'Start') {
						ret.setStart(elt, sStart);
						todo = 'End';
						continue;
					}
					else {
						ret.setEnd(elt, sStart);
						break;
					}
				}
			}
			
			// right of - sibling
			if (!$defined(elt.nextSibling)) { // Ascend
				if (elt.parentNode == scope || !$defined(elt.parentNode) || !$defined(elt.parentNode.nextSibling))
					return new Selection(); // Out of scope
				elt = elt.parentNode.nextSibling;
			}
			else
				elt = elt.nextSibling;
		}
		
		if (!emptyElement && !textrange.isEqual(ret.toNativeRange()))
			alert(gettext("Internet Explorer bug:\nCould not create Selection object from TextRange!"));
		
		return ret;
	};
	/**
	 * Returns the current selection of the user.
	 * 
	 * @function {static public Selection} currentSelection
	 * @param {optional Element} scope Limit the scope to this element, i.e.
	 *        return an invalid selection if it is not inside this element.
	 * @param {optional Document} ownerDocument Owner document for the selection.
	 *        Defaults to the current window's document.
	 */
	Selection.currentSelection = function (scope, ownerDocument) {
		if (!$defined(ownerDocument))
			ownerDocument = window.document;
		if (!$defined(scope))
			scope = ownerDocument.body;
		
		var win = ownerDocument.window;
		var sel = $defined(win.getSelection) ? win.getSelection() : ownerDocument.selection;
		if (!$defined(sel))
			return new Selection();
		var rng = (sel.rangeCount > 0 ? sel.getRangeAt(0) : (sel.createRange ? sel.createRange() : null));
		if (!$defined(sel))
			return new Selection();
			
		if (Browser.Engine.trident)
			return Selection.tridentFindRange(ownerDocument, scope, rng);
		else {
			// Check scope
			if (scope != ownerDocument.body) {
				var elt = rng.startContainer;
				while ($type(elt) != "element" || (elt != scope && elt.get('tag') != "html"))
					elt = elt.parentNode;
				if (elt != scope)
					return new Selection();
				elt = rng.endContainer;
				while ($type(elt) != "element" || (elt != scope && elt.get('tag') != "html"))
					elt = elt.parentNode;
				if (elt != scope)
					return new Selection();
			}
			return new Selection(
				rng.startContainer,
				rng.startOffset,
				rng.endContainer,
				rng.endOffset,
				ownerDocument
			);
		}
	};
	
	pygowave.view.extend({
		Selection: Selection
	});
})();
