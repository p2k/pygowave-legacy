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

/**
 * Common classes and widgets.
 * 
 * @module pygowave.view.common
 */
pygowave.view = $defined(pygowave.view) ? pygowave.view : new Hash();
(function () {
	
	/**
	 * Base class for all view Widgets.
	 * 
	 * @class {private} pygowave.view.Widget
	 */
	var Widget = new Class({
		Implements: Events,
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {Element} contentElement DOM element to be inserted into the
		 *        parent element
		 * @param {optional String} where Where to inject the contentElement
		 *        relative to the parent element. Can be 'top', 'bottom',
		 *        'after', or 'before'.
		 */
		initialize: function(parentElement, contentElement, where) {
			this.contentElement = contentElement;
			this.inject(parentElement, where);
		},
		
		/**
		 * Removes the widget from the DOM. Sets the parentElement to null.
		 * @function {public Widget} dispose
		 * @return Returns a reference to this widget.
		 */
		dispose: function () {
			this.contentElement.dispose();
			this.parentElement = null;
			return this;
		},
		
		/**
		 * Destroys the widget, its content element and all children of the
		 * content element.
		 */
		destroy: function () {
			this.dispose();
			this.contentElement.destroy();
		},
		
		/**
		 * Inject this widget somewhere else. Sets the parent element.<br/>
		 * Note: Called by initialize.
		 * @function {public Widget} inject
		 * @param {Element} parentElement New parent element
		 * @param {optional String} where Where to inject the contentElement
		 *        relative to the parent element. Can be 'top', 'bottom',
		 *        'after', or 'before'.
		 * @return Returns a reference to this widget.
		 */
		inject: function (parentElement, where) {
			this.parentElement = parentElement;
			this.contentElement.inject(this.parentElement, where);
			return this;
		}
	});
	
	/**
	 * Renders a fancy search box.
	 *
	 * @class {public} pygowave.view.SearchWidget
	 * @extends pygowave.view.Widget
	 */
	var SearchWidget = new Class({
		Extends: Widget,
		
		// --- Event documentation ---
		/**
		 * Fired when the content of the search field changes.
		 * @event onChange
		 * @param {String} text The current (and recently changed) text of the
		 *        search field
		 */
		// ---------------------------
		
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {optional String} where Where to inject the contentElement
		 *        relative to the parent element. Can be 'top', 'bottom',
		 *        'after', or 'before'.
		 */
		initialize: function (parentElement, where) {
			var contentElement = new Element('div', {'class': 'search_widget'});
			this._lcorner = new Element('div', {'class': 'left_corner inactive'}).inject(contentElement);
			this._iwrapper = new Element('div', {'class': 'input_wrapper'}).inject(contentElement);
			this._input = new Element('input', {'type': 'text', 'class': 'inactive'}).inject(this._iwrapper);
			this._input.addEvent('focus', this._onFocus.bind(this));
			this._input.addEvent('blur', this._onBlur.bind(this));
			this._input.addEvent('keyup', this._onChange.bind(this));
			this._rcorner = new Element('div', {'class': 'right_corner inactive'}).inject(contentElement);
			this.parent(parentElement, contentElement, where);
			this._last_value = "";
		},
		
		/**
		 * Called when the input box receives focus. Toggles
		 * active/inactive style classes.
		 * @function {private} _onFocus
		 */
		_onFocus: function () {
			this._lcorner.addClass('active').removeClass('inactive');
			this._input.addClass('active').removeClass('inactive');
			this._rcorner.addClass('active').removeClass('inactive');
		},
		
		/**
		 * Called when the input box looses focus. Toggles
		 * active/inactive style classes.
		 * @function {private} _onBlur
		 */
		_onBlur: function () {
			this._lcorner.addClass('inactive').removeClass('active');
			this._input.addClass('inactive').removeClass('active');
			this._rcorner.addClass('inactive').removeClass('active');
		},
		
		/**
		 * Forwards the onChange event.
		 * @function {private} _onChange
		 */
		_onChange: function () {
			var new_value = this.text();
			if (new_value != this._last_value) {
				this.fireEvent('change', new_value);
				this._last_value = new_value;
			}
		},
		
		/**
		 * Set focus to the input box.
		 * @function {public} setFocus
		 */
		setFocus: function () {
			this._input.focus();
		},
		
		/**
		 * Returns the text of the search field.
		 * @function {public String} text
		 */
		text: function () {
			return this._input.get('value');
		}
	});
	
	pygowave.view.extend({
		Widget: Widget,
		SearchWidget: SearchWidget
	});
})();
