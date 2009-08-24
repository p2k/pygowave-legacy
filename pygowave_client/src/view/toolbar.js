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
 * A common toolbar for wavelets and the blip editor.
 * 
 * @module pygowave.view.toolbar
 */
(function () {
	/* Imports */
	var Widget = pygowave.view.Widget;
	
	/**
	 * A multi-purpose toolbar widget.
	 * 
	 * @class {public} pygowave.view.ToolbarWidget
	 * @extends {pygowave.view.Widget}
	 */
	var ToolbarWidget = new Class({
		Extends: Widget,
		
		/**
		 * Called on instantiation.
		 *
		 * @constructor {public} initialize
		 * @param {Element} parentElement Parent DOM element to insert the widget
		 * @param {String} css_class CSS class to use on this toolbar
		 * @param {optional String} where Where to inject the widget
		 *        relative to the parent element. Can be 'top', 'bottom',
		 *        'after', or 'before'.
		 */
		initialize: function (parentElement, css_class, where) {
			var contentElement = new Element('div', {'class': css_class});
			this.items = new Hash();
			this.parent(parentElement, contentElement, where);
			this.sep_count = 0;
		},
		
		/**
		 * Add a seperator to the toolbar.
		 *
		 * @function {public} addSeperator
		 */
		addSeperator: function () {
			this.sep_count++;
			this.items.set('seperator_%d'.sprintf(this.sep_count), new Element('div', {'class': 'seperator'}).inject(this.contentElement));
		},
		
		/**
		 * Add a button to the toolbar.
		 *
		 * @function {public} addButton
		 * @param {ToolbarButton} button The button to add
		 */
		addButton: function (button) {
			button.elt.inject(this.contentElement);
			this.items.set(button.name(), button);
		}
	});
	
	/**
	 * Represents a button on the toolbar. A button can be toggleable, it can
	 * be enabled and disabled and it has an action associated on click.
	 *
	 * @class {public} ToolbarButton
	 */
	var ToolbarButton = new Class({
		Implements: [Events, Options],
		
		options: {
			icon_class: "",
			label: "",
			tooltip: "",
			onClick: $empty,
			toggleable: false
		},
		
		/**
		 * Called on instantiation.
		 *
		 * @constructor {public} initialize
		 * @param {String} name Name of the button
		 * @param {Object} options Options include:
		 * @... {String} icon_class CSS class for the button icon
		 * @... {String} label Title of the button
		 * @... {String} tooltip Tiptext of the button
		 * @... {Function} onClick Function to be executed on click
		 * @... {Boolean} toggleable Set to true if the button can be toggled
		 */
		initialize: function (name, options) {
			this._name = name;
			this.setOptions(options);
			this.elt = new Element(
				'div',
				{
					'class': 'toolbar_button',
					'title': (this.options.tooltip != "" ? this.options.tooltip : this.options.label)
				}
			);
			this.icon = new Element('div', {'class': this.options.icon_class}).inject(this.elt);
			this.text = new Element('div', {'class': "toolbar_text", 'text': this.options.label}).inject(this.elt);
			this.elt.addEvent('click', this._onClick.bind(this));
			
			this._enabled = true;
			this._toggled = false;
		},
		
		/**
		 * Return the button's name
		 *
		 * @function {public String} name
		 */
		name: function () {
			return this._name;
		},
		
		/**
		 * Callback on click. Fires onClick event.
		 * @function {private} _onClick
		 */
		_onClick: function (e) {
			if (!this.disabled) {
				this.toggle();
				this.fireEvent('click', e);
			}
		},
		
		/**
		 * Disable/enable the button.
		 *
		 * @function {public} setEnabled
		 * @param {Boolean} enabled True for enabled, false for disabled
		 */
		setEnabled: function (enabled) {
			this._enabled = enabled;
			if (enabled)
				this.elt.removeClass("disabled");
			else
				this.elt.addClass("disabled");
		},
		
		/**
		 * Return weather the button is enabled.
		 *
		 * @function {public Boolean} isEnabled
		 */
		isEnabled: function () {
			return this._enabled;
		},
		
		/**
		 * Toggle the button (if toggleable).
		 *
		 * @function {public} toggle
		 * @param {optional Boolean} value Set to true to set the button toggled
		 *        false to un-toggle, omit to actually toggle the value.
		 */
		toggle: function (value) {
			if (!this.options.toggleable)
				return;
			if (!$defined(value))
				value = !this._toggled;
			this._toggled = value;
			if (value)
				this.elt.addClass("toggled");
			else
				this.elt.removeClass("toggled");
		},
		
		/**
		 * Return weather the button is toggled.
		 *
		 * @function {public Boolean} isToggled
		 */
		isToggled: function () {
			return this._toggled;
		}
	});
	
	pygowave.view.extend({
		ToolbarWidget: ToolbarWidget,
		ToolbarButton: ToolbarButton
	});
	
})();
