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

/**
 * View module.
 * @module pygowave.view
 */

/**
 * Base class for all view Widgets.
 * 
 * @class {public} pygowave.view.Widget
 */
Widget = new Class({
	Implements: Events,
	initialize: function(parentElement, contentElement) {
		this.parentElement = parentElement;
		this.contentElement = contentElement;
		
		this.contentElement.inject(this.parentElement, 'bottom');
	}
});

/**
 * This Widget renders a standard Wavelet view. The participants are listed
 * at top and the "Add participant" button is placed there as well.
 * Below the participants the actual Wavelet content is rendered.
 * 
 * @class {public} pygowave.view.WaveletWidget
 * @extends pygowave.view.Widget
 */
WaveletWidget = new Class({
	Extends: Widget,
	initialize: function(parentElement) {
		var contentElement = new Element('div', {
			'class': 'wavelet_widget'
		});
		this.participantsDiv = new Element('div', {
			'class': 'wavelet_participants_div'
		}).inject(content);
		this.parent(parentElement, contentElement);
	},
	updateParticipants: function (wavelet) {
		
	}
});

WaveView = new Class({
	Implements: [Options, Events],
	options: {
		model: null,
		container: null
	},
	initialize: function(options) {
		this.setOptions(options);
		this.waveletWidgets = {};
		
		if ($defined(this.options.model) && $defined(this.options.container)) {
			this.rootWavelet = new WaveletWidget(this.options.container);
			this.waveletWidgets[this.options.model.rootWaveletId()] = this.rootWavelet;
			this.options.model.addEvent('participantsChanged', _onModelParticipantsChanged.bind(this));
		}
	},
	_onModelParticipantsChanged: function (waveletId) {
		this.waveletWidgets[waveletId].updateParticipants(this.options.model.wavelet(waveletId));
	}
});
