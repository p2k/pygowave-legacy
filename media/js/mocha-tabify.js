/*

Script: mocha-tabify.js
	Functionality for tabifying divs. Dirty hack, but works like jQuery.

Copyright:
	Copyright (c) 2009 Patrick Schneider

License:
	MIT-style license.

Requires:
	Core.js, Window.js (for tabbed windows) or Layout.js (for tabbed panels)

*/
MochaUI.extend({
	tabify: function (el, onSelect) {
		if (!$defined(onSelect))
			onSelect = $empty;
		$e = $(el);
		var ul = $e.getFirst('ul');
		ul.addClass("mochaTabsNav");
		ul.getElements('li').addClass("mochaTab");
		$e.getChildren('div').setStyle("display", "none").addClass("mochaTabContent");
		$each(ul.getElements('a'), function (a) {
			$(a).addEvent('click', function (event) {
				var spl = this.hash.split("-");
				onSelect(parseInt(spl[1]))
				MochaUI.tabSelect(spl[0].substring(1), parseInt(spl[1]));
				event.stop();
			});
		});
		MochaUI.tabSelect(el, 1);
	},
	tabSelect: function (el, id) {
		$e = $(el);
		$e.getChildren('div').setStyle("display", "none");
		$($e.id + "-" + id).setStyle("display", "block");
		var ul = $e.getFirst('ul');
		var link = "#" + $e.id + "-" + id;
		$each(ul.getElements('li'), function (li) {
			if (li.getFirst('a').hash == link)
				li.addClass("mochaTabSelected");
			else
				li.removeClass("mochaTabSelected");
		}, this);
	}
});
