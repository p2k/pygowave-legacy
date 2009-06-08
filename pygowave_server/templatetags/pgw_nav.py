
#
# PyGoWave Server - The Python Google Wave Server
# Copyright 2009 Patrick Schneider <patrick.p2k.schneider@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

from django.template import Library
from django.conf import settings

from django.utils.translation import ugettext as _
from django.core.urlresolvers import reverse

register = Library()

STATIC_NAVBAR = [
	{
		"id":	"home",
		"name": _(u"Home"),
		"url":	reverse("pygowave_server.views.home"),
		"sub":	[
			{
				"id":	"welcome",
				"name":	_(u"Welcome"),
				"url":	reverse("pygowave_server.views.home"),
			},
			{
				"id":	"settings",
				"name":	_(u"Settings"),
				"url":	reverse("pygowave_server.views.settings"),
			}
		]
	},
	{
		"id":	"gadgets",
		"name":	_(u'Gadgets'),
		"url":	reverse("pygowave_server.views.my_gadgets"),
		"sub":	[
			{
				"id":	"my_gadgets",
				"name":	_(u'My Gadgets'),
				"url":	reverse("pygowave_server.views.my_gadgets"),
			},
			{
				"id":	"all_gadgets",
				"name":	_(u'All Gadgets'),
				"url":	reverse("pygowave_server.views.all_gadgets"),
			}
		]
	},
	{
		"id":	"waves",
		"name":	_(u'Waves'),
		"url":	reverse("pygowave_server.views.wave_list"),
		"sub":	[
			{
				"id":	"wave_list",
				"name":	_(u'All Waves'),
				"url":	reverse("pygowave_server.views.wave_list"),
			},
		]
	},
]

@register.inclusion_tag('pygowave_server/nav.html', takes_context=True)
def pgw_navbar(context, active_nav, active_sub_nav=None):
	sub = None
	for nav in STATIC_NAVBAR:
		if nav["id"] == active_nav:
			sub = nav["sub"]
	return {"navigation": STATIC_NAVBAR, "active_nav": active_nav, "sub_navigation": sub, "active_sub_nav": active_sub_nav}
