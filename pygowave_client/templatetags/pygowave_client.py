
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
from django.core.urlresolvers import reverse

register = Library()

STATIC_LOAD_ORDER = (
	(
		"utils", ("sha1",)
	),
	(
		"model", ("model",)
	),
	(
		"view",
		(
			"selection",
			"common",
			"toolbar",
			"participants",
			"gadgets",
			"blip_editor",
			"debug_tools",
			"view",
		),
	),
	(
		"operations", ("operations",)
	),
	(
		"controller", ("controller",)
	),
)

@register.simple_tag
def client_scripts():
	out = ""
	for package, modules in STATIC_LOAD_ORDER:
		for module in modules:
			out += '\t<script type="text/javascript" src="%s"></script>\n' % (reverse("pygowave_client.views.view_module", args=(package, module)))
	return out
