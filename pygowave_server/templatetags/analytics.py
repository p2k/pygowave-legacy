
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

register = Library()

@register.simple_tag
def track_event(category=None, action=None, opt_label=None, opt_value=None):
	l = []
	for arg in (category, action, opt_label, opt_value):
		if arg != None:
			l.append('"%s"' % (arg))
	return "<script type=\"text/javascript\">pageTracker._trackEvent(%s);</script>" % (", ".join(l))
