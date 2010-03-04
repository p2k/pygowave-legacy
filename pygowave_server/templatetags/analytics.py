
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

register = Library()

@register.simple_tag
def track_event(category=None, action=None, opt_label=None, opt_value=None):
	if getattr(settings, "ANALYTICS_ID", "") == "":
		return ""
	
	l = []
	for arg in (category, action, opt_label, opt_value):
		if arg != None:
			l.append('"%s"' % (arg))
	return "<script type=\"text/javascript\">pageTracker._trackEvent(%s);</script>" % (", ".join(l))

@register.simple_tag
def tracker_code():
	if getattr(settings, "ANALYTICS_ID", "") == "":
		return ""
	
	return """<!-- Begin Google Analytics Code-->
<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%%3E%%3C/script%%3E"));
</script>
<script type="text/javascript">
try {
	var pageTracker = _gat._getTracker("%s");
	pageTracker._trackPageview();
} catch(err) {}
</script>
<!-- End Google Analytics Code-->""" % (settings.ANALYTICS_ID)
