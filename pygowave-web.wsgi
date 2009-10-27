
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

# Intended for use with apache httpd mod_wsgi
#   WSGIScriptAlias / /srv/http/pygowave_project/pygowave-web.wsgi

import os
import sys

if not os.environ.has_key("DJANGO_SETTINGS_MODULE"):
	os.environ["DJANGO_SETTINGS_MODULE"] = "settings"
sys.path.append(os.path.dirname(__file__))

from django.core.handlers.wsgi import WSGIHandler
application = WSGIHandler()
