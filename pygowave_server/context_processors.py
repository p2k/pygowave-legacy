
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

from django.conf import settings

def server(request):
	"""
	Puts IS_LOCAL setting, the VERSION and the Orbited configuration into context.
	
	"""
	if settings.ORBITED_PORT == "auto":
		oport = request.META["SERVER_PORT"]
	else:
		oport = settings.ORBITED_PORT
	return {'IS_LOCAL': settings.IS_LOCAL,
			'ORBITED_SERVER': settings.ORBITED_SERVER,
			'ORBITED_PORT': oport,
			'VERSION': settings.VERSION}

def storage_urls(request):
	"""
	Puts AVATAR_URL and GADGET_URL into context.
	
	"""
	return {'AVATAR_URL': settings.AVATAR_URL, 'GADGET_URL': settings.GADGET_URL}
