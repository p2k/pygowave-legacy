
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
	Puts DEVELOPER_MODE setting, the VERSION and the Orbited configuration
	into context.
	
	"""
	if getattr(settings, "ORBITED_PORT", "auto") == "auto":
		oport = request.META["SERVER_PORT"]
	else:
		oport = settings.ORBITED_PORT
	return {
		'DEVELOPER_MODE': getattr(settings, "DEVELOPER_MODE", ""),
		'ORBITED_SERVER': getattr(settings, "ORBITED_SERVER", "localhost"),
		'ORBITED_PORT': oport,
		'VERSION': getattr(settings, "VERSION", "(not set)"),
		'PING_INTERVAL_SECONDS': getattr(settings, "PING_INTERVAL_SECONDS", 20)
	}

def storage_urls(request):
	"""
	Puts AVATAR_URL and GADGET_URL into context.
	
	"""
	return {
		'AVATAR_URL': getattr(settings, "AVATAR_URL", "/media/avatars/"),
		'GADGET_URL': getattr(settings, "GADGET_URL", "/media/gadgets/")
	}

def user_profile(request):
	"""
	Puts user_profile into context if available.
	
	"""
	
	profile = None
	
	if request.user.is_authenticated():
		from models import Participant
		
		try:
			profile = Participant.objects.get(user__id=request.user.id)
		except ObjectDoesNotExist:
			pass
	
	return {'user_profile': profile}
