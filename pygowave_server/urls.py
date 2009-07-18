
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

from django.conf.urls.defaults import *

from pygowave_server.views import *
from pygowave_server.views import all_gadgets
from django.conf import settings as django_settings

urlpatterns = patterns('',
	(r'^$', index),
	(r'^home/$', home),
	(r'^settings/$', settings),
	(r'^waves/$', wave_list),
	(r'^waves/(?P<wave_id>\w+)/$', wave),
	(r'^participants/search$', search_participants),
	(r'^gadgets/$', all_gadgets),
	(r'^gadgets/mine/$', my_gadgets),
	(r'^gadgets/load/$', gadget_loader),
	(r'^pycow/(?P<filename>\w+\.js)', pycow),
)

if 'rosetta' in django_settings.INSTALLED_APPS:
	urlpatterns += patterns('',
		url(r'^rosetta/', include('rosetta.urls')),
	)
