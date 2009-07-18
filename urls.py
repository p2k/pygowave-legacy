
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

from django.contrib import admin
admin.autodiscover()

from registration.views import register
from pygowave_server.forms import MyRegistrationForm

js_info_dict = {
    'packages': ('pygowave_client',),
}

urlpatterns = patterns('',
	(r'^admin/(.*)', admin.site.root),
	url(r'^accounts/register/$', register, name='registration_register', kwargs={"form_class": MyRegistrationForm}),
	(r'^accounts/', include('registration.urls')),
	(r'^jsi18n/$', 'django.views.i18n.javascript_catalog', js_info_dict),
	(r'^i18n/', include('django.conf.urls.i18n')),
	(r'', include('pygowave_server.urls')),
)
