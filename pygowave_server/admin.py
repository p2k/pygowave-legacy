
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

from pygowave_server.models import Wave, Wavelet, Blip, GadgetElement, Gadget
from pygowave_server.models import Participant, Element
from django.contrib import admin

admin.site.register(Participant)
admin.site.register(Gadget)

admin.site.register(Wave)
admin.site.register(Wavelet)
admin.site.register(Blip)
admin.site.register(Element)
admin.site.register(GadgetElement)
