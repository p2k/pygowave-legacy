
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

# You can run this .tac file directly with:
#    twistd -ny pygowave-rpc.tac

# This is PyGoWave's RPC script used for client-to-server communication (and
# also for server-to-server communication in a future release).
# It can act as a STOMP server or client depending if a message broker is used
# or not.

import sys, os, logging, stomper

if not os.environ.has_key("DJANGO_SETTINGS_MODULE"):
	os.environ["DJANGO_SETTINGS_MODULE"] = "settings"

from twisted.application import service, internet

from pygowave_rpc.stomp_client import StompClientFactory
import pygowave_rpc.logger

from django.conf import settings

pygowave_rpc.logger.setupLogging()

application = service.Application("PyGoWave RPC Server")

if getattr(settings, "RPC_MODE", "client") == "client":
	scf = StompClientFactory()
	srv = internet.TCPClient(
		getattr(settings, "RPC_SERVER", "localhost"),
		getattr(settings, "STOMP_PORT", 61613),
		scf
	)
	srv.setServiceParent(application)
