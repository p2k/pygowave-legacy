
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

sys.path.append(os.path.dirname(__file__))
if not os.environ.has_key("DJANGO_SETTINGS_MODULE"):
	os.environ["DJANGO_SETTINGS_MODULE"] = "settings"

from twisted.application import service, internet
from twisted.python import log

import pygowave_rpc.logger

from django.conf import settings

pygowave_rpc.logger.setupLogging()

application = service.Application("PyGoWave RPC Server")
#def NullLogger(data): pass
#application.setComponent(log.ILogObserver, NullLogger)

if getattr(settings, "STOMP_MODE", "client") == "client":
	from pygowave_rpc.stomp_client import StompClientFactory
	scf = StompClientFactory()
	cli = internet.TCPClient(
		getattr(settings, "RPC_SERVER", "localhost"),
		getattr(settings, "STOMP_PORT", 61613),
		scf
	)
	cli.setServiceParent(application)
elif getattr(settings, "STOMP_MODE", "client") == "server":
	from pygowave_rpc.stomp_server import StompServerFactory
	ssf = StompServerFactory()
	srv = internet.TCPServer(
		getattr(settings, "STOMP_PORT", 61613),
		ssf
	)
	srv.setServiceParent(application)
