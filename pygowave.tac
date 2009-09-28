
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
#    twistd -ny pygowave.tac

import os, sys, pkg_resources
from twisted.application import service, internet

pkg_resources.require("twisted==8.2.0") # Specific workarounds needed

def addOrbitedService(site):
	# Duplicates functionality of orbited's start script
	pkg_resources.require("orbited>=0.7.7")
	
	from orbited import config
	from orbited import logging
	
	class EmptyConfig(object):
		def __init__(self):
			config = None
			version = False
			profile = False
	
	config.setup(options=EmptyConfig())
	logging.setup(config.map)
	
	from twisted.web import static
	import orbited.system
	import orbited.start
	
	orbited.start.logger = logging.get_logger('orbited.start')
	
	root = site.resource
	static_files = static.File(pkg_resources.resource_filename("orbited", "static"))
	root.putChild('static', static_files)
	root.putChild('system', orbited.system.SystemResource())
	
	orbited.start._setup_protocols(root)
	orbited.start._setup_static(root, config.map)

def setupDjangoService():
	pkg_resources.require("django>=1.1")
	
	from twisted.web import wsgi, resource, server, static, error
	from twisted.python.threadpool import ThreadPool
	from django.core.handlers.wsgi import WSGIHandler
	from twisted.internet import reactor
	import copy
	
	wsgiThreadPool = ThreadPool()
	wsgiThreadPool.start()
	reactor.addSystemEventTrigger('after', 'shutdown', wsgiThreadPool.stop)
	
	root = resource.Resource()
	root.putChild('media', static.File("media"))
	root.putChild('admin_media', static.File(pkg_resources.resource_filename("django", os.path.join("contrib", "admin", "media"))))
	
	class CombinedWSGISite(server.Site):
		def __init__(self, resource, wsgi_resource, logPath=None, timeout=60 * 60 * 12):
			server.Site.__init__(self, resource, logPath, timeout)
			self.wsgi_resource = wsgi_resource
		
		def getResourceFor(self, request):
			saved_path = (copy.copy(request.prepath), copy.copy(request.postpath))
			ret = server.Site.getResourceFor(self, request)
			if isinstance(ret, error.NoResource):
				request.prepath, request.postpath = saved_path
				request.content.seek(0,0) # Note: May be removed on newer versions of Twisted
				return resource.getChildForRequest(self.wsgi_resource, request)
			return ret
	
	def WSGIScriptNamePatch(handler): # Note: May be removed on newer versions of Twisted
		def call(environ, start_response):
			environ["SCRIPT_NAME"] = ""
			return handler(environ, start_response)
		return call

	wsgi_resource = wsgi.WSGIResource(reactor, wsgiThreadPool, WSGIScriptNamePatch(WSGIHandler()))

	return CombinedWSGISite(root, wsgi_resource)

sys.path.append(os.path.dirname(__file__))
os.environ["DJANGO_SETTINGS_MODULE"] = "settings"

application = service.Application("PyGoWave Server")

# attach the service to its parent application
pygo_srv = setupDjangoService()
addOrbitedService(pygo_srv)

srv = internet.TCPServer(80, pygo_srv)
srv.setServiceParent(application)
