
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

from django.conf import settings as django_settings
from django.http import Http404, HttpResponse, HttpResponseNotModified

from datetime import datetime, timedelta
import os, gzip

SRC_FOLDER = os.path.dirname(os.path.abspath(__file__)) + os.path.sep + "src" + os.path.sep
CACHE_FOLDER = os.path.dirname(os.path.abspath(__file__)) + os.path.sep + "cache" + os.path.sep

RFC_1123_DATETIME = "%a, %d %b %Y %H:%M:%S GMT"
PARSE_ERROR_MESSAGE = """alert("PyCow: Error while parsing '%s':\\n\\n%s");"""

def view_module(request, package, module):
	"""
	Return the requested JavaScript module, converts files with .py ending via
	PyCow and caches results. Supports If-Modified-Since and gzip compression.
	
	"""
	
	namespace = "pygowave.%s" % (package)
	module = package + os.path.sep + module
	cachefile = CACHE_FOLDER + module + ".js"
	srcfile = SRC_FOLDER + module
	
	if os.path.exists(srcfile + ".py"):
		srcfile += ".py"
		if not os.path.exists(cachefile) or os.path.getmtime(srcfile) > os.path.getmtime(cachefile):
			from pycow import translate_file, ParseError # Import has been placed here, so PyCow is not a dependency
			if not os.path.exists(CACHE_FOLDER + package):
				os.mkdir(CACHE_FOLDER + package)
			try:
				translate_file(srcfile, cachefile, namespace=namespace, warnings=False)
			except ParseError, e:
				os.unlink(cachefile)
				return HttpResponse(PARSE_ERROR_MESSAGE % (filename, e.value), mimetype="text/javascript")
	elif os.path.exists(srcfile + ".js"):
		srcfile += ".js"
		if not os.path.exists(cachefile) or os.path.getmtime(srcfile) > os.path.getmtime(cachefile):
			if not os.path.exists(CACHE_FOLDER + package):
				os.mkdir(CACHE_FOLDER + package)
			# Just symlink/copy for now; TODO: offer minification
			if os.name == "posix":
				target = os.path.relpath(srcfile, os.path.dirname(cachefile))
				os.symlink(target, cachefile)
			else:
				open(cachefile, 'w').write(open(srcfile, 'r').read())
	else:
		raise Http404
	
	mtime = datetime.utcfromtimestamp(os.path.getmtime(srcfile)) # Beware 2038 - the year when the POSIX timestamp dies
	
	# Handle If-Modified-Since
	if request.META.has_key("HTTP_IF_MODIFIED_SINCE"):
		try:
			mstime = datetime.strptime(request.META["HTTP_IF_MODIFIED_SINCE"], RFC_1123_DATETIME)
			if mtime <= mstime:
				return HttpResponseNotModified()
		except ValueError:
			pass
	
	response = HttpResponse(mimetype="text/javascript")
	response["Last-Modified"] = mtime.strftime(RFC_1123_DATETIME)
	
	# Support for gzip
	if "gzip" in request.META["HTTP_ACCEPT_ENCODING"].split(","):
		if not os.path.exists(cachefile + ".gz") or os.path.getmtime(cachefile) > os.path.getmtime(cachefile + ".gz"):
			gzip.open(cachefile + ".gz", 'wb').write(open(cachefile, 'r').read())
		cachefile = cachefile + ".gz"
		response["Content-Encoding"] = "gzip"

	response.write(open(cachefile, "r").read())

	return response
