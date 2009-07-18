
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

from django.core.files.uploadedfile import UploadedFile
from django.conf import settings
from django.db.models import get_model

import string, random, time

class AlreadyUploadedFile(UploadedFile):
	"""
	This class is used to mark files, which have already been uploaded and
	stored. It still can be opened and read.
	
	"""
	def __init__(self, path, name, size):
		super(AlreadyUploadedFile, self).__init__(name, size=size)
		self.path = path
		self._file = None
	
	def open(self):
		self._file = open(path, "r")
		
	def seek(self, pos):
		if self._file != None:
			self._file.seek(pos)
	
	def tell(self):
		if self._file == None: return 0
		return self._file.tell()

	def read(self, num_bytes=None):
		if self._file == None: self.open()
		if num_bytes == None:
			return self._file.read()
		else:
			return self._file.read(num_bytes)
		
	def reset(self):
		self._file.reset()

	def close(self):
		self._file.close()
	
	def temporary_file_path(self):
		"""
		Emulate behaviour of temporary uploaded files.
		
		"""
		return self.path

def get_profile_model():
	"""
	Return the user profile model class as defined by the AUTH_PROFILE_MODULE
	setting. If that setting is missing SiteProfileNotAvailable is raised.
	
	"""
	if not hasattr(settings, 'AUTH_PROFILE_MODULE') or settings.AUTH_PROFILE_MODULE == None:
		raise SiteProfileNotAvailable
	
	profile_mod = get_model(*settings.AUTH_PROFILE_MODULE.split('.'))
	if profile_mod is None:
		raise SiteProfileNotAvailable
	
	return profile_mod

RANDOM_ID_BASE = string.letters+string.digits
def gen_random_id(length):
	"""
	Generate a random string with the given length.
	Characgters are taken from RANDOM_ID_BASE.
	
	"""
	return "".join([random.choice(RANDOM_ID_BASE) for x in xrange(length)])

def find_random_id(manager, length, suffix="", prefix=""):
	"""
	Generates a random id for obj, checks if it exists and retries in
	case of collision. Returns the found number.
	
	"""
	rnd_id = prefix + gen_random_id(length) + suffix
	while manager.filter(pk=rnd_id).count() > 0:
		rnd_id = prefix + gen_random_id(length) + suffix
	return rnd_id

def datetime2milliseconds(dt):
	"""
	Convert a python datetime instance to milliseconds since the epoc.
	
	"""
	return int(time.mktime(dt.timetuple())) * 1000 + dt.microsecond / 1000
