
from django.core.files.uploadedfile import UploadedFile
from django.conf import settings
from django.db.models import get_model

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