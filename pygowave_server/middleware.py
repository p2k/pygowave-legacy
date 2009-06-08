
from django.core.exceptions import ObjectDoesNotExist
from django.conf import settings
from  datetime import datetime

from pygowave_server.models import Participant

class UserOnlineMiddleware:
	"""
	Update the last_contact field of the user's participant object. Used to
	determine how many users are online. This also creates a participant object
	if it is missing.
	
	"""
	def process_request(self, request):
		if request.user.is_authenticated():
			try:
				profile_obj = request.user.get_profile()
			except ObjectDoesNotExist:
				profile_obj = Participant()
				profile_obj.id = "%s@%s" % (request.user.username, settings.WAVE_DOMAIN)
				profile_obj.name = request.user.username
				profile_obj.user = request.user
			profile_obj.last_contact = datetime.now()
			profile_obj.save()
