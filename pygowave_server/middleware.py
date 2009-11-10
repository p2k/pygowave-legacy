
from django.core.exceptions import ObjectDoesNotExist
from django.conf import settings
from datetime import datetime

from pygowave_server.models import Participant
from couchdbkit.resource import ResourceConflict

class UserOnlineMiddleware:
	"""
	Update the last_contact field of the user's participant object. Used to
	determine how many users are online. This also creates a participant object
	if it is missing.
	
	"""
	def process_request(self, request):
		if request.user.is_authenticated():
			try:
				profile_obj = Participant.objects.get(user__id=request.user.id)
			except ObjectDoesNotExist:
				profile_obj = Participant()
				profile_obj.id = "%s@%s" % (request.user.username, settings.WAVE_DOMAIN)
				profile_obj.name = request.user.username
				profile_obj.user = request.user
			except:
				return # Ignore other errors
			profile_obj.last_contact = datetime.now()
			try:
				profile_obj.save()
			except ResourceConflict:
				pass # Ignore
