
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

from datetime import datetime, timedelta

from django.db import models
from django.contrib.auth.models import User
from django.utils.translation import ugettext_lazy as _
from django.conf import settings

__author__ = "patrick.p2k.schneider@gmail.com"

ROOT_WAVELET_ID_SUFFIX = '!conv+root'

# Note: The term 'client' specifies all entities, that directly communicate
#       with this server by the means of a proprietary protocol.
#       Wave Foundation clients and servers will be treated sightly different,
#       but that is not implemented yet.
#
# BIG NOTE: All models represent local Waves at the moment - they will later
#           be extended to have a domain field which specifies external Waves.

class ParticipantManager(models.Manager):
	"""
	This class provides a method to determine the number of online users.
	
	"""
	
	def online_count(self):
		timeout = datetime.now() - timedelta(minutes=settings.ONLINE_TIMEOUT_MINUTES)
		return self.filter(last_contact__gte=timeout).count()

class Participant(models.Model):
	"""
	Represents participant information. It contains id, display name, avatar's
	URL, and an external URL to view the participant's profile page. The
	instances may be coming from external Wave servers (not yet).
	
	Note: This model also serves as user profile on this server.
	
	"""
	
	objects = ParticipantManager()
	
	id = models.CharField(max_length=255, primary_key=True)
	user = models.ForeignKey(User, unique=True)
	is_bot = models.BooleanField()
	last_contact = models.DateTimeField()
	
	name = models.CharField(max_length=255, blank=True)
	avatar = models.URLField(verify_exists=False, blank=True)
	profile = models.URLField(verify_exists=False, blank=True)

class Wave(models.Model):
	"""
	Models wave instances. These are the core of Google Wave.
	A single wave is composed of its id and any wavelets that belong to it.
	
	"""
	
	id = models.CharField(max_length=42, primary_key=True) # Don't panic :P

class Wavelet(models.Model):
	"""
	A wavlet within a wave. Wavelets have an id, exactly one creator and one
	root blip, a creation date/time and a last modified date/time, a title,
	a version, zero or more participants, data documents which serve as
	free-form metadata and (potentially) a lot of blips.
	
	"""
	
	id = models.CharField(max_length=42, primary_key=True)
	wave = models.ForeignKey(Wave, related_name="wavelets")
	creator = models.ForeignKey(Participant, related_name="created_wavelets")
	root_blip = models.OneToOneField("Blip", related_name="rootblip_wavelet")
	created = models.DateTimeField(auto_now_add=True)
	last_modified = models.DateTimeField(auto_now_add=True)
	title = models.CharField(max_length=255, blank=True)
	version = models.IntegerField(default=0)
	participants = models.ManyToManyField(Participant)
	
class DataDocument(models.Model):
	"""
	A DataDocument contains arbitrary data for storage of metadata in wavelets.
	
	"""
	
	name = models.CharField(primary_key=True, max_length=42)
	wavelet = models.ForeignKey(Wavelet, related_name="documents")
	data = models.TextField()

class Blip(models.Model):
	"""
	A Blip is a unit of conversation in a Wave. It is a node in a tree of
	other nodes and may have a parent and children. It contains metadata to
	keep track of contributors and versioning.
	The Blip contains raw text and special non-text elements which have a
	position within the text. Elements have no representation within the text
	in this implementation. Note that arbitrary HTML tags are placed as normal
	text within the Blip.
	
	"""
	
	id = models.CharField(max_length=42, primary_key=True)
	wavelet = models.ForeignKey(Wavelet, related_name="blips")
	parent = models.ForeignKey("self", related_name="children", null=True)
	creator = models.ForeignKey(Participant, related_name="created_blips")
	version = models.IntegerField(default=0)
	last_modified = models.DateTimeField(auto_now=True)
	submitted = models.BooleanField()
	
	contributors = models.ManyToManyField(Participant, related_name="contributed_blips")
	
	text = models.TextField()

class Annotation(models.Model):
	"""
	An annotation is metadata that augments a range of text in a Blip's text.
	Example uses of annotations include styling text, supplying spelling
	corrections, and links to refer that area of text to another Blip or
	web site. The size of an annotation range must be positive and non-zero.
	
	Note: The above is a restriction to the current Wavelet model. No annotation
	can span multiple Blips or be outside of them although the Federation
	Protocol allows this. The server will dissallow this even when the
	Federation Protocol gets implemented. It will simply send out Wave
	operations to split spanning annotations or delete the out-of-blip
	annotations.
	However, you can still do whatever you want in those DataDocuments in the
	Wavelets. Note that they are not very efficiantly stored and maintained at
	the moment.
	
	"""
	
	blip = models.ForeignKey(Blip, related_name="annotations")
	name = models.CharField(max_length=255)
	start = models.IntegerField()
	end = models.IntegerField()
	value = models.CharField(max_length=4096)

class Element(models.Model):
	"""
	Element-objects are all the non-text elements in a Blip.
	An element has no physical presence in the text of a Blip.
	Only special Wave Client elements are treated here. Arbitrary HTML
	is placed directly in the text stream.
	
	"""
	ELEMENT_TYPES = (
		(0, "NOTHING"),
		
		(1, "INLINE_BLIP"),
		
		(2, "GADGET"),
		
		(3, "INPUT"),
		(4, "CHECK"),
		(5, "LABEL"),
		(6, "BUTTON"),
		(7, "RADIO_BUTTON"),
		(8, "RADIO_BUTTON_GROUP"),
		(9, "PASSWORD"),
		
		(10, "IMAGE"),
	)
	
	blip = models.ForeignKey(Blip, related_name="elements")
	position = models.IntegerField()
	type = models.IntegerField(choices=ELEMENT_TYPES)
	properties = models.TextField() # JSON is used here

# Now some fancy subclasses

class InlineBlip(Element):
	"""
	An inline blip element.
	
	"""
	
	blip = models.ForeignKey(Blip)

class GadgetElement(Element):
	"""
	A gadget element.
	
	"""
	
	url = models.URLField(verify_exists=False, max_length=1024)
	fields = models.TextField() # JSON is used here

class FormElement(Element):
	"""
	A form element.
	
	"""
	
	label = models.CharField(max_length=255)
	name = models.CharField(max_length=255)
	value = models.CharField(max_length=255)
	default_value = models.CharField(max_length=255)

class Image(Element):
	"""
	An image element.
	
	"""
	
	attachment_id = models.CharField(max_length=255)
	caption = models.CharField(max_length=255)
	url = models.URLField(verify_exists=False)
	height = models.IntegerField()
	width = models.IntegerField()

class Delta(models.Model):
	"""
	A Delta object is a collection of (reversible) operations that can be
	applied to a Wavlet. This includes operations on Blips, i.e. its text or
	elements.
	
	The operations placed in the Delta objects are those which result from
	OT funcitions used in the server.
	
	General workflow: Operations sent from a client generate a Delta -> the
	Delta is applied internally to the Wave object model -> the Delta is then
	convdeted into Events -> the Events are sent to the other clients.
	
	"""
	
	timestamp = models.DateTimeField(auto_now_add=True)
	wavelet = models.ForeignKey(Wavelet, related_name="diffs")
	
	operations = models.TextField() # JSON again

class Gadget(models.Model):
	"""
	A gadget that has been uploaded or is referenced on this server.
	Users can register their gadgets on this server to conveniently add them
	to a Wave.
	
	Note: A uploaded gadget is never deleted as it may be used by other waves.
	
	"""
	
	by_user = models.ForeignKey(User, related_name="my_gadgets")
	title = models.CharField(max_length=255, unique=True, verbose_name=_(u'Title'))
	description = models.CharField(max_length=255, blank=True, verbose_name=_(u'Description'))
	url = models.URLField(verbose_name=_(u'URL'), blank=True)
	hosted_filename = models.CharField(max_length=255, blank=True)
	
	def is_hosted(self):
		return len(self.hosted_filename) > 0
	