
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
from django.db import transaction
from django.core.exceptions import ObjectDoesNotExist
from django.utils.hashcompat import sha_constructor as sha1

from django.utils import simplejson

from pygowave_server.utils import find_random_id, gen_random_id, datetime2milliseconds
from pygowave_server.common.operations import OpManager, DOCUMENT_DELETE, DOCUMENT_INSERT, \
	DOCUMENT_ELEMENT_INSERT, DOCUMENT_ELEMENT_DELETE, DOCUMENT_ELEMENT_DELTA, DOCUMENT_ELEMENT_SETPREF

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
	user = models.ForeignKey(User, blank=True, null=True, unique=True, related_name="participants")
	is_bot = models.BooleanField()
	last_contact = models.DateTimeField()
	
	name = models.CharField(max_length=255, blank=True)
	avatar = models.URLField(verify_exists=False, blank=True)
	profile = models.URLField(verify_exists=False, blank=True)
	
	def create_new_connection(self):
		"""
		Creates a new connection object for this participant and returns it.
		This generates the participant's new access keys.
		
		"""
		new_conn = ParticipantConn(participant=self)
		new_conn.save()
		return new_conn
	
	def serialize(self):
		"""
		Serialize participant into Google's format (taken from Gadgets API).
		
		"""
		return {
			"id": self.id,
			"displayName": self.name,
			"thumbnailUrl": self.avatar,
			"profileUrl": self.profile,
			"isBot": self.is_bot
		}
	
	def __unicode__(self):
		return u"Participant '%s'" % (self.id)

class ParticipantConn(models.Model):
	"""
	Represents a particular connection from the wave server to the wave client.
	Holds wave access keys. These access keys are only valid for one connection.
	However, they are valid for multiple open wavelets on that connection.
	
	"""
	
	participant = models.ForeignKey(Participant, related_name="connections")
	created = models.DateTimeField(auto_now_add=True)
	rx_key = models.CharField(max_length=42)
	tx_key = models.CharField(max_length=42)
	
	def save(self, force_insert=False, force_update=False):
		if not self.id:
			self.rx_key, self.tx_key = self.find_random_keys()
			super(ParticipantConn, self).save(True)
		else:
			super(ParticipantConn, self).save(force_insert, force_update)
	
	@classmethod
	def find_random_keys(cls):
		rx_key = gen_random_id(10)
		while cls.objects.filter(rx_key=rx_key).count() > 0:
			rx_key = gen_random_id(10)
		tx_key = gen_random_id(10)
		while cls.objects.filter(tx_key=tx_key).count() > 0:
			tx_key = gen_random_id(10)
		return rx_key, tx_key
	
	def __unicode__(self):
		return u"ParticipantConn '%s/%d'" % (self.participant.id, self.id)
	
	class Meta:
		verbose_name = _('participant connection')
		verbose_name_plural = _('participant connections')

class WaveManager(models.Manager):
	
	@transaction.commit_on_success
	def create_and_init_new_wave(self, creator, title):
		wave = self.create()
		
		wavelet = Wavelet(wave=wave, creator=creator, title=title, is_root=True)
		wavelet.save()
		wavelet.participants.add(creator)
		
		blip = Blip(wavelet=wavelet, creator=creator)
		blip.save()
		wavelet.root_blip = blip
		wavelet.save()
		return wave

class Wave(models.Model):
	"""
	Models wave instances. These are the core of Google Wave.
	A single wave is composed of its id and any wavelets that belong to it.
	
	"""
	
	objects = WaveManager()
	
	id = models.CharField(max_length=42, primary_key=True) # Don't panic :P
	
	def root_wavelet(self):
		return self.wavelets.get(is_root=True)
	
	def save(self, force_insert=False, force_update=False):
		if not self.id:
			self.id = find_random_id(Wave.objects, 10)
			super(Wave, self).save(True)
		else:
			super(Wave, self).save(force_insert, force_update)
	
	def __unicode__(self):
		return u"Wave %s" % (self.id)

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
	is_root = models.BooleanField()
	root_blip = models.OneToOneField("Blip", related_name="rootblip_wavelet", null=True)
	created = models.DateTimeField(auto_now_add=True)
	last_modified = models.DateTimeField(auto_now=True)
	title = models.CharField(max_length=255, blank=True)
	version = models.IntegerField(default=0)
	participants = models.ManyToManyField(Participant, related_name="wavelets")
	participant_conns = models.ManyToManyField(ParticipantConn, related_name="wavelets", verbose_name=_(u'connections'))
	
	def blipById(self, id):
		"""
		Returns the Blip object with the given id, if the Blip resides on this
		Wavelet. Returns None otherwise.
		
		"""
		
		try:
			return self.blips.get(pk=id)
		except ObjectDoesNotExist:
			return None
	
	def save(self, force_insert=False, force_update=False):
		if not self.id:
			if self.is_root:
				self.id = self.wave.id + ROOT_WAVELET_ID_SUFFIX
			else:
				self.id = find_random_id(Wavelet.objects, 10, prefix=self.wave.id+"!")
			super(Wavelet, self).save(True)
		else:
			super(Wavelet, self).save(force_insert, force_update)
	
	def __unicode__(self):
		return u"Wavelet '%s' (%s)" % (self.title, self.id)
	
	def serialize(self):
		"""
		Serialize the wavelet into a format that is compatible with robots and
		the client.
		
		"""
		return {
			"rootBlipId": getattr(self.root_blip, "id", None),
			"title": self.title,
			"creator": self.creator.id,
			"creationTime": datetime2milliseconds(self.created),
			"dataDocuments": None, #TODO (is not declared in the robot protocol example)
			"waveletId": self.id,
			"participants": map(lambda p: p.id, self.participants.all()),
			"version": self.version,
			"lastModifiedTime": datetime2milliseconds(self.last_modified),
			"waveId": self.wave.id,
		}
	
	def serialize_blips(self):
		"""
		Serialize the wavelet's blips into a format that is compatible with
		robots and the client.
		
		"""
		blipmap = {}
		for blip in self.blips.all():
			blipmap[blip.id] = blip.serialize()
		return blipmap
	
	def applyOperations(self, ops):
		"""
		Apply the operations on the wavelet.
		
		"""
		
		for op in ops:
			if op.blipId != "":
				blip = self.blipById(op.blipId)
				if op.type == DOCUMENT_DELETE:
					blip.deleteText(op.index, op.property)
				elif op.type == DOCUMENT_INSERT:
					blip.insertText(op.index, op.property)
				elif op.type == DOCUMENT_ELEMENT_DELETE:
					blip.deleteElement(op.index)
				elif op.type == DOCUMENT_ELEMENT_INSERT:
					blip.insertElement(op.index, op.property["type"], op.property["properties"])
				elif op.type == DOCUMENT_ELEMENT_DELTA:
					try:
						blip.applyElementDelta(op.index, op.property)
					except:
						pass #TODO: error handling
				elif op.type == DOCUMENT_ELEMENT_SETPREF:
					try:
						blip.setElementUserpref(op.index, op.property["key"], op.property["value"])
					except:
						pass #TODO: error handling
				blip.save()
	
	def blipsums(self):
		"""
		Calculates the checksums of all Blips.
		
		"""
		blipsums = {}
		for blip in self.blips.all():
			blipsums[blip.id] = blip.checksum()
		return blipsums
	
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
	parent = models.ForeignKey("self", related_name="children", null=True, blank=True)
	creator = models.ForeignKey(Participant, related_name="created_blips")
	version = models.IntegerField(default=0)
	last_modified = models.DateTimeField(auto_now=True)
	submitted = models.BooleanField()
	
	contributors = models.ManyToManyField(Participant, related_name="contributed_blips", blank=True)
	
	text = models.TextField(blank=True)
	
	@transaction.commit_on_success
	def insertText(self, index, text):
		"""
		Insert a text at the specified index. This moves annotations and
		elements as appropriate.
		
		"""
		
		self.text = self.text[:index] + text + self.text[index:]
		length = len(text)
		
		for anno in self.annotations.filter(start__gte=index):
			anno.start += length
			anno.end += length
			anno.save()
		
		for elt in self.elements.filter(position__gte=index):
			elt.position += length
			elt.save()
	
	@transaction.commit_on_success
	def deleteText(self, index, length):
		"""
		Delete text at the specified index. This moves annotations and
		elements as appropriate.
		
		"""
		
		self.text = self.text[:index] + self.text[index+length:]
		
		for anno in self.annotations.filter(start__gte=index):
			anno.start -= length
			anno.end -= length
			anno.save()
		
		for elt in self.elements.filter(position__gte=index):
			elt.position -= length
			elt.save()
	
	@transaction.commit_on_success
	def insertElement(self, index, type, properties):
		"""
		Insert an element at the specified index. This implicitly adds a
		protected newline character at the index.
		
		"""
		
		self.insertText(index, "\n")
		if type == 2:
			elt = GadgetElement(blip=self, position=index)
		else:
			elt = Element(blip=self, position=index, type=type)
		elt.set_data(properties)
		elt.save()
	
	@transaction.commit_on_success
	def deleteElement(self, index):
		"""
		Delete an element at the specified index. This implicitly deletes the
		protected newline character at the index.
		
		"""
		
		elt = self.elements.get(position=index)
		if elt.type == 2:
			elt = elt.to_gadget()
		elt.delete()
		self.deleteText(index, 1)
	
	@transaction.commit_on_success
	def applyElementDelta(self, index, delta):
		"""
		Apply an element delta. Currently only for gadget elements.
		
		"""
		
		elt = self.elements.get(position=index)
		if elt.type != 2:
			raise TypeError("Element #%d is not a Gadget Element" % (id))
		elt.to_gadget().apply_delta(delta)
	
	@transaction.commit_on_success
	def setElementUserpref(self, index, key, value):
		"""
		Set an UserPref of an element. Currently only for gadget elements.
		
		"""
		
		elt = self.elements.get(position=index)
		if elt.type != 2:
			raise TypeError("Element #%d is not a Gadget Element" % (id))
		elt.to_gadget().set_userpref(key, value)
	
	def save(self, force_insert=False, force_update=False):
		if not self.id:
			self.id = find_random_id(Blip.objects, 10)
			super(Blip, self).save(True)
		else:
			super(Blip, self).save(force_insert, force_update)
	
	def serialize(self):
		"""
		Serialize the blip into a format that is compatible with robots and the
		client.
		
		"""
		return {
			"blipId": self.id,
			"content": self.text,
			"elements": map(lambda e: e.serialize(), self.elements.all()),
			"contributors": map(lambda p: p.id, self.contributors.all()),
			"creator": self.creator.id,
			"parentBlipId": getattr(self.parent, "id", None),
			"annotations": map(lambda a: a.serialize(), self.annotations.all()),
			"waveletId": self.wavelet.id,
			"version": self.version,
			"lastModifiedTime": datetime2milliseconds(self.last_modified),
			"childBlipIds": map(lambda c: c.id, self.children.all()),
			"waveId": self.wavelet.wave.id,
			"submitted": bool(self.submitted),
			"checksum": self.checksum() # Note: This is tentative and subject to change
		}
	
	def checksum(self):
		"""
		Calculate a checksum of this Blip.
		Note: Currently this is only the SHA-1 of the Blip's text. This is
		tentative and subject to change
		
		"""
		return sha1(self.text.encode("utf-8")).hexdigest()

	def __unicode__(self):
		return u"Blip %s on %s" % (self.id, unicode(self.wavelet))

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
	
	def serialize(self):
		"""
		Serialize the annotation into a format that is compatible with robots
		and the client.
		
		"""
		return {
			"range": {
				"start": self.start,
				"end": self.end,
			},
			"name": self.name,
			"value": self.value
		}

class Element(models.Model):
	"""
	Element-objects are all the non-text elements in a Blip.
	An element has no physical presence in the text of a Blip, but it maintains
	an implicit protected newline character to keep positions distinct.
	
	Only special Wave Client elements are treated here.
	There are no HTML elements in any Blip. All markup is handled by Annotations.
	
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
	
	def get_data(self):
		"""
		Return data as python map (JSON decoded).
		
		"""
		if self.properties == "":
			return {}
		else:
			return simplejson.loads(self.properties)
	
	def set_data(self, data):
		"""
		Set data by a python map (encoding to JSON).
		
		"""
		self.properties = simplejson.dumps(data)
	
	def to_gadget(self):
		"""
		Returns a GadgetElement for this Element if possible.
		
		"""
		return GadgetElement.objects.get(pk=self.id)
	
	def serialize(self):
		"""
		Serialize the annotation into a format that is compatible with robots
		and the client.
		
		"""
		return {
			"id": self.id,
			"index": self.position,
			"type": self.type,
			"properties": self.get_data(),
		}
	
	def type_name(self):
		"""
		Return the type of this element as string.
		
		"""
		return self.conv_type_name(self.type)
	
	@classmethod
	def conv_type_name(cls, typeId):
		"""
		Convert a type ID to the corresponding string.
		
		"""
		for id, name in cls.ELEMENT_TYPES:
			if id == typeId:
				return name
		return "NOTHING"

# Now some fancy subclasses

class InlineBlip(Element):
	"""
	An inline blip element.
	
	"""
	
	i_blip = models.ForeignKey(Blip)

class GadgetElement(Element):
	"""
	A gadget element. Note that these are individual instances unlike Gadget
	objects which act like classes for GadgetElements.
	
	"""
	
	@property
	def url(self):
		d = self.get_data()
		if d.has_key("url"):
			return d["url"]
		else:
			return ""
	
	@url.setter
	def url(self, value):
		d = self.get_data()
		d["url"] = value
		self.set_data(d)
	
	def apply_delta(self, delta, save=True):
		"""
		Apply a delta map to the fields.
		Also saves the object.
		
		"""
		d = self.get_data()
		if not d.has_key("fields"):
			d["fields"] = {}
		fields = d["fields"]
		
		fields.update(delta)
		
		# Check for null-value keys and delete them
		for key in delta.iterkeys():
			if delta[key] == None:
				del fields[key]
		
		self.set_data(d)
		
		if save:
			self.save()
	
	def set_userpref(self, key, value, save=True):
		"""
		Set a UserPref value.
		Also saves the object.
		
		"""
		d = self.get_data()
		if not d.has_key("userprefs"):
			d["userprefs"] = {}
		prefs = d["userprefs"]
		
		prefs[key] = value
		
		self.set_data(d)
		
		if save:
			self.save()
	
	def set_userprefs(self, data):
		"""
		Set userprefs (name:value) by a python map (encoding to JSON).
		
		"""
		d = self.get_data()
		d["userprefs"] = data
		self.set_data(d)
	
	def get_userprefs(self):
		"""
		Return userprefs (name:value) as python map (JSON decoded).
		
		"""
		d = self.get_data()
		if not d.has_key("userprefs"):
			return {}
		return d["userprefs"]

	def save(self, force_insert=False, force_update=False):
		self.type = 2
		super(GadgetElement, self).save(force_insert, force_update)
		
	def __unicode__(self):
		if self.url.rindex("/") != -1:
			desc = "...%s" % (self.url[self.url.rindex("/"):])
		else:
			desc = "?"
		return u"GadgetElement #%s (%s)" % (self.id, desc)

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
	
	"""
	
	timestamp = models.DateTimeField(auto_now_add=True)
	version = models.IntegerField()
	wavelet = models.ForeignKey(Wavelet, related_name="deltas")
	
	operations = models.TextField() # JSON again
	
	@classmethod
	def createByOpManager(cls, opman, version):
		"""
		Set this delta's values based on the given OpManager and a version.
		
		"""
		newobj = cls(
			version=version,
			wavelet= Wavelet.objects.get(pk=opman.waveletId),
			operations=simplejson.dumps(opman.serialize())
		)
		newobj._OpManager = opman
		
		return newobj
	
	def getOpManager(self):
		"""
		Retrieve an OpManager based on this delta.
		
		"""
		opman = getattr(self, "_OpManager", None)
		if opman == None:
			opman = OpManager(self.wavelet.wave.id, self.wavelet.id)
			opman.unserialize(simplejson.loads(self.operations))
			self._OpManager = opman
		
		return opman
	
	def __unicode__(self):
		return u"Delta #%d v%d@%s" % (self.id, self.version, self.wavelet.id)

class Gadget(models.Model):
	"""
	A gadget that has been uploaded or is referenced on this server.
	Users can register their gadgets on this server to conveniently add them
	to a Wave.
	
	Note: A uploaded gadget is never deleted from disk as it may be used by
	other waves. An exception to this are Gadget marked as "devel" versions.
	Those Gadgets can be simply overwritten and thus replace already running
	instances.
	
	"""
	
	by_user = models.ForeignKey(User, related_name="my_gadgets")
	title = models.CharField(max_length=255, unique=True, verbose_name=_(u'Title'))
	description = models.CharField(max_length=255, blank=True, verbose_name=_(u'Description'))
	url = models.URLField(blank=True, verbose_name=_(u'URL'))
	hosted_filename = models.CharField(max_length=255, blank=True, verbose_name=_(u'Hosted filename'))
	devel = models.BooleanField(default=False, verbose_name=_(u'Development version'))
	
	def is_hosted(self):
		return len(self.hosted_filename) > 0
		
	def instantiate(self):
		"""
		Create a GadgetElement from this Gadget.
		
		"""
		return GadgetElement(url=self.url)
	
	def __unicode__(self):
		return u"Gadget '%s' by '%s'" % (self.title, self.by_user.username)
	