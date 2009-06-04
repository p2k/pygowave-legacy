
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

from django.db import models

__author__ = "patrick.p2k.schneider@gmail.com"

ROOT_WAVELET_ID_SUFFIX = '!conv+root'

class Participant(models.Model):
	"""
	Represents participant information. It contains id, display name, avatar's
	URL, and an external URL to view the participant's profile page. The
	instances may be coming from external Wave servers.
	
	"""
	
	id = models.CharField(max_length=255, primary_key=True)
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

class Document(models.Model):
	"""
	A Document is the contents of a Blip. There's only the content field here,
	as all the other things are defined in other models with a ForeignKey-field
	pointing back to the Document.
	In fact, the Document could be merged into the Blip, but they say there
	are Documents without Blips...
	
	Note: The text defines the content of the wave as well as "hook-in" points
	for the elements. Elements are placed within the text by there position.
	
	"""
	
	content = models.TextField() # All the text of the wave
	
class Blip(models.Model):
	"""
	A Blip is a unit of conversation in a Wave. It is a node in a tree of
	other nodes and may have a parent and children. It contains metadata to
	keep track of contributors and versioning.
	
	"""
	
	id = models.CharField(max_length=42, primary_key=True)
	wavelet = models.ForeignKey(Wavelet, related_name="blips")
	parent = models.ForeignKey("self", related_name="children", null=True)
	creator = models.ForeignKey(Participant, related_name="created_blips")
	version = models.IntegerField(default=0)
	last_modified = models.DateTimeField(auto_now=True)
	submitted = models.BooleanField()
	
	contributors = models.ManyToManyField(Participant, related_name="contributed_blips")
	
	document = models.OneToOneField(Document)

class Annotation(models.Model):
	"""
	An annotation is metadata that augments a range of text in a Document.
	Example uses of annotations include styling text, supplying spelling
	corrections, and links to refer that area of text to another document or
	web site. The size of an annotation range must be positive and non-zero.
	
	"""
	
	document = models.ForeignKey(Document, related_name="annotations")
	name = models.CharField(max_length=255)
	start = models.IntegerField()
	end = models.IntegerField()
	value = models.CharField(max_length=4096)

class Element(models.Model):
	"""
	Element-objects are all the non-text things in a Document.
	
	"""
	TYPES = (
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
	
	document = models.ForeignKey(Document, related_name="elements")
	position = models.IntegerField()
	type = models.IntegerField(choices=TYPES)
	properties = models.TextField() # JSON is used here

# Now some fancy subclasses

class InlineBlip(Element):
	"""
	An inline blip element.
	"""
	
	blip = models.ForeignKey(Blip)

class Gadget(Element):
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
	