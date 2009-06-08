
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

from django import forms
from django.forms import widgets

from django.conf import settings
from django.utils.translation import ugettext_lazy as _
from django.utils.safestring import mark_safe
from django.core.files.uploadedfile import UploadedFile
from django.utils.hashcompat import sha_constructor

import os
from random import random
from lxml.etree import XMLSyntaxError

from pygowave_server.utils import AlreadyUploadedFile, get_profile_model
from pygowave_server.models import Gadget
from pygowave_server.engine import GadgetLoader

class AvatarWidget(widgets.FileInput):
	def __init__(self, size=(32, 32), prefix="", attrs=None):
		super(AvatarWidget, self).__init__(attrs)
		self.size = tuple(size)
		self.hidden_input = widgets.HiddenInput()
		self.prefix = prefix
	
	def value_from_datadict(self, data, files, name):
		v = files.get(name, None)
		if v == None: # Nothing uploaded, look for already uploaded file
			oldfile = self.prefix + data.get(name + "_old", None)
			path = settings.AVATAR_ROOT + oldfile
			if os.path.exists(path):
				return AlreadyUploadedFile(path, oldfile, os.path.getsize(path))
		return v
	
	def render(self, name, value, attrs=None):
		if value == "" or value == None:
			value = "default.png"
		elif isinstance(value, UploadedFile):
			value = self.prefix + value.name
		imgattrs = self.build_attrs(
			src=settings.AVATAR_URL + value,
			alt=_(u'(Avatar)'),
			width=self.size[0],
			height=self.size[1]
		)
		return mark_safe(u'<img%s /> ' % forms.util.flatatt(imgattrs)) \
			+ self.hidden_input.render(name + "_old", value) \
			+ super(AvatarWidget, self).render(name, None, attrs=attrs)

class AvatarField(forms.ImageField):
	widget = AvatarWidget
	
	def clean(self, value, initial):
		value = super(AvatarField, self).clean(value, initial)
		if value == None:
			return initial
		
		if not isinstance(value, AlreadyUploadedFile):
			# Add prefix to avoid overwriting of files with the same name
			prefix = sha_constructor(str(random())).hexdigest()[:4] + "_"
			filename = settings.AVATAR_ROOT + prefix + value.name
			destination = open(filename, 'wb')
			for chunk in value.chunks():
				destination.write(chunk)
			destination.close()
			
			if isinstance(self.widget, AvatarWidget):
				# Resize
				from PIL import Image
				img = Image.open(filename)
				size = self.widget.size
				if img.size[0] < size[0] or img.size[1] < size[1]:
					img.resize(self.widget.size, Image.CUBIC).save(filename)
				elif img.size != size:
					img.resize(self.widget.size, Image.ANTIALIAS).save(filename)
				
				# Set prefix
				self.widget.prefix = prefix
			
			return prefix + value.name
		
		return value.name

class ParticipantProfileForm(forms.ModelForm):
	avatar = AvatarField(required=False)
	profile = forms.CharField(max_length=200, required=False, label=_(u'Profile URL'))
	
	class Meta:
		model = get_profile_model()
		fields = ('avatar', 'profile')

class GadgetRegistryForm(forms.ModelForm):
	description = forms.CharField(max_length=255, required=False, widget=widgets.Textarea(attrs={"cols": 45, "rows": 4}))
	external = forms.BooleanField(widget=widgets.HiddenInput, initial=0)
	upload = forms.FileField(required=False)
	title = forms.CharField(required=False)
	
	def clean_upload(self):
		if self.cleaned_data["upload"] == None:
			return ""
		
		value = self.cleaned_data["upload"]
		
		# Add prefix to avoid overwriting of files with the same name
		prefix = sha_constructor(str(random())).hexdigest()[:4] + "_"
		filename = settings.GADGET_ROOT + prefix + value.name
		# Write file
		destination = open(filename, 'wb')
		for chunk in value.chunks():
			destination.write(chunk)
		destination.close()
		
		# Returns generated filename
		return prefix + value.name
	
	def external_url(self):
		return self.cleaned_data["external"] == "1"

	def clean(self):
		if len(self.errors) > 0:
			return self.cleaned_data
		
		if self.external_url():
			if self.cleaned_data["url"] == "":
				raise forms.ValidationError(_(u'You must enter an URL or choose "Upload" and upload a file.'))
			if self.cleaned_data["upload"] != "":
				# Discard uploaded file
				os.remove(settings.GADGET_ROOT + self.cleaned_data["upload"])
				self.cleaned_data["upload"] = ""
			
			# Pass URL
			url = self.cleaned_data["url"]
		else:
			if self.cleaned_data["upload"] == "":
				raise forms.ValidationError(_(u'You must upload a file or choose "External URL" and enter an URL.'))
			
			# Set URL by uploaded file
			self.cleaned_data["url"] = settings.GADGET_URL + self.cleaned_data["upload"]
			
			# Local path
			url = "file://" + settings.GADGET_ROOT + self.cleaned_data["upload"]
		
		# Check Gadget
		try:
			gadget = GadgetLoader(url)
		except urllib2.HTTPError:
			raise forms.ValidationError(_(u'Gadget could not be downloaded.'))
		except XMLSyntaxError:
			raise forms.ValidationError(_(u'Gadget quick-check failed: Bad XML format.'))
		except ValueError as e:
			raise forms.ValidationError(_(u'Gadget quick-check failed: %s.') % (e.args[0]))
		
		# Get title
		self.cleaned_data["title"] = gadget.title
		
		return self.cleaned_data
	
	class Meta:
		model = Gadget
		exclude = ('by_user')
