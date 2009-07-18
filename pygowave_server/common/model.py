
"""
Models module.
@module pygowave.model
"""

__license__ = """
PyGoWave Client Script a.k.a. Microwave
Copyright (C) 2009 by p2k

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

from pycow.decorators import Class, Implements
from pycow.utils import Events, Options, Hash

__all__ = ["WaveModel", "Participant"]

@Implements(Options, Events)
@Class
class Participant(object):
	"""
	Models a participant to a Wavelet. Note that the implementation (i.e.
	the controller) should only create one participant object per participant
	because view objects are connected to only one participant instance.
	If the state of a participant changes they can be updated as appropriate.
	@class {public} pygowave.model.Participant
	"""
	
	options = {
		"displayName": "",
		"thumbnailUrl": "",
		"profileUrl": "",
		"isOnline": False,
		"isBot": False,
	}
	
	# --- Event documentation ---
	"""
	Fired if the participant's online state changes.
	@event onOnlineStateChanged
	@param {Boolean} online True, if the participant is now online.
	"""
	# ---------------------------
	
	def __init__(self, id, options):
		"""
		Called on instantiation.
		@constructor {public} initialize
		@param {String} id ID (address) of the participant
		@param {Object} options Additional information:
		@... {String} displayName Display name
		@... {String} thumbnailUrl URL of the participant's avatar
		@... {String} profileUrl A link to the participant's profile
		@... {Boolean} isOnline True, if the participant is online
		@... {Boolean} isBot True, if the participant is a robot
		"""
		self.setOptions(options)
		self._id = id
		if self.options["name"] == "":
			self.options["name"] = id
	
	def id(self):
		"""
		Returns the ID (address) of the participant.
		@function {public String} id
		"""
		return self._id
	
	def displayName(self):
		"""
		Returns the participant's display name.
		@function {public String} displayName
		"""
		return self.options["displayName"]
	
	def thumbnailUrl(self):
		"""
		Returns the URL of the participant's avatar.
		@function {public String} thumbnailUrl
		"""
		return self.options["thumbnailUrl"]
	
	def profileUrl(self):
		"""
		Returns the URL to the participant's profile.
		@function {public String} profileUrl
		"""
		return self.options["profileUrl"]
	
	def isBot(self):
		"""
		Returns weather the participant is a bot.
		@function {public Boolean} isBot
		"""
		return self.options["isBot"]
	
	def isOnline(self):
		"""
		Returns weather the participant is a online.
		@function {public Boolean} isOnline
		"""
		return self.options["isOnline"]
	
	def setOnline(self, online):
		"""
		Set the participant's online state.<br/>
		Fires onOnlineStateChanged if changed.
		@function {public} setOnline
		@param {Boolean} online True, if the participant is now online.
		"""
		if self.options["isOnline"] != online:
			self.options["isOnline"] = online
			self.fireEvent('onlineStateChanged', online)
	
	def toGadgetFormat(self):
		"""
		Convenience function to serialize a participant object into the
		Wave Gadget API format.
		@function {public Object} toGadgetFormat
		"""
		return {
			"id": self._id,
			"displayName": self.options["displayName"],
			"thumbnailUrl": self.options["thumbnailUrl"]
		}

@Implements(Options)
@Class
class Wavelet(object):
	"""
	Models a Wavelet on a Wave.<br/>
	This is a private class and cannot be instanitated directly. Please
	use {@link pygowave.model.WaveModel.createWavelet WaveModel.createWavelet}.
	@class {private} pygowave.model.Wavelet
	"""

	options = {
		"creator": None,
		"is_root": False,
		"root_blip": None,
		"created": None,
		"last_modified": None,
		"title": "",
		"version": 0
	}
	
	def __init__(self, wave, id, options):
		"""
		Called on instantiation. Documented for internal purposes.
		@constructor {public} initialize
		@param {WaveModel} wave Parent WaveModel object
		@param {String} id Wavelet ID
		@param {Object} options Information about the Wavelet. Possible values:
		@... {Participant} creator Creator of this Wavelet
		@... {Boolean} is_root True if this is the root Wavelet; if this value
		     is set and the parent WaveModel does not have a root Wavelet yet,
		     this Wavelet is set as the Wave's root Wavelet
		@... {Date} created Date of creation
		@... {Date} last_modified Date of last modification
		@... {String} title Title of the Wavelet
		@... {int} version Version of the Wavelet
		"""
		self.setOptions(options)
		self._wave = wave
		if self.options["is_root"]:
			if self._wave.rootWavelet() == None:
				self._wave._setRootWavelet(self)
			else:
				self.options["is_root"] = False
		self._id = id
		self._participants = Hash()
		self._blips = []
	
	def isRoot(self):
		"""
		Returns true, if this Wavelet is the Wave's root Wavelet.
		@function {public Boolean} isRoot
		"""
		return self.options["is_root"]
	
	def id(self):
		"""
		Returns the ID of this Wavelet.
		@function {public String} id
		"""
		return self._id
	
	def addParticipant(self, participant):
		"""
		Add a participant to this Wavelet.<br/>
		Note: Triggers {@link pygowave.model.WaveModel.onParticipantsChanged onParticipantsChanged}
		of the WaveModel.
		@function {public} addParticipant
		@param {Participant} participant Participant to be added
		"""
		self._participants.set(participant.id(), participant)
		self._wave.fireEvent('participantsChanged', self._id)
	
	def participant(self, id):
		"""
		Returns the Participant object with the given id, if the participant
		resides on this Wavelet. Returns null otherwise.
		@function {Participant} participant
		@param {String} id ID of the 
		"""
		return self._participants.get(id)
	
	def allParticipants(self):
		"""
		Returns a list of all participants on this Wavelet.
		@function {public Participant[]} allParticipants
		"""
		return self._participants.getValues()
	
	def allParticipantIDs(self):
		"""
		Returns a list of all IDs of the participants on this Wavelet.
		@function {public String[]} allParticipantIDs
		"""
		return self._participants.getKeys()

	def allParticipantsForGadget(self):
		"""
		Convenience function to serialize all participants into the Wave
		Gadget API format.
		@function {public Object} allParticipantsForGadget
		"""
		ret = {}
		for id, participant in self._participants.iteritems():
			ret[id] = participant.toGadgetFormat()
		return ret

@Implements(Events)
@Class
class WaveModel(object):
	"""
	Wave model class. Top level model which sends all events.
	
	@class {public} pygowave.model.WaveModel
	"""
	
	# --- Event documentation ---
	"""
	Fired if a participant joins or leaves.
	@event onParticipantsChanged
	@param {String} waveletId ID of the Wavelet whose participants changed
	"""
	
	"""
	Fired if a Wavelet has been added.
	@event onWaveletAdded
	@param {String} waveletId ID of the Wavelet that has been added
	@param {Boolean} isRoot True if this is the (new) root Wavelet
	"""
	# ---------------------------
	
	def __init__(self, waveId, viewerId):
		"""
		Called on instantiation.
		@constructor {public} initialize
		@param {String} waveId ID of the Wave
		@param {String} viewerId ID of the viewer
		"""
		self._rootWavelet = None
		self._waveId = waveId
		self._viewerId = viewerId
		self._wavelets = Hash()

	def createWavelet(self, id, options):
		"""
		Create a Wavelet and add it to this Wave. For options see the
		{@link pygowave.model.Wavelet.initialize Wavelet constructor}.<br/>
		Hint: Emits {@link pygowave.model.WaveModel.onWaveletAdded onWaveletAdded}
		@function {public Wavelet} createWavelet
		@param {String} id Wavelet ID
		@param {Object} options Information about the Wavelet.
		"""
		w = Wavelet(self, id, options)
		self._wavelets.set(id, w)
		self.fireEvent('waveletAdded', [id, w.isRoot()])
		return w
	
	def wavelet(self, waveletId):
		"""
		Return a Wavelet of this Wave by its ID.
		@function {public Wavelet} wavelet
		@param {String} waveletId ID of the Wavelet
		"""
		return self._wavelets.get(waveletId)
	
	def rootWavelet(self):
		"""
		Returns the root Wavelet of this Wave
		@function {public Wavelet} rootWavelet
		"""
		return self._rootWavelet
	
	def _setRootWavelet(self, wavelet):
		"""
		Internal method to set the root Wavelet. Not intended to be called
		outside of this implementation.
		@function {private} _setRootWavelet
		@param {Wavelet} wavelet Wavelet to be set as root Wavelet
		"""
		self._rootWavelet = wavelet
