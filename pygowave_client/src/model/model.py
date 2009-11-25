
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

from hashlib import sha1 as sha_constructor

__all__ = ["WaveModel", "Participant", "ELEMENT_TYPE"]

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
	"""
	Fired if the participant's data changes.
	@event onDataChanged
	"""
	# ---------------------------
	
	def __init__(self, id, options = None):
		"""
		Called on instantiation.
		@constructor {public} initialize
		@param {String} id ID (address) of the participant
		@param {optional Object} options Additional information:
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
	
	def setDisplayName(self, value):
		"""
		Sets the participant's display name.
		@function {public} setDisplayName
		@param {String} value The new name
		"""
		if self.options["displayName"] != value:
			self.options["displayName"] = value
			self.fireEvent('dataChanged')
	
	def thumbnailUrl(self):
		"""
		Returns the URL of the participant's avatar.
		@function {public String} thumbnailUrl
		"""
		return self.options["thumbnailUrl"]
	
	def setThumbnailUrl(self, value):
		"""
		Sets the URL of the participant's avatar.
		@function {public} setThumbnailUrl
		@param {String} value The new URL
		"""
		if self.options["thumbnailUrl"] != value:
			self.options["thumbnailUrl"] = value
			self.fireEvent('dataChanged')

	def profileUrl(self):
		"""
		Returns the URL to the participant's profile.
		@function {public String} profileUrl
		"""
		return self.options["profileUrl"]
	
	def setProfileUrl(self, value):
		"""
		Sets the URL to the participant's profile.
		@function {public} setProfileUrl
		@param {String} value The new URL
		"""
		if self.options["profileUrl"] != value:
			self.options["profileUrl"] = value
			self.fireEvent('dataChanged')

	def isBot(self):
		"""
		Returns weather the participant is a bot.
		@function {public Boolean} isBot
		"""
		return self.options["isBot"]
	
	def setBot(self, value):
		"""
		Sets weather the participant is a bot.
		@function {public} setBot
		@param {String} value True, if this participant is a bot
		"""
		if self.options["isBot"] != value:
			self.options["isBot"] = value
			self.fireEvent('dataChanged')

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
	
	def updateData(self, obj):
		"""
		Updates participant data from a JSON-serialized map/dict.<br/>
		Fires onDataChanged.
		
		@function {public} updateData
		@param {Object} obj JSON-serialized participant data
		"""
		self.options["displayName"] = obj["displayName"]
		self.options["thumbnailUrl"] = obj["thumbnailUrl"]
		self.options["profileUrl"] = obj["profileUrl"]
		self.options["isBot"] = obj["isBot"]
		self.fireEvent('dataChanged')
	
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

@Class
class Annotation(object):
	"""
	An annotation is metadata that augments a range of text in a Blip's text.
	Example uses of annotations include styling text, supplying spelling
	corrections, and links to refer that area of text to another Blip or
	web site. The size of an annotation range must be positive and non-zero.
	
	@class {private} pygowave.model.Annotation
	"""
	
	def __init__(self, blip, name, start, end, value):
		"""
		Called on instantiation. Documented for internal purposes.
		@constructor {private} initialize
		@param {Blip} blip The annotation's parent Blip
		@param {String} name The annotation's name
		@param {int} start Start index
		@param {int} end End index
		@param {String} value The annotation's value
		"""
		self._blip = blip
		self._name = name
		self._start = start
		self._end = end
		self._value = value
	
	def blip(self):
		"""
		Returns the Blip on which this element resides.
		
		@function {public Blip} blip
		"""
		return self._blip
	
	def name(self):
		"""
		Returns the name of this annotation.
		
		@function {public String} name
		"""
		return self._name
	
	def start(self):
		"""
		Returns the start index of this annotation.
		
		@function {public int} start
		"""
		return self._start
	
	def setStart(self, index):
		"""
		Sets the start index of this annotation.
		
		@function {public} setStart
		@param {int} index The new start index
		"""
		self._start = index
	
	def end(self):
		"""
		Returns the end index of this annotation.
		
		@function {public int} end
		"""
		return self._end
	
	def setEnd(self, index):
		"""
		Sets the end index of this annotation.
		
		@function {public} setEnd
		@param {int} index The new end index
		"""
		self._end = index

ELEMENT_TYPE = {
	"NOTHING": 0,
	
	"INLINE_BLIP": 1,
	
	"GADGET": 2,
	
	"INPUT": 3,
	"CHECK": 4,
	"LABEL": 5,
	"BUTTON": 6,
	"RADIO_BUTTON": 7,
	"RADIO_BUTTON_GROUP": 8,
	"PASSWORD": 9,
	
	"IMAGE": 10,
}

@Implements(Events)
@Class
class Element(object):
	"""
	Element-objects are all the non-text elements in a Blip.
	An element has no physical presence in the text of a Blip, but it maintains
	an implicit protected newline character to keep positions distinct.
	
	Only special Wave Client elements are treated here.
	There are no HTML elements in any Blip. All markup is handled by Annotations.
	
	@class {private} pygowave.model.Element
	"""
	
	def __init__(self, blip, id, position, type, properties):
		"""
		Called on instantiation. Documented for internal purposes.
		@constructor {private} initialize
		@param {Blip} blip The element's parent Blip
		@param {int} id ID of the element, setting this to null will assign
		             a new temporaty ID
		@param {int} position Index where this element resides
		@param {int} type Type of the element
		@param {Object} properties The element's properties
		"""
		self._blip = blip
		if id == None:
			self._id = Element.newTempId()
		else:
			self._id = id
		self._pos = position
		self._type = type
		if properties == None:
			self._properties = Hash()
		else:
			self._properties = Hash(properties)
	
	def blip(self):
		"""
		Returns the Blip on which this element resides.
		
		@function {public Blip} blip
		"""
		return self._blip
	
	def setBlip(self, blip):
		"""
		Set the parent Blip to the given Blip.
		
		@function {public} setBlip
		"""
		self._blip = blip

	def id(self):
		"""
		Return the ID of the element. A negative ID represents a temporary ID,
		that is only valid for this session and will be replaced by a real ID
		on reload.
		
		@function {public int} id
		"""
		return self._id

	def type(self):
		"""
		Returns the type of the element.
		
		@function {public int} type
		"""
		return self._type
	
	def position(self):
		"""
		Returns the index where this element resides
		
		@function {public int} position
		"""
		return self._pos
	
	def setPosition(self, pos):
		"""
		Sets the index where this element resides
		
		@function {public} setPosition
		@param {int} pos New position
		"""
		self._pos = pos

	@staticmethod
	def newTempId():
		Element.lastTempId -= 1
		return Element.lastTempId

Element.lastTempId = 0

@Class
class GadgetElement(Element):
	"""
	A gadget element.
	
	@class pygowave.model.GadgetElement
	@extends pygowave.model.Element
	"""
	
	# --- Event documentation ---
	"""
	Fired on gadget state change.
	
	@event onStateChange
	"""
	
	"""
	Fired on UserPref setting.
	
	@event onSetUserPref
	@param {String} key
	@param {Object} value
	"""
	# ---------------------------
	
	def __init__(self, blip, id, position, properties):
		"""
		Called on instantiation. Documented for internal purposes.
		@constructor {private} initialize
		@param {Blip} blip The gadget's parent Blip
		@param {int} id ID of the element, setting this to null will assign
		             a new temporaty ID
		@param {int} position Index where this gadget resides
		@param {Object} properties The gadget element's properties
		@param {int} id
		"""
		super(GadgetElement, self).__init__(blip, id, position, ELEMENT_TYPE["GADGET"], properties)
		if self._properties.has("fields"): # Convert fields to hash
			self._properties.set("fields", Hash(self._properties.get("fields")))
		if self._properties.has("userprefs"): # Convert userprefs to hash
			self._properties.set("userprefs", Hash(self._properties.get("userprefs")))
	
	def fields(self):
		"""
		Return the gadget's state object i.e. the field map.
		
		@function {public Object} fields
		"""
		if self._properties.has("fields"):
			return self._properties.get("fields").getClean()
		else:
			return {}
	
	def userPrefs(self):
		"""
		Return all UserPrefs as hash.
		
		@function {public Hash} userPrefs
		"""
		if self._properties.has("userprefs"):
			return self._properties.get("userprefs")
		else:
			return {}

	def url(self):
		"""
		Returns the gadget xml URL.
		
		@function {public String} url
		"""
		if self._properties.has("url"):
			return self._properties.get("url")
		else:
			return ""
	
	def applyDelta(self, delta):
		"""
		Apply a delta to this gadget's state.
		
		@function {public} applyDelta
		@param {Object} delta An object whose fields will be merged into the
			gadget state.
		"""
		if not self._properties.has("fields"):
			self._properties.set("fields", Hash())
		
		fields = self._properties.get("fields")
		fields.update(delta)
		
		# Delete keys with null values
		for key, value in fields.iteritems():
			if value == None:
				del fields[key]
		
		self.fireEvent("stateChange")
	
	def setUserPref(self, key, value, noevent = False):
		"""
		Set a UserPref.
		
		@function {public} setUserPref
		@param {String} key
		@param {Object} value
		"""
		if not self._properties.has("userprefs"):
			self._properties.set("userprefs", Hash())
		
		self._properties.get("userprefs").set(key, value)
		if not noevent:
			self.fireEvent("setUserPref", [key, value])

@Implements(Options, Events)
@Class
class Blip(object):
	"""
	Models a Blip in a Wavelet.<br/>
	This is a private class and cannot be instanitated directly. Please
	use {@link pygowave.model.Wavelet.addBlip Wavelet.addBlip}.
	@class {private} pygowave.model.Blip
	"""
	
	options = {
		"creator": None,
		"is_root": False,
		"last_modified": None,
		"version": 0,
		"submitted": False
	}
	
	# --- Event documentation ---
	"""
	Fired on text insertion.
	
	@event onInsertText
	@param {int} index Offset where the text is inserted
	@param {String} text The text to be inserted
	"""
	
	"""
	Fired on text deletion.
	
	@event onDeleteText
	@param {int} index Offset where the text is deleted
	@param {int} length Number of characters to delete
	"""
	
	"""
	Fired on element insertion.
	
	@event onInsertElement
	@param {int} index Offset where the element is inserted
	"""
	
	"""
	Fired on element deletion.
	
	@event onDeleteElement
	@param {int} index Offset where the element is deleted
	"""
	
	"""
	Fired if the Blip has gone out of sync with the server.
	
	@event onOutOfSync
	"""
	# ---------------------------
	
	def __init__(self, wavelet, id, options, content = "", elements = [], parent = None):
		"""
		Called on instantiation. Documented for internal purposes.
		@constructor {private} initialize
		@param {Wavelet} wavelet Parent Wavelet object
		@param {String} id ID of this Blip
		@param {Object} options Information about the Blip. Possible values:
		@... {Participant} creator Creator of this Blip
		@... {Boolean} is_root True if this is the root Blip; if this value
		     is set and the parent Wavelet does not have a root Blip yet,
		     this Blip is set as the Wavelet's root Blip
		@... {Date} last_modified Date of last modification
		@... {int} version Version of the Blip
		@... {Boolean} submitted True if this Blip is submitted
		@param {options String} content Content of the Blip
		@param {optional Element[]} elements Element objects which initially
		       reside in this Blip
		@param {optional Blip} parent Parent Blip if this is a nested Blip
		"""
		self.setOptions(options)
		self._wavelet = wavelet
		self._id = id
		self._parent = parent
		
		self._content = content
		self._elements = elements
		for element in self._elements:
			element.setBlip(self)
		
		self._annotations = []
		
		self._outofsync = False
	
	def id(self):
		"""
		Returns the ID of this Blip.
		
		@function {public String} id
		"""
		return self._id
	
	def wavelet(self):
		"""
		Returns the parent Wavelet of this Blip.
		
		@function {public Wavelet} wavelet
		"""
		return self._wavelet

	def isRoot(self):
		"""
		Returns true, if this Blip is the Wavelet's root Blip.
		@function {public Boolean} isRoot
		"""
		return self.options["is_root"]

	def elementById(self, id):
		"""
		Returns an Element by its id.
		@function {public Element} elementById
		@param {int} id ID of the element
		"""
		for i in xrange(len(self._elements)):
			elt = self._elements[i]
			if elt.id() == id:
				return elt
		
		return None

	def elementAt(self, index):
		"""
		Returns the Element object at the given position or null.
		
		@function {public Element} elementAt
		@param {int} index Index of the element to retrieve
		"""
		
		for i in xrange(len(self._elements)):
			elt = self._elements[i]
			if elt.position() == index:
				return elt
		
		return None

	def elementsWithin(self, start, end):
		"""
		Returns the Elements between the start and end index.
		
		@function {public Element[]} elementsWithin
		@param {int} start Start index
		@param {int} end End index
		"""
		
		lst = []
		for i in xrange(len(self._elements)):
			elt = self._elements[i]
			if elt.position() >= start and elt.position() < end:
				lst.append(elt)
		
		return lst

	def allElements(self):
		"""
		Returns all Elements of this Blip.
		
		@function {public Element[]} allElements
		"""
		
		return self._elements

	def insertText(self, index, text, noevent = False):
		"""
		Insert a text at the specified index. This moves annotations and
		elements as appropriate.<br/>
		Note: This sets the wavelet status to 'dirty'.
		
		@function {public} insertText
		@param {int} index Index of insertion
		@param {String} text Text to be inserted
		@param {optional Boolean} noevent Set to true if no event should be generated
		"""
		
		self._content = self._content[:index] + text + self._content[index:]
		
		length = len(text)
		
		for elt in self._elements:
			if elt.position() >= index:
				elt.setPosition(elt.position() + length)
		
		for anno in self._annotations:
			if anno.start() >= index:
				anno.setStart(anno.start() + length)
				anno.setEnd(anno.end() + length)
		
		self._wavelet._setStatus("dirty")
		if not noevent:
			self.fireEvent("insertText", [index, text])
	
	def deleteText(self, index, length, noevent = False):
		"""
		Delete text at the specified index. This moves annotations and
		elements as appropriate.<br/>
		Note: This sets the wavelet status to 'dirty'.
		
		@function {public} deleteText
		@param {int} index Index of deletion
		@param {int} length Number of characters to delete
		@param {optional Boolean} noevent Set to true if no event should be generated
		"""
		
		self._content = self._content[:index] + self._content[index+length:]
		
		for elt in self._elements:
			if elt.position() >= index:
				elt.setPosition(elt.position() - length)
		
		for anno in self._annotations:
			if anno.start() >= index:
				anno.setStart(anno.start() - length)
				anno.setEnd(anno.end() - length)
		
		self._wavelet._setStatus("dirty")
		if not noevent:
			self.fireEvent("deleteText", [index, length])
	
	def insertElement(self, index, type, properties, noevent = False):
		"""
		Insert an element at the specified index. This implicitly adds a
		protected newline character at the index.<br/>
		Note: This sets the wavelet status to 'dirty'.
		
		@function {public} insertElement
		@param {int} index Position of the new element
		@param {String} type Element type
		@param {Object} properties Element properties
		@param {optional Boolean} noevent Set to true if no event should be generated
		"""
		
		self.insertText(index, "\n", True)
		elt = None
		if type == 2:
			elt = GadgetElement(self, None, index, properties)
		else:
			elt = Element(self, None, index, type, properties)
		self._elements.append(elt)
		
		self._wavelet._setStatus("dirty")
		if not noevent:
			self.fireEvent("insertElement", index)
	
	def deleteElement(self, index, noevent = False):
		"""
		Delete an element at the specified index. This implicitly deletes the
		protected newline character at the index.<br/>
		Note: This sets the wavelet status to 'dirty'.
		
		@function {public} deleteElement
		@param {int} index Position of the element to delete
		@param {optional Boolean} noevent Set to true if no event should be generated
		"""
		
		for i in xrange(len(self._elements)):
			elt = self._elements[i]
			if elt.position() == index:
				self._elements.pop(i)
				break
		self.deleteText(index, 1, True)
		if not noevent:
			self.fireEvent("deleteElement", index)
	
	def applyElementDelta(self, index, delta):
		"""
		Apply an element delta. Currently only for gadget elements.<br/>
		Note: This action always emits stateChange.
		
		@function {public} applyElementDelta
		@param {int} index Position of the element
		@param {Object} delta Delta to apply to the element
		"""
		
		for elt in self._elements:
			if elt.position() == index:
				elt.applyDelta(delta)
				break
	
	def setElementUserpref(self, index, key, value, noevent = False):
		"""
		Set an UserPref of an element. Currently only for gadget elements.
		
		@function {public} setElementUserpref
		param {int} index Position of the element
		@param {Object} key Name of the UserPref
		@param {Object} value Value of the UserPref
		@param {optional Boolean} noevent Set to true if no event should be generated
		"""
		
		for elt in self._elements:
			if elt.position() == index:
				elt.setUserPref(key, value, noevent)
				break

	def content(self):
		"""
		Returns the text content of this Blip.
		@function {public String} content
		"""
		return self._content
	
	def checkSync(self, sum):
		"""
		Calculate a checksum of this Blip and compare it against the given
		checksum. Fires {@link pygowave.model.Blip.onOutOfSync onOutOfSync} if
		the checksum is wrong. Returns true if the checksum is ok.
		
		Note: Currently this only calculates the SHA-1 of the Blip's text. This
		is tentative and subject to change
		
		@function {public Boolean} checkSync
		@param {String} sum Input checksum to compare against
		"""
		if self._outofsync:
			return False
		mysum = sha_constructor(self._content.encode("utf-8")).hexdigest()
		if mysum != sum:
			self.fireEvent("outOfSync")
			self._outofsync = True
			return False
		else:
			return True

@Implements(Options, Events)
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
		"created": None,
		"last_modified": None,
		"title": "",
		"version": 0,
		"status": "clean"
	}
	
	# --- Event documentation ---
	"""
	Fired if a participant joins or leaves.
	@event onParticipantsChanged
	"""
	
	"""
	Fired if a Blip was inserted.
	@event onBlipInserted
	@param {int} index Index of the inserted Blip
	@param {String} blip_id ID of the inserted Blip
	"""
	
	"""
	Fired when the Wavelet's status changed.
	
	@event onStatusChange
	@param {String} status The new status; can be 'clean', 'dirty' or 'invalid'
	"""
	
	"""
	Fired when the Wavelet's title changed.
	@event onTitleChanged
	@param {String} title The Wavelet's new title
	"""
	
	"""
	Fired when the Wavelet's last modification date/time changed.
	@event onLastModifiedChanged
	@param {Date} datetime New date of last modification
	"""
	# ---------------------------
	
	def __init__(self, wave, id, options):
		"""
		Called on instantiation. Documented for internal purposes.
		@constructor {private} initialize
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
		@... {String} status Status of the Wavelet; can be 'clean', 'dirty' or 'invalid'
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
		self._rootBlip = None
	
	def title(self):
		"""
		Returns the title of this Wavelet
		@function {public String} title
		"""
		return self.options["title"]
	
	def setTitle(self, title):
		"""
		Sets the title of this Wavelet
		@function {public} setTitle
		@param {String} title The new title
		"""
		if self.options["title"] != title:
			self.options["title"] = title
			self.fireEvent('titleChanged', title)
	
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
	
	def waveId(self):
		"""
		Returns the ID of this Wavelet's Wave.
		@function {public String} waveId
		"""
		return self._wave.id()
	
	def waveModel(self):
		"""
		Returns the parent WaveModel object.
		@function {public WaveModel} waveModel
		"""
		return self._wave

	def participantCount(self):
		"""
		Returns the number of participants on this Wavelet.
		@function {public int} participantCount
		"""
		return self._participants.getLength()

	def addParticipant(self, participant):
		"""
		Add a participant to this Wavelet.<br/>
		Note: Fires {@link pygowave.model.Wavelet.onParticipantsChanged onParticipantsChanged}
		
		@function {public} addParticipant
		@param {Participant} participant Participant to be added
		"""
		if not self._participants.has(participant.id()):
			self._participants.set(participant.id(), participant)
			self.fireEvent('participantsChanged', self._id)
	
	def removeParticipant(self, participantId):
		"""
		Removes a participant from this Wavelet.<br/>
		Note: Fires {@link pygowave.model.Wavelet.onParticipantsChanged onParticipantsChanged}
		
		@function {public} removeParticipant
		@param {String} participantId ID of the participant to remove
		"""
		if self._participants.has(participantId):
			del self._participants[participantId]
			self.fireEvent('participantsChanged', self._id)
	
	def participant(self, id):
		"""
		Returns the Participant object with the given id, if the participant
		resides on this Wavelet. Returns null otherwise.
		@function {Participant} participant
		@param {String} id ID of the Participant
		"""
		if self._participants.has(id):
			return self._participants.get(id)
		else:
			return None
	
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
	
	def appendBlip(self, id, options, content = "", elements = []):
		"""
		Convenience function for inserting a new Blip at the end.
		For options see the {@link pygowave.model.Blip.initialize Blip constructor}.<br/>
		Note: Fires {@link pygowave.model.Wavelet.onBlipInserted onBlipInserted}
		
		@function {public Blip} appendBlip
		@param {String} id ID of the new Blip
		@param {Object} options Information about the Blip
		@param {optional String} content Content of the Blip
		@param {optional Element[]} elements Element objects which initially
		    reside in this Blip
		"""
		return self.insertBlip(len(self._blips), id, options, content, elements)

	def insertBlip(self, index, id, options, content = "", elements = []):
		"""
		Insert a new Blip at the specified index.
		For options see the {@link pygowave.model.Blip.initialize Blip constructor}.<br/>
		Note: Fires {@link pygowave.model.Wavelet.onBlipInserted onBlipInserted}
		
		@function {public Blip} insertBlip
		@param {int} index Index where to insert the Blip
		@param {String} id ID of the new Blip
		@param {Object} options Information about the Blip
		@param {optional String} content Content of the Blip
		@param {optional Element[]} elements Element objects which initially
		    reside in this Blip
		"""
		blip = Blip(self, id, options, content, elements)
		self._blips.insert(index, blip)
		self.fireEvent('blipInserted', [index, id])
		return blip
	
	def blipByIndex(self, index):
		"""
		Returns the Blip object at the given index.
		
		@function {Blip} blipByIndex
		@param {int} index Index of the Blip
		"""
		return self._blips[index]

	def blipById(self, id):
		"""
		Returns the Blip object with the given id, if the Blip resides on this
		Wavelet. Returns null otherwise.
		
		@function {Blip} blipById
		@param {String} id ID of the Blip
		"""
		
		for blip in self._blips:
			if blip.id() == id:
				return blip
		
		return None

	def allBlips(self):
		"""
		Returns a list of all Blips on this Wavelet, starting with the root Blip.
		
		@function {Blip[]} allBlips
		"""
		return self._blips
	
	def allBlipIDs(self):
		"""
		Returns a list of all IDs of the Blips on this Wavelet, starting with
		the root Blip.
		
		@function {public String[]} allBlipIDs
		"""
		ids = []
		for blip in self._blips:
			ids.append(blip.id())
		return ids

	def _setRootBlip(self, blip):
		"""
		Internal method to set the root Blip. Not intended to be called
		outside of this implementation.
		
		@function {private} _setRootBlip
		@param {Blip} blip Blip to be set as root Blip
		"""
		self._rootBlip = blip
	
	def _setStatus(self, status):
		"""
		Internal method to set the status. Fires
		{@link pygowave.model.Wavelet.onStatusChange onStatusChange} if the
		status changed.
		
		@function {private} _setStatus
		@param {String} status The status to set
		"""
		if self.options["status"] != status:
			self.options["status"] = status
			self.fireEvent("statusChange", status)
	
	def status(self):
		"""
		Returns the Wavelet's current status. Can be "clean", "dirty" or "invalid".
		@function {public String} status
		"""
		return self.options["status"]
	
	def created(self):
		"""
		Returns the creation date/time of this Wavelet.
		@function {public Date} created
		"""
		return self.options["created"]
	
	def lastModified(self):
		"""
		Returns the date/time of the last modification of this Wavelet.
		@function {public Date} lastModified
		"""
		return self.options["last_modified"]
	
	def setLastModified(self, value):
		"""
		Sets the date/time of the last modification of this Wavelet.
		@function {public} setLastModified
		@param {Date} value The new date/time of the last modification
		"""
		if value != self.options["last_modified"]:
			self.options["last_modified"] = value
			self.fireEvent('lastModifiedChanged', value)
	
	def checkSync(self, blipsums):
		"""
		Calculate and compare checksums of all Blips to the given map.
		Fires {@link pygowave.model.Wavelet.onStatusChange onStatusChange} if
		the status changes.
		
		@function {public} checkSync
		@param {Object} blipsums Checksums to compare to
		"""
		valid = True
		for blipId, checksum in blipsums.iteritems():
			blip = self.blipById(blipId);
			if blip != None:
				if not blip.checkSync(checksum):
					valid = False
		if valid:
			self._setStatus("clean")
		else:
			self._setStatus("invalid")
	
	def applyOperations(self, ops, participants):
		"""
		Apply the operations on the wavelet.
		
		@function {public} applyOperations
		@param {pygowave.operations.Operation[]} ops List of operations to apply
		"""
		
		for op in ops:
			if op.blipId != "":
				blip = self.blipById(op.blipId)
				if op.type == pygowave.operations.DOCUMENT_DELETE:
					blip.deleteText(op.index, op.property)
				elif op.type == pygowave.operations.DOCUMENT_INSERT:
					blip.insertText(op.index, op.property)
				elif op.type == pygowave.operations.DOCUMENT_ELEMENT_DELETE:
					blip.deleteElement(op.index)
				elif op.type == pygowave.operations.DOCUMENT_ELEMENT_INSERT:
					blip.insertElement(op.index, op.property["type"], op.property["properties"])
				elif op.type == pygowave.operations.DOCUMENT_ELEMENT_DELTA:
					blip.applyElementDelta(op.index, op.property)
				elif op.type == pygowave.operations.DOCUMENT_ELEMENT_SETPREF:
					blip.setElementUserpref(op.index, op.property["key"], op.property["value"])
			else:
				if op.type == pygowave.operations.WAVELET_ADD_PARTICIPANT:
					self.addParticipant(participants[op.property])
				elif op.type == pygowave.operations.WAVELET_REMOVE_PARTICIPANT:
					self.removeParticipant(op.property)
	
	def loadBlipsFromSnapshot(self, blips, rootBlipId, participants):
		"""
		Load the blips from a snapshot.
		@function {public} loadBlipsFromSnapshot
		@param {Object} blips The JSON-serialized snapshot to load
		@param {String} rootBlipId ID to identify the root Blip
		@param {Hash} participants A map of participant objects
		"""
		
		for blip_id, blip in blips.iteritems():
			#TODO: handle Blip contributors
			blip_options = {
				"creator": participants[blip["creator"]],
				"is_root": blip_id == rootBlipId,
				"last_modified": blip["lastModifiedTime"],
				"version": blip["version"],
				"submitted": blip["submitted"]
			}
			blip_elements = []
			for serialelement in blip["elements"]:
				if serialelement["type"] == ELEMENT_TYPE["GADGET"]:
					blip_elements.append(GadgetElement(None, serialelement["id"], serialelement["index"], serialelement["properties"]))
				else:
					blip_elements.append(Element(None, serialelement["id"], serialelement["index"], serialelement["type"], serialelement["properties"]))
			blipObj = self.appendBlip(blip_id, blip_options, blip["content"], blip_elements)

@Implements(Events)
@Class
class WaveModel(object):
	"""
	Wave model class. Top level model which sends all events.
	
	@class {public} pygowave.model.WaveModel
	"""
	
	# --- Event documentation ---
	"""
	Fired if a Wavelet has been added.
	@event onWaveletAdded
	@param {String} waveletId ID of the Wavelet that has been added
	@param {Boolean} isRoot True if this is the (new) root Wavelet
	"""
	
	"""
	Fired before a wavelet is removed.
	@event onWaveletAboutToBeRemoved
	@param {String} waveletId ID of the Wavelet that will be removed
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

	def id(self):
		"""
		Returns the ID of this Wave.
		
		@function {public String} id
		"""
		return self._waveId

	def viewerId(self):
		"""
		Returns the ID of the viewer.
		
		@function {public String} viewerId
		"""
		return self._viewerId

	def loadFromSnapshot(self, obj, participants):
		"""
		Load the wave's contents from a JSON-serialized snapshot and a map of
		participant objects.
		
		@function {public} loadFromSnapshot
		@param {Object} obj The JSON-serialized snapshot to load
		@param {Hash} participants A map of participant objects
		"""
		
		rootWavelet = obj["wavelet"]
		
		wvl_options = {
			"creator": participants[rootWavelet["creator"]],
			"is_root": True,
			"created": rootWavelet["creationTime"],
			"last_modified": rootWavelet["lastModifiedTime"],
			"title": rootWavelet["title"],
			"version": rootWavelet["version"]
		}
		
		rootWaveletObj = self.createWavelet(rootWavelet["waveletId"], wvl_options)
		
		for part_id in rootWavelet["participants"]:
			rootWaveletObj.addParticipant(participants[part_id])
		
		rootWaveletObj.loadBlipsFromSnapshot(obj["blips"], rootWavelet["rootBlipId"], participants);

	def createWavelet(self, id, options):
		"""
		Create a Wavelet and add it to this Wave. For options see the
		{@link pygowave.model.Wavelet.initialize Wavelet constructor}.<br/>
		Note: Fires {@link pygowave.model.WaveModel.onWaveletAdded onWaveletAdded}
		
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
	
	def allWavelets(self):
		"""
		Return a list of all Wavelets on this Wave.
		
		@function {public Wavelet[]} allWavelets
		"""
		return self._wavelets.getValues()

	def rootWavelet(self):
		"""
		Returns the root Wavelet of this Wave.
		
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
	
	def removeWavelet(self, waveletId):
		"""
		Removes and deletes a wavelet by its id. Fires
		{@link pygowave.model.WaveModel.onWaveletAboutToBeRemoved} beforehand.
		
		@function {public} removeWavelet
		@param {String} waveletId ID of the Wavelet to remove
		"""
		if not self._wavelets.has(waveletId):
			return
		
		self.fireEvent('waveletAboutToBeRemoved', waveletId)
		wavelet = self._wavelets.get(waveletId)
		self._wavelets.erase(waveletId)
		if wavelet == self._rootWavelet:
			self._rootWavelet = None
