
#
# PyCow - Python to JavaScript with MooTools translator
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

#
# This file contains some compatibility classes which implement parts of
# MooTool's functionality.
#

from types import FunctionType, ClassType
import copy, re, simplejson

__all__ = ["Events", "Options", "Array", "Hash", "JSON"]

class Events(object):
	def addEvent(self, type, fn, internal = False):
		type = Events.removeOn(type)
		evts = getattr(self, "$events", None)
		if evts == None:
			evts = {}
			setattr(self, "$events", evts)
		if not evts.has_key(type):
			evts[type] = set()
		evts[type].add(fn)
		if internal: fn.internal = True
		return self

	def addEvents(self, events):
		for type in events.iterkeys():
			self.addEvent(type, events[type])
		return self

	def fireEvent(self, type, args = None, delay = None):
		type = Events.removeOn(type)
		evts = getattr(self, "$events", None)
		if evts == None or not evts.has_key(type):
			return self
		
		for fn in evts[type]:
			if isinstance(args, list):
				fn(*args)
			elif args != None:
				fn(args)
			else:
				fn()
			#fn.create({'bind': this, 'delay': delay, 'arguments': args})()
		
		return self

	def removeEvent(self, type, fn):
		type = Events.removeOn(type)
		evts = getattr(self, "$events", None)
		if evts == None or not evts.has_key(type):
			return self
		
		if not getattr(fn, "internal", False):
			evts[type].remove(fn)
		return self

	def removeEvents(self, type):
		evts = getattr(self, "$events", None)
		if evts == None:
			return self
		for e in evts.iterkeys():
			if type != e: continue
			for fn in evts[e]:
				self.removeEvent(e, fn)
		return self

	@staticmethod
	def removeOn(string):
		return re.sub(r"(?i)^on([A-Z])", lambda match: match.group(1).lower(), string)

class Options(object):
	def setOptions(self, options = {}):
		if getattr(self, "options", None) == None: return self
		self.options.update(options)
		if getattr(self, "addEvent", None): return self
		for option in self.options.iterkeys():
			if not isinstance(self.options[option], FunctionType) or not re.match(r"(?i)^on([A-Z])", option): continue
			self.addEvent(option, self.options[option])
			del self.options[option]
		return self

class Array(list):
	@property
	def length(self):
		return self.__len__()
	
	def contains(self, value):
		return self.__contains__(value)
	
	def sort(self):
		list.sort(self)
		return self
	
	def reverse(self):
		list.reverse(self)
		return self
	
	def indexOf(self, value):
		try:
			return self.index(value)
		except ValueError:
			return -1
	
	def lastIndexOf(self, value):
		l = list(self)
		l.reverse()
		try:
			return self.__len__() - l.index(value)
		except ValueError:
			return -1
	
	def push(self, value):
		self.append(value)
	
	def erase(self, value):
		try:
			while True:
				self.remove(value)
		except ValueError:
			pass
		return self

class Hash(dict):
	def set(self, key, value):
		self[key] = value
	
	def get(self, key):
		try:
			return self[key]
		except KeyError:
			return None
	
	def has(self, key):
		return self.has_key(key)

	def getValues(self):
		return self.values()
	
	def getKeys(self):
		return self.keys()
	
	def getClean(self):
		return dict(self)
	
	def extend(self, other):
		self.update(other)
	
	def erase(self, key):
		try:
			del self[key]
		except:
			pass

class JSON(object):
	def encode(self, obj):
		return simplejson.dumps(obj)
	
	def decode(self, string, secure=None):
		return simplejson.loads(string)

# Singleton
JSON = JSON()
