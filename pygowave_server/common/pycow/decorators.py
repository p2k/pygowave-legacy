
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
# This file contains some compatibility decorators which implement parts of
# MooTool's functionality.
#

import copy
from types import ClassType, FunctionType

__all__ = ["Implements", "Class"]

def Implements(*args):
	"""
	This class decorator is used to implement methods of given classes into the
	decorated class. Your class needs a `implement` method for this to work.
	See the `Class` decorator.
	
	"""
	def do_implement(target):
		for cls in args:
			target.implement(cls)
		return target
	return do_implement

@classmethod
def implement(cls, properties):
	"""
	Implements the passed in properties into the base Class, altering the
	base Class.
	The same as creating a new Class with the Implements decorator, but
	handy when you need to modify existing classes.
	
	Note: `properties` must be either a dictionary or a class.
	"""
	oldstyle = isinstance(cls, ClassType)
	if isinstance(properties, type):
		properties = properties.__dict__
	for name, value in properties.iteritems():
		if name.startswith("__") and name.endswith("__"): continue
		if oldstyle:
			cls.__dict__[name] = value
		else:
			setattr(cls, name, value)
	return cls

def parent(self, *args):
	"""
	Call the parent class' method. This uses a stacktrace and thus may not be
	very efficient. Use super() instead.
	"""
	import traceback
	getattr(super(self.__class__, self), traceback.extract_stack(None, 2)[0][2])(*args)

def Class(cls):
	"""
	This class decorator patches some things in your classes for MooTools
	compatibility. Namely it modifies the constructor to create shallow copies
	of class-bound properties (to reflect MooTools' behaviour) and adds a
	`implement` and `parent` method to the class.
	
	"""
	
	__orig_init__ = cls.__init__
	
	def __moo__init__(self, *args, **kwargs):
		"""
		MooTools compatibility constructor. Makes shallow copies of all class
		properties and calls the original constructor.
		"""
		for name, value in self.__class__.__dict__.iteritems():
			if name.startswith("__") and name.endswith("__"): continue
			if isinstance(value, FunctionType): continue
			setattr(self, name, copy.copy(value))
		
		__orig_init__(self, *args, **kwargs)
	
	cls.__init__ = __moo__init__
	if isinstance(cls, ClassType):
		cls.__dict__["implement"] = implement
	else:
		cls.implement = implement
		cls.parent = parent
	
	return cls
