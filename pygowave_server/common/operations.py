

"""
Operations module.
@module pygowave.operations
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
from pycow.utils import Events

DOCUMENT_INSERT = 'DOCUMENT_INSERT'
DOCUMENT_DELETE = 'DOCUMENT_DELETE'
DOCUMENT_REPLACE = 'DOCUMENT_REPLACE'

__all__ = ["Range", "OpManager", "DOCUMENT_INSERT", "DOCUMENT_DELETE", "DOCUMENT_REPLACE"]

@Class
class Range(object):
	"""
	Represents a start and end range with integers.
	
	Ranges map positions in the document. A range must have at least a length
	of zero. If zero, the range is considered to be a single point (collapsed).
	
	@class {public} Range
	"""
	
	def __init__(self, start=0, end=1):
		"""
		Initializes the range with a start and end position.
		
		@constructor {public} initialize
		
		@param {int} start Start index of the range.
		@param {int} end End index of the range.
		
		#@throws ValueError Value error if the range is invalid (less than zero).
		"""
		self.start = start
		self.end = end
		#if self.end - self.start < 0:
		#	raise ValueError('Range cannot be less than 0')
	
	def __repr__(self):
		return 'Range(%d,%d)' % (self.start, self.end)
	
	def isCollapsed(self):
		""""
		Returns true if this represents a single point as opposed to a range.
		
		@function {public Boolean} isCollapsed
		"""
		return self.end == self.start

@Class
class Operation(object):
	"""
	Represents a generic operation applied on the server.
	
	This operation class contains data that is filled in depending on the
	operation type.
	
	It can be used directly, but doing so will not result
	in local, transient reflection of state on the blips. In other words,
	creating a "delete blip" operation will not remove the blip from the local
	context for the duration of this session. It is better to use the OpBased
	model classes directly instead.
	
	@class {private} pygowave.operations.Operation
	"""
	
	def __init__(self, op_type, wave_id, wavelet_id, blip_id='', index=-1, prop=None):
		"""
		Initializes this operation with contextual data.
		
		@constructor {public} initialize
		@param {String} op_type Type of operation
		@param {String} wave_id The id of the wave that this operation is to
			be applied.
		@param {String} wavelet_id The id of the wavelet that this operation is
		    to be applied.
		@param {optional String} blip_id The optional id of the blip that this
			operation is to be applied.
		@param {optional int} index Optional integer index for content-based
			operations.
		@param {optional Object} prop A weakly typed property object is based
			on the context of this operation.
		"""
		self.type = op_type
		self.wave_id = wave_id
		self.wavelet_id = wavelet_id
		self.blip_id = blip_id
		self.index = index
		self.property = prop
	
	def clone(self):
		"""
		Create a copy of this operation.
		
		@function {public Boolean} clone
		"""
		return Operation(self.type, self.wave_id, self.wavelet_id, self.blip_id,
						 self.index, self.property)
	
	def isNull(self):
		"""
		Return weather this operation is a null operation i.e. it does not
		change anything.
		
		@function {public Boolean} isNull
		"""
		if self.type == DOCUMENT_INSERT:
			return len(self.property) == 0
		elif self.type == DOCUMENT_DELETE:
			return self.property == 0
		return False
	
	def isCompatibleTo(self, other_op):
		"""
		Check if the operation can be influenced by `other_op` and vice-versa.
		
		@function {public Boolean} isCompatibleTo
		@param {Operation} other_op
		"""
		if (self.type == DOCUMENT_INSERT or self.type == DOCUMENT_DELETE) and \
				(other_op.type == DOCUMENT_INSERT or other_op.type == DOCUMENT_DELETE):
			if self.wave_id != other_op.wave_id \
					or self.wavelet_id != other_op.wavelet_id \
					or self.blip_id != self.blip_id:
				return False
			return True
		return False

	def isInsert(self):
		"""
		Returns true, if this op is an insertion operation.
		
		@function {public Boolean} isInsert
		"""
		return self.type == DOCUMENT_INSERT
	
	def isDelete(self):
		"""
		Returns true, if this op is a deletion operation.
		
		@function {public Boolean} isDelete
		"""
		return self.type == DOCUMENT_DELETE

	def length(self):
		"""
		Returns the length of this operation.
		This can be interpreted as the distance a concurrent operation's index
		must be moved to include the effects of this operation.
		
		@function {public int} length
		"""
		if self.type == DOCUMENT_INSERT:
			return len(self.property)
		elif self.type == DOCUMENT_DELETE:
			return self.property
		return 0
	
	def resize(self, value):
		"""
		Delete operations: Sets the amount of deleted characters/elements to
		`value`.
		
		Other operations: No effect.
		
		@function {public} resize
		@param {int} value
		"""
		if self.type == DOCUMENT_DELETE:
			self.property = value

	def serialize(self):
		"""
		Serialize this operation into a dictionary.
		
		@function {public String} serialize
		"""
		return {
			"type": self.type,
			"wave_id": self.wave_id,
			"wavelet_id": self.wavelet_id,
			"blip_id": self.blip_id,
			"index": self.index,
			"property": self.property,
		}

	def __repr__(self):
		return "%s(\"%s\",%d,%s)" % (self.type.lower(), self.blip_id,
									 self.index, repr(self.property))

	@staticmethod
	def unserialize(obj):
		"""
		Unserialize an operation from a dictionary.
		
		@function {public static Operation} unserialize
		"""
		return Operation(obj["type"], obj["wave_id"], obj["wavelet_id"],
						 obj["blip_id"], obj["index"], obj["property"])

@Implements(Events)
@Class
class OpManager(object):
	"""
	Manages operations: Creating, merging, transforming, serializing.
	
	The operation manager wraps single operations as functions and generates
	operations in-order. It keeps a list of operations and allows
	transformation, merging and serializing.
	
	An OpManager is always associated with exactly one wave/wavelet.
	
	@class {public} pygowave.operations.OpManager
	"""
	
	# --- Event documentation ---
	"""
	Fired if an operation in this manager has been changed.
	@event onOperationChanged
	@param {int} index Index of the changed operation
	"""
	
	"""
	Fired if one or more operations are about to be removed.
	@event onBeforeOperationsRemoved
	@param {int} start Start index of the removal.
	@param {int} end End index of the removal.
	"""
	
	"""
	Fired if one or more operations have been removed.
	@event onAfterOperationsRemoved
	@param {int} start Start index of the removal.
	@param {int} end End index of the removal.
	"""
	
	"""
	Fired if one or more operations are about to be inserted.
	@event onBeforeOperationsInserted
	@param {int} start Start index of the insertion.
	@param {int} end End index of the insertion.
	"""
	
	"""
	Fired if one or more operations have been inserted.
	@event onAfterOperationsInserted
	@param {int} start Start index of the insertion.
	@param {int} end End index of the insertion.
	"""
	# ---------------------------
	
	def __init__(self, wave_id, wavelet_id):
		"""
		Initializes the op manager with a wave and wavelet ID.
		
		@constructor {public} initialize
		@param {String} wave_id The ID of the wave
		@param {String} wavelet_id The ID of the wavelet
		"""
		self.wave_id = wave_id
		self.wavelet_id = wavelet_id
		self.operations = []

	def isEmpty(self):
		"""
		Return true if this manager is not holding operations.
		
		@function {public Boolean} isEmpty
		"""
		return len(self.operations) == 0

	def transform(self, input_op):
		"""
		Transform the input operation on behalf of the manager's operations
		list. This will simultaneously transform the operations list on behalf
		of the input operation.
		This method returns a list of applicable operations. This list may be
		empty or it may contain any number of new operations (according to
		results of deletion, modification and splitting; i.e. the input
		operation is not modified by itself).
		
		@function {public Operation[]} transform
		@param {Operation} input_op
		"""
		
		new_op = None
		op_lst = [input_op.clone()]
		
		# From The Zen of Python, by Tim Peters:
		# "Complex is better than complicated."
		
		i = 0
		while i < len(self.operations):
			myop = self.operations[i]
			j = 0
			while j < len(op_lst):
				op = op_lst[j]
				
				# Do not handle incompatible operations
				if not op.isCompatibleTo(myop): continue
				
				# Check all possible cases
				
				end = None
				if op.isDelete() and myop.isDelete():
					if op.index < myop.index:
						end = op.index + op.length()
						if end <= myop.index:
							myop.index -= op.length()
							self.fireEvent("operationChanged", i)
						elif end < myop.index + myop.length(): # and end > myop.index
							op.resize(myop.index - op.index)
							myop.resize(myop.length() - (end - myop.index))
							myop.index = op.index
							self.fireEvent("operationChanged", i)
						else: # end >= myop.index + myop.length()
							op.resize(op.length() - myop.length())
							self.fireEvent("beforeOperationsRemoved", [i, i])
							self.operations.pop(i)
							self.fireEvent("afterOperationsRemoved", [i, i])
							i -= 1
							break
					else: # op.index >= myop.index
						end = myop.index + myop.length()
						if op.index >= end:
							op.index -= myop.length()
						elif op.index + op.length() <= end: # and op.index < end
							op_lst.pop(j)
							j -= 1
							myop.resize(myop.length() - op.length())
							if myop.isNull():
								self.fireEvent("beforeOperationsRemoved", [i, i])
								self.operations.pop(i)
								self.fireEvent("afterOperationsRemoved", [i, i])
								i -= 1
								break
							else:
								self.fireEvent("operationChanged", i)
						else: # op.index + op.length() > end
							myop.resize(myop.length() - (end - op.index))
							self.fireEvent("operationChanged", i)
							op.resize(op.length() - (end - op.index))
							op.index = myop.index
				
				elif op.isDelete() and myop.isInsert():
					if op.index < myop.index:
						if op.index + op.length() <= myop.index:
							myop.index -= op.length()
							self.fireEvent("operationChanged", i)
						else: # op.index + op.length() > myop.index
							new_op = op.clone()
							op.resize(myop.index - op.index)
							new_op.resize(new_op.length() - op.length())
							op_lst.insert(j+1, new_op)
							myop.index -= op.length()
							self.fireEvent("operationChanged", i)
					else: # op.index >= myop.index
						op.index += myop.length()
				
				elif op.isInsert() and myop.isDelete():
					if op.index <= myop.index:
						myop.index += op.length()
						self.fireEvent("operationChanged", i)
					elif op.index >= myop.index + myop.length(): # op.index > myop.index
						op.index -= myop.length()
					else: # op.index < myop.index + myop.length()
						new_op = myop.clone()
						myop.resize(op.index - myop.index)
						self.fireEvent("operationChanged", i)
						new_op.resize(new_op.length() - myop.length())
						self.fireEvent("beforeOperationsInserted", [i+1, i+1])
						self.operations.insert(i+1, new_op)
						self.fireEvent("afterOperationsInserted", [i+1, i+1])
						op.index = myop.index
				
				elif op.isInsert() and myop.isInsert():
					if op.index <= myop.index:
						myop.index += op.length()
						self.fireEvent("operationChanged", i)
					else: # op.index > myop.index
						op.index += myop.length()
				
				j += 1
				
			i += 1
		
		return op_lst
	
	def fetch(self):
		"""
		Returns the pending operations and removes them from this manager.
		
		@function {public Operation[]} fetch
		"""
		ops = self.operations
		self.fireEvent("beforeOperationsRemoved", [0, len(ops)-1])
		self.operations = []
		self.fireEvent("afterOperationsRemoved", [0, len(ops)-1])
		return ops
	
	def put(self, ops):
		"""
		Opposite of fetch. Inserts all given operations into this manager.
		
		@function {public} put
		@param {Operation[]} ops
		"""
		if len(ops) == 0:
			return
		start = len(self.operations)
		end = start + len(ops) - 1
		self.fireEvent("beforeOperationsInserted", [start, end])
		self.operations.extend(ops)
		self.fireEvent("afterOperationsInserted", [start, end])

	def serialize(self, fetch = False):
		"""
		Serialize this manager's operations into a list of dictionaries.
		Set fetch to true to also clear this manager.
		
		@function {public Object[]} serialize
		@param {optional Boolean} fetch
		"""
		if fetch:
			ops = self.fetch()
		else:
			ops = self.operations
		
		out = []
		
		for op in ops:
			out.append(op.serialize())
		
		return out
	
	def unserialize(self, serial_ops):
		"""
		Unserialize a list of dictionaries to operations and add them to this
		manager.
		
		@function {public} unserialize
		@param {Object[]} serial_ops
		"""
		
		ops = []
		
		for op in serial_ops:
			ops.append(Operation.unserialize(op))
		
		self.put(ops)

	def __insert(self, newop):
		"""
		Inserts and probably merges an operation into the manager's
		operation list.
		
		@function {private} __insert
		@param {Operation} newop
		"""
		
		# Only merge with the last op (otherwise this may get a bit complicated)
		i = len(self.operations) - 1
		if i >= 0:
			op = self.operations[i]
			if newop.type == DOCUMENT_INSERT and op.type == DOCUMENT_INSERT:
				if newop.index == op.index: # Prepending
					op.property = newop.property + op.property
					self.fireEvent("operationChanged", i)
					return
				end = op.index + len(op.property)
				if newop.index == end: # Appending
					op.property += newop.property
					self.fireEvent("operationChanged", i)
					return
				if op.index < newop.index and newop.index < end: # Inserting
					op.property = op.property[:newop.index-op.index] + newop.property + op.property[newop.index-op.index:]
					self.fireEvent("operationChanged", i)
					return
			if newop.type == DOCUMENT_DELETE and op.type == DOCUMENT_INSERT:
				if newop.index == op.index: # Delete from start
					l = len(op.property)
					op.property = op.property[newop.property:]
					if op.property == "":
						self.fireEvent("beforeOperationsRemoved", [i, i])
						self.operations.pop(i)
						self.fireEvent("afterOperationsRemoved", [i, i])
					else:
						self.fireEvent("operationChanged", i)
					newop.property -= l
					if newop.property <= 0: # Deleted less than last op's length
						return
				end = op.index + len(op.property)
				if op.index < newop.index and newop.index < end: # Delete from within
					l = len(op.property) - (newop.index - op.index + newop.property)
					op.property = op.property[:newop.index - op.index] + op.property[newop.index - op.index + newop.property:]
					self.fireEvent("operationChanged", i)
					newop.property = -l
					if newop.property <= 0: # Deleted less than last op's length
						return
			if newop.type == DOCUMENT_DELETE and op.type == DOCUMENT_DELETE:
				if newop.index == op.index: # Delete at start
					op.property += newop.property
					self.fireEvent("operationChanged", i)
					return
				if newop.index == op.index-newop.property: # Deleta at end
					op.index -= newop.property
					op.property += newop.property
					self.fireEvent("operationChanged", i)
					return
		
		# If we reach this the operation could not be merged, so add it.
		self.fireEvent("beforeOperationsInserted", [i+1, i+1])
		self.operations.append(newop)
		self.fireEvent("afterOperationsInserted", [i+1, i+1])

	# --------------------------------------------------------------------------

	def documentInsert(self, blip_id, index, content):
		"""
		Requests to insert content into a document at a specific location.
		
		@function {public} documentInsert
		@param {String} blip_id The blip id that this operation is applied to
		@param {int} index The position insert the content at in ths document
		@param {String} content The content to insert
		"""
		self.__insert(Operation(
			DOCUMENT_INSERT,
			self.wave_id, self.wavelet_id, blip_id,
			index,
			content
		))
	
	def documentDelete(self, blip_id, start, end):
		"""
		Requests to delete content in a given range.
		
		@function {public} documentDelete
		@param {String} blip_id The blip id that this operation is applied to
		@param {int} start Start of the range
		@param {int} end End of the range
		"""
		self.__insert(Operation(
			DOCUMENT_DELETE,
			self.wave_id, self.wavelet_id, blip_id,
			start,
			end-start # = length
		))
