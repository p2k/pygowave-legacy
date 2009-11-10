
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

# PyGoWave Client-to-Server Message Processor

import sys, datetime, logging

from django.core.exceptions import ObjectDoesNotExist
from django.conf import settings

from pygowave_server.models import Participant, ParticipantConn, Gadget, GadgetElement, Delta
from pygowave_server.common.operations import OpManager

__all__ = ["PyGoWaveClientMessageProcessor"]

# Progress tracker - implemented messages
# Legend:
# (sync)  - message is handled synchronously i.e. the client does not perform
#           any actions and waits for the server's response
# (async) - message is handled asynchronously and the server does not care if
#           there are inconsistencies
# (OT)    - full Operational Transformation implemented
#
# --------
#
# WAVELET_OPEN (sync)
# PARTICIPANT_INFO (sync)
# PARTICIPANT_SEARCH (sync)
# GADGET_LIST (sync)
# WAVELET_ADD_PARTICIPANT (sync)
# WAVELET_REMOVE_SELF (sync)
# OPERATION_MESSAGE_BUNDLE (OT)
#   DOCUMENT_INSERT
#   DOCUMENT_DELETE
#   DOCUMENT_ELEMENT_INSERT
#   DOCUMENT_ELEMENT_DELETE
#   DOCUMENT_ELEMENT_DELTA
#   DOCUMENT_ELEMENT_SETPREF
# -> OPERATION_MESSAGE_BUNDLE_ACK

class PyGoWaveClientMessageProcessor(object):
	"""
	Handle all incoming messages.
	
	Routing key structure:
	<participant_conn_guid>.<wavelet_id>.[waveop|clientop]
	
	participant_conn_guid is created every time the user enters the wave viewer.
	Multiple instances of the wave viewer can be opened per user at the same time.
	A participant can "listen" to messages from multiple wavlets, by subscribing
	to multiple routing keys.
	In this scenario, a message may be sent to a queue, that the participant has
	not subscribed to. This is intended and it's RabbitMQ's job to drop the
	message. This keeps things simple for now and may be changed in the future.
	
	Message comsumers can be optimized in a multi-threading environment. If
	one consumer starts handling messages of a particular wavelet, it should
	block others from handling them.
	
	Some messages are handled synchronously (i.e. the client does not perform
	any actions and waits for the server's response). Those are in particular:
	WAVELET_ADD_PARTICIPANT
	
	"""
	
	logger = logging.getLogger("pygowave")
	conn_lifetime = datetime.timedelta(minutes=getattr(settings, "ACCESS_KEY_TIMEOUT_MINUTES", 2))
	
	def process(self, routing_key, message_data):
		"""
		Process a message in JSON format with the given routing key (= sender ID).
		This function may generate new messages which should be sent by the caller.
		The returned message dictionary has the form {routing_key: message, ...}.
		"""
		
		participant_conn_key, wavelet_id, message_category = routing_key.split(".")
		
		if message_category != "clientop":
			return {}
		
		self.logger.debug("Received Message from %s.%s.%s:\n%s" % (participant_conn_key, wavelet_id, message_category, repr(message_data)))
		
		# Get participant connection
		try:
			pconn = ParticipantConn.objects.get(tx_key=participant_conn_key)
		except ObjectDoesNotExist:
			self.logger.error("{%s} ParticipantConn not found" % (routing_key))
			return {} # Fail silently
		
		# Get wavelet
		try:
			wavelet = pconn.participant.wavelets.get(id=wavelet_id)
		except ObjectDoesNotExist:
			self.logger.error("{%s} Wavelet not found (or not participating)" % (routing_key))
			return {} # Fail silently
		
		# Handle message and reply to sender and/or broadcast an event
		self.out_queue = {}
		if isinstance(message_data, list): # multi-message?
			for sub_message in message_data:
				try:
					if not self.handle_participant_message(wavelet, pconn, sub_message): break
				except:
					self.logger.exception("{%s} Exception in message handler" % (routing_key))
					break
		else:
			try:
				self.handle_participant_message(wavelet, pconn, message_data)
			except:
				self.logger.exception("{%s} Exception in message handler" % (routing_key))
		
		# Create message dictionary
		msg_dict = {}
		for receiver, messages in self.out_queue.iteritems():
			msg_dict["%s.%s.waveop" % (receiver, wavelet_id)] = messages
		self.out_queue = {}
		
		return msg_dict
	
	def handle_participant_message(self, wavelet, pconn, message):
		"""
		Handle a participant's operation.
		If True is returned, go on with processing the next message.
		If False is returned, discard any following messages.
		
		"""
		participant = pconn.participant
		pconn.last_contact = datetime.datetime.now()
		pconn.save()
		
		if message.has_key(u"type"):
			
			if message["type"] == "PING":
				self.emit(pconn, "PONG", message["property"]) # Traditionally
			
			elif message["type"] == "WAVELET_OPEN":
				self.logger.info("[%s/%s@%s] Opening wavelet" % (participant.name, pconn.id, wavelet.wave.id))
				pconn.wavelets.add(wavelet)
				# I know this is neat :)
				self.emit(pconn, "WAVELET_OPEN", {
					"wavelet": wavelet.serialize(),
					"blips": wavelet.serialize_blips(),
				})
			
			elif message["type"] == "PARTICIPANT_INFO":
				self.logger.info("[%s/%s@%s] Sending participant information" % (participant.name, pconn.id, wavelet.wave.id))
				p_info = {}
				for p_id in message["property"]:
					try:
						p_info[p_id] = Participant.objects.get(id=p_id).serialize()
					except ObjectDoesNotExist:
						p_info[p_id] = None
				self.emit(pconn, "PARTICIPANT_INFO", p_info)
			
			elif message["type"] == "PARTICIPANT_SEARCH":
				if len(message["property"]) < getattr(settings, "PARTICIPANT_SEARCH_LENGTH", 0):
					self.emit(pconn, "PARTICIPANT_SEARCH", {"result": "TOO_SHORT", "data": getattr(settings, "PARTICIPANT_SEARCH_LENGTH", 0)})
					self.logger.debug("[%s/%s@%s] Participant search query too short" % (participant.name, pconn.id, wavelet.wave.id))
				else:
					self.logger.info("[%s/%s@%s] Performing participant search" % (participant.name, pconn.id, wavelet.wave.id))
					
					lst = []
					for p in Participant.objects.filter(name__icontains=message["property"]).exclude(id=participant.id):
						lst.append(p.id)
					self.emit(pconn, "PARTICIPANT_SEARCH", {"result": "OK", "data": lst})
			
			elif message["type"] == "GADGET_LIST":
				all_gadgets = map(lambda g: {"id": g.id, "uploaded_by": g.by_user.participants.all()[0].name, "name": g.title, "descr": g.description, "url": g.url}, Gadget.objects.all())
				self.logger.info("[%s/%s@%s] Sending Gadget list" % (participant.name, pconn.id, wavelet.wave.id))
				self.emit(pconn, "GADGET_LIST", all_gadgets)
			
			elif message["type"] == "WAVELET_ADD_PARTICIPANT":
				# Find participant
				try:
					p = Participant.objects.get(id=message["property"])
				except ObjectDoesNotExist:
					self.logger.error("[%s/%s@%s] Target participant '%s' not found" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
					return True # Fail silently (TODO: report error to user)
				# Check if already participating
				if wavelet.participants.filter(id=message["property"]).count() > 0:
					self.logger.error("[%s/%s@%s] Target participant '%s' already there" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
					return True # Fail silently (TODO: report error to user)
				wavelet.participants.add(p)
				self.logger.info("[%s/%s@%s] Added new participant '%s'" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
				self.broadcast(wavelet, "WAVELET_ADD_PARTICIPANT", message["property"])
				
			elif message["type"] == "WAVELET_REMOVE_SELF":
				self.broadcast(wavelet, "WAVELET_REMOVE_PARTICIPANT", participant.id)
				wavelet.participants.remove(participant) # Bye bye
				pconn.wavelets.remove(wavelet) # Also for your connection
				self.logger.info("[%s/%s@%s] Participant removed himself" % (participant.name, pconn.id, wavelet.wave.id))
				if wavelet.participants.count() == 0: # Oh my god, you killed the Wave! You bastard!
					self.logger.info("[%s/%s@%s] Wave got killed!" % (participant.name, pconn.id, wavelet.wave.id))
					wavelet.wave.delete()
				return False
			
			elif message["type"] == "OPERATION_MESSAGE_BUNDLE":
				# Build OpManager
				newdelta = OpManager(wavelet.wave.id, wavelet.id)
				newdelta.unserialize(message["property"]["operations"])
				version = message["property"]["version"]
				
				# Transform
				for delta in wavelet.deltas.filter(version__gt=version):
					for op in delta.getOpManager().operations:
						newdelta.transform(op) # Trash results (an existing delta cannot be changed)
				
				# Apply
				old_blipsums = wavelet.blipsums()
				wavelet.applyOperations(newdelta.operations)
				
				# Raise version and store
				wavelet.version += 1
				wavelet.save()
				
				Delta.createByOpManager(newdelta, wavelet.version).save()
				
				# Create tentative checksums
				blipsums = wavelet.blipsums()
				
				# Respond
				self.emit(pconn, "OPERATION_MESSAGE_BUNDLE_ACK", {"version": wavelet.version, "blipsums": blipsums})
				self.broadcast(wavelet, "OPERATION_MESSAGE_BUNDLE", {"version": wavelet.version, "operations": newdelta.serialize(), "blipsums": blipsums}, [pconn])
				
				self.logger.debug("[%s/%s@%s] Processed delta #%d -> v%d" % (participant.name, pconn.id, wavelet.wave.id, version, wavelet.version))
				
			else:
				self.logger.error("[%s/%s@%s] Unknown message: %s" % (participant.name, pconn.id, wavelet.wave.id, message))
		
		else:
			self.logger.error("[%s/%s@%s] Unknown message: %s" % (participant.name, pconn.id, wavelet.wave.id, message))
		
		return True
	
	def broadcast(self, wavelet, type, property, except_connections=[]):
		"""
		Send messages to all participants.
		
		`wavelet` must be a Wavelet object.
		`except_connections` is a list of ParticipantConn objects to be
		excluded from the broadcast.
		
		"""
		except_connections = map(lambda c: c.id, except_connections)
		msg_dict = {
			"type": type,
			"property": property
		}
		self.logger.debug("Broadcasting Message:\n" + repr(msg_dict))
		for p in wavelet.participants.all():
			for conn in p.connections.all():
				if not conn.id in except_connections:
					if self.out_queue.has_key(conn.rx_key):
						self.out_queue[conn.rx_key].append(msg_dict)
					else:
						self.out_queue[conn.rx_key] = [msg_dict]
	
	def emit(self, to, type, property, except_connections=[]):
		"""
		Collect messages to be sent.
		`to` must be a ParticipantConn object.
		
		"""
		msg_dict = {
			"type": type,
			"property": property
		}
		self.logger.debug("Emiting Message to %s/%s:\n%s" % (to.participant.name, to.id, repr(msg_dict)))
		if self.out_queue.has_key(to.rx_key):
			self.out_queue[to.rx_key].append(msg_dict)
		else:
			self.out_queue[to.rx_key] = [msg_dict]
	
	def purge_connections(self):
		"""
		This method will check all connections to the server and throw out
		everyone which has timed out.
		This can be run asynchronously every 10 minutes or so.
		
		"""
		for conn in ParticipantConn.objects.all():
			if datetime.datetime.now() > conn.last_contact + self.conn_lifetime:
				conn_id, conn_participant_name = conn.id, conn.participant.name
				for wavelet in conn.wavelets.all():
					wavelet.participant_conns.remove(conn)
					self.logger.info("[%s/%s@%s] Connection to wavelet closed" % (conn.participant.name, conn_id, wavelet.wave.id))
				conn.delete()
				self.logger.info("[%s/%s] Connection to server closed" % (conn_participant_name, conn_id))
