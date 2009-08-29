
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

import sys, datetime, logging
import logging.handlers

from carrot.connection import DjangoAMQPConnection
from carrot.messaging import Consumer, Publisher
from carrot.backends import DefaultBackend
from django.core.exceptions import ObjectDoesNotExist

from pygowave_server.models import Participant, ParticipantConn, Gadget, GadgetElement, Delta
from pygowave_server.common.operations import OpManager
from django.conf import settings

logger = logging.getLogger("pygowave")

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

class PyGoWaveMessageProcessor(object):
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
	
	purge_every = datetime.timedelta(minutes=10)
	conn_min_lifetime = datetime.timedelta(minutes=getattr(settings, "ACCESS_KEY_TIMEOUT_MINUTES", 2))
	
	def __init__(self, connection):
		self.consumer = Consumer(
			connection,
			queue="wavelet_rpc_singlethread",
			exchange="wavelet.topic",
			routing_key="#.#.clientop",
			exchange_type="topic",
			serializer="json",
			auto_ack=True,
		)
		self.consumer.register_callback(self.receive)
		self.publisher = Publisher(
			connection,
			exchange="wavelet.direct",
			exchange_type="direct",
			delivery_mode=1,
			serializer="json",
		)
		
		self.out_queue = {}
		self.purge_connections()
	
	def broadcast(self, wavelet, type, property, except_connections=[]):
		"""
		Send messages to all participants.
		
		`wavelet` must be a Wavelet object.
		`except_connections` is a list of ParticipantConn objects to be
		excluded from the broadcast.
		
		"""
		msg_dict = {
			"type": type,
			"property": property
		}
		logger.debug("Broadcasting Message:\n" + repr(msg_dict))
		for p in wavelet.participants.all():
			for conn in p.connections.all():
				if not conn in except_connections:
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
		logger.debug("Emiting Message to %s/%d:\n%s" % (to.participant.name, to.id, repr(msg_dict)))
		if self.out_queue.has_key(to.rx_key):
			self.out_queue[to.rx_key].append(msg_dict)
		else:
			self.out_queue[to.rx_key] = [msg_dict]
	
	def wait(self, limit=None):
		self.consumer.wait(limit)
	
	def send(self, message_data, routing_key):
		self.publisher.send(message_data, routing_key=routing_key, delivery_mode=1)
	
	def receive(self, message_data, message):
		rkey = message.amqp_message.routing_key
		participant_conn_key, wavelet_id, message_category = rkey.split(".")
		
		if message_category != "clientop":
			return
		
		logger.debug("Received Message from %s.%s.%s:\n%s" % (participant_conn_key, wavelet_id, message_category, repr(message_data)))
		
		# Get participant connection
		try:
			pconn = ParticipantConn.objects.get(tx_key=participant_conn_key)
		except ObjectDoesNotExist:
			logger.error("{%s} ParticipantConn not found" % (rkey))
			return # Fail silently
		
		# Get wavelet
		try:
			wavelet = pconn.participant.wavelets.get(id=wavelet_id)
		except ObjectDoesNotExist:
			logger.error("{%s} Wavelet not found (or not participating)" % (rkey))
			return # Fail silently
		
		# Handle message and reply to sender and/or broadcast an event
		self.out_queue = {}
		if isinstance(message_data, list): # multi-message?
			for sub_message in message_data:
				if not self.handle_participant_message(wavelet, pconn, sub_message): break
		else:
			self.handle_participant_message(wavelet, pconn, message_data)
		for receiver, messages in self.out_queue.iteritems():
			self.send(messages, "%s.%s.waveop" % (receiver, wavelet_id))
		self.out_queue = {}
		
		# Cleanup time?
		if datetime.datetime.now() > self.next_purge:
			self.purge_connections()
	
	def handle_participant_message(self, wavelet, pconn, message):
		"""
		Handle a participant's operation.
		If True is returned, go on with processing the next message.
		If False is returned, discard any following messages.
		
		"""
		participant = pconn.participant
		
		if message.has_key(u"type"):
			
			if message["type"] == "WAVELET_OPEN":
				logger.info("[%s/%d@%s] Opening wavelet" % (participant.name, pconn.id, wavelet.wave.id))
				pconn.wavelets.add(wavelet)
				# I know this is neat :)
				self.emit(pconn, "WAVELET_OPEN", {
					"wavelet": wavelet.serialize(),
					"blips": wavelet.serialize_blips(),
				})
			
			elif message["type"] == "PARTICIPANT_INFO":
				logger.info("[%s/%d@%s] Sending participant information" % (participant.name, pconn.id, wavelet.wave.id))
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
					logger.debug("[%s/%d@%s] Participant search query too short" % (participant.name, pconn.id, wavelet.wave.id))
				else:
					logger.info("[%s/%d@%s] Performing participant search" % (participant.name, pconn.id, wavelet.wave.id))
					
					lst = []
					for p in Participant.objects.filter(name__icontains=message["property"]).exclude(id=participant.id):
						lst.append(p.id)
					self.emit(pconn, "PARTICIPANT_SEARCH", {"result": "OK", "data": lst})
			
			elif message["type"] == "GADGET_LIST":
				all_gadgets = map(lambda g: {"id": g.id, "uploaded_by": g.by_user.participants.all()[0].name, "name": g.title, "descr": g.description, "url": g.url}, Gadget.objects.all())
				logger.info("[%s/%d@%s] Sending Gadget list" % (participant.name, pconn.id, wavelet.wave.id))
				self.emit(pconn, "GADGET_LIST", all_gadgets)
			
			elif message["type"] == "WAVELET_ADD_PARTICIPANT":
				# Find participant
				try:
					p = Participant.objects.get(id=message["property"])
				except ObjectDoesNotExist:
					logger.error("[%s/%d@%s] Target participant '%s' not found" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
					return # Fail silently (TODO: report error to user)
				# Check if already participating
				if wavelet.participants.filter(id=message["property"]).count() > 0:
					logger.error("[%s/%d@%s] Target participant '%s' already there" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
					return # Fail silently (TODO: report error to user)
				wavelet.participants.add(p)
				logger.info("[%s/%d@%s] Added new participant '%s'" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
				self.broadcast(wavelet, "WAVELET_ADD_PARTICIPANT", message["property"])
				
			elif message["type"] == "WAVELET_REMOVE_SELF":
				self.broadcast(wavelet, "WAVELET_REMOVE_PARTICIPANT", participant.id)
				wavelet.participants.remove(participant) # Bye bye
				pconn.wavelets.remove(wavelet) # Also for your connection
				logger.info("[%s/%d@%s] Participant removed himself" % (participant.name, pconn.id, wavelet.wave.id))
				if wavelet.participants.count() == 0: # Oh my god, you killed the Wave! You bastard!
					logger.info("[%s/%d@%s] Wave got killed!" % (participant.name, pconn.id, wavelet.wave.id))
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
				
				logger.debug("[%s/%d@%s] Processed delta #%d -> v%d" % (participant.name, pconn.id, wavelet.wave.id, version, wavelet.version))
				
			else:
				logger.error("[%s/%d@%s] Unknown message: %s" % (participant.name, pconn.id, wavelet.wave.id, message))
		
		else:
			logger.error("[%s/%d@%s] Unknown message: %s" % (participant.name, pconn.id, wavelet.wave.id, message))
		
		return True
	
	def purge_connections(self):
		"""
		This method will check all connections to the server and throw out all
		closed ones.
		This can be run asynchronously every 10 minutes or so.
		
		"""

		for conn in ParticipantConn.objects.all():
			for wavelet in conn.wavelets.all():
				if not self.queue_exists("%s.%s.waveop" % (conn.rx_key, wavelet.id)):
					wavelet.participant_conns.remove(conn)
					logger.info("[%s/%d@%s] Connection to wavelet closed" % (conn.participant.name, conn.id, wavelet.wave.id))
			if conn.wavelets.count() == 0 and datetime.datetime.now() > conn.created + self.conn_min_lifetime:
				conn_id, conn_participant_name = conn.id, conn.participant.name
				conn.delete()
				logger.info("[%s/%d] Connection to server closed" % (conn_participant_name, conn_id))
		self.next_purge = datetime.datetime.now() + self.purge_every

	def queue_exists(self, queue):
		"""
		Check if a queue exists, i.e. a user is connected to it (because
		auto-delete is always turned on).
		
		"""
		
		logger.debug("Checking queue %s" % (queue))
		qex = self.publisher.backend.queue_exists(queue)
		
		# Re-open the channel if it was closed (this is a pyamqlib issue)
		if self.publisher.backend.channel.connection == None:
			self.publisher.backend.channel = self.publisher.backend.connection.connection.channel()
		
		return qex

# Single threaded for now

if __name__ == '__main__':
	logger.setLevel(logging.INFO)
	log_formatter = logging.Formatter('%(asctime)s %(name)-8s -- %(levelname)-5s %(message)s')
	
	amqplogger = None
	if "--verbose" in sys.argv:
		logger.setLevel(logging.DEBUG)
		amqplogger = logging.getLogger("amqplib")
		amqplogger.setLevel(logging.DEBUG)
	
	if "--quiet" in sys.argv:
		logger.setLevel(logging.CRITICAL)
	
	if "--logfile" in sys.argv:
		info_handler = logging.handlers.WatchedFileHandler("/var/log/pygowave/info.log", 'a', 'utf-8')
		info_handler.setLevel(logging.INFO)
		class InfoOnlyFilter:
			def filter(self, record): return record.levelno == logging.INFO
		info_handler.addFilter(InfoOnlyFilter())
		info_handler.setFormatter(log_formatter)
		logger.addHandler(info_handler)
		
		error_handler = logging.handlers.WatchedFileHandler("/var/log/pygowave/error.log", 'a', 'utf-8')
		error_handler.setLevel(logging.ERROR)
		error_handler.setFormatter(log_formatter)
		logger.addHandler(error_handler)
	else:
		log_handler = logging.StreamHandler()
		log_handler.setFormatter(log_formatter)
		logger.addHandler(log_handler)
		if amqplogger != None:
			amqplogger.addHandler(log_handler)
	
	logger.info("=> RabbitMQ RPC Server starting <=")
	
	import signal
	# Python Ctrl-C handler
	signal.signal(signal.SIGINT, signal.SIG_DFL)
	
	try:
		amqpconn = DjangoAMQPConnection()
		omc = PyGoWaveMessageProcessor(amqpconn)
		logger.info("=> RabbitMQ RPC Server ready <=")
		omc.wait()
	except:
		import traceback
		logger.critical("Crash!\n" + traceback.format_exc())
		sys.exit(1)
