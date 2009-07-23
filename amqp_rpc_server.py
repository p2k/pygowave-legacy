
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

from carrot.connection import DjangoAMQPConnection
from carrot.messaging import Consumer, Publisher
from carrot.backends import DefaultBackend
from django.core.exceptions import ObjectDoesNotExist

from pygowave_server.models import Participant, ParticipantConn, Gadget, GadgetElement
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
# WAVELET_ADD_PARTICIPANT (sync)
# WAVELET_REMOVE_SELF (sync)
# DOCUMENT_ELEMENT_REPLACE (sync)
# DOCUMENT_ELEMENT_DELTA (async)

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
	conn_min_lifetime = datetime.timedelta(minutes=getattr(settings, "ACCESS_KEY_TIMEOUT_MINUTES", 5))
	
	def __init__(self, connection):
		self.consumer = Consumer(
			connection,
			queue="wavelet_rpc_singlethread",
			exchange="wavelet.topic",
			routing_key="#.#.clientop",
			exchange_type="topic",
			serializer="json",
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
		Send messages to all participants. Should only be used with synchronous
		events.
		`wavelet` must be a Wavelet object.
		`except_participants` is a list of ParticipantConn objects to be
		excluded from the broadcast.
		
		"""
		msg_dict = {
			"type": type,
			"property": property
		}
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
			logger.error("{%s} Error: ParticipantConn not found\n" % (rkey))
			message.ack()
			return # Fail silently
		
		# Get wavelet
		try:
			wavelet = pconn.participant.wavelets.get(id=wavelet_id)
		except ObjectDoesNotExist:
			logger.error("{%s} Error: Wavelet not found (or not participating)\n" % (rkey))
			message.ack()
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
		
		message.ack()
		
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
				# Hand over info in Google's format
				self.broadcast(wavelet, "WAVELET_ADD_PARTICIPANT", p.serialize())
				
			elif message["type"] == "WAVELET_REMOVE_SELF":
				self.broadcast(wavelet, "WAVELET_REMOVE_PARTICIPANT", participant.id)
				wavelet.participants.remove(participant) # Bye bye
				pconn.wavelets.remove(wavelet) # Also for your connection
				logger.info("[%s/%d@%s] Participant removed himself" % (participant.name, pconn.id, wavelet.wave.id))
				if wavelet.participants.count() == 0: # Oh my god, you killed the Wave! You bastard!
					logger.info("[%s/%d@%s] Wave got killed!" % (participant.name, pconn.id, wavelet.wave.id))
					wavelet.wave.delete()
				return False
			
			elif message["type"] == "DOCUMENT_ELEMENT_REPLACE":
				# Find Gadget
				try:
					g = Gadget.objects.get(pk=message["property"])
				except ObjectDoesNotExist:
					logger.error("[%s/%d@%s] Gadget #%s not found" % (participant.name, pconn.id, wavelet.wave.id, message["property"]))
					return # Fail silently (TODO: report error to user)
				
				blip = wavelet.root_blip
				if blip.elements.count() > 0:
					blip.elements.all().delete()
				
				ge = g.instantiate()
				ge.blip = blip
				ge.position = 0
				ge.save()
				
				logger.info("[%s/%d@%s] Gadget #%s (%s) set -> GadgetElement #%d" % (participant.name, pconn.id, wavelet.wave.id, message["property"], g.title, ge.id))
				
				self.broadcast(wavelet, "DOCUMENT_ELEMENT_REPLACE", {"url": ge.url, "id": ge.id, "data": {}})
			
			elif message["type"] == "DOCUMENT_ELEMENT_DELTA":
				elt_id = int(message["property"]["id"])
				delta = message["property"]["delta"]
				# Find GadgetElement
				try:
					ge = GadgetElement.objects.get(pk=elt_id, blip=wavelet.root_blip)
				except ObjectDoesNotExist:
					logger.error("[%s/%d@%s] GadgetElement #%d not found (or not accessible)" % (participant.name, pconn.id, wavelet.wave.id, elt_id))
					return # Fail silently (TODO: report error to user)
				
				ge.apply_delta(delta) # Apply delta and save
				logger.info("[%s/%d@%s] Applied delta to GadgetElement #%d" % (participant.name, pconn.id, wavelet.wave.id, elt_id))
				
				# Asynchronous event, so send to all part. except the sender
				self.broadcast(wavelet, "DOCUMENT_ELEMENT_DELTA", {"id": elt_id, "delta": delta}, [pconn])
			
			elif message["type"] == "DOCUMENT_ELEMENT_SETPREF":
				elt_id = int(message["property"]["id"])
				key = message["property"]["key"]
				value = message["property"]["value"]
				# Find GadgetElement
				try:
					ge = GadgetElement.objects.get(pk=elt_id, blip=wavelet.root_blip)
				except ObjectDoesNotExist:
					logger.error("[%s/%d@%s] GadgetElement #%d not found (or not accessible)" % (participant.name, pconn.id, wavelet.wave.id, elt_id))
					return # Fail silently (TODO: report error to user)
				
				ge.set_userpref(key, value)
				logger.info("[%s/%d@%s] Set UserPref '%s' on GadgetElement #%d" % (participant.name, pconn.id, wavelet.wave.id, key, elt_id))
				
				# Asynchronous event, so send to all part. except the sender
				self.broadcast(wavelet, "DOCUMENT_ELEMENT_SETPREF", {"id": elt_id, "key": key, "value": value}, [pconn])
		
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
		info_handler = logging.FileHandler("/var/log/pygowave/info", 'a', 'utf-8')
		error_handler.setLevel(logging.INFO)
		class InfoOnlyFilter:
			def filter(self, record): return record.levelno == logging.INFO
		info_handler.addFilter(InfoOnlyFilter())
		info_handler.setFormatter(log_formatter)
		logger.addHandler(info_handler)
		
		error_handler = logging.FileHandler("/var/log/pygowave/error", 'a', 'utf-8')
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
	
	amqpconn = DjangoAMQPConnection()
	omc = PyGoWaveMessageProcessor(amqpconn)
	logger.info("=> RabbitMQ RPC Server ready <=")
	omc.wait()
