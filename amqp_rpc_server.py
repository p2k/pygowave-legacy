
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

import sys, datetime

from carrot.connection import DjangoAMQPConnection
from carrot.messaging import Messaging, Consumer, Publisher
from carrot.backends import DefaultBackend
from django.core.exceptions import ObjectDoesNotExist

from pygowave_server.models import Participant, Gadget, GadgetElement

class TopicConsumer(Consumer):
	exchange_type = "topic"

class TopicPublisher(Publisher):
	exchange_type = "topic"

verbose = False
quiet = False
if "--verbose" in sys.argv:
	verbose = True

class Logger:
	def __init__(self, std, err):
		self.std = std
		self.err = err
	
	def __log_to(self, f, text):
		f.write("%s -- %s\n" % (datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"), text))
		f.flush()

	def error(self, text):
		self.__log_to(self.err, text)
	
	def info(self, text):
		self.__log_to(self.std, text)

if "--logfile" in sys.argv:
	logger = Logger(open("/var/log/pygowave/info","a"), open("/var/log/pygowave/error","a"))
elif "--quiet" in sys.argv:
	verbose = False
	logger = Logger(open("/dev/null"), open("/dev/null"))
else:
	logger = Logger(sys.stdout, sys.stderr)

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

class PyGoWaveMessageProcessor(Messaging):
	"""
	Handle all incoming messages.
	
	Routing key structure:
	<participant_guid>.<wavelet_id>.[waveop|clientop]
	
	participant_guid is created every time the user enters the wave viewer.
	Only one instance of the wave viewer can be opened per user at the same time.
	
	Message comsumers can be optimized in a multi-threading environment. If
	one consumer starts handling messages of a particular wavelet, it should
	block others from handling them.
	
	Some messages are handled synchronously (i.e. the client does not perform
	any actions and waits for the server's response). Those are in particular:
	WAVELET_ADD_PARTICIPANT
	
	"""
	queue = "wavelet_rpc_singlethread"
	exchange = "wavelet.topic"
	routing_key = "#.#.clientop"
	
	publisher_cls = TopicPublisher
	consumer_cls = TopicConsumer
	
	def __init__(self, connection):
		super(PyGoWaveMessageProcessor, self).__init__(connection, backend_cls=DefaultBackend)
		
		self.out_queue = {}
	
	def broadcast(self, wavelet, type, property, except_participants=[]):
		"""
		Send messages to all participants. Should only be used with synchronous
		events.
		`wavelet` must be a Wavelet object.
		`except_participants` is a list of Participant objects to be excluded from the broadcast.
		
		"""
		for p in wavelet.participants.all():
			if p not in except_participants:
				self.emit(p, type, property)
	
	def emit(self, to, type, property):
		"""
		Collect messages to be sent.
		`to` must be a Participant object.
		
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
		participant_key, wavelet_id, message_category = rkey.split(".")
		
		if message_category != "clientop":
			return
		
		if verbose: print "Received Message from %s.%s.%s:\n%s" % (participant_key, wavelet_id, message_category, repr(message_data))
		
		# Get participant
		try:
			participant = Participant.objects.get(tx_key=participant_key)
		except ObjectDoesNotExist:
			logger.error("{%s} Error: Participant not found\n" % (rkey))
			return # Fail silently
		
		# Get wavelet
		try:
			wavelet = participant.wavelets.get(id=wavelet_id)
		except ObjectDoesNotExist:
			logger.error("{%s} Error: Wavelet not found\n" % (rkey))
			return # Fail silently
		
		# Handle message and reply to sender and/or broadcast an event
		self.out_queue = {}
		if isinstance(message_data, list): # multi-message?
			for sub_message in message_data:
				if not self.handle_participant_message(wavelet, participant, sub_message): break
		else:
			self.handle_participant_message(wavelet, participant, message_data)
		for receiver, messages in self.out_queue.iteritems():
			self.send(messages, "%s.%s.waveop" % (receiver, wavelet_id))
		self.out_queue = {}
		
		message.ack()
	
	def handle_participant_message(self, wavelet, participant, message):
		"""
		Handle a participant's operation.
		If True is returned, go on with processing the next message.
		If False is returned, discard any following messages.
		
		"""
		if message.has_key(u"pygowave"): # Special message
			if message["pygowave"] == "hi": # Login message
				logger.info("[%s@%s] Hi!" % (participant.name, wavelet.wave.id))
				# Emit participant added messages
				for p in wavelet.participants.all():
					self.emit(participant, "WAVELET_ADD_PARTICIPANT", p.serialize())
				# Emit gadget
				if wavelet.root_blip.elements.count() > 0:
					ge = wavelet.root_blip.elements.all()[0].to_gadget()
					self.emit(participant, "DOCUMENT_ELEMENT_REPLACE", {"url": ge.url, "id": ge.id, "data": ge.get_data()})
		elif message.has_key(u"type"): # Regular message
			
			if message["type"] == "WAVELET_ADD_PARTICIPANT":
				# Find participant
				try:
					p = Participant.objects.get(id=message["property"])
				except ObjectDoesNotExist:
					logger.error("[%s@%s] Target participant '%s' not found" % (participant.name, wavelet.wave.id, message["property"]))
					return # Fail silently (TODO: report error to user)
				# Check if already participating
				if wavelet.participants.filter(id=message["property"]).count() > 0:
					logger.error("[%s@%s] Target participant '%s' already there" % (participant.name, wavelet.wave.id, message["property"]))
					return # Fail silently (TODO: report error to user)
				wavelet.participants.add(p)
				logger.info("[%s@%s] Added new participant '%s'" % (participant.name, wavelet.wave.id, message["property"]))
				# Hand over info in Google's format
				self.broadcast(wavelet, "WAVELET_ADD_PARTICIPANT", p.serialize())
				
			elif message["type"] == "WAVELET_REMOVE_SELF":
				self.broadcast(wavelet, "WAVELET_REMOVE_PARTICIPANT", participant.id)
				wavelet.participants.remove(participant) # Bye bye
				logger.info("[%s@%s] Participant removed himself" % (participant.name, wavelet.wave.id))
				if wavelet.participants.count() == 0: # Oh my god, you killed the Wave! You bastard!
					logger.info("[%s@%s] Wave got killed!" % (participant.name, wavelet.wave.id))
					wavelet.wave.delete()
				return False
			
			elif message["type"] == "DOCUMENT_ELEMENT_REPLACE":
				# Find Gadget
				try:
					g = Gadget.objects.get(pk=message["property"])
				except ObjectDoesNotExist:
					logger.error("[%s@%s] Gadget #%s not found" % (participant.name, wavelet.wave.id, message["property"]))
					return # Fail silently (TODO: report error to user)
				
				blip = wavelet.root_blip
				if blip.elements.count() > 0:
					blip.elements.all().delete()
				
				ge = g.instantiate()
				ge.blip = blip
				ge.position = 0
				ge.save()
				
				logger.info("[%s@%s] Gadget #%s (%s) set -> GadgetElement #%d" % (participant.name, wavelet.wave.id, message["property"], g.title, ge.id))
				
				self.broadcast(wavelet, "DOCUMENT_ELEMENT_REPLACE", {"url": ge.url, "id": ge.id, "data": {}})
			
			elif message["type"] == "DOCUMENT_ELEMENT_DELTA":
				elt_id = int(message["property"]["id"])
				delta = message["property"]["delta"]
				# Find GadgetElement
				try:
					ge = GadgetElement.objects.get(pk=elt_id, blip=wavelet.root_blip)
				except ObjectDoesNotExist:
					logger.error("[%s@%s] GadgetElement #%d not found (or not accessible)" % (participant.name, wavelet.wave.id, elt_id))
					return # Fail silently (TODO: report error to user)
				
				ge.apply_delta(delta) # Apply delta and save
				logger.info("[%s@%s] Applied delta to GadgetElement #%d" % (participant.name, wavelet.wave.id, elt_id))
				
				# Asynchronous event, so send to all part. except the sender
				self.broadcast(wavelet, "DOCUMENT_ELEMENT_DELTA", {"id": elt_id, "delta": delta}, [participant])
			
			elif message["type"] == "DOCUMENT_ELEMENT_SETPREF":
				elt_id = int(message["property"]["id"])
				key = message["property"]["key"]
				value = message["property"]["value"]
				# Find GadgetElement
				try:
					ge = GadgetElement.objects.get(pk=elt_id, blip=wavelet.root_blip)
				except ObjectDoesNotExist:
					logger.error("[%s@%s] GadgetElement #%d not found (or not accessible)" % (participant.name, wavelet.wave.id, elt_id))
					return # Fail silently (TODO: report error to user)
				
				ge.set_userpref(key, value)
				logger.info("[%s@%s] Set UserPref '%s' on GadgetElement #%d" % (participant.name, wavelet.wave.id, key, elt_id))
				
				# Asynchronous event, so send to all part. except the sender
				self.broadcast(wavelet, "DOCUMENT_ELEMENT_SETPREF", {"id": elt_id, "key": key, "value": value}, [participant])
		
		return True

# Single threaded for now

if __name__ == '__main__':
	logger.info("=> RabbitMQ RPC Server starting <=")
	
	import signal
	# Python Ctrl-C handler
	signal.signal(signal.SIGINT, signal.SIG_DFL)
	
	amqpconn = DjangoAMQPConnection()
	omc = PyGoWaveMessageProcessor(amqpconn)
	omc.wait()
