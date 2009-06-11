
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

from carrot.connection import DjangoAMQPConnection
from carrot.messaging import Messaging, Consumer, Publisher
from carrot.backends import DefaultBackend
from django.core.exceptions import ObjectDoesNotExist

from pygowave_server.models import Participant

class TopicConsumer(Consumer):
	exchange_type = "topic"

class TopicPublisher(Publisher):
	exchange_type = "topic"

# Progress tracker - implemented messages
# Legend:
# (sync) - message is handled synchronously i.e. the client does not perform
#          any actions and waits for the server's response
# (OT)   - full Operational Transformation implemented
#
# --------
#
# WAVELET_ADD_PARTICIPANT (sync)

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
	
	def receive(self, message_data, message):
		participant_key, wavelet_id, message_category = message.amqp_message.routing_key.split(".")
		
		if message_category != "clientop":
			return
		
		print "Received Message from %s.%s.%s:\n%s" % (participant_key, wavelet_id, message_category, repr(message_data))
		
		# Get participant
		try:
			participant = Participant.objects.get(tx_key=participant_key)
		except ObjectDoesNotExist:
			print "-- Participant not found"
			return # Fail silently
		
		# Get wavelet
		try:
			wavelet = participant.wavelets.get(id=wavelet_id)
		except ObjectDoesNotExist:
			print "-- Wavelet not found"
			return # Fail silently
		
		# Handle message and reply to sender
		self.out_queue = {}
		if isinstance(message_data, list): # multi-message?
			for sub_message in message_data:
				self.handle_participant_message(wavelet, participant, sub_message)
		else:
			self.handle_participant_message(wavelet, participant, message_data)
		for receiver, messages in self.out_queue.iteritems():
			self.send(messages, "%s.%s.waveop" % (receiver, wavelet_id))
		self.out_queue = {}
		
		message.ack()
	
	def handle_participant_message(self, wavelet, participant, message):
		if message.has_key(u"pygowave"): # Special message
			if message["pygowave"] == "hi": # Login message
				# Emit participant added messages
				for p in wavelet.participants.all():
					self.emit(participant, "WAVELET_ADD_PARTICIPANT", p.id)
		elif message.has_key(u"type"): # Regular message
			if message["type"] == "WAVELET_ADD_PARTICIPANT":
				# Find participant
				try:
					p = Participant.objects.get(id=message["property"])
				except ObjectDoesNotExist:
					print "-- Target participant not found"
					return # Fail silently (TODO: report error to user)
				# Check if already participating
				if wavelet.participants.filter(id=message["property"]).count() > 0:
					print "-- Target participant already there"
					return # Fail silently (TODO: report error to user)
				wavelet.participants.add(p)
				print "-- Added new participant"
				self.broadcast(wavelet, "WAVELET_ADD_PARTICIPANT", message["property"])
	
	def broadcast(self, wavelet, type, property):
		"""
		Send messages to all participants. Should only be used with synchronous
		events.
		`wavelet` must be a Wavelet object.
		
		"""
		for p in wavelet.participants.all():
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

# Single threaded for now

if __name__ == '__main__':
	print "=> RabbitMQ RPC Server starting <="
	
	import signal
	# Python Ctrl-C handler
	signal.signal(signal.SIGINT, signal.SIG_DFL)
	
	amqpconn = DjangoAMQPConnection()
	omc = PyGoWaveMessageProcessor(amqpconn)
	omc.wait()
