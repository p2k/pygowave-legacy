
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

import datetime
from carrot.messaging import Consumer, Publisher

from c2s_mp import PyGoWaveClientMessageProcessor

from django.conf import settings
from pygowave_server.models import ParticipantConn

__all__ = ["AmqpMessageProcessor"]

class AmqpMessageProcessor(object):
	purge_every = datetime.timedelta(minutes=10)
	conn_lifetime = datetime.timedelta(minutes=getattr(settings, "ACCESS_KEY_TIMEOUT_MINUTES", 2))
	
	def __init__(self, connection):
		self.pygo_mp = PyGoWaveClientMessageProcessor()
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
		
		self.pygo_mp.purge_connections()
	
	def wait(self, limit=None):
		self.consumer.wait(limit)
	
	def send(self, routing_key, message_data):
		self.publisher.send(message_data, routing_key=routing_key, delivery_mode=1)
	
	def receive(self, message_data, message):
		routing_key = message.amqp_message.routing_key
		
		msg_dict = self.pygo_mp.process(routing_key, message_data)
		
		for out_rkey, messages in msg_dict.iteritems():
			self.send(out_rkey, messages)
		
		# Cleanup time?
		if datetime.datetime.now() > self.next_purge:
			self.pygo_mp.purge_connections()
			self.next_purge = datetime.datetime.now() + self.purge_every
