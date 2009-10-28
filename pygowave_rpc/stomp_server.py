
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

from twisted.internet.protocol import Protocol, ServerFactory
from twisted.internet.task import LoopingCall

import stomper, anyjson
from morbid import StompProtocol

from c2s_mp import PyGoWaveClientMessageProcessor
import logger

__all__ = ["StompServerFactory"]

class StompServerFactory(ServerFactory):
	protocol = StompProtocol
	
	def __init__(self):
		self.subscriptions = {}
		self.destinations = {}
	
	def startFactory(self):
		self.pygo_mp = PyGoWaveClientMessageProcessor()
		self.pygo_mp.logger.info("=> PyGoWave RPC Server starting <=")
		self.lc = LoopingCall(self.pygo_mp.purge_connections)
		self.lc.start(10 * 60) # Purge every 10 minutes
		self.pygo_mp.logger.info("=> PyGoWave RPC Server ready <=")
	
	def stopFactory(self):
		if self.lc.running:
			self.lc.stop()
	
	def subscribe(self, proto, name):
		self.subscriptions[proto.id].append(name)
		self.destinations[name] = proto
	
	def unsubscribe(self, proto, name):
		self.subscriptions[proto.id].remove(name)
		del self.destinations[name]
	
	def connected(self, proto):
		self.subscriptions[proto.id] = []
	
	def disconnected(self, proto):
		for sub in self.subscriptions[proto.id]:
			self.unsubscribe(proto, sub)
		del self.subscriptions[proto.id]
	
	def send(self, dest_name, body, headers={}):
		msg_dict = self.pygo_mp.process(dest_name, anyjson.deserialize(body))
		
		for out_rkey, messages in msg_dict.iteritems():
			if self.destinations.has_key(out_rkey):
				self.destinations[out_rkey].sendFrame('MESSAGE', {'destination': str(out_rkey)}, anyjson.serialize(messages).encode("utf-8"))
	
	def __repr__(self):
		return "StompServerFactory"
