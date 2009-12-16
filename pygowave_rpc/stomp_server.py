
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

import stomper, simplejson, traceback

from c2s_mp import PyGoWaveClientMessageProcessor
import logger

__all__ = ["StompServerFactory"]

class StompServerProtocol(Protocol):
	id = 0
	def __init__(self):
		self.state = 'initial'
		self.stompBuffer = stomper.stompbuffer.StompBuffer()
		StompServerProtocol.id += 1
		self.id = StompServerProtocol.id
	
	def dataReceived(self, data):
		self.stompBuffer.appendData(data.replace('\0', '\0\n'))
		
		while True:
			msg = self.stompBuffer.getOneMessage()
			
			if self.stompBuffer.buffer.startswith('\n'):
				self.stompBuffer.buffer = self.stompBuffer.buffer[1:]
			
			if msg is None or (not msg['headers'] and not msg['body'] and not msg['cmd']):
				break
			
			msg['cmd'] = msg['cmd'].lower()
			getattr(self, 'read_%s' % self.state)(**msg)
	
	def read_initial(self, cmd, headers, body):
		assert cmd == 'connect', "Invalid cmd: expected CONNECT"
		self.state = 'connected'
		self.sendFrame('CONNECTED', {"session": self.id}, "")
		self.factory.connected(self)
	
	def sendError(self, e):
		exception, instance, tb = traceback.sys.exc_info()
		tbOutput= "".join(traceback.format_tb(tb))
		self.sendFrame('ERROR', {'message': str(e) }, tbOutput)
	
	def sendFrame(self, cmd, headers, body):
		f = stomper.Frame()
		f.cmd = cmd
		f.headers.update(headers)
		f.body = body
		self.transport.write(f.pack())
	
	def read_connected(self, cmd, headers, body):
		return getattr(self, 'frame_%s' % cmd)(headers, body)
	
	def frame_subscribe(self, headers, body):
		self.factory.subscribe(self, headers['destination'])
	
	def frame_unsubscribe(self, headers, body):
		self.factory.unsubscribe(self, headers['destination'])
	
	def frame_send(self, headers, body):
		self.factory.send(headers['destination'], body, headers)
	
	def frame_disconnect(self, headers, body):
		self.transport.loseConnection()
	
	def connectionLost(self, reason):
		self.factory.disconnected(self)

class StompServerFactory(ServerFactory):
	protocol = StompServerProtocol
	
	def __init__(self):
		self.subscriptions = {}
		self.destinations = {}
	
	def startFactory(self):
		self.pygo_mp = PyGoWaveClientMessageProcessor()
		self.pygo_mp.logger.info("=> PyGoWave RPC Server starting <=")
		self.lc = LoopingCall(self.pygo_mp.purge_connections)
		self.lc.start(10 * 60) # Purge every 10 minutes
		self.lc2 = LoopingCall(self.pygo_mp.log_stats)
		self.lc2.start(15 * 60, now=False) # Stats every 15 minutes
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
		msg_dict = self.pygo_mp.process(dest_name, simplejson.loads(body.decode("utf-8")))
		
		for out_rkey, messages in msg_dict.iteritems():
			if self.destinations.has_key(out_rkey):
				self.destinations[out_rkey].sendFrame('MESSAGE', {'destination': str(out_rkey)}, simplejson.dumps(messages).encode("utf-8"))
	
	def __repr__(self):
		return "StompServerFactory"

