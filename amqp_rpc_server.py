
from carrot.connection import DjangoAMQPConnection
from carrot.messaging import Consumer

class PyGoWaveMessageConsumer(Consumer):
	"""
	Handle all incoming messages.
	
	Routing key structure:
	<participant_guid>.<wavelet_id>.waveop
	
	participant_guid is created every time the user enters the wave viewer.
	Only one instance of the wave viewer can be opened per user at the same time.
	
	Message comsumers can be optimized in a multi-threading environment. If
	one consumer starts handling messages of a particular wavelet, it should
	block others from handling them.
	
	"""
	queue = "wavelet_rpc_singlethread"
	exchange = "wavelet.topic"
	exchange_type = "topic"
	routing_key = "#.#.waveop"
	
	def receive(self, message_data, message):
		key = message.amqp_message.routing_key
		print repr(message_data) + " " + key
		message.ack()

# Single threaded for now

if __name__ == '__main__':
	print "=> RabbitMQ RPC Server starting <="
	
	import signal
	# Python Ctrl-C handler
	signal.signal(signal.SIGINT, signal.SIG_DFL)
	
	amqpconn = DjangoAMQPConnection()
	omc = PyGoWaveMessageConsumer(amqpconn)
	omc.wait()
