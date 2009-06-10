/*
 * PyGoWave Client Script a.k.a. Microwave
 * Copyright (C) 2009 by p2k and avital
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/*
	Note to our valued source code readers and potential hackers:
	The ID numbers used in this script are completely random and are generated
	each time a user visits a Wave. If the user leaves the Wave, the IDs will
	be rendered useless. Suspicious traffic sent from a client will be detected
	and results in instant disconnection from the server.
*/

$(document).ready(function() {
		var stomp = new STOMPClient();
		
		$.extend(stomp, {
			onclose: function(c) {
				alert('Lost Connection, Code: ' + c);
			},
			onerror: function(error) {
				alert("Error: " + error);
			},
			onconnectedframe: function() {
				stomp.subscribe("", {id: WaveAccessKeyRx+"-queue", exchange: "wavelet.topic", routing_key: WaveAccessKeyRx});
			},
			onmessageframe: function(frame) {
				alert(frame.body);
			}
		});
		
		//stomp.connect("localhost", 61613, "pygowave", "pygowave");
		
		/*
		$('#chatbox_input').keypress(function(e) {
			var key = e.charCode || e.keyCode || 0;
			if (key == ENTER_KEY) {
				msg = $(this).val();
				stomp.send("[\"" + msg + "\"]", WaveAccessKeyTx, {exchange: "wavelet.topic"});
				$(this).val("");
			}
		});
		*/
});