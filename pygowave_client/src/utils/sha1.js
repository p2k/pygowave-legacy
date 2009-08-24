/*
 * PyGoWave Client Script a.k.a. Microwave
 * Copyright (C) 2009 by p2k
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

/**@scope pygowave*/
window.pygowave = $defined(window.pygowave) ? window.pygowave : new Hash(); 

pygowave.utils = $defined(pygowave.utils) ? pygowave.utils : new Hash();

/**
 * Utility classes and functions.
 * 
 * @module pygowave.view.utils
 */
(function () {
	
	/**
	 * Internal function to rotate an integer to the left.
	 *
	 * @function {private int} rotate_left
	 * @param {int} n
	 * @param {int} s
	 */
	var rotate_left = function (n,s) {
		return (( n<<s ) | (n>>>(32-s)));
	};
	
	/**
	 * Internal function to convert a 32-bit integer to a binary string.
	 *
	 * @function {private String} cvt_hex
	 * @param {int} val
	 */
	var cvt_bin = function (val) {
		var str="";
		for(var i = 3; i >= 0; i--)
			str += String.fromCharCode((val>>>(i*8))&0xff);
		return str;
	};
	
	/**
	 * Internal function to convert a binary string to a hex string.
	 *
	 * @function {private String} cvt_hex
	 * @param {String} val
	 */
	var cvt_hex = function (val) {
		var str="", v;
		for (var i = 0; i < val.length; i++) {
			v = val.charCodeAt(i).toString(16);
			str += (v.length == 1 ? "0"+v : v);
		}
		return str;
	};
	
	/**
	 * Sha-1 class which can be used like in python (hashlib).<br/>
	 * Derived from:<br/>
	 * Secure Hash Algorithm (SHA1)<br/>
	 * http://www.webtoolkit.info/
	 *
	 * @class {public} sha1
	 */
	var sha1 = new Class({
		/**
		 * Called on instantiation.
		 * @constructor {public} initialize
		 * @param {optional String} initial Initial string to hash
		 */
		initialize: function (initial) {
			this.digest_size = 20;
			this.block_size = 64;
			
			this.aligned_hash = new Hash({
				H0: 0x67452301,
				H1: 0xEFCDAB89,
				H2: 0x98BADCFE,
				H3: 0x10325476,
				H4: 0xC3D2E1F0
			});
			
			this.hash = new Hash();
			this.hash.extend(this.aligned_hash);
			this.alignment_str = "";
			this.hash_length = 0;
			
			this.W = new Array(80);
			
			if ($defined(initial))
				this.update(initial);
		},
		
		/**
		 * Update the hash object with the string arg. Repeated calls are
		 * equivalent to a single call with the concatenation of all the
		 * arguments: m.update(a); m.update(b) is equivalent to m.update(a+b).
		 * 
		 * @function {public} update
		 * @param {String} arg String to hash. This must not be a unicode string.
		 */
		update: function (arg) {
			var blockstart;
			var i, j;
			var A, B, C, D, E;
			var done = false;
			
			this.hash.extend(this.aligned_hash);
			arg = this.alignment_str + arg
			
			var arg_len = arg.length;
			
			var word_array = new Array();
			for( i=0; i<arg_len-3; i+=4 ) {
				j = arg.charCodeAt(i)<<24 | arg.charCodeAt(i+1)<<16 |
				arg.charCodeAt(i+2)<<8 | arg.charCodeAt(i+3);
				word_array.push( j );
			}
			
			switch( arg_len % 4 ) {
				case 0:
					i = 0x080000000;
					break;
				case 1:
					i = arg.charCodeAt(arg_len-1)<<24 | 0x0800000;
					break;
				case 2:
					i = arg.charCodeAt(arg_len-2)<<24 | arg.charCodeAt(arg_len-1)<<16 | 0x08000;
					break;
				case 3:
					i = arg.charCodeAt(arg_len-3)<<24 | arg.charCodeAt(arg_len-2)<<16 | arg.charCodeAt(arg_len-1)<<8	| 0x80;
					break;
			}
			
			word_array.push( i );
			
			while( (word_array.length % 16) != 14 ) word_array.push(0);
			
			word_array.push( (arg_len+this.hash_length)>>>29 );
			word_array.push( ((arg_len+this.hash_length)<<3)&0x0ffffffff );
			
			for (blockstart=0; blockstart < word_array.length; blockstart += 16) {
				if (arg.length < 64) {
					if (!done) {
						done = true;
						this.aligned_hash.extend(this.hash);
						this.alignment_str = arg;
					}
				}
				else {
					arg = arg.substr(64);
					this.hash_length += 64;
				}
				for( i=0; i<16; i++ ) this.W[i] = word_array[blockstart+i];
				this.hash_block();
			}
		},
		
		/**
		 * Internal block hashing method.
		 *
		 * @function {private} hash_block
		 */
		hash_block: function () {
			var W = this.W;
			var i, j;
			var A, B, C, D, E;
			var temp;
			
			for( i=16; i<=79; i++ ) W[i] = rotate_left(W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16], 1);
			
			A = this.hash.H0;
			B = this.hash.H1;
			C = this.hash.H2;
			D = this.hash.H3;
			E = this.hash.H4;
			
			for( i= 0; i<=19; i++ ) {
				temp = (rotate_left(A,5) + ((B&C) | (~B&D)) + E + W[i] + 0x5A827999) & 0x0ffffffff;
				E = D;
				D = C;
				C = rotate_left(B,30);
				B = A;
				A = temp;
			}
			
			for( i=20; i<=39; i++ ) {
				temp = (rotate_left(A,5) + (B ^ C ^ D) + E + W[i] + 0x6ED9EBA1) & 0x0ffffffff;
				E = D;
				D = C;
				C = rotate_left(B,30);
				B = A;
				A = temp;
			}
			
			for( i=40; i<=59; i++ ) {
				temp = (rotate_left(A,5) + ((B&C) | (B&D) | (C&D)) + E + W[i] + 0x8F1BBCDC) & 0x0ffffffff;
				E = D;
				D = C;
				C = rotate_left(B,30);
				B = A;
				A = temp;
			}
			
			for( i=60; i<=79; i++ ) {
				temp = (rotate_left(A,5) + (B ^ C ^ D) + E + W[i] + 0xCA62C1D6) & 0x0ffffffff;
				E = D;
				D = C;
				C = rotate_left(B,30);
				B = A;
				A = temp;
			}
			
			this.hash.H0 = (this.hash.H0 + A) & 0x0ffffffff;
			this.hash.H1 = (this.hash.H1 + B) & 0x0ffffffff;
			this.hash.H2 = (this.hash.H2 + C) & 0x0ffffffff;
			this.hash.H3 = (this.hash.H3 + D) & 0x0ffffffff;
			this.hash.H4 = (this.hash.H4 + E) & 0x0ffffffff;
		},
		
		/**
		 * Return the digest of the strings passed to the update() method so
		 * far. This is a string of digest_size bytes which may contain
		 * non-ASCII characters, including null bytes.
		 * 
		 * @function {public} digest
		 */
		digest: function () {
			return cvt_bin(this.hash.H0) + cvt_bin(this.hash.H1)
				+ cvt_bin(this.hash.H2) + cvt_bin(this.hash.H3)
				+ cvt_bin(this.hash.H4);
		},
		
		/**
		 * Like digest() except the digest is returned as a string of double
		 * length, containing only hexadecimal digits. This may be used to
		 * exchange the value safely in email or other non-binary environments.
		 * 
		 * @function {public} hexdigest
		 */
		hexdigest: function () {
			return cvt_hex(this.digest()).toLowerCase();
		},
		
		/**
		 * Return a copy ("clone") of the hash object. This can be used to
		 * efficiently compute the digests of strings that share a common
		 * initial substring.
		 *
		 * @function {public} copy
		 */
		copy: function () {
			var cp = new sha1();
			cp.hash.extend(this.hash);
			cp.aligned_hash.extend(this.aligned_hash);
			cp.alignment_str = this.alignment_str;
			cp.hash_length = this.hash_length;
		}
	});
	
	/**
	 * Global helper function for pycow compatibility.
	 * @function {public} sha_constructor
	 * @param {optional String} initial Initial string to hash
	 */
	window.sha_constructor = function (initial) {
		return new sha1(initial);
	};
	
	pygowave.utils.extend({
		utf8encode: utf8encode,
		sha1: sha1
	});
})();
