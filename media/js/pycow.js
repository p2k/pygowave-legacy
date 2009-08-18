
/*
 * PyCow - Python to JavaScript with MooTools translator
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

/*
 This script contains some Python-compatibility functions and objects.
*/

/**
 * len(object) -> integer
 *
 * Return the number of items of a sequence or mapping.
 */
len = function (obj) {
	var l = 0;
	switch ($type(obj)) {
		case 'array':
		case 'string':
			return obj.length;
		case 'object':
			for (var key in obj)
				l++;
			return l;
		default:
			return 1;
	}
};

/**
 * str(object) -> string
 * 
 * Return a nice string representation of the object.
 * If the argument is a string, the return value is the same object.
 */
str = function (obj) {
	if ($defined(obj.__str__))
		return obj.__str__();
	else if ($type(obj) == "number")
		return String(obj);
	else if ($type(obj) == "string")
		return obj;
	else
		return repr(obj);
};

/**
 * repr(object) -> string
 *
 * Return the canonical string representation of the object.
 * For most object types, eval(repr(object)) == object.
 */
repr = function (obj) {
	if ($defined(obj.__repr__))
		return obj.__repr__();
	else if ($type(obj) == "boolean")
		return String(obj);
	else
		return JSON.encode(obj);
};

/**
 * range([start,] stop[, step]) -> list of integers
 *
 * Return a list containing an arithmetic progression of integers.
 * range(i, j) returns [i, i+1, i+2, ..., j-1]; start (!) defaults to 0.
 * When step is given, it specifies the increment (or decrement).
 * For example, range(4) returns [0, 1, 2, 3].  The end point is omitted!
 * These are exactly the valid indices for a list of 4 elements.
 */
range = function (start, stop, step) {
	if (!$defined(stop)) {
		stop = start;
		start = 0;
	}
	if (!$defined(step) || step == 0)
		step = 1;
	out = new Array();
	if (step > 0) {
		for (var i = start; i < stop; i += step)
			out.push(i);
	}
	else {
		for (var i = start; i > stop; i += step)
			out.push(i);
	}
	return out;
};

dbgprint = function () {
	var s = "";
	var first = true;
	
	for (var i = 0; i < arguments.length; i++) {
		if (first)
			first = false;
		else
			s += " ";
		s += str(arguments[i]);
	}
	
	if ($defined(window.console))
		window.console.info(s);
	else if (Browser.Engine.presto && $defined(opera.postError))
		opera.postError(s);
	else
		alert(s);
}

/*  
 *  Javascript sprintf
 *  http://www.webtoolkit.info/
 */

sprintfWrapper = {
	init : function () {
		if (!$defined(arguments))
			return null;
		if (arguments.length < 1)
			return null;
		if ($type(arguments[0]) != "string")
			return null;
		if (!$defined(RegExp))
			return null;
		
		var string = arguments[0];
		var exp = new RegExp(/(%([%]|(\-)?(\+|\x20)?(0)?(\d+)?(\.(\d)?)?([bcdfosxX])))/g);
		var matches = new Array();
		var strings = new Array();
		var convCount = 0;
		var stringPosStart = 0;
		var stringPosEnd = 0;
		var matchPosEnd = 0;
		var newString = '';
		var match = null;
		
		while ((match = exp.exec(string))) {
			if (match[9])
				convCount += 1;
				
			stringPosStart = matchPosEnd;
			stringPosEnd = exp.lastIndex - match[0].length;
			strings[strings.length] = string.substring(stringPosStart, stringPosEnd);
			
			matchPosEnd = exp.lastIndex;
			matches[matches.length] = {
				match: match[0],
				left: match[3] ? true : false,
				sign: match[4] || '',
				pad: match[5] || ' ',
				min: match[6] || 0,
				precision: match[8],
				code: match[9] || '%',
				negative: parseInt(arguments[convCount]) < 0 ? true : false,
				argument: String(arguments[convCount])
			};
		}
		strings[strings.length] = string.substring(matchPosEnd);

		if (matches.length == 0)
			return string;
		if ((arguments.length - 1) < convCount)
			return null;

		var code = null;
		var match = null;
		var i = null;

		for (i=0; i<matches.length; i++) {
			if (matches[i].code == '%') { substitution = '%' }
			else if (matches[i].code == 'b') {
				matches[i].argument = String(Math.abs(parseInt(matches[i].argument)).toString(2));
				substitution = sprintfWrapper.convert(matches[i], true);
			}
			else if (matches[i].code == 'c') {
				matches[i].argument = String(String.fromCharCode(parseInt(Math.abs(parseInt(matches[i].argument)))));
				substitution = sprintfWrapper.convert(matches[i], true);
			}
			else if (matches[i].code == 'd') {
				matches[i].argument = String(Math.abs(parseInt(matches[i].argument)));
				substitution = sprintfWrapper.convert(matches[i]);
			}
			else if (matches[i].code == 'f') {
				matches[i].argument = String(Math.abs(parseFloat(matches[i].argument)).toFixed(matches[i].precision ? matches[i].precision : 6));
				substitution = sprintfWrapper.convert(matches[i]);
			}
			else if (matches[i].code == 'o') {
				matches[i].argument = String(Math.abs(parseInt(matches[i].argument)).toString(8));
				substitution = sprintfWrapper.convert(matches[i]);
			}
			else if (matches[i].code == 's') {
				matches[i].argument = matches[i].argument.substring(0, matches[i].precision ? matches[i].precision : matches[i].argument.length)
				substitution = sprintfWrapper.convert(matches[i], true);
			}
			else if (matches[i].code == 'x') {
				matches[i].argument = String(Math.abs(parseInt(matches[i].argument)).toString(16));
				substitution = sprintfWrapper.convert(matches[i]);
			}
			else if (matches[i].code == 'X') {
				matches[i].argument = String(Math.abs(parseInt(matches[i].argument)).toString(16));
				substitution = sprintfWrapper.convert(matches[i]).toUpperCase();
			}
			else {
				substitution = matches[i].match;
			}
			
			newString += strings[i];
			newString += substitution;
		}
		
		newString += strings[i];
		return newString;
	},
	convert : function(match, nosign){
		if (nosign)
			match.sign = '';
		else
			match.sign = match.negative ? '-' : match.sign;
		
		var l = match.min - match.argument.length + 1 - match.sign.length;
		var pad = new Array(l < 0 ? 0 : l).join(match.pad);
		if (!match.left) {
			if (match.pad == "0" || nosign)
				return match.sign + pad + match.argument;
			else
				return pad + match.sign + match.argument;
		} else {
			if (match.pad == "0" || nosign)
				return match.sign + match.argument + pad.replace(/0/g, ' ');
			else
				return match.sign + match.argument + pad;
		}
	}
};
 
sprintf = sprintfWrapper.init;
Hash.alias("extend", "update");

String.implement({
	sprintf: function () {
		var args = $A(arguments);
		args.splice(0, 0, this);
		return sprintfWrapper.init.apply(this, args);
	},
	startswith: function (s) {
		return this.slice(0,s.length) == s;
	},
	endswith: function (s) {
		return this.slice(this.length-s.length) == s;
	}
});

String.alias("toLowerCase", "lower");

Array.implement({
	/**
	 * A.insert(index, object) -- insert object before index
	 */
	insert: function (index, object) {
		this.splice(index, 0, object);
	},
	/**
	 * A.append(object) -- append object to array
	 */
	append: function (object) {
		this[this.length] = object;
	}
});

/**
 * A.pop([index]) -> item -- remove and return item at index (default last).
 * Returns undefined if list is empty or index is out of range.
 */
Array.prototype.pop = function (index) {
	if (!$defined(index))
		index = this.length-1;

	if (index == -1 || index >= this.length)
		return undefined;
	var elt = this[index];
	this.splice(index, 1);
	return elt;
};

IndexError = function () {};
IndexError.prototype = new Error;

/**
 * Java-Style iterator class.
 */
_Iterator = new Class({
	initialize: function (object) {
		this.obj = object;
		this.pos = -1;
		if (object instanceof Array)
			this.elts = null;
		else {
			this.elts = new Array();
			for (var x in object) {
				if (object.hasOwnProperty(x))
					this.elts.push(x);
			}
		}
	},
	hasNext: function () {
		if (this.elts == null) {
			if (this.pos >= this.obj.length-1)
				return false;
		}
		else {
			if (this.pos >= this.elts.length-1)
				return false;
		}
		return true;
	},
	next: function () {
		if (this.elts == null) {
			if (this.pos >= this.obj.length-1)
				throw new IndexError();
			return this.obj[++this.pos];
		}
		else {
			if (this.pos >= this.elts.length-1)
				throw new IndexError();
			return this.obj[this.elts[++this.pos]];
		}
	},
	key: function () {
		if (this.elts == null)
			return this.pos;
		else {
			if (this.pos >= this.elts.length)
				throw new IndexError();
			return this.elts[this.pos];
		}
	}
});

XRange = new Class({
	initialize: function (start, stop, step) {
		if (!$defined(stop)) {
			stop = start;
			start = 0;
		}
		if (!$defined(step) || step == 0)
			step = 1;
		this.start = start;
		this.stop = stop;
		this.step = step;
	},
	hasNext: function () {
		return !((this.step > 0 && this.start >= this.stop) || (this.step < 0 && this.start <= this.stop));
	},
	next: function () {
		if ((this.step > 0 && this.start >= this.stop) || (this.step < 0 && this.start <= this.stop))
			throw new IndexError();
		var ret = this.start;
		this.start += this.step;
		return ret;
	}
});
