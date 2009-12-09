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

/**
 * Simple utility function to get a localized month name (three letters)
 * @function {public} monthname
 * @param {int} n Month number
 */
window.monthname = function (n) {
	if (!$defined(window.monthname.data)) {
		window.monthname.data = [
			gettext("Jan"),
			gettext("Feb"),
			gettext("Mar"),
			gettext("Apr"),
			gettext("May"),
			gettext("Jun"),
			gettext("Jul"),
			gettext("Aug"),
			gettext("Sep"),
			gettext("Oct"),
			gettext("Nov"),
			gettext("Dec")
		];
	}
	if (n < 1 || n > 12)
		return "???";
	return window.monthname.data[n-1];
}
