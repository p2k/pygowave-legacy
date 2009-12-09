
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

import ez_setup
ez_setup.use_setuptools()

from setuptools import setup, find_packages

import settings

setup(
	name = "PyGoWave",
	version = "0.4",
	packages = (
		"pygowave_server",
		"pygowave_server.common",
		"pygowave_server.common.pycow",
		"pygowave_server.templatetags",
		"pygowave_client",
		"pygowave_client.templatetags",
		"pygowave_rpc"
	),
	package_data = {
		'': ["locale/*/*/*"],
		'pygowave_server': [
			"templates/pygowave_server/*.html",
			"templates/pygowave_server/*/*.html"
		],
		'pygowave_client': [
			"build.xml",
			"jgrousedoc.properties",
			"src/*/*",
			"shipped/*/*.js",
		],
	},
	
	install_requires = [
		'django>=1.1',
		'django-registration>=0.7',
		'twisted==8.2.0',
		'orbited>=0.7.10',
		'uuid>=1.2',
		'PIL>=1.1.6',
		'anyjson>=0.2.1',
		'simplejson>=2.0.9',
		'demjson>=1.4',
	],
	extras_require = {
		'amqp-rpc-server': ['carrot>=0.5.1', 'amqplib>=0.6'],
		'mysql': ['mysql-python>=1.2.3'],
		'lxml': ['lxml>=2.2.2']
	},
	
	author = "p2k",
	author_email = "patrick.p2k.schneider@gmail.com",
	description = "A Python-based Google Wave Server implementation",
	license = "APACHE",
	keywords = "google wave server pygowave",
	url = "http://github.com/p2k/pygowave"
)
