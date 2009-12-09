# -*- coding: utf-8 -*-

# Django settings for PyGoWave. Template for development servers.

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

# --- Important Settings ---

ADMINS = (
    #('Your name', 'Your email'),
)

DATABASE_ENGINE = 'sqlite3'    # 'postgresql_psycopg2', 'postgresql', 'mysql', 'sqlite3' or 'oracle'.
DATABASE_NAME = 'pygowave.sqlite3' # Or path to database file if using sqlite3.
DATABASE_USER = ''             # Not used with sqlite3.
DATABASE_PASSWORD = ''         # Not used with sqlite3.
DATABASE_HOST = ''             # Set to empty string for localhost. Not used with sqlite3.
DATABASE_PORT = ''             # Set to empty string for default. Not used with sqlite3.

# The domain used in wave URLs; also used as base in other settings
WAVE_DOMAIN = 'localhost'

# Set to True on a development system, False on a production system
DEVELOPER_MODE = True

# Local time zone for this installation. Choices can be found here:
# http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
# although not all choices may be available on all operating systems.
# If running in a Windows environment this must be set to the same as your
# system time zone.
TIME_ZONE = 'Europe/Berlin'

# Language code for this installation. All choices can be found here:
# http://www.i18nguy.com/unicode/language-identifiers.html
LANGUAGE_CODE = 'en-us'

LANGUAGES = (
	('de', 'German - Deutsch'),
	('en', 'English - English'),
	('nl', 'Dutch - Nederlands'),
	('es', u'Spanish - Español'),
	('ar', u'Arabic - al-ʿarabīyah'),
	('ru', u'Russian - русский'),
	('ja', u'Japanese - 日本語'),
)

# Make this unique, and don't share it with anybody.
SECRET_KEY = 'n*hye&*_3ry)ds-6xwkp9f$u^$))nwt1j^332(+hj!qt@1mk!y'

# Set this to your Google Analytics ID (UA-xxxxxxx-x) to activate tracking,
# leave empty to disable tracking.
ANALYTICS_ID = ""

# --- Derived, Auto-Generated And Other Settings ---

DEFAULT_FROM_EMAIL = 'noreply@' + WAVE_DOMAIN

DEBUG = DEVELOPER_MODE
TEMPLATE_DEBUG = DEVELOPER_MODE

MANAGERS = ADMINS

LOGIN_URL = '/accounts/login/'
LOGIN_REDIRECT_URL = '/home/'

SITE_ID = 1

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# Absolute path to the directory where this file resides
import os
PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
if os.name == "nt":
	PROJECT_ROOT = PROJECT_ROOT.replace(os.path.sep, "/")

# Absolute path to the directory that holds media.
# Example: "/home/media/media.lawrence.com/"
MEDIA_ROOT = PROJECT_ROOT+'/media/'

# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash if there is a path component (optional in other cases).
# Examples: "http://media.lawrence.com/", "http://example.com/media/"
MEDIA_URL = '/media/'

# URL prefix for admin media -- CSS, JavaScript and images. Make sure to use a
# trailing slash.
# Examples: "http://foo.com/media/", "/media/".
ADMIN_MEDIA_PREFIX = '/admin_media/'

# List of callables that know how to import templates from various sources.
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.load_template_source',
    'django.template.loaders.app_directories.load_template_source',
#    'django.template.loaders.eggs.load_template_source',
)

MIDDLEWARE_CLASSES = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.locale.LocaleMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'pygowave_server.middleware.UserOnlineMiddleware',
)

TEMPLATE_CONTEXT_PROCESSORS = (
	"django.core.context_processors.auth",
	"django.core.context_processors.debug",
	"django.core.context_processors.i18n",
	"django.core.context_processors.media",
	#"django.core.context_processors.request",
	"pygowave_server.context_processors.server",
	"pygowave_server.context_processors.storage_urls",
)

ROOT_URLCONF = 'urls'

TEMPLATE_DIRS = (
    PROJECT_ROOT+'/templates',
)

INSTALLED_APPS = (
	'django.contrib.auth',
	'django.contrib.contenttypes',
	'django.contrib.sessions',
	'django.contrib.sites',
	'django.contrib.admin',
	'registration',
	'pygowave_server',
	'pygowave_client',
# Uncomment the following line for translation support with django-rosetta
	#'rosetta',
)

# Don't change this:
AUTH_PROFILE_MODULE = 'pygowave_server.participant'

# --- PyGoWave Specific Settings ---

VERSION = 'alpha-0.4'

ACCOUNT_ACTIVATION_DAYS = 7

AVATAR_ROOT = MEDIA_ROOT + 'avatars/'
AVATAR_URL = MEDIA_URL + 'avatars/'

GADGET_ROOT = MEDIA_ROOT + 'gadgets/'
GADGET_URL = MEDIA_URL + 'gadgets/'

# Used if a user doesn't log out properly
ONLINE_TIMEOUT_MINUTES = 10

# Ping interval for wavelet connections
PING_INTERVAL_SECONDS = 20

# Used if a user somehow doesn't use his generated access key in time or
# a ping timeout occurs
ACCESS_KEY_TIMEOUT_MINUTES = 2

# Minimum characters to engage a search
PARTICIPANT_SEARCH_LENGTH = 3

# This setting holds the path for pygowave_client to store converted and
# compressed versions of the source files.
CLIENT_CACHE_FOLDER = os.path.dirname(os.path.abspath(__file__)) + os.path.sep + "client_cache" + os.path.sep

# RPC settings here
RPC_SERVER = WAVE_DOMAIN
RPC_USER = "pygowave_server"
RPC_PASSWORD = "pygowave_server"
RPC_LOGGING = "normal" # Possible values: "normal", "quiet", "verbose"
RPC_LOGFILE_INFO = "" # Leave empty to write to console (stderr)
RPC_LOGFILE_ERROR = ""

# AMQP RPC specific
AMQP_PORT = 5672
AMQP_VHOST = "/"

# STOMP RPC specific
STOMP_PORT = 61613
STOMP_MODE = "server" # Possible values: "client" (use message broker), "server" (no message broker)

# Orbited settings here
ORBITED_SERVER = WAVE_DOMAIN
ORBITED_PORT = "auto" # Set to the string "auto" to use the http server's current port; set to a number to override
