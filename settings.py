# -*- coding: utf-8 -*-

# Django settings for pygowave_project project.

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

DEBUG = True
TEMPLATE_DEBUG = DEBUG

ADMINS = (
    ('Patrick "p2k" Schneider', 'patrick.p2k.schneider@gmail.com'),
)

MANAGERS = ADMINS

DATABASE_ENGINE = 'mysql'      # 'postgresql_psycopg2', 'postgresql', 'mysql', 'sqlite3' or 'oracle'.
DATABASE_NAME = 'pygowave'     # Or path to database file if using sqlite3.
DATABASE_USER = 'pygowave'     # Not used with sqlite3.
DATABASE_PASSWORD = 'pygowave' # Not used with sqlite3.
DATABASE_HOST = 'localhost'    # Set to empty string for localhost. Not used with sqlite3.
DATABASE_PORT = ''             # Set to empty string for default. Not used with sqlite3.

DEFAULT_FROM_EMAIL = 'noreply@localhost'

# The domain used in wave URLs
WAVE_DOMAIN = 'localhost'

LOGIN_URL = '/pygowave/accounts/login/'

# Set to False to enable some non-localhost features
IS_LOCAL = True

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
	('ar', u'Arabic - al-ʿarabīyah'),
	('ja', u'Japanese - 日本語'),
)

VERSION = 'alpha-0.3'

SITE_ID = 1

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# Absolute path to the directory that holds media.
# Example: "/home/media/media.lawrence.com/"
MEDIA_ROOT = '/srv/http/pygowave_project/media/'

# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash if there is a path component (optional in other cases).
# Examples: "http://media.lawrence.com/", "http://example.com/media/"
MEDIA_URL = '/pygowave/media/'

# URL prefix for admin media -- CSS, JavaScript and images. Make sure to use a
# trailing slash.
# Examples: "http://foo.com/media/", "/media/".
ADMIN_MEDIA_PREFIX = '/admin/media/'

# Make this unique, and don't share it with anybody.
SECRET_KEY = 'n*hye&*_3ry)ds-6xwkp9f$u^$))nwt1j^332(+hj!qt@1mk!y'

# List of callables that know how to import templates from various sources.
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.load_template_source',
    'django.template.loaders.app_directories.load_template_source',
#     'django.template.loaders.eggs.load_template_source',
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
    '/srv/http/pygowave_project/templates',
)

INSTALLED_APPS = (
	'django.contrib.auth',
	'django.contrib.contenttypes',
	'django.contrib.sessions',
	'django.contrib.sites',
	'django.contrib.admin',
	'registration',
	'rosetta',
	'pygowave_server',
	'pygowave_client',
)

AUTH_PROFILE_MODULE = 'pygowave_server.participant'

ACCOUNT_ACTIVATION_DAYS = 7

AVATAR_ROOT = MEDIA_ROOT + 'avatars/'
AVATAR_URL = MEDIA_URL + 'avatars/'

GADGET_ROOT = MEDIA_ROOT + 'gadgets/'
GADGET_URL = MEDIA_URL + 'gadgets/'

# Used if a user doesn't log out properly
ONLINE_TIMEOUT_MINUTES = 10

# RabbitMQ settings here
AMQP_SERVER = "localhost"
AMQP_PORT = 5672
AMQP_USER = "pygowave"
AMQP_PASSWORD = "pygowave"
AMQP_VHOST = "/"

# Orbited settings here
ORBITED_SERVER = "localhost"
ORBITED_PORT = 9000
