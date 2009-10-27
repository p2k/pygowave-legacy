
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

import logging
import logging.handlers

from django.conf import settings

def getLogger():
	return logging.getLogger("pygowave")

def logMode():
	"""Returns the log mode. Can be 'normal', 'quiet' or 'verbose'"""
	return getattr(settings, "RPC_LOGGING", "normal")

def setupLogging():
	"""
	Setup PyGoWave RPC Server logging features.
	"""
	logger = getLogger()
	logger.setLevel(logging.INFO)
	log_formatter = logging.Formatter('%(asctime)s %(name)-8s -- %(levelname)-5s %(message)s')
	
	class LevelOnlyFilter:
		def __init__(self, level): self.level = level
		def filter(self, record): return record.levelno == self.level
	
	if logMode() == "verbose":
		logger.setLevel(logging.DEBUG)
		debug_handler = logging.StreamHandler()
		debug_handler.setFormatter(log_formatter)
		debug_handler.addFilter(LevelOnlyFilter(logging.DEBUG))
		logger.addHandler(info_handler)
	
	if logMode() == "quiet":
		logger.setLevel(logging.CRITICAL)
	
	if getattr(settings, "RPC_LOGFILE_INFO", "") != "":
		info_handler = logging.handlers.WatchedFileHandler(getattr(settings, "RPC_LOGFILE_INFO"), 'a', 'utf-8')
	else:
		info_handler = logging.StreamHandler()
	
	info_handler.setLevel(logging.INFO)
	info_handler.setFormatter(log_formatter)
	info_handler.addFilter(LevelOnlyFilter(logging.INFO))
	logger.addHandler(info_handler)
	
	if getattr(settings, "RPC_LOGFILE_ERROR", "") != "":
		error_handler = logging.handlers.WatchedFileHandler(getattr(settings, "RPC_LOGFILE_ERROR"), 'a', 'utf-8')
	else:
		error_handler = logging.StreamHandler()
	
	error_handler.setLevel(logging.ERROR)
	error_handler.setFormatter(log_formatter)
	logger.addHandler(error_handler)
	
	return logger
