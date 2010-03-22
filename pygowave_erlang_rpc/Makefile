
#
# PyGoWave Server - The Python Google Wave Server
# Copyright 2009-2010 Patrick Schneider <patrick.p2k.schneider@gmail.com>
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

VERSION=0.4.1
DEPS_FILE=deps.mk
SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
BEAM_TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
TARGETS=$(EBIN_DIR)/pygowave_rpc.boot $(BEAM_TARGETS)
SBIN_DIR=sbin
START_SCRIPT=pygowave-erlang-rpc
DESTDIR=/usr/local

ERL       = erl
ERLC      = erlc
ESCRIPT   = escript
ERLC_OPTS = -I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall

all: $(TARGETS)

.PHONY: all

$(DEPS_FILE): $(SOURCES) $(INCLUDES) generate_deps
	$(ESCRIPT) generate_deps $(INCLUDE_DIR) $(SOURCE_DIR) \$$\(EBIN_DIR\) $@

$(EBIN_DIR)/pygowave_rpc.app: $(EBIN_DIR)/pygowave_rpc_app.in $(BEAM_TARGETS) generate_app
	$(ESCRIPT) generate_app $(EBIN_DIR) $@ < $<

$(EBIN_DIR)/pygowave_rpc.rel: $(EBIN_DIR)/pygowave_rpc.app generate_rel
	$(ESCRIPT) generate_rel $@ < $<

$(EBIN_DIR)/pygowave_rpc.boot: $(EBIN_DIR)/pygowave_rpc.rel generate_boot
	$(ESCRIPT) generate_boot $(basename $@) $(dir $@)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	$(ERLC) $(ERLC_OPTS) -pa $(EBIN_DIR) $<

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -f $(EBIN_DIR)/pygowave_rpc.app $(EBIN_DIR)/pygowave_rpc.boot $(EBIN_DIR)/pygowave_rpc.script $(EBIN_DIR)/pygowave_rpc.rel
	rm -f $(DEPS_FILE)
	rm -f erl_crash.dump

.PHONY: clean

run: all
	$(ERL) -boot pygowave_rpc -pa $(EBIN_DIR) -noshell

.PHONY: run

install: all
	[ -d $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin ] || mkdir -p $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin
	install -m 644 $(BEAM_TARGETS) $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin
	install -m 644 $(EBIN_DIR)/pygowave_rpc.app $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin
	install -m 644 $(EBIN_DIR)/pygowave_rpc.rel $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin
	install -m 644 $(EBIN_DIR)/pygowave_rpc.script $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin
	install -m 644 $(EBIN_DIR)/pygowave_rpc.boot $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/ebin
	[ -d $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/include ] || mkdir -p $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/include
	install -m 644 $(INCLUDES) $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/include
	[ -d $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/sbin ] || mkdir -p $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/sbin
	install -m 755 $(SBIN_DIR)/$(START_SCRIPT) $(DESTDIR)/lib/erlang/lib/pygowave_rpc-$(VERSION)/sbin/$(START_SCRIPT)
	[ -d $(DESTDIR)/bin ] || mkdir -p $(DESTDIR)/bin
	ln -s ../lib/erlang/lib/pygowave_rpc-$(VERSION)/sbin/$(START_SCRIPT) $(DESTDIR)/bin/$(START_SCRIPT)

.PHONY: install

-include $(DEPS_FILE)
