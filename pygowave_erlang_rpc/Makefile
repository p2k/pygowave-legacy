
# Simple makefile for the PyGoWave Erlang RPC Server

.SUFFIXES: .erl .beam

ERLC      = erlc
ERLCFLAGS = -W

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) $<

MYSQL_MODULES = mysql_auth.beam mysql_conn.beam mysql_recv.beam mysql.beam
STOMP_MODULES = stomp.beam
JSON_MODULES  = rfc4627.beam
OTHER_MODULES = uuid.beam sha1.beam

MAIN_MODULES = c2s_mp.beam models.beam operations.beam stomp_client.beam

MODULES = $(MYSQL_MODULES) $(STOMP_MODULES) $(JSON_MODULES) $(OTHER_MODULES) $(MAIN_MODULES)

all: $(MODULES)

.PHONY: all

clean:
	rm -rf $(MODULES) erl_crash.dump

.PHONY: clean

