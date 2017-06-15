ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib
REBAR_CT_SUITES=chat_server

ERL=erl

REBAR=./rebar

.PHONY: all compile clean dialyzer test

all: compile

dialyzer:
	dialyzer -Wrace_conditions -r ./ebin

compile:
	$(REBAR)  compile

ct:
	$(REBAR) ct suites=$(REBAR_CT_SUITES)

test: all
	make ct
	make dialyzer

clean:
	$(REBAR) clean
