ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib
REBAR_CT_SUITES=chat_server

ERL=erl

REBAR=./rebar

.PHONY: all compile clean test

all: compile

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
		 --apps $(DEPS) -r deps -nn

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin -nn

compile:
	$(REBAR)  compile

ct:
	$(REBAR) ct suites=$(REBAR_CT_SUITES)

test: all
	make ct
	make dialyzer

clean:
	$(REBAR) clean
