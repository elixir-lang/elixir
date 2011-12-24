# Variables definitions:

#  variable definition ecursively expanded when the variable is used,
#  not when it's declared
REBAR =$(shell which rebar || echo ./rebar)

DIALYZER = dialyzer
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions -Wunderspecs

#  conditional variable definition, assign only if not yet assigned.
ERL = erl -I include -noshell -pa ebin

#  variable definition expanded when it's declared
APP := elixir

# Makefile targets format:
#
# 	target: dependencies
# 	[tab] system command

.PHONY: deps

all: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean:
	@$(REBAR) delete-deps

doc:
	@$(REBAR) doc skip_deps=true

test: deps all
	@$(REBAR) skip_deps=true eunit

release: all test
	dialyzer --src src $(DIALYZER_WARNINGS)

exbin: lib/*.ex lib/*/*.ex
	@ mkdir -p exbin
	@ touch exbin
	$(ERL) -s elixir_compiler core -s erlang halt