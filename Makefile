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

# Compile erlang source and then
# compile elixir as a post-hook
compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean:
	@$(REBAR) delete-deps

doc:
	@$(REBAR) doc skip_deps=true

test: test_erlang test_elixir

test_erlang: deps compile
	@ $(REBAR) skip_deps=true eunit

test_elixir: deps compile
	@ echo "==> elixir (exunit)"
	time bin/exunit test/elixir/*_test.exs

release: compile test
	dialyzer --src src $(DIALYZER_WARNINGS)

exbin: lib/*.ex lib/*/*.ex
	@ mkdir -p exbin
	@ touch exbin
	$(ERL) -s elixir_compiler core -s erlang halt