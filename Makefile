# Variables definitions:

# Variable definition recursively expanded when the variable is used,
# Not when it's declared
REBAR = $(shell which rebar || echo ./rebar)

DIALYZER = dialyzer
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions -Wunderspecs

ERL = erl -I include -noshell -pa ebin

# Variable definition expanded when it's declared
APP := elixir

# Mark deps and ebin as phony because
# they are managed by rebar.
.PHONY: deps ebin

# Disable parallel in compile
.NOTPARALLEL: compile

# Makefile targets format:
#
# 	target: dependencies
# 	[tab] system command
#
compile: deps ebin exbin

deps:
	@ $(REBAR) get-deps

clean:
	@ $(REBAR) clean

distclean:
	@ $(REBAR) delete-deps

doc:
	@ $(REBAR) doc skip_deps=true

docs: deps compile
	@ bin/elixirc "lib/**/*.ex" --ignore-module-conflict --docs -o for_docs
	@ rm -rf exbin
	@ mv for_docs exbin

test: test_erlang test_elixir

test_erlang: deps compile
	@ $(REBAR) skip_deps=true eunit

test_elixir: deps compile
	@ echo "==> elixir (exunit)"
	@ time bin/elixir -r "test/elixir/**/*_test.exs"

release: compile test
	dialyzer --src src $(DIALYZER_WARNINGS)

ebin:
	@ $(REBAR) compile

exbin: lib/*.ex lib/*/*.ex
	@ rm -rf exbin
	@ mkdir -p exbin
	@ touch exbin
	$(ERL) -s elixir_compiler core -s erlang halt