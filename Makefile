# Variables definitions:

# Variable definition recursively expanded when the variable is used,
# Not when it's declared
REBAR = $(shell which rebar || echo ./rebar)

DIALYZER = dialyzer
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions -Wunderspecs

ERL = erl -I include -noshell -pa ebin

EBIN_DIR=ebin
EXBIN_DIR=exbin
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin

ERLC=erlc -I $(INCLUDE_DIR) -W0
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

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
	@ echo "==> erlang (eunit)"
	@ mkdir -p $(TEST_EBIN_DIR)
	@ # Compile test files
	@ $(ERLC) -o $(TEST_EBIN_DIR) $(TEST_SOURCE_DIR)/*.erl
	@ # Look and execute each file
	time $(ERL) $(TEST_EBIN_DIR) -pa exbin -s test_helper test -s erlang halt
	@ echo

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