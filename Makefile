EBIN_DIR=ebin
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin

ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -I $(INCLUDE_DIR) $(ERLC_FLAGS)
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

.PHONY: test test_erlang test_elixir clean

# This is the default task
compile: src/elixir_lexer.erl src/elixir_parser.erl ebin

# install:
# We will need to do this one at some point

src/elixir_lexer.erl: src/elixir_lexer.xrl
	$(ERL) -eval 'leex:file("$<"), halt().'

src/elixir_parser.erl: src/elixir_parser.yrl
	$(ERL) -eval 'yecc:file("$<"), halt().'

ebin: src/*.erl
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $?

test_erlang: compile
	@ echo Running Erlang tests ...
	@ mkdir -p $(TEST_EBIN_DIR)
	@ # Compile test files
	@ $(ERLC) -o $(TEST_EBIN_DIR) $(TEST_SOURCE_DIR)/*.erl
	@ # Look and execute each file
	time $(ERL) $(TEST_EBIN_DIR) -eval 'test_helper:test(), halt().'
	@ echo

test_elixir: compile
	@ echo Running Elixir tests ...
	time bin/exunit test/elixir/*_test.ex
	@ echo

test: test_erlang test_elixir

clean:
	@ rm -f src/elixir_lexer.erl
	@ rm -f src/elixir_parser.erl
	@ rm -f $(EBIN_DIR)/*.beam
	@ rm -f $(TEST_EBIN_DIR)/*.beam
