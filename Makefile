EBIN_DIR=ebin
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin

ERLC=erlc -I $(INCLUDE_DIR) -W0
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

.PHONY: test test_erlang test_elixir clean

# This is the default task
compile: ebin/elixir.beam ebin | src/elixir_lexer.erl src/elixir_parser.erl

# install:
# We will need to do this one at some point

ebin/elixir.beam: include/elixir.hrl
	@ echo Compiling source ...
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) src/*.erl
	@ echo

src/elixir_lexer.erl: src/elixir_lexer.xrl
	@ echo Compiling lexer ...
	$(ERL) -eval 'leex:file("$<"), halt().'
	@ echo

src/elixir_parser.erl: src/elixir_parser.yrl
	@ echo Compiling parser ...
	$(ERL) -eval 'yecc:file("$<"), halt().'
	@ echo

ebin: src/*.erl
	@ rm -rf lib/*.exb
	@ echo Compiling source ...
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $?
	@ echo

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
	rm -f src/elixir_lexer.erl
	rm -f src/elixir_parser.erl
	rm -rf lib/*.exb
	rm -rf $(EBIN_DIR)/*.beam
	rm -rf $(TEST_EBIN_DIR)/*.beam
	rm -rf libc
	@ echo
