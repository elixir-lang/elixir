EBIN_DIR=ebin
EXBIN_DIR=exbin
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin

ERLC=erlc -I $(INCLUDE_DIR) -W0
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

.PHONY: test test_erlang test_elixir clean clean_lib

# This is the default task
compile: ebin/elixir.beam ebin exbin | src/elixir_lexer.erl src/elixir_parser.erl

# install:
# We will need to do this one at some point

ebin/elixir.beam: include/elixir.hrl
	@ echo Compiling Erlang source ...
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) src/*.erl
	@ echo

src/elixir_lexer.erl: src/elixir_lexer.xrl
	@ echo Compiling lexer ...
	$(ERL) -eval 'leex:file("$<"), halt().'
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $@
	@ echo

src/elixir_parser.erl: src/elixir_parser.yrl
	@ echo Compiling parser ...
	$(ERL) -eval 'yecc:file("$<"), halt().'
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $@
	@ echo

ebin: src/*.erl
	@ echo Compiling Erlang source ...
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $?
	@ echo

exbin: lib/*.ex
	@ echo Compiling Elixir source ...
	@ mkdir -p $(EXBIN_DIR)
	$(ERL) -s elixir compile_core -s erlang halt

test_erlang: compile clean_lib
	@ echo Running Erlang tests ...
	@ mkdir -p $(TEST_EBIN_DIR)
	@ # Compile test files
	@ $(ERLC) -o $(TEST_EBIN_DIR) $(TEST_SOURCE_DIR)/*.erl
	@ # Look and execute each file
	time $(ERL) $(TEST_EBIN_DIR) -pa exbin -s test_helper test -s erlang halt
	@ echo

test_elixir: compile clean_lib
	@ echo Running Elixir tests ...
	time bin/exunit test/elixir/*_test.ex
	@ echo

test: test_erlang test_elixir

clean: clean_lib
	rm -f src/elixir_lexer.erl
	rm -f src/elixir_parser.erl
	rm -rf $(EBIN_DIR)/*.beam
	rm -rf $(TEST_EBIN_DIR)/*.beam
	rm -rf $(EXBIN_DIR)
	@ echo

clean_lib:
	find . -type f -name "*.exb" -exec rm -f {} \;