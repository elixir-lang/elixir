EBIN_DIR=ebin
SOURCE_DIR=src
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin

ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -I $(INCLUDE_DIR) $(ERLC_FLAGS)
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

PARSER_BASE_NAME=elixir
LEXER_NAME=$(PARSER_BASE_NAME)_lexer
PARSER_NAME=$(PARSER_BASE_NAME)_parser

.PHONY: test test_erlang test_elixir clean

compile: ebin

ebin: src/*
	@ # Clean any .beam file
	@ make clean
	@ # Start compiling
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ # Generate the lexer
	@ $(ERL) -eval 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME)"), halt().'
	@ # Generate the parser
	@ $(ERL) -eval 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'
	@ # Compile everything
	$(ERLC) -o $(EBIN_DIR) $(SOURCE_DIR)/*.erl
	@ echo

test_erlang: compile
	@ echo Running Erlang tests ...
	@ mkdir -p $(TEST_EBIN_DIR)
	@ # Compile test files
	@ $(ERLC) -o $(TEST_EBIN_DIR) $(TEST_SOURCE_DIR)/*.erl
	@ # Look and execute each file
	@ time $(ERL) $(TEST_EBIN_DIR) -eval 'test_helper:test(), halt().'
	@ echo 

test_elixir: compile
	@ echo Running Elixir tests ...
	@ time $(ERL) -eval 'elixir:boot(), elixir:require_file("test_helper"), halt().'
	@ echo

test: compile
	@ make test_erlang
	@ make test_elixir

clean:
	@ rm -f $(SOURCE_DIR)/$(LEXER_NAME).erl
	@ rm -f $(SOURCE_DIR)/$(PARSER_NAME).erl
	@ rm -f $(EBIN_DIR)/*.beam
	@ rm -f $(TEST_EBIN_DIR)/*.beam
