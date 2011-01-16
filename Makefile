EBIN_DIR=ebin
SOURCE_DIR=src
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin
TEST_INCLUDE_DIR=$(TEST_SOURCE_DIR)/include

ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -I $(INCLUDE_DIR) $(ERLC_FLAGS)
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

PARSER_BASE_NAME=elixir
LEXER_NAME=$(PARSER_BASE_NAME)_lexer
PARSER_NAME=$(PARSER_BASE_NAME)_parser

compile:
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ # Generate the lexer
	@ $(ERL) -eval 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME)"), halt().'
	@ # Generate the parser
	@ $(ERL) -eval 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'
	@ # Compile everything
	$(ERLC) -o $(EBIN_DIR) $(SOURCE_DIR)/*.erl
	@ echo

all: compile

test: compile
	@ echo Running tests ...
	@ mkdir -p $(TEST_EBIN_DIR)
	@ # Compile test files
	@ $(ERLC) -o $(TEST_EBIN_DIR) $(TEST_SOURCE_DIR)/*.erl
	@ # Look and execute each file
	@ time $(ERL) $(TEST_EBIN_DIR) -I $(TEST_INCLUDE_DIR) -eval 'test_helper:test(), halt().'
	@ echo

clean:
	rm $(SOURCE_DIR)/$(LEXER_NAME).erl
	rm $(SOURCE_DIR)/$(PARSER_NAME).erl
	rm $(EBIN_DIR)/*.beam
	rm $(TEST_EBIN_DIR)/*.beam
