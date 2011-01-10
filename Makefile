EBIN_DIR=ebin
SOURCE_DIR=src
INCLUDE_DIR=include
ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -o $(EBIN_DIR) $(ERLC_FLAGS) $(SOURCE_DIR)
ERL=erl -pa ebin -noshell -eval
PARSER_BASE_NAME=elixir
LEXER_NAME=$(PARSER_BASE_NAME)_lexer
PARSER_NAME=$(PARSER_BASE_NAME)_parser

compile:
	mkdir -p $(EBIN_DIR)
	# Generate the lexer
	$(ERL) 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME)"), halt().'
	# Generate the parser
	$(ERL) 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'
	# Compile everything
	$(ERLC)/*.erl

all: compile

test: compile
	$(ERL) 'selector_test:test(), halt().'

clean:
	rm $(SOURCE_DIR)/$(LEXER_NAME).erl
	rm $(SOURCE_DIR)/$(PARSER_NAME).erl
	rm $(EBIN_DIR)/*.beam
