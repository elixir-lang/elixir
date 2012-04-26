REBAR = $(shell which rebar || echo ./rebar)
ERLC=erlc -I include
ERL=erl -I include -noshell -pa ebin

.PHONY: ebin
.NOTPARALLEL: compile

# Makefile targets format:
#
# 	target: dependencies
# 	[tab] system command
#
compile: ebin exbin

clean:
	@ $(REBAR) clean

docs: compile
	@ bin/elixirc "lib/**/*.ex" --ignore-module-conflict --docs -o for_docs
	@ rm -rf exbin
	@ mv for_docs exbin

test: test_erlang test_elixir

test_erlang: compile
	@ echo "==> erlang (eunit)"
	@ mkdir -p test/ebin
	@ # Compile test files
	@ $(ERLC) -o test/ebin test/erlang/*.erl
	@ # Look and execute each file
	@ time $(ERL) -pa test/ebin -pa exbin -s test_helper test -s erlang halt
	@ echo

test_elixir: compile
	@ echo "==> elixir (exunit)"
	@ time bin/elixir -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs"

ebin:
	@ $(REBAR) compile

exbin: lib/*.ex lib/*/*.ex
	@ rm -rf exbin
	@ mkdir -p exbin
	@ touch exbin
	$(ERL) -s elixir_compiler core -s erlang halt