REBAR:=$(shell which rebar || echo ./rebar)
ERLC=erlc -I include
ERL=erl -I include -noshell -pa ebin
VERSION=0.5.0
BUILTIN=ebin/__MAIN__-Elixir-Builtin.beam
FULLFLAG=ebin/__MAIN__.full

.PHONY: ebin full zip release_docs
.NOTPARALLEL: compile

compile: ebin $(BUILTIN) ebin/elixir.app

zip: $(FULLFLAG)
	rm -rf v$(VERSION).zip
	zip -r v$(VERSION).zip ebin `git ls-files`
	zip v$(VERSION).zip -d .git .gitignore .travis.yml $(FULLFLAG)

ebin/elixir.app:
	@ $(REBAR) compile

ebin:
	@ $(REBAR) compile

$(BUILTIN): lib/*.ex lib/*/*.ex
	@ rm -rf ebin/__MAIN__*
	$(ERL) -s elixir_compiler core -s erlang halt
	@ rm -rf ebin/elixir.app

clean:
	@ rm -rf ebin
	@ $(REBAR) clean

full:
	@ bin/elixirc "lib/**/*.ex" --ignore-module-conflict --docs --debug-info -o full
	@ rm -rf ebin/__MAIN__*
	@ mv full/__MAIN__* ebin
	@ rm -rf full
	@ touch $(FULLFLAG)

release_docs: $(FULLFLAG)
	bin/elixir ../exdoc/bin/exdoc
	rm -rf ../elixir-lang.github.com/docs
	mv output ../elixir-lang.github.com/docs

test: test_erlang test_elixir

test_erlang: compile
	@ echo "==> erlang (eunit)"
	@ mkdir -p test/ebin
	@ # Compile test files
	@ $(ERLC) -o test/ebin test/erlang/*.erl
	@ # Look and execute each file
	@ time $(ERL) -pa test/ebin -s test_helper test -s erlang halt
	@ echo

test_elixir: compile
	@ echo "==> elixir (exunit)"
	@ time bin/elixir -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs"

rel: compile
	@ rm -rf rel/elixir
	@ cd rel && ../rebar generate
