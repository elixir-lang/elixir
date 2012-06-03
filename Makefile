REBAR:=$(shell which rebar || echo ./rebar)
ERLC=erlc -I include
ERL=erl -I include -noshell -pa ebin
VERSION=0.5.0

.PHONY: ebin docs zip
.NOTPARALLEL: compile

compile: ebin ebin/__MAIN__ ebin/elixir.app

zip:
	rm -rf v$(VERSION).zip
	zip -r v$(VERSION).zip ebin `git ls-files`
	zip v$(VERSION).zip -d .git .gitignore .travis.yml

ebin/elixir.app:
	@ $(REBAR) compile

ebin:
	@ $(REBAR) compile

ebin/__MAIN__: lib/*.ex lib/*/*.ex
	@ rm -rf ebin/__MAIN__
	$(ERL) -s elixir_compiler core -s erlang halt
	@ rm -rf ebin/elixir.app

clean:
	@ $(REBAR) clean

docs: compile
	@ bin/elixirc "lib/**/*.ex" --ignore-module-conflict --docs -o for_docs
	@ rm -rf ebin/__MAIN__
	@ mv for_docs/__MAIN__ ebin/__MAIN__
	@ rm -rf for_docs

release_docs: docs
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
	@ cp -r ebin/__MAIN__ rel/elixir/lib/elixir-$(VERSION)/ebin/ 
