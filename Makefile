REBAR:=$(shell which rebar || echo ./rebar)
ELIXIRC=bin/elixirc --ignore-module-conflict -o ebin
ERLC=erlc -I include
ERL=erl -I include -noshell -pa ebin
FULLFLAG=.full
VERSION=0.6.0.dev
.NOTPARALLEL: compile

#==> Compilation tasks
BUILTIN=ebin/__MAIN__-Elixir-Builtin.beam
EXUNIT=ebin/__MAIN__-ExUnit.beam

compile: erl $(BUILTIN) $(EXUNIT)

erl:
	@ $(REBAR) compile

$(BUILTIN): lib/kernel/*.ex lib/kernel/*/*.ex
	@ if [ -f $(BUILTIN) ]; then                       \
		echo "==> kernel (compile)";                   \
		$(ELIXIRC) "lib/kernel/**/*.ex";               \
	else                                               \
		echo "==> bootstrap (compile)";                \
		$(ERL) -s elixir_compiler core -s erlang halt; \
	fi
	@ touch $(BUILTIN)

$(EXUNIT): lib/ex_unit/*.ex lib/ex_unit/*/*.ex
	@ echo "==> ex_unit (compile)"
	@ $(ELIXIRC) "lib/ex_unit/**/*.ex"

clean:
	@ rm -rf .full
	@ rm -rf ebin
	@ $(REBAR) clean

#==> Release tasks
$(FULLFLAG): ebin/*.beam
	@ bin/elixirc "lib/**/*.ex" --ignore-module-conflict --docs --debug-info -o full
	@ rm -rf ebin/__MAIN__*
	@ mv full/__MAIN__* ebin
	@ rm -rf full

	@ rm -rf ebin/elixir.app
	@ $(REBAR) compile
	@ touch $(FULLFLAG)

zip: $(FULLFLAG)
	rm -rf v$(VERSION).zip
	zip -r v$(VERSION).zip ebin `git ls-files`
	zip v$(VERSION).zip -d .git .gitignore .travis.yml

release_docs: $(FULLFLAG)
	bin/elixir ../exdoc/bin/exdoc
	rm -rf ../elixir-lang.github.com/docs/master
	mv output ../elixir-lang.github.com/docs/master

release_erl: $(FULLFLAG)
	@ rm -rf rel/elixir
	@ cd rel && ../rebar generate

#==> Tests tasks
test: test_erlang test_elixir

test_erlang: compile
	@ echo "==> elixir (eunit)"
	@ mkdir -p test/ebin
	@ $(ERLC) -o test/ebin test/erlang/*.erl
	@ time $(ERL) -pa test/ebin -s test_helper test -s erlang halt
	@ echo

test_elixir: test_kernel test_exunit

test_kernel: compile
	@ echo "==> kernel (exunit)"
	@ time bin/elixir -r "test/kernel/test_helper.exs" -pr "test/kernel/**/*_test.exs"

test_exunit: compile
	@ echo "==> exunit (exunit)"
	@ time bin/elixir -r "test/ex_unit/test_helper.exs" -pr "test/ex_unit/**/*_test.exs"