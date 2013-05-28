REBAR := $(shell echo `pwd`/rebar)
ELIXIRC := bin/elixirc --ignore-module-conflict $(ELIXIRC_OPTS)
ERLC := erlc -I lib/elixir/include
ERL := erl -I lib/elixir/include -noshell -pa lib/elixir/ebin
VERSION := $(strip $(shell cat VERSION))
INSTALL_PATH := /usr/local

.PHONY: install compile erlang elixir dialyze test clean docs release_docs release_zip release_erl
.NOTPARALLEL: compile

#==> Templates

define APP_TEMPLATE
$(1): lib/$(1)/ebin/Elixir.$(2).beam lib/$(1)/ebin/$(1).app

lib/$(1)/ebin/$(1).app:
	@ cd lib/$(1) && ../../bin/elixir -e "Mix.Server.start_link(:dev)" -r mix.exs -e "Mix.Task.run('compile.app')"

lib/$(1)/ebin/Elixir.$(2).beam: $(wildcard lib/$(1)/lib/*.ex) $(wildcard lib/$(1)/lib/*/*.ex) $(wildcard lib/$(1)/lib/*/*/*.ex)
	@ echo "==> $(1) (compile)"
	@ $$(ELIXIRC) "lib/$(1)/lib/**/*.ex" -o lib/$(1)/ebin

test_$(1): $(1)
	@ echo "==> $(1) (exunit)"
	@ cd lib/$(1) && ../../bin/elixir -r "test/test_helper.exs" -pr "test/**/*_test.exs";
endef

#==> Compilation tasks

KERNEL:=lib/elixir/ebin/Elixir.Kernel.beam
UNICODE:=lib/elixir/ebin/Elixir.String.Unicode.beam

default: compile

compile: lib/elixir/src/elixir.app.src erlang elixir

lib/elixir/src/elixir.app.src: src/elixir.app.src
	@ rm -rf lib/elixir/src/elixir.app.src
	@ cp src/elixir.app.src lib/elixir/src/elixir.app.src

erlang:
	@ cd lib/elixir && $(REBAR) compile

# Since Mix depends on EEx and EEx depends on
# Mix, we first compile EEx without the .app
# file, then mix and then compile eex fully
elixir: kernel lib/eex/ebin/Elixir.EEx.beam mix ex_unit eex iex

kernel: $(KERNEL) VERSION
$(KERNEL): lib/elixir/lib/*.ex lib/elixir/lib/*/*.ex
	@ if [ ! -f $(KERNEL) ]; then                       \
		echo "==> bootstrap (compile)";                 \
		$(ERL) -s elixir_compiler core -s erlang halt;  \
	fi
	@ echo "==> kernel (compile)";
	@ $(ELIXIRC) "lib/elixir/lib/**/*.ex" -o lib/elixir/ebin;
	@ $(MAKE) unicode
	@ rm -rf lib/elixir/ebin/elixir.app
	@ cd lib/elixir && $(REBAR) compile

unicode: $(UNICODE)
$(UNICODE): lib/elixir/priv/unicode.ex lib/elixir/priv/UnicodeData.txt lib/elixir/priv/NamedSequences.txt
	@ echo "==> unicode (compile)";
	@ echo "This step can take up to a minute to compile in order to embed the Unicode database"
	@ $(ELIXIRC) lib/elixir/priv/unicode.ex -o lib/elixir/ebin;

$(eval $(call APP_TEMPLATE,ex_unit,ExUnit))
$(eval $(call APP_TEMPLATE,eex,EEx))
$(eval $(call APP_TEMPLATE,mix,Mix))
$(eval $(call APP_TEMPLATE,iex,IEx))

install: compile
	@ echo "==> elixir (install)"
	for dir in lib/*; do \
		install -m755 -d $(INSTALL_PATH)/lib/elixir/$$dir/ebin; \
		install -m644 $$dir/ebin/* $(INSTALL_PATH)/lib/elixir/$$dir/ebin; \
	done
	install -m755 -d $(INSTALL_PATH)/lib/elixir/bin
	install -m755 $(filter-out %.bat, $(wildcard bin/*)) $(INSTALL_PATH)/lib/elixir/bin
	install -m755 -d $(INSTALL_PATH)/bin
	ln -sf $(INSTALL_PATH)/lib/elixir/bin/* $(INSTALL_PATH)/bin

clean:
	@ cd lib/elixir && $(REBAR) clean
	rm -rf ebin
	rm -rf lib/*/ebin
	rm -rf lib/*/test/tmp
	rm -rf lib/mix/test/fixtures/git_repo
	rm -rf lib/*/tmp
	rm -rf lib/elixir/src/elixir.app.src
	rm -rf lib/elixir/src/*_lexer.erl
	rm -rf lib/elixir/src/*_parser.erl
	rm -rf lib/elixir/test/ebin

#==> Release tasks

SOURCE_REF = $(shell head="$$(git rev-parse HEAD)" tag="$$(git tag --points-at $$head | tail -1)" ; echo "$${tag:-$$head}\c")

docs: compile
	mkdir -p ebin
	rm -rf docs
	cp -R -f lib/*/ebin/*.beam ./ebin
	bin/elixir ../ex_doc/bin/ex_doc "Elixir" "$(VERSION)" -m Kernel -u "https://github.com/elixir-lang/elixir" --source-ref "$(call SOURCE_REF)"
	rm -rf ebin

release_zip: compile
	rm -rf v$(VERSION).zip
	zip -9 -r v$(VERSION).zip bin CHANGELOG.md LEGAL lib/*/ebin LICENSE README.md rel VERSION

release_docs: docs
	cd ../elixir-lang.github.com && git checkout master
	rm -rf ../elixir-lang.github.com/docs/master
	mv docs ../elixir-lang.github.com/docs/master

release_erl: compile
	@ rm -rf rel/elixir
	@ cd rel && ../rebar generate

#==> Tests tasks

test: test_erlang test_elixir

test_erlang: compile
	@ echo "==> elixir (eunit)"
	@ mkdir -p lib/elixir/test/ebin
	@ $(ERLC) -pa lib/elixir/ebin -o lib/elixir/test/ebin lib/elixir/test/erlang/*.erl
	@ $(ERL) -pa lib/elixir/test/ebin -s test_helper test -s erlang halt;
	@ echo

test_elixir: test_kernel test_ex_unit test_doctest test_mix test_eex test_iex

test_doctest: compile
	@ echo "==> doctest (exunit)"
	@ cd lib/elixir && ../../bin/elixir -r "test/doctest.exs";

test_kernel: compile
	@ echo "==> kernel (exunit)"
	@ cd lib/elixir && ../../bin/elixir -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs";

.dialyzer.base_plt:
	@ echo "==> Adding Erlang/OTP basic applications to a new base PLT"
	@ dialyzer --output_plt .dialyzer.base_plt --build_plt --apps erts kernel stdlib compiler syntax_tools inets crypto ssl

dialyze: .dialyzer.base_plt
	@ rm -f .dialyzer_plt
	@ cp .dialyzer.base_plt .dialyzer_plt
	@ echo "==> Adding Elixir to PLT..."
	@ dialyzer --plt .dialyzer_plt --add_to_plt -r lib/elixir/ebin lib/ex_unit/ebin lib/mix/ebin lib/iex/ebin lib/eex/ebin
	@ echo "==> Dialyzing Elixir..."
	@ dialyzer --plt .dialyzer_plt -r lib/elixir/ebin lib/ex_unit/ebin lib/mix/ebin lib/iex/ebin lib/eex/ebin
