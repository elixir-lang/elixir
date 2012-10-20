REBAR:=$(shell echo `pwd`/rebar)
ELIXIRC:=bin/elixirc --ignore-module-conflict $(ELIXIRC_OPTS)
ERLC:=erlc -I lib/elixir/include
ERL:=erl -I lib/elixir/include -noshell -env ERL_LIBS $ERL_LIBS:lib
FULLFLAG:=.full
VERSION:=0.7.1.dev

.PHONY: 1
.NOTPARALLEL: compile

#==> Templates
define TASK_TEMPLATE
$(1): lib/$(1)/ebin/Elixir-$(2).beam lib/$(1)/ebin/$(1).app

lib/$(1)/ebin/$(1).app:
	@ cd lib/$(1) && ../../bin/elixir ../../bin/mix compile.app

lib/$(1)/ebin/Elixir-$(2).beam: $(wildcard lib/$(1)/lib/*.ex) $(wildcard lib/$(1)/lib/*/*.ex) $(wildcard lib/$(1)/lib/*/*/*.ex) $$(FORCE)
	@ echo "==> $(1) (compile)"
	@ $$(ELIXIRC) "lib/$(1)/lib/**/*.ex" -o lib/$(1)/ebin

test_$(1): $(1)
	@ echo "==> $(1) (exunit)"
	@ cd lib/$(1) && time ../../bin/elixir -r "test/test_helper.exs" -pr "test/**/*_test.exs";
endef

#==> Compilation tasks
KERNEL:=lib/elixir/ebin/Elixir-Kernel.beam

compile: lib/elixir/src/elixir.app.src erlang elixir

lib/elixir/src/elixir.app.src: src/elixir.app.src
	@ rm -rf lib/elixir/src/elixir.app.src
	@ cp src/elixir.app.src lib/elixir/src/elixir.app.src

erlang:
	@ cd lib/elixir && $(REBAR) compile

# We need to compile only EEx (without the app)
# file so we can compile Mix
elixir: kernel lib/eex/ebin/Elixir-EEx.beam mix ex_unit eex iex

kernel: $(KERNEL)
$(KERNEL): lib/elixir/lib/*.ex lib/elixir/lib/*/*.ex $(FORCE)
	@ if [ -f $(KERNEL) ]; then                                 \
		echo "==> kernel (compile)";                            \
		$(ELIXIRC) "lib/elixir/lib/**/*.ex" -o lib/elixir/ebin; \
	else                                                        \
		echo "==> bootstrap (compile)";                         \
		$(ERL) -s elixir_compiler core -s erlang halt;          \
	fi
	@ rm -rf lib/elixir/ebin/elixir.app
	@ cd lib/elixir && $(REBAR) compile

$(eval $(call TASK_TEMPLATE,ex_unit,ExUnit))
$(eval $(call TASK_TEMPLATE,eex,EEx))
$(eval $(call TASK_TEMPLATE,mix,Mix))
$(eval $(call TASK_TEMPLATE,iex,IEx))

clean:
	@ cd lib/elixir && $(REBAR) clean
	rm -rf .full
	rm -rf ebin
	rm -rf lib/*/ebin
	rm -rf lib/*/test/tmp
	rm -rf lib/mix/test/fixtures/git_repo
	rm -rf lib/mix/tmp

#==> Release tasks
$(FULLFLAG): $(wildcard lib/*/ebin/*)
	make ELIXIRC_OPTS="--debug-info" FORCE=1
	touch $(FULLFLAG)

zip: $(FULLFLAG)
	rm -rf v$(VERSION).zip
	zip -9 -r v$(VERSION).zip bin CHANGELOG.md LEGAL lib/*/ebin LICENSE README.md rel

docs: $(FULLFLAG)
	mkdir -p ebin
	rm -rf docs
	cp -R -f lib/*/ebin/*.beam ./ebin
	bin/elixir ../exdoc/bin/exdoc
	rm -rf ebin

release_docs: docs
	cd ../elixir-lang.github.com && git checkout master
	rm -rf ../elixir-lang.github.com/docs/master
	mv output ../elixir-lang.github.com/docs/master

release_erl: $(FULLFLAG)
	@ rm -rf rel/elixir
	@ cd rel && ../rebar generate

#==> Tests tasks
test: test_erlang test_elixir

test_erlang: compile
	@ echo "==> elixir (eunit)"
	@ mkdir -p lib/elixir/test/ebin
	@ $(ERLC) -pa lib/elixir/ebin -o lib/elixir/test/ebin lib/elixir/test/erlang/*.erl
	@ time $(ERL) -pa lib/elixir/test/ebin -s test_helper test -s erlang halt;
	@ echo

test_elixir: test_kernel test_mix test_ex_unit test_eex test_iex

test_kernel: compile
	@ echo "==> kernel (exunit)"
	@ cd lib/elixir && time ../../bin/elixir -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs";
