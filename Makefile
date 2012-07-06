REBAR:=$(shell which rebar || echo `pwd`/rebar)
ELIXIRC:=bin/elixirc --ignore-module-conflict $(ELIXIRC_OPTS)
ERLC:=erlc -I lib/elixir/include
ERL:=erl -I lib/elixir/include -noshell -env ERL_LIBS $ERL_LIBS:lib
FULLFLAG:=.full
VERSION:=0.6.0.dev

.PHONY: 1
.NOTPARALLEL: compile

#==> Templates
define APP_TEMPLATE
@ rm -rf lib/$(1)/ebin/$(1).app;                  \
for f in lib/$(1)/ebin/__MAIN__-$(2)-*.beam; do   \
	filename=$$(basename $$f);                    \
	export RES=`echo \'$${filename%.*}\',$$RES`;  \
done;                                             \
echo <<EOF > lib/$(1)/ebin/$(1).app               \
{application, $(1),                               \
[{description, \"$(1)\"},                         \
 {vsn, \"$(VERSION)\"},                           \
 {modules, [                                      \
    $$RES                                         \
    \'__MAIN__-$(2)\'                             \
  ]},                                             \
 {applications, [kernel,stdlib,elixir]}           \
]}.
endef

define TASK_TEMPLATE
$(1): lib/$(1)/ebin/__MAIN__-$(2).beam

lib/$(1)/ebin/__MAIN__-$(2).beam: $(wildcard lib/$(1)/lib/*.ex) $(wildcard lib/$(1)/lib/*/*.ex) $(wildcard lib/$(1)/lib/*/*/*.ex) $$(FORCE)
	@ echo "==> $(1) (compile)"
	@ $$(ELIXIRC) "lib/$(1)/lib/**/*.ex" -o lib/$(1)/ebin
	@ $$(call APP_TEMPLATE,$(1),$(2))

test_$(1): $(1)
	@ echo "==> $(1) (exunit)"
	@ cd lib/$(1) && time ../../bin/elixir -r "test/test_helper.exs" -pr "test/**/*_test.exs"
endef

#==> Compilation tasks
KERNEL:=lib/elixir/ebin/__MAIN__-Elixir-Builtin.beam

compile: lib/elixir/src/elixir.app.src erlang elixir

lib/elixir/src/elixir.app.src: src/elixir.app.src
	@ rm -rf lib/elixir/src/elixir.app.src
	@ cp src/elixir.app.src lib/elixir/src/elixir.app.src

erlang:
	@ cd lib/elixir && $(REBAR) compile

elixir: kernel ex_unit eex mix

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

clean:
	@ rm -rf .full
	@ rm -rf lib/*/ebin
	@ cd lib/elixir && $(REBAR) clean

#==> Release tasks
$(FULLFLAG): $(wildcard lib/*/ebin/*)
	make ELIXIRC_OPTS="--docs --debug-info" FORCE=1
	touch $(FULLFLAG)

zip: $(FULLFLAG)
	@ echo "==> elixir (full)"
	rm -rf v$(VERSION).zip
	zip -r v$(VERSION).zip ebin `git ls-files`
	zip v$(VERSION).zip -d .git .gitignore .travis.yml

docs: $(FULLFLAG)
	mkdir -p ebin
	rm -rf docs
	cp -R -f lib/*/ebin/__*.beam ./ebin
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
	@ time $(ERL) -pa lib/elixir/test/ebin -s test_helper test -s erlang halt
	@ echo

test_elixir: test_kernel test_ex_unit test_eex test_mix

test_kernel: kernel
	@ echo "==> kernel (exunit)"
	@ cd lib/elixir && time ../../bin/elixir -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs"