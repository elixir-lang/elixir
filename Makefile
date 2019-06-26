PREFIX ?= /usr/local
SHARE_PREFIX ?= $(PREFIX)/share
MAN_PREFIX ?= $(SHARE_PREFIX)/man
CANONICAL := master/ # master/ or vMAJOR.MINOR/
ELIXIRC := bin/elixirc --verbose --ignore-module-conflict $(ELIXIRC_OPTS)
ERLC := erlc -I lib/elixir/include $(ERLC_OPTS)
ERL := erl -I lib/elixir/include -noshell -pa lib/elixir/ebin
GENERATE_APP := $(CURDIR)/lib/elixir/generate_app.escript
VERSION := $(strip $(shell cat VERSION))
Q := @
LIBDIR := lib
BINDIR := bin
INSTALL = install
INSTALL_DIR = $(INSTALL) -m755 -d
INSTALL_DATA = $(INSTALL) -m644
INSTALL_PROGRAM = $(INSTALL) -m755
GIT_REVISION = $(strip $(shell git rev-parse HEAD 2> /dev/null ))
GIT_TAG = $(strip $(shell head="$(call GIT_REVISION)"; git tag --points-at $$head 2> /dev/null | tail -1) )
SOURCE_DATE_EPOCH_PATH = lib/elixir/tmp/ebin_reproducible
SOURCE_DATE_EPOCH_FILE = $(SOURCE_DATE_EPOCH_PATH)/SOURCE_DATE_EPOCH

.PHONY: install compile erlang elixir unicode app build_plt clean_plt dialyze test check_reproducible clean clean_residual_files install_man clean_man docs Docs.zip Precompiled.zip zips
.NOTPARALLEL: compile

#==> Functions

define CHECK_ERLANG_RELEASE
	erl -noshell -eval '{V,_} = string:to_integer(erlang:system_info(otp_release)), io:fwrite("~s", [is_integer(V) and (V >= 20)])' -s erlang halt | grep -q '^true'; \
		if [ $$? != 0 ]; then \
		  echo "At least Erlang/OTP 20.0 is required to build Elixir"; \
		  exit 1; \
		fi
endef

define APP_TEMPLATE
$(1): lib/$(1)/ebin/Elixir.$(2).beam lib/$(1)/ebin/$(1).app

lib/$(1)/ebin/$(1).app: lib/$(1)/mix.exs
	$(Q) cd lib/$(1) && ../../bin/elixir -e 'Mix.start(:permanent, [])' -r mix.exs -e 'Mix.Task.run("compile.app", ~w[--compile-path ebin])'

lib/$(1)/ebin/Elixir.$(2).beam: $(wildcard lib/$(1)/lib/*.ex) $(wildcard lib/$(1)/lib/*/*.ex) $(wildcard lib/$(1)/lib/*/*/*.ex)
	@ echo "==> $(1) (compile)"
	@ rm -rf lib/$(1)/ebin
	$(Q) cd lib/$(1) && ../../$$(ELIXIRC) "lib/**/*.ex" -o ebin

test_$(1): compile $(1)
	@ echo "==> $(1) (ex_unit)"
	$(Q) cd lib/$(1) && ../../bin/elixir -r "test/test_helper.exs" -pr "test/**/*_test.exs";
endef

define WRITE_SOURCE_DATE_EPOCH
$(shell mkdir -p $(SOURCE_DATE_EPOCH_PATH) && bin/elixir -e \
  'IO.puts System.build_info()[:date] \
   |> DateTime.from_iso8601() \
   |> elem(1) \
   |> DateTime.to_unix()' > $(SOURCE_DATE_EPOCH_FILE))
endef

define READ_SOURCE_DATE_EPOCH
$(strip $(shell cat $(SOURCE_DATE_EPOCH_FILE)))
endef

#==> Compilation tasks

APP := lib/elixir/ebin/elixir.app
PARSER := lib/elixir/src/elixir_parser.erl
KERNEL := lib/elixir/ebin/Elixir.Kernel.beam
UNICODE := lib/elixir/ebin/Elixir.String.Unicode.beam

default: compile

compile: erlang $(APP) elixir

erlang: $(PARSER)
	$(Q) if [ ! -f $(APP) ]; then $(call CHECK_ERLANG_RELEASE); fi
	$(Q) cd lib/elixir && mkdir -p ebin && erl -make

$(PARSER): lib/elixir/src/elixir_parser.yrl
	$(Q) erlc -o $@ +'{verbose,true}' +'{report,true}' $<

# Since Mix depends on EEx and EEx depends on Mix,
# we first compile EEx without the .app file,
# then Mix and then compile EEx fully
elixir: stdlib lib/eex/ebin/Elixir.EEx.beam mix ex_unit logger eex iex

stdlib: $(KERNEL) VERSION
$(KERNEL): lib/elixir/lib/*.ex lib/elixir/lib/*/*.ex lib/elixir/lib/*/*/*.ex
	$(Q) if [ ! -f $(KERNEL) ]; then \
		echo "==> bootstrap (compile)"; \
		$(ERL) -s elixir_compiler bootstrap -s erlang halt; \
	fi
	$(Q) $(MAKE) unicode
	@ echo "==> elixir (compile)";
	$(Q) cd lib/elixir && ../../$(ELIXIRC) "lib/**/*.ex" -o ebin;
	$(Q) $(MAKE) app

app: $(APP)
$(APP): lib/elixir/src/elixir.app.src lib/elixir/ebin VERSION $(GENERATE_APP)
	$(Q) $(GENERATE_APP) $< $@ $(VERSION)

unicode: $(UNICODE)
$(UNICODE): lib/elixir/unicode/*
	@ echo "==> unicode (compile)";
	$(Q) $(ELIXIRC) lib/elixir/unicode/unicode.ex -o lib/elixir/ebin;
	$(Q) $(ELIXIRC) lib/elixir/unicode/properties.ex -o lib/elixir/ebin;
	$(Q) $(ELIXIRC) lib/elixir/unicode/tokenizer.ex -o lib/elixir/ebin;

$(eval $(call APP_TEMPLATE,ex_unit,ExUnit))
$(eval $(call APP_TEMPLATE,logger,Logger))
$(eval $(call APP_TEMPLATE,eex,EEx))
$(eval $(call APP_TEMPLATE,mix,Mix))
$(eval $(call APP_TEMPLATE,iex,IEx))

install: compile
	@ echo "==> elixir (install)"
	$(Q) for dir in lib/*; do \
		rm -rf $(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/$$dir/ebin; \
		$(INSTALL_DIR) "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/$$dir/ebin"; \
		$(INSTALL_DATA) $$dir/ebin/* "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/$$dir/ebin"; \
	done
	$(Q) $(INSTALL_DIR) "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/bin"
	$(Q) $(INSTALL_PROGRAM) $(filter-out %.ps1, $(filter-out %.bat, $(wildcard bin/*))) "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/bin"
	$(Q) $(INSTALL_DIR) "$(DESTDIR)$(PREFIX)/$(BINDIR)"
	$(Q) for file in "$(DESTDIR)$(PREFIX)"/$(LIBDIR)/elixir/bin/*; do \
		ln -sf "../$(LIBDIR)/elixir/bin/$${file##*/}" "$(DESTDIR)$(PREFIX)/$(BINDIR)/"; \
	done
	$(MAKE) install_man

check_reproducible: compile
	$(Q) echo "==> Checking for reproducible builds..."
	$(Q) rm -rf lib/*/tmp/ebin_reproducible/
	$(call WRITE_SOURCE_DATE_EPOCH)
	$(Q) mkdir -p lib/elixir/tmp/ebin_reproducible/ \
	              lib/eex/tmp/ebin_reproducible/ \
	              lib/iex/tmp/ebin_reproducible/ \
	              lib/logger/tmp/ebin_reproducible/ \
	              lib/mix/tmp/ebin_reproducible/
	$(Q) mv lib/elixir/ebin/* lib/elixir/tmp/ebin_reproducible/
	$(Q) mv lib/eex/ebin/* lib/eex/tmp/ebin_reproducible/
	$(Q) mv lib/iex/ebin/* lib/iex/tmp/ebin_reproducible/
	$(Q) mv lib/logger/ebin/* lib/logger/tmp/ebin_reproducible/
	$(Q) mv lib/mix/ebin/* lib/mix/tmp/ebin_reproducible/
	SOURCE_DATE_EPOCH=$(call READ_SOURCE_DATE_EPOCH) $(MAKE) compile
	$(Q) echo "Diffing..."
	$(Q) diff -r lib/elixir/ebin/ lib/elixir/tmp/ebin_reproducible/
	$(Q) diff -r lib/eex/ebin/ lib/eex/tmp/ebin_reproducible/
	$(Q) diff -r lib/iex/ebin/ lib/iex/tmp/ebin_reproducible/
	$(Q) diff -r lib/logger/ebin/ lib/logger/tmp/ebin_reproducible/
	$(Q) diff -r lib/mix/ebin/ lib/mix/tmp/ebin_reproducible/
	$(Q) echo "Builds are reproducible"

clean:
	rm -rf ebin
	rm -rf lib/*/ebin
	rm -rf $(PARSER)
	$(Q) $(MAKE) clean_residual_files

clean_elixir:
	$(Q) rm -f lib/*/ebin/Elixir.*.beam

clean_residual_files:
	rm -rf lib/*/_build/
	rm -rf lib/*/tmp/
	rm -rf lib/elixir/test/ebin/
	rm -rf lib/mix/test/fixtures/deps_on_git_repo/
	rm -rf lib/mix/test/fixtures/git_rebar/
	rm -rf lib/mix/test/fixtures/git_repo/
	rm -rf lib/mix/test/fixtures/git_sparse_repo/
	rm -f erl_crash.dump
	$(Q) $(MAKE) clean_man

#==> Documentation tasks

LOGO_PATH = $(shell test -f ../docs/logo.png && echo "--logo ../docs/logo.png")
SOURCE_REF = $(shell tag="$(call GIT_TAG)" revision="$(call GIT_REVISION)"; echo "$${tag:-$$revision}\c")
DOCS_FORMAT = html
COMPILE_DOCS = bin/elixir ../ex_doc/bin/ex_doc "$(1)" "$(VERSION)" "lib/$(2)/ebin" -m "$(3)" -u "https://github.com/elixir-lang/elixir" --source-ref "$(call SOURCE_REF)" $(call LOGO_PATH) -o doc/$(2) -n https://hexdocs.pm/$(2)/$(CANONICAL) -p https://elixir-lang.org/docs.html -f "$(DOCS_FORMAT)" $(4)

docs: compile ../ex_doc/bin/ex_doc docs_elixir docs_eex docs_mix docs_iex docs_ex_unit docs_logger

docs_elixir: compile ../ex_doc/bin/ex_doc
	@ echo "==> ex_doc (elixir)"
	$(Q) rm -rf doc/elixir
	$(call COMPILE_DOCS,Elixir,elixir,Kernel,-c lib/elixir/docs.exs)

docs_eex: compile ../ex_doc/bin/ex_doc
	@ echo "==> ex_doc (eex)"
	$(Q) rm -rf doc/eex
	$(call COMPILE_DOCS,EEx,eex,EEx)

docs_mix: compile ../ex_doc/bin/ex_doc
	@ echo "==> ex_doc (mix)"
	$(Q) rm -rf doc/mix
	$(call COMPILE_DOCS,Mix,mix,Mix)

docs_iex: compile ../ex_doc/bin/ex_doc
	@ echo "==> ex_doc (iex)"
	$(Q) rm -rf doc/iex
	$(call COMPILE_DOCS,IEx,iex,IEx)

docs_ex_unit: compile ../ex_doc/bin/ex_doc
	@ echo "==> ex_doc (ex_unit)"
	$(Q) rm -rf doc/ex_unit
	$(call COMPILE_DOCS,ExUnit,ex_unit,ExUnit)

docs_logger: compile ../ex_doc/bin/ex_doc
	@ echo "==> ex_doc (logger)"
	$(Q) rm -rf doc/logger
	$(call COMPILE_DOCS,Logger,logger,Logger)

../ex_doc/bin/ex_doc:
	@ echo "ex_doc is not found in ../ex_doc as expected. See README for more information."
	@ false

#==> Zip tasks

Docs.zip: docs
	rm -f Docs-v$(VERSION).zip
	zip -9 -r Docs-v$(VERSION).zip CHANGELOG.md doc NOTICE LICENSE README.md
	@ echo "Docs file created $(CURDIR)/Docs-v$(VERSION).zip"

Precompiled.zip: build_man compile
	rm -f Precompiled-v$(VERSION).zip
	zip -9 -r Precompiled-v$(VERSION).zip bin CHANGELOG.md lib/*/ebin lib/*/lib LICENSE man NOTICE README.md VERSION
	@ echo "Precompiled file created $(CURDIR)/Precompiled-v$(VERSION).zip"

zips: Precompiled.zip Docs.zip
	@ echo ""
	@ echo "### Checksums"
	@ echo ""
	@ shasum -a 1 < Precompiled-v$(VERSION).zip | sed -e "s/-//" | xargs echo "  * Precompiled.zip SHA1:"
	@ shasum -a 512 < Precompiled-v$(VERSION).zip | sed -e "s/-//" | xargs echo "  * Precompiled.zip SHA512:"
	@ shasum -a 1 < Docs-v$(VERSION).zip | sed -e "s/-//" | xargs echo "  * Docs.zip SHA1:"
	@ shasum -a 512 < Docs-v$(VERSION).zip | sed -e "s/-//" | xargs echo "  * Docs.zip SHA512:"
	@ echo ""

#==> Test tasks

test: test_formatted test_erlang test_elixir

test_windows: test test_taskkill

test_taskkill:
	taskkill //IM erl.exe //F //T //FI "MEMUSAGE gt 0"
	taskkill //IM epmd.exe //F //T //FI "MEMUSAGE gt 0"

TEST_ERL = lib/elixir/test/erlang
TEST_EBIN = lib/elixir/test/ebin
TEST_ERLS = $(addprefix $(TEST_EBIN)/, $(addsuffix .beam, $(basename $(notdir $(wildcard $(TEST_ERL)/*.erl)))))

test_formatted: compile
	bin/elixir bin/mix format --check-formatted

test_erlang: compile $(TEST_ERLS)
	@ echo "==> elixir (eunit)"
	$(Q) $(ERL) -pa $(TEST_EBIN) -s test_helper test;
	@ echo ""

$(TEST_EBIN)/%.beam: $(TEST_ERL)/%.erl
	$(Q) mkdir -p $(TEST_EBIN)
	$(Q) $(ERLC) -o $(TEST_EBIN) $<

test_elixir: test_stdlib test_ex_unit test_logger test_mix test_eex test_iex

test_stdlib: compile
	@ echo "==> elixir (ex_unit)"
	$(Q) exec epmd & exit
	$(Q) if [ "$(OS)" = "Windows_NT" ]; then \
		cd lib/elixir && cmd //C call ../../bin/elixir.bat -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs"; \
	else \
		cd lib/elixir && ../../bin/elixir -r "test/elixir/test_helper.exs" -pr "test/elixir/**/*_test.exs"; \
	fi

#==> Dialyzer tasks

DIALYZER_OPTS = --no_check_plt --fullpath -Werror_handling -Wunmatched_returns -Wunderspecs
PLT = .elixir.plt

$(PLT):
	@ echo "==> Building PLT with Elixir's dependencies..."
	$(Q) dialyzer --output_plt $(PLT) --build_plt --apps erts kernel stdlib compiler syntax_tools parsetools tools ssl inets

clean_plt:
	$(Q) rm -f $(PLT)

build_plt: clean_plt $(PLT)

dialyze: compile $(PLT)
	@ echo "==> Dialyzing Elixir..."
	$(Q) dialyzer -pa lib/elixir/ebin --plt $(PLT) $(DIALYZER_OPTS) lib/*/ebin

#==> Man page tasks

build_man: man/iex.1 man/elixir.1

man/iex.1:
	$(Q) cp man/iex.1.in man/iex.1
	$(Q) sed -i.bak "/{COMMON}/r man/common" man/iex.1
	$(Q) sed -i.bak "/{COMMON}/d" man/iex.1
	$(Q) rm -f man/iex.1.bak

man/elixir.1:
	$(Q) cp man/elixir.1.in man/elixir.1
	$(Q) sed -i.bak "/{COMMON}/r man/common" man/elixir.1
	$(Q) sed -i.bak "/{COMMON}/d" man/elixir.1
	$(Q) rm -f man/elixir.1.bak

clean_man:
	rm -f man/elixir.1
	rm -f man/elixir.1.bak
	rm -f man/iex.1
	rm -f man/iex.1.bak

install_man: build_man
	$(Q) mkdir -p $(DESTDIR)$(MAN_PREFIX)/man1
	$(Q) $(INSTALL_DATA) man/elixir.1  $(DESTDIR)$(MAN_PREFIX)/man1
	$(Q) $(INSTALL_DATA) man/elixirc.1 $(DESTDIR)$(MAN_PREFIX)/man1
	$(Q) $(INSTALL_DATA) man/iex.1     $(DESTDIR)$(MAN_PREFIX)/man1
	$(Q) $(INSTALL_DATA) man/mix.1     $(DESTDIR)$(MAN_PREFIX)/man1
	$(MAKE) clean_man
