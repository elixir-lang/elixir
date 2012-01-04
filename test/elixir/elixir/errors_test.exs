defmodule Elixir::ErrorsTest do
  use ExUnit::Case

  defmodule __MODULE__ :: UnproperMacro do
    defmacro unproper(args), do: args
    defmacro case(x, _), do: x
  end

  def test_invalid_token do
    "nofile:1: invalid token: \end" = format_catch '\end'
  end

  def test_syntax_error do
    "nofile:1: syntax error before: '}'" = format_catch 'case 1 do }'
  end

  def test_syntax_error_with_no_token do
    "nofile:1: syntax error" = format_catch 'case 1 do'
  end

  def test_bad_form do
    "nofile:2: function bar/0 undefined" = format_catch 'defmodule Foo do\ndef foo, do: bar\nend'
  end

  def test_unbound_var do
    "nofile:1: unbound variable: ^x" = format_catch('^x = 1')
  end

  def test_unbound_not_assignment do
    "nofile:1: non-assignment scope for: ^x" = format_catch('^x')
  end

  def test_unbound_expr do
    "nofile:1: cannot bind expression at token: ^x" = format_catch('^x(1)')
  end

  def test_name_for_defmodule do
    "nofile:1: invalid module name: 3" = format_catch 'defmodule 1 + 2, do: 3'
  end

  def test_invalid_scope_for_function do
    "nofile:1: cannot define function outside module. invalid scope for: def" = format_catch 'def Foo, do: 2'
    "nofile:3: cannot define function outside module. invalid scope for: defmacro" = format_catch '\n\ndefmacro Foo, do: 2'
  end

  def test_invalid_quote_args do
    "nofile:1: invalid args for: quote" = format_catch 'quote 1'
  end

  def test_invalid_fn_args do
    "nofile:1: no block given for: fn" = format_catch 'fn 1'
  end

  def test_unproper_macro do
    "nofile:4: key value blocks not supported by: ::Elixir::ErrorsTest::UnproperMacro.unproper/1" =
      format_catch 'defmodule Foo do\nrequire Elixir::ErrorsTest::UnproperMacro\nElixir::ErrorsTest::UnproperMacro.unproper do\nmatch: 1\nmatch: 2\nend\nend'
  end

  def test_macro_conflict do
    "nofile:1: imported macro ::Elixir::Macros.defrecord/2 conflicts with local function or import" =
      format_catch 'defmodule Foo do\ndefrecord(::Elixir::ErrorsTest::MacroConflict, a: 1)\ndef defrecord(_, _), do: OMG\nend'
  end

  def test_require_invalid_macro do
    "nofile:2: ::Elixir::Macros.invalid/1 isn't a macro and can't be imported using require. Maybe you wanted to use import?" =
      format_catch 'defmodule Foo do\nrequire Elixir::Macros, only: [invalid: 1]\nend'
  end

  def test_unrequired_macro do
    "nofile:2: tried to use ::Record.getters_and_setters/3 but module was not required. Required: ['::Elixir::Macros']" =
      format_catch 'defmodule Foo do\nRecord.getters_and_setters([], 0, [])\nend'
  end

  def test_def_defmacro_clause_change do
    "nofile:3: defmacro foo/1 already defined as def" =
      format_catch 'defmodule Foo do\ndef foo(1), do: 1\ndefmacro foo(x), do: x\nend'
  end

  def test_visibility_clause_change do
    "nofile:3: function foo/1 already defined with visibility public" =
      format_catch 'defmodule Foo do\ndef foo(1), do: 1\ndefp foo(x), do: x\nend'
  end

  def test_clause_change do
    "nofile:4: function foo/1 clause does not match with previous one" =
      format_catch 'defmodule Foo do\ndef foo(1), do: 1\ndef bar(x), do: x\ndef foo(x), do: x\nend'
  end

  def test_internal_function_overriden do
    "nofile:1: function __macros__/0 is internal and should not be overriden" =
      format_catch 'defmodule Foo do\ndef __macros__, do: []\nend'
  end

  def test_no_macros do
    "nofile:2: could not load macros from module lists" =
      format_catch 'defmodule Foo do\nrequire Erlang.lists, import: true\nend'
  end

  def test_unloaded_module do
    "nofile:2: module ::Certainly::Doesnt::Exist is not loaded, reason: nofile"
      format_catch 'require Certainly::Doesnt::Exist, import: true'
  end

  def test_already_compiled_module do
    "nofile:1: could not invoke eval, module ::Record already compiled" =
      format_catch 'Module.eval ::Record, __FILE__, __LINE__, quote { 1 }'
  end

  def test_interpolation_error do
    "nofile:1: syntax error before: ')'" = format_catch '"foo\#{case 1 do )}bar"'
  end

  def test_invalid_kv_for_match do
    "nofile:1: invalid key: invalid" =
      format_catch 'case true do\ninvalid: 2\nafter: 3\nend'
  end

  def test_cant_import_in_erlang_macros_with_require do
    "nofile:1: cannot import ::Elixir::ErrorsTest::UnproperMacro.case/2 because it conflicts with Elixir internal macros" =
      format_catch 'require Elixir::ErrorsTest::UnproperMacro, import: true'
  end

  def test_cant_import_in_erlang_macros_with_import do
    "nofile:1: cannot import ::Elixir::ErrorsTest::UnproperMacro.case/2 because it conflicts with Elixir internal macros" =
      format_catch 'defmodule Foo, do: import Elixir::ErrorsTest::UnproperMacro, only: [case: 2]'
  end

  def test_cant_import_due_to_erlang_conflict do
    "nofile:1: import directive overrides pre R14 auto-imported BIF element/2\n - use \"-compile({no_auto_import,[element/2]}).\" to resolve name clash" =
      format_catch 'defmodule Foo, do: import Erlang.lists, only: [element: 2]'
  end

  def test_cant_define_local_due_to_in_erlang_macros_conflict do
    "nofile:1: cannot invoke local quote/1 because it conflicts with Elixir internal macros" =
      format_catch 'defmodule Foo do\ndef quote(x), do: x\ndef bar(x), do: quote(do: x)\nend'
  end

  def test_already_defined_module do
    "nofile:1: module ::Record already defined" = format_catch 'defmodule Record, do: true'
  end

  def test_duplicated_bitstring_size do
    "nofile:1: duplicated size specifier for: <<>>" = format_catch '<<1|12-12>>'
  end

  def test_invalid_bitstring_specified do
    "nofile:1: invalid specifier for: <<>>" = format_catch '<<1|12-binary()>>'
  end

  ## Helpers

  defp format_catch(expr) do
    try do
      Erlang.elixir.eval(expr)
      error { :bad_assertion, "Expected function given to format_catch to fail" }
    catch: { kind, error, _ }
      Elixir::Formatter.format_catch(kind, error)
    end
  end
end