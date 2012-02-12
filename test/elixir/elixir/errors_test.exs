Code.require_file "../../test_helper", __FILE__

defmodule Elixir::ErrorsTest do
  use ExUnit::Case

  defmodule __MODULE__ :: UnproperMacro do
    defmacro unproper(args), do: args
    defmacro exit(args), do: args
  end

  def test_invalid_token do
    assert_equal "invalid token: \end", format_rescue '\end'
  end

  def test_invalid_partial do
    assert_equal "partial variable &2 cannot be defined without &1", format_rescue '&2 + 3'
  end

  def test_syntax_error do
    assert_equal "syntax error before: '}'", format_rescue 'case 1 do }'
  end

  def test_syntax_error_with_no_token do
    assert_equal "syntax error: expression is incomplete", format_rescue 'case 1 do'
  end

  def test_bad_form do
    assert_equal "function bar/0 undefined", format_rescue 'defmodule Foo do\ndef foo, do: bar\nend'
  end

  def test_unbound_var do
    assert_equal "unbound variable ^x", format_rescue('^x = 1')
  end

  def test_unbound_not_assignment do
    assert_equal "cannot access variable ^x outside of assignment", format_rescue('^x')
  end

  def test_unbound_expr do
    assert_equal "cannot use ^ with expression at ^x, ^ must be used only with variables", format_rescue('^x(1)')
  end

  def test_name_for_defmodule do
    assert_equal "invalid module name: 3", format_rescue 'defmodule 1 + 2, do: 3'
  end

  def test_invalid_scope_for_function do
    assert_equal "cannot invoke def outside module", format_rescue 'def Foo, do: 2'
    assert_equal "cannot invoke defmacro outside module", format_rescue '\n\ndefmacro Foo, do: 2'
  end

  def test_invalid_quote_args do
    assert_equal "invalid args for quote", format_rescue 'quote 1'
  end

  def test_invalid_fn_args do
    assert_equal "no block given to fn", format_rescue 'fn 1'
  end

  def test_unproper_macro do
    assert_equal "key value blocks not supported by ::Elixir::ErrorsTest::UnproperMacro.unproper/1",
      format_rescue 'defmodule Foo do\nrequire Elixir::ErrorsTest::UnproperMacro\nElixir::ErrorsTest::UnproperMacro.unproper do\nmatch: 1\nmatch: 2\nend\nend'
  end

  def test_macro_conflict do
    assert_equal "imported ::Elixir::Builtin.defrecord/2 conflicts with local function",
      format_rescue 'defmodule Foo do\ndefrecord(::Elixir::ErrorsTest::MacroConflict, a: 1)\ndef defrecord(_, _), do: OMG\nend'
  end

  def test_erlang_function_conflict do
    assert_equal "function exit/1 already imported from erlang",
      format_rescue 'defmodule Foo do import Elixir::ErrorsTest::UnproperMacro, only: [exit: 1]\nend'
  end

  def test_import_invalid_macro do
    assert_equal "cannot import ::Elixir::Builtin.invalid/1 because it doesn't exist",
      format_rescue 'defmodule Foo do\nimport Elixir::Builtin, only: [invalid: 1]\nend'
  end

  def test_unrequired_macro do
    assert_equal "tried to invoke macro ::Elixir::ErrorsTest::UnproperMacro.unproper/1 but module was not required. Required: ['::Elixir::Builtin']",
      format_rescue 'defmodule Foo do\nElixir::ErrorsTest::UnproperMacro.unproper([])\nend'
  end

  def test_def_defmacro_clause_change do
    assert_equal "defmacro foo/1 already defined as def",
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndefmacro foo(x), do: x\nend'
  end

  def test_visibility_clause_change do
    assert_equal "function foo/1 already defined with visibility public",
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndefp foo(x), do: x\nend'
  end

  def test_clause_change do
    assert_equal "function foo/1 does not match previous clause",
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndef bar(x), do: x\ndef foo(x), do: x\nend'
  end

  def test_internal_function_overriden do
    assert_equal "function __info__/1 is internal and should not be overriden",
      format_rescue 'defmodule Foo do\ndef __info__(_), do: []\nend'
  end

  def test_no_macros do
    assert_equal "could not load macros from module lists",
      format_rescue 'defmodule Foo do\nimport :macros, Erlang.lists\nend'
  end

  def test_unloaded_module do
    assert_equal "module ::Certainly::Doesnt::Exist is not loaded, reason: nofile",
      format_rescue 'import Certainly::Doesnt::Exist'
  end

  def test_scheduled_module do
    assert_equal "module ::Hygiene is not loaded but was defined. This may happen because the module is nested inside another module. Try defining the module outside the context that requires it.",
      format_rescue 'defmodule Foo do; defmodule Hygiene do; end; import Hygiene; end'
  end

  def test_already_compiled_module do
    assert_equal "could not call eval_quoted on module ::Record because it was already compiled",
      format_rescue 'Module.eval_quoted ::Record, quote(do: 1), [], __FILE__, __LINE__'
  end

  def test_interpolation_error do
    assert_equal "syntax error before: ')'", format_rescue '"foo\#{case 1 do )}bar"'
  end

  def test_invalid_kv_for_match do
    assert_equal "invalid key invalid",
      format_rescue 'case true do\ninvalid: 2\nafter: 3\nend'
  end

  def test_cant_define_local_due_to_in_erlang_macros_conflict do
    assert_equal "cannot define local quote/1 because it conflicts with Elixir internal macros",
      format_rescue 'defmodule Foo do\ndef quote(x), do: x\ndef bar(x), do: quote(do: x)\nend'
  end

  def test_already_defined_module do
    assert_equal "module ::Record already defined", format_rescue 'defmodule Record, do: true'
  end

  def test_duplicated_bitstring_size do
    assert_equal "duplicated size specifier 12 in <<>>", format_rescue '<<1|12-12>>'
  end

  def test_invalid_bitstring_specified do
    assert_equal "invalid specifier for <<>>", format_rescue '<<1|12-binary()>>'
  end

  ## Helpers

  defp format_rescue(expr) do
    result = try do
      Erlang.elixir.eval(expr)
      nil
    rescue: error
      error.message
    end

    result || raise(ExUnit::AssertionError, message: "Expected function given to format_rescue to fail")
  end
end
