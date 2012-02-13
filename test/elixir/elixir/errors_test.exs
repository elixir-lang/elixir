Code.require_file "../../test_helper", __FILE__

defmodule Elixir::ErrorsTest do
  use ExUnit::Case

  defmodule __MODULE__ :: UnproperMacro do
    defmacro unproper(args), do: args
    defmacro exit(args), do: args
  end

  test :invalid_token do
    assert_equal "nofile:1: invalid token: \end", format_rescue '\end'
  end

  test :invalid_partial do
    assert_equal "nofile:1: partial variable &2 cannot be defined without &1", format_rescue '&2 + 3'
  end

  test :syntax_error do
    assert_equal "nofile:1: syntax error before: '}'", format_rescue 'case 1 do }'
  end

  test :syntax_error_with_no_token do
    assert_equal "nofile:1: syntax error: expression is incomplete", format_rescue 'case 1 do'
  end

  test :bad_form do
    assert_equal "nofile:2: function bar/0 undefined", format_rescue 'defmodule Foo do\ndef foo, do: bar\nend'
  end

  test :unbound_var do
    assert_equal "nofile:1: unbound variable ^x", format_rescue('^x = 1')
  end

  test :unbound_not_assignment do
    assert_equal "nofile:1: cannot access variable ^x outside of assignment", format_rescue('^x')
  end

  test :unbound_expr do
    assert_equal "nofile:1: cannot use ^ with expression at ^x, ^ must be used only with variables", format_rescue('^x(1)')
  end

  test :name_for_defmodule do
    assert_equal "nofile:1: invalid module name: 3", format_rescue 'defmodule 1 + 2, do: 3'
  end

  test :invalid_scope_for_function do
    assert_equal "nofile:1: cannot invoke def outside module", format_rescue 'def Foo, do: 2'
    assert_equal "nofile:3: cannot invoke defmacro outside module", format_rescue '\n\ndefmacro Foo, do: 2'
  end

  test :invalid_quote_args do
    assert_equal "nofile:1: invalid args for quote", format_rescue 'quote 1'
  end

  test :invalid_fn_args do
    assert_equal "nofile:1: no block given to fn", format_rescue 'fn 1'
  end

  test :unproper_macro do
    assert_equal "nofile:3: key value blocks not supported by ::Elixir::ErrorsTest::UnproperMacro.unproper/1",
      format_rescue 'defmodule Foo do\nrequire Elixir::ErrorsTest::UnproperMacro\nElixir::ErrorsTest::UnproperMacro.unproper do\nmatch: 1\nmatch: 2\nend\nend'
  end

  test :macro_conflict do
    assert_equal "nofile:1: imported ::Elixir::Builtin.defrecord/2 conflicts with local function",
      format_rescue 'defmodule Foo do\ndefrecord(::Elixir::ErrorsTest::MacroConflict, a: 1)\ndef defrecord(_, _), do: OMG\nend'
  end

  test :erlang_function_conflict do
    assert_equal "nofile:1: function exit/1 already imported from erlang",
      format_rescue 'defmodule Foo do import Elixir::ErrorsTest::UnproperMacro, only: [exit: 1]\nend'
  end

  test :import_invalid_macro do
    assert_equal "nofile:2: cannot import ::Elixir::Builtin.invalid/1 because it doesn't exist",
      format_rescue 'defmodule Foo do\nimport Elixir::Builtin, only: [invalid: 1]\nend'
  end

  test :unrequired_macro do
    assert_equal "nofile:2: tried to invoke macro ::Elixir::ErrorsTest::UnproperMacro.unproper/1 but module was not required. Required: ['::Elixir::Builtin']",
      format_rescue 'defmodule Foo do\nElixir::ErrorsTest::UnproperMacro.unproper([])\nend'
  end

  test :def_defmacro_clause_change do
    assert_equal "nofile:3: defmacro foo/1 already defined as def",
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndefmacro foo(x), do: x\nend'
  end

  test :visibility_clause_change do
    assert_equal "nofile:3: function foo/1 already defined with visibility public",
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndefp foo(x), do: x\nend'
  end

  test :clause_change do
    assert_equal "nofile:4: function foo/1 does not match previous clause",
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndef bar(x), do: x\ndef foo(x), do: x\nend'
  end

  test :internal_function_overriden do
    assert_equal "nofile:1: function __info__/1 is internal and should not be overriden",
      format_rescue 'defmodule Foo do\ndef __info__(_), do: []\nend'
  end

  test :no_macros do
    assert_equal "nofile:2: could not load macros from module lists",
      format_rescue 'defmodule Foo do\nimport :macros, Erlang.lists\nend'
  end

  test :unloaded_module do
    assert_equal "nofile:1: module ::Certainly::Doesnt::Exist is not loaded, reason: nofile",
      format_rescue 'import Certainly::Doesnt::Exist'
  end

  test :scheduled_module do
    assert_equal "nofile:1: module ::Hygiene is not loaded but was defined. This may happen because the module is nested inside another module. Try defining the module outside the context that requires it.",
      format_rescue 'defmodule Foo do; defmodule Hygiene do; end; import Hygiene; end'
  end

  test :already_compiled_module do
    assert_equal "could not call eval_quoted on module ::Record because it was already compiled",
      format_rescue 'Module.eval_quoted ::Record, quote(do: 1), [], __FILE__, __LINE__'
  end

  test :interpolation_error do
    assert_equal "nofile:1: syntax error before: ')'", format_rescue '"foo\#{case 1 do )}bar"'
  end

  test :invalid_kv_for_match do
    assert_equal "nofile:1: invalid key invalid",
      format_rescue 'case true do\ninvalid: 2\nafter: 3\nend'
  end

  test :cant_define_local_due_to_in_erlang_macros_conflict do
    assert_equal "nofile:1: cannot define local quote/1 because it conflicts with Elixir internal macros",
      format_rescue 'defmodule Foo do\ndef quote(x), do: x\ndef bar(x), do: quote(do: x)\nend'
  end

  test :already_defined_module do
    assert_equal "nofile:1: module ::Record already defined", format_rescue 'defmodule Record, do: true'
  end

  test :duplicated_bitstring_size do
    assert_equal "nofile:1: duplicated size specifier 12 in <<>>", format_rescue '<<1|12-12>>'
  end

  test :invalid_bitstring_specified do
    assert_equal "nofile:1: invalid specifier for <<>>", format_rescue '<<1|12-binary()>>'
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
