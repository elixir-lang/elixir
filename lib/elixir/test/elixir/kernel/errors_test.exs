Code.require_file "../../test_helper", __FILE__

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true

  defmodule UnproperMacro do
    defmacro unproper(args), do: args
    defmacro exit(args), do: args
  end

  defrecord Config, integer: 0

  test :invalid_token do
    assert "nofile:1: invalid token: \end" == format_rescue '\end'
  end

  test :invalid_partial do
    assert "nofile:1: partial variable &2 cannot be defined without &1" == format_rescue '&2 + 3'
  end

  test :syntax_error do
    assert "nofile:1: syntax error before: '}'" == format_rescue 'case 1 do }'
  end

  test :syntax_error_with_no_token do
    assert "nofile:1: syntax error: expression is incomplete" == format_rescue 'case 1 do'
  end

  test :bad_form do
    assert "nofile:2: function bar/0 undefined" == format_rescue 'defmodule Foo do\ndef foo, do: bar\nend'
  end

  test :unbound_var do
    assert "nofile:1: unbound variable ^x" == format_rescue('^x = 1')
  end

  test :unbound_not_assignment do
    assert "nofile:1: cannot access variable ^x outside of assignment" == format_rescue('^x')
  end

  test :unbound_expr do
    assert "nofile:1: cannot use ^ with expression at ^x, ^ must be used only with variables" == format_rescue('^x(1)')
  end

  test :name_for_defmodule do
    assert "nofile:1: invalid module name: 3" == format_rescue 'defmodule 1 + 2, do: 3'
  end

  test :invalid_scope_for_function do
    assert "nofile:1: cannot invoke def outside module" == format_rescue 'def Foo, do: 2'
    assert "nofile:3: cannot invoke defmacro outside module" == format_rescue '\n\ndefmacro Foo, do: 2'
  end

  test :invalid_quote_args do
    assert "nofile:1: invalid args for quote" == format_rescue 'quote 1'
  end

  test :invalid_fn_args do
    assert "nofile:1: invalid args for fn" == format_rescue 'fn 1'
  end

  test :macro_conflict do
    assert "nofile:1: imported Elixir.Builtin.defrecord/2 conflicts with local function" ==
      format_rescue 'defmodule Foo do\ndefrecord(Kernel.ErrorsTest.MacroConflict, a: 1)\ndef defrecord(_, _), do: OMG\nend'
  end

  test :macro_with_undefined_local do
    assert "undefined function: Foo.unknown/1" ==
      format_rescue 'defmodule Foo do\ndefmacrop bar, do: unknown(1)\ndef baz, do: bar()\nend'
  end

  test :private_macro do
    assert "undefined function: Foo.foo/0" ==
      format_rescue 'defmodule Foo do\ndefmacrop foo, do: 1\ndefmacro bar, do: __MODULE__.foo\ndefmacro baz, do: bar\nend'
  end

  test :erlang_function_conflict do
    assert "nofile:1: function exit/1 already imported from Elixir.Builtin" ==
      format_rescue 'defmodule Foo do import Kernel.ErrorsTest.UnproperMacro, only: [exit: 1]\nend'
  end

  test :import_invalid_macro do
    assert "nofile:2: cannot import Elixir.Builtin.invalid/1 because it doesn't exist" ==
      format_rescue 'defmodule Foo do\nimport Elixir.Builtin, only: [invalid: 1]\nend'
  end

  test :unrequired_macro do
    assert "nofile:2: tried to invoke macro Kernel.ErrorsTest.UnproperMacro.unproper/1 but module was not required. Required: Elixir.Builtin, Elixir.Typespec" ==
      format_rescue 'defmodule Foo do\nKernel.ErrorsTest.UnproperMacro.unproper([])\nend'
  end

  test :def_defmacro_clause_change do
    assert "nofile:3: defmacro foo/1 already defined as def" ==
      format_rescue 'defmodule Foo do\ndef foo(1), do: 1\ndefmacro foo(x), do: x\nend'
  end

  test :internal_function_overriden do
    assert "nofile:1: function __info__/1 is internal and should not be overriden" ==
      format_rescue 'defmodule Foo do\ndef __info__(_), do: []\nend'
  end

  test :no_macros do
    assert "nofile:2: could not load macros from module lists" ==
      format_rescue 'defmodule Foo do\nimport :macros, Erlang.lists\nend'
  end

  test :unloaded_module do
    assert "nofile:1: module Certainly.Doesnt.Exist is not loaded and could not be found" ==
      format_rescue 'import Certainly.Doesnt.Exist'
  end

  test :scheduled_module do
    assert "nofile:1: module Foo.Hygiene is not loaded but was defined. This happens because you are trying to use a module in the same context it is defined. Try defining the module outside the context that requires it." ==
      format_rescue 'defmodule Foo do; defmodule Hygiene do; end; import Foo.Hygiene; end'
  end

  test :already_compiled_module do
    assert "could not call eval_quoted on module Record because it was already compiled" ==
      format_rescue 'Module.eval_quoted Record, quote(do: 1), [], file: __FILE__'
  end

  test :interpolation_error do
    assert "nofile:1: syntax error before: ')'" == format_rescue '"foo\#{case 1 do )}bar"'
  end

  test :cant_define_local_due_to_in_erlang_macros_conflict do
    assert "nofile:1: cannot define local quote/1 because it conflicts with Elixir internal macros" ==
      format_rescue 'defmodule Foo do\ndef quote(x), do: x\ndef bar(x), do: quote(do: x)\nend'
  end

  test :already_defined_module do
    assert "nofile:1: module Record already defined (please remove already compiled files before recompiling a module)" ==
      format_rescue 'defmodule Record, do: true'
  end

  test :in_definition_module do
    assert "nofile:1: cannot define module Foo because it is currently being defined" ==
      format_rescue 'defmodule Foo, do: (defmodule __MAIN__.Foo, do: true)'
  end

  test :duplicated_bitstring_size do
    assert "nofile:1: duplicated size specifier 12 in <<>>" == format_rescue '<<1|12-12>>'
  end

  test :invalid_bitstring_specified do
    assert "nofile:1: invalid specifier for <<>>" == format_rescue '<<1|12-binary()>>'
  end

  test :invalid_alias do
    assert "nofile:1: invalid args for alias, cannot create nested alias Sample.Lists" ==
      format_rescue 'alias Erlang.lists, as: Sample.Lists'
  end

  test :invalid_access_protocol_not_alias do
    assert "invalid usage of access protocol in signature" ==
      format_rescue 'defmodule Foo do\ndef sample(config[integer: 0]), do: true\nend'
  end

  test :invalid_access_protocol_not_available do
    assert "expected module Unknown to be loaded and defined" ==
      format_rescue 'defmodule Foo do\ndef sample(Unknown[integer: 0]), do: true\nend'
  end

  test :invalid_access_protocol_not_record do
    assert "cannot use module Kernel.ErrorsTest in access protocol because it does not export __access__/2" ==
      format_rescue 'defmodule Foo do\ndef sample(Kernel.ErrorsTest[integer: 0]), do: true\nend'
  end

  test :invalid_access_protocol_not_keywords do
    assert "expected contents inside brackets to be a Keyword" ==
      format_rescue 'defmodule Foo do\ndef sample(Kernel.ErrorsTest.Config[0]), do: true\nend'
  end

  test :invalid_access_protocol_invalid_keywords do
    assert "record Kernel.ErrorsTest.Config does not have the keys: [:foo]" ==
      format_rescue 'defmodule Foo do\ndef sample(Kernel.ErrorsTest.Config[foo: :bar]), do: true\nend'
  end

  test :invalid_access_protocol_invalid_keywords_outside_assignment do
    assert "record Kernel.ErrorsTest.Config does not have the keys: [:foo]" ==
      format_rescue 'Kernel.ErrorsTest.Config[foo: :bar]'
  end

  test :invalid_access_protocol_on_rescue do
    assert "nofile:1: cannot (yet) pattern match against erlang exceptions" ==
      format_rescue 'try do\n1\nrescue\nUndefinedFunctionError[arity: 1] -> false\nend'
  end

  test :invalid_bc do
    assert "nofile:1: a bit comprehension expects a bit string << >> to be returned" ==
      format_rescue 'bc x in [1,2,3], do: x'
  end

  test :unbound_cond do
    assert "unbound variable _ inside cond. If you want the last clause to match, you probably meant to use true ->" ==
      format_rescue 'cond do _ -> true end'
  end

  test :macros_error_stacktrace do
    assert [{:erlang,:+,[1,:foo],_},{Foo,:sample,1,_}|_] =
      rescue_stacktrace("defmodule Foo do\ndefmacro sample(num), do: num + :foo\ndef other, do: sample(1)\nend")
  end

  test :macros_function_clause_stacktrace do
    assert [{__MODULE__,:sample,1,_}|_] =
      rescue_stacktrace("defmodule Foo do\nimport Kernel.ErrorsTest\nsample(1)\nend")
  end

  test :macros_interpreted_function_clause_stacktrace do
    assert [{Foo,:sample,1,_}|_] =
      rescue_stacktrace("defmodule Foo do\ndefmacro sample(0), do: 0\ndef other, do: sample(1)\nend")
  end

  test :macros_compiled_callback do
    assert [{Kernel.ErrorsTest,:before_compile,[Foo],_}|_] =
      rescue_stacktrace("defmodule Foo do\nModule.add_attribute(__MODULE__, :before_compile, Kernel.ErrorsTest)\nend")
  end

  defmacro sample(0), do: 0

  defmacro before_compile(_) do
    quote(do: _)
  end

  ## Helpers

  defp format_rescue(expr) do
    result = try do
      Erlang.elixir.eval(to_char_list(expr), [])
      nil
    rescue
      error -> error.message
    end

    result || raise(ExUnit.AssertionError, message: "Expected function given to format_rescue to fail")
  end

  defp rescue_stacktrace(expr) do
    result = try do
      Erlang.elixir.eval(to_char_list(expr), [])
      nil
    rescue
      error -> System.stacktrace
    end

    result || raise(ExUnit.AssertionError, message: "Expected function given to format_rescue to fail")
  end
end