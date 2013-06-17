Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true
  import CompileAssertion

  defmodule UnproperMacro do
    defmacro unproper(args), do: args
    defmacro exit(args), do: args
  end

  defrecord Config, integer: 0

  test :invalid_token do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid token: \end",
      '\end\nlol\nbarbecue'
  end

  test :sigil_terminator do
    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: \" (for sigil %r\" starting at line 1)",
      '%r"foo\n\n'

    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: } (for sigil %r{ starting at line 1)",
      '%r{foo\n\n'
  end

  test :dot_terminator do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: \" (for function name starting at line 1)",
      'foo."bar'
  end

  test :string_terminator do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: \" (for string starting at line 1)",
      '"bar'
  end

  test :heredoc_terminator do
    assert_compile_fail TokenMissingError,
      "nofile:2: missing terminator: \"\"\" (for heredoc starting at line 1)",
      '"""\nbar'
  end

  test :invalid_partial do
    assert_compile_fail SyntaxError,
      "nofile:1: partial variable &2 cannot be defined without &1",
      '&2 + 3'
  end

  test :unexpected_end do
    assert_compile_fail SyntaxError,
      "nofile:1: unexpected token: end",
      '1 end'
  end

  test :syntax_error do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: '.'",
      '+.foo'
  end

  test :syntax_error_on_parens_call do
    assert_compile_fail SyntaxError, "nofile:1: invalid comma inside parenthesis. If you are making a function call, " <>
     "do not insert spaces in between the function name and the opening parentheses. " <>
     "Syntax error before: )", 'foo (hello, world)'
  end

  test :syntax_error_with_no_token do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: ) (for \"(\" starting at line 1)",
      'case 1 ('
  end

  test :clause_with_defaults do
    assert_compile_fail CompileError,
      "nofile:3: def hello/1 has default values and multiple clauses, " <>
      "use a separate clause for declaring defaults",
      '''
      defmodule Sample do
        def hello(arg // 0), do: nil
        def hello(arg // 1), do: nil
      end
      '''
  end

  test :bad_form do
    assert_compile_fail CompileError,
      "nofile:2: function bar/0 undefined",
      '''
      defmodule Foo do
        def foo, do: bar
      end
      '''
  end

  test :unbound_var do
    assert_compile_fail SyntaxError,
      "nofile:1: unbound variable ^x",
      '^x = 1'
  end

  test :unbound_not_match do
    assert_compile_fail SyntaxError,
      "nofile:1: cannot use ^x outside of match clauses",
      '^x'
  end

  test :unbound_expr do
    assert_compile_fail SyntaxError,
      "nofile:1: the unary operator ^ can only be used with variables, invalid expression ^x(1)",
      '^x(1)'
  end

  test :name_for_defmodule do
    assert_compile_fail CompileError,
      "nofile:1: invalid module name: 3",
      'defmodule 1 + 2, do: 3'
  end

  test :invalid_scope_for_function do
    assert_compile_fail SyntaxError,
      "nofile:1: cannot invoke def outside module",
      'def Foo, do: 2'
    assert_compile_fail SyntaxError,
      "nofile:3: cannot invoke defmacro outside module",
      '\n\ndefmacro Foo, do: 2'
  end

  test :invalid_unquote do
    assert_compile_fail SyntaxError,
      "nofile:1: unquote called outside quote",
      'unquote 1'
  end

  test :invalid_quote_args do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid args for quote",
      'quote 1'
  end

  test :invalid_parens do
    assert_compile_fail SyntaxError,
      "nofile:1: unexpected parenthesis after foo(1)",
      'foo(1)(2)'
  end

  test :invalid_fn_args do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: end (for \"fn\" starting at line 1)",
      'fn 1'
  end

  test :macro_conflict do
    assert_compile_fail CompileError,
      "nofile:1: imported Kernel.defrecord/2 conflicts with local function",
      '''
      defmodule Foo do
        defrecord(Kernel.ErrorsTest.MacroConflict, a: 1)
        def defrecord(_, _), do: OMG
      end
      '''
  end

  test :macro_with_undefined_local do
    assert_compile_fail UndefinedFunctionError,
      "undefined function: Foo.unknown/1",
      '''
      defmodule Foo do
        defmacrop bar, do: unknown(1)
        def baz, do: bar()
      end
      '''
  end

  test :private_macro do
    assert_compile_fail UndefinedFunctionError,
      "undefined function: Foo.foo/0",
      '''
      defmodule Foo do
        defmacrop foo, do: 1
        defmacro bar, do: __MODULE__.foo
        defmacro baz, do: bar
      end
      '''
  end

  test :function_definition_with_alias do
    assert_compile_fail SyntaxError,
      "nofile:2: function names should start with lowercase characters or underscore, invalid name Bar",
      '''
      defmodule Foo do
        def Bar do
          :baz
        end
      end
      '''
  end

  test :function_import_conflict do
    assert_compile_fail CompileError,
      "nofile:3: function exit/1 imported from both :erlang and Kernel, call is ambiguous",
      '''
      defmodule Foo do
        import :erlang
        def foo, do: exit(:test)
      end
      '''
  end

  test :import_invalid_macro do
    assert_compile_fail CompileError,
      "nofile:1: cannot import Kernel.invalid/1 because it doesn't exist",
      'import Kernel, only: [invalid: 1]'
  end

  test :unrequired_macro do
    assert_compile_fail CompileError,
      "nofile:2: tried to invoke macro Kernel.ErrorsTest.UnproperMacro.unproper/1 " <>
      "but module was not required. Required: Kernel, Kernel.Typespec, Record",
      '''
      defmodule Foo do
        Kernel.ErrorsTest.UnproperMacro.unproper([])
      end
      '''
  end

  test :def_defmacro_clause_change do
    assert_compile_fail CompileError,
      "nofile:3: defmacro foo/1 already defined as def",
      '''
      defmodule Foo do
        def foo(1), do: 1
        defmacro foo(x), do: x
      end
      '''
  end

  test :internal_function_overridden do
    assert_compile_fail CompileError,
      "nofile:1: function __info__/1 is internal and should not be overridden",
      '''
      defmodule Foo do
        def __info__(_), do: []
      end
      '''
  end

  test :no_macros do
    assert_compile_fail CompileError,
      "nofile:2: could not load macros from module :lists",
      '''
      defmodule Foo do
        import :macros, :lists
      end
      '''
  end

  test :unloaded_module do
    assert_compile_fail CompileError,
      "nofile:1: module Certainly.Doesnt.Exist is not loaded and could not be found",
      'import Certainly.Doesnt.Exist'
  end

  test :scheduled_module do
    assert_compile_fail CompileError,
      "nofile:4: module Foo.Hygiene is not loaded but was defined. " <>
      "This happens because you are trying to use a module in the same context it is defined. " <>
      "Try defining the module outside the context that requires it.",
      '''
      defmodule Foo do
        defmodule Hygiene do
        end
        import Foo.Hygiene
      end
      '''
  end

  test :already_compiled_module do
    assert_compile_fail ArgumentError,
      "could not call eval_quoted on module Record " <>
      "because it was already compiled",
      'Module.eval_quoted Record, quote(do: 1), [], file: __FILE__'
  end

  test :interpolation_error do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: end (for \"do\" starting at line 1)",
      '"foo\#{case 1 do )}bar"'
  end

  test :cant_define_local_due_to_in_erlang_macros_conflict do
    assert_compile_fail CompileError,
      "nofile:1: cannot define local quote/1 because it conflicts with Elixir special forms",
      '''
      defmodule Foo do
        def quote(x), do: x
        def bar(x), do: quote(do: x)
      end
      '''
  end

  test :in_definition_module do
    assert_compile_fail CompileError,
      "nofile:1: cannot define module Foo because it is currently being defined",
      'defmodule Foo, do: (defmodule Elixir.Foo, do: true)'
  end

  test :invalid_definition do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid syntax in def 1.(hello)",
      'defmodule Foo, do: (def 1.(hello), do: true)'
  end

  test :duplicated_bitstring_size do
    assert_compile_fail SyntaxError,
      "nofile:1: duplicated size definition for bitstring",
      '<<1 :: [size(12), size(13)]>>'
  end

  test :invalid_bitstring_specified do
    assert_compile_fail SyntaxError,
      "nofile:1: unknown bitstring specifier :atom",
      '<<1 :: :atom>>'

    assert_compile_fail SyntaxError,
      "nofile:1: unknown bitstring specifier unknown",
      '<<1 :: unknown>>'

    assert_compile_fail SyntaxError,
      "nofile:1: unknown bitstring specifier another(12)",
      '<<1 :: another(12)>>'

    assert_compile_fail SyntaxError,
      "nofile:1: size in bitstring expects an integer or a variable as argument",
      '<<1 :: size(:a)>>'

    assert_compile_fail SyntaxError,
      "nofile:1: unit in bitstring expects an integer as argument",
      '<<1 :: unit(x)>>'
  end

  test :invalid_var! do
    assert_compile_fail SyntaxError,
      "nofile:1: expected var!(x) to expand to an existing variable or be a part of a match",
      'var!(x)'
  end

  test :invalid_alias do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid args for alias, cannot create nested alias Sample.Lists",
      'alias :lists, as: Sample.Lists'
  end

  test :invalid_import_option do
    assert_compile_fail SyntaxError,
      "nofile:1: unsupported option ops given to import",
      'import :lists, [ops: 1]'
  end

  test :invalid_access_protocol_not_available do
    assert_compile_fail CompileError,
      "nofile:2: module Unknown is not loaded and could not be found",
      '''
      defmodule Foo do
        def sample(Unknown[integer: 0]), do: true
      end
      '''
  end

  test :invalid_access_protocol_not_alias do
    assert_raise ArgumentError, "the access protocol cannot be used inside match clauses (for example, on the left hand side of a match or in function signatures)", fn ->
      defmodule Foo do
        def sample(config[integer: 0]), do: true
      end
    end
  end

  test :invalid_access_protocol_not_record do
    assert_raise ArgumentError, "cannot use module Kernel.ErrorsTest in access protocol because it does not export __record__/1", fn ->
      defmodule Foo do
        def sample(Kernel.ErrorsTest[integer: 0]), do: true
      end
    end
  end

  test :invalid_access_protocol_not_keywords do
    assert_raise ArgumentError, "expected contents inside brackets to be a Keyword", fn ->
      defmodule Foo do
        def sample(Kernel.ErrorsTest.Config[0]), do: true
      end
    end
  end

  test :invalid_access_protocol_invalid_keywords do
    assert_raise ArgumentError, "record Kernel.ErrorsTest.Config does not have the keys: [:foo]", fn ->
      defmodule Foo do
        def sample(Kernel.ErrorsTest.Config[foo: :bar]), do: true
      end
    end
  end

  test :invalid_access_protocol_invalid_keywords_outside_match do
    assert_compile_fail ArgumentError,
      "record Kernel.ErrorsTest.Config does not have the keys: [:foo]",
      'Kernel.ErrorsTest.Config[foo: :bar]'
  end

  test :invalid_rescue_clause do
    assert_compile_fail SyntaxError,
      "nofile:4: invalid rescue clause. The clause should match on an alias, a variable or be in the `var in [alias]` format",
      'try do\n1\nrescue\nUndefinedFunctionError[arity: 1] -> false\nend'
  end

  test :invalid_bc_return do
    assert_compile_fail SyntaxError,
      "nofile:1: a bit comprehension expects a bit string << >> to be returned",
      'bc x inlist [1, 2, 3], do: x'
  end

  test :invalid_bc_inbits_gen do
    assert_compile_fail SyntaxError,
      "nofile:1: a bit comprehension expects a bit string << >> to be used in inbits generators",
      'bc x inbits "123", do: <<x>>'
  end

  test :unbound_cond do
    assert_compile_fail ArgumentError,
      "unbound variable _ inside cond. If you want the last clause to match, you probably meant to use true ->",
      'cond do _ -> true end'
  end

  test :fun_different_arities do
    assert_compile_fail SyntaxError,
      "nofile:1: cannot mix clauses with different arities in function definition",
      'fn x -> x; x, y -> x + y end'
  end

  test :new_line_error do
    assert_compile_fail SyntaxError,
      "nofile:3: syntax error before: newline",
      'if true do\n  foo = [],\n  baz\nend'
  end

  test :invalid_var_or_function_on_guard do
    assert_compile_fail SyntaxError,
      "nofile:2: unknown variable something_that_does_not_exist or " <>
      "cannot invoke function something_that_does_not_exist/0 inside guard",
      '''
      case [] do
        [] when something_that_does_not_exist == [] -> :ok
      end
      '''
  end

  test :bodyless_function_with_guard do
    assert_compile_fail SyntaxError,
      "nofile:2: missing keyword do in def",
      '''
      defmodule Foo do
        def foo(n) when is_number(n)
      end
      '''
  end

  test :invalid_function_on_match do
    assert_compile_fail SyntaxError,
      "nofile:1: cannot invoke function something_that_does_not_exist/0 inside match",
      'case [] do; something_that_does_not_exist() -> :ok; end'
  end

  test :invalid_remote_on_match do
    assert_compile_fail SyntaxError,
      "nofile:1: cannot invoke remote function Hello.something_that_does_not_exist/0 inside match",
      'case [] do; Hello.something_that_does_not_exist() -> :ok; end'
  end

  test :invalid_remote_on_guard do
    assert_compile_fail SyntaxError,
      "nofile:1: cannot invoke remote function Hello.something_that_does_not_exist/0 inside guard",
      ('case [] do; [] when Hello.something_that_does_not_exist == [] -> :ok; end')
  end

  test :macros_error_stacktrace do
    assert [{:erlang, :+, [1, :foo], _}, {Foo, :sample, 1, _}|_] =
      rescue_stacktrace("defmodule Foo do\ndefmacro sample(num), do: num + :foo\ndef other, do: sample(1)\nend")
  end

  test :macros_function_clause_stacktrace do
    assert [{__MODULE__, :sample, 1, _}|_] =
      rescue_stacktrace("defmodule Foo do\nimport Kernel.ErrorsTest\nsample(1)\nend")
  end

  test :macros_interpreted_function_clause_stacktrace do
    assert [{Foo, :sample, 1, _}|_] =
      rescue_stacktrace("defmodule Foo do\ndefmacro sample(0), do: 0\ndef other, do: sample(1)\nend")
  end

  test :macros_compiled_callback do
    assert [{Kernel.ErrorsTest, :__before_compile__, [Macro.Env[module: Foo]], _}|_] =
      rescue_stacktrace("defmodule Foo do\nModule.put_attribute(__MODULE__, :before_compile, Kernel.ErrorsTest)\nend")
  end

  defmacro sample(0), do: 0

  defmacro before_compile(_) do
    quote(do: _)
  end

  ## Helpers

  defp rescue_stacktrace(expr) do
    result = try do
      :elixir.eval(to_char_list(expr), [])
      nil
    rescue
      _ -> System.stacktrace
    end

    result || raise(ExUnit.AssertionError, message: "Expected function given to rescue_stacktrace to fail")
  end
end
