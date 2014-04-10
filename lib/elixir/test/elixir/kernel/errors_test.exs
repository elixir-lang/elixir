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

  test :invalid_or_reserved_codepoint do
    assert_compile_fail ArgumentError,
      "invalid or reserved unicode codepoint 55296",
      '?\\x{D800}'
  end

  test :sigil_terminator do
    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: \" (for sigil ~r\" starting at line 1)",
      '~r"foo\n\n'

    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: } (for sigil ~r{ starting at line 1)",
      '~r{foo\n\n'

    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: ) (for sigil ~r( starting at line 1)",
      '~r(f(oo)\n\n'
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

  test :heredoc_start do
    assert_compile_fail TokenMissingError,
      "nofile:1: heredoc start \"\"\" must be followed by a new line",
      '"""bar'
  end

  test :heredoc_terminator do
    assert_compile_fail TokenMissingError,
      "nofile:2: missing terminator: \"\"\" (for heredoc starting at line 1)",
      '"""\nbar'
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

  test :compile_error_on_op_ambiguity do
    msg = "nofile:1: \"a -1\" looks like a function call but there is a variable named \"a\", " <>
          "please use explicit parenthesis or even spaces"
    assert_compile_fail CompileError, msg, 'a = 1; a -1'

    max = 1
    assert max == 1
    assert (max 1, 2) == 2
  end

  test :syntax_error_on_parens_call do
    msg = "nofile:1: unexpected parenthesis. If you are making a function call, do not " <>
          "insert spaces in between the function name and the opening parentheses. " <>
          "Syntax error before: '('"

    assert_compile_fail SyntaxError, msg, 'foo (hello, world)'
    assert_compile_fail SyntaxError, msg, 'foo ()'
    assert_compile_fail SyntaxError, msg, 'foo (), 1'
  end

  test :syntax_error_on_nested_no_parens_call do
    msg = "nofile:1: unexpected comma. Parentheses are required to solve ambiguity in " <>
          "nested calls. Syntax error before: ','"

    assert_compile_fail SyntaxError, msg, '[foo 1, 2]'
    assert_compile_fail SyntaxError, msg, '[do: foo 1, 2]'
    assert_compile_fail SyntaxError, msg, 'foo(do: bar 1, 2)'
    assert_compile_fail SyntaxError, msg, '{foo 1, 2}'
    assert_compile_fail SyntaxError, msg, 'foo 1, foo 2, 3'
    assert_compile_fail SyntaxError, msg, 'foo(1, foo 2, 3)'

    assert is_list List.flatten [1]
    assert is_atom is_record range, Range
    assert is_atom(is_record range, Range)
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
      ~C'''
      defmodule ErrorsTest do
        def hello(arg \\ 0), do: nil
        def hello(arg \\ 1), do: nil
      end
      '''
  end

  test :invalid_match_pattern do
    assert_compile_fail CompileError,
    "nofile:2: invalid expression in match",
    '''
    case true do
      true && true -> true
    end
    '''
  end

  test :different_defs_with_defaults do
    assert_compile_fail CompileError,
      "nofile:3: def hello/3 defaults conflicts with def hello/2",
      ~C'''
      defmodule ErrorsTest do
        def hello(a, b \\ nil), do: a + b
        def hello(a, b \\ nil, c \\ nil), do: a + b + c
      end
      '''

    assert_compile_fail CompileError,
      "nofile:3: def hello/2 conflicts with defaults from def hello/3",
      ~C'''
      defmodule ErrorsTest do
        def hello(a, b \\ nil, c \\ nil), do: a + b + c
        def hello(a, b \\ nil), do: a + b
      end
      '''
  end

  test :bad_form do
    assert_compile_fail CompileError,
      "nofile:2: function bar/0 undefined",
      '''
      defmodule ErrorsTest do
        def foo, do: bar
      end
      '''
  end

  test :unbound_var do
    assert_compile_fail CompileError,
      "nofile:1: unbound variable ^x",
      '^x = 1'
  end

  test :unbound_not_match do
    assert_compile_fail CompileError,
      "nofile:1: cannot use ^x outside of match clauses",
      '^x'
  end

  test :unbound_expr do
    assert_compile_fail CompileError,
      "nofile:1: invalid argument for unary operator ^, expected an existing variable, got: ^x(1)",
      '^x(1) = 1'
  end

  test :literal_on_map_and_struct do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: '}'",
      '%{ { :a, :b } }'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: '{'",
      '%{ :a, :b }{ a: :b }'
  end

  test :struct_fields_on_defstruct do
    assert_compile_fail ArgumentError,
      "defstruct fields must be a keyword list, got: my_fields",
      '''
      defmodule TZ do
        defstruct my_fields
      end
      '''
  end

  test :struct_access_on_body do
    assert_compile_fail CompileError,
      "nofile:3: cannot access struct TZ in body of the module that defines it " <>
      "as the struct fields are not yet accessible",
      '''
      defmodule TZ do
        defstruct %{name: "Brasilia"}
        %TZ{}
      end
      '''
  end

  test :unbound_map_key_var do
    assert_compile_fail CompileError,
      "nofile:1: illegal use of variable x in map key",
      '%{ x => 1 } = %{}'

    assert_compile_fail CompileError,
      "nofile:1: illegal use of variable x in map key",
      '%{ x = 1 => 1 }'
  end

  test :struct_errors do
    assert_compile_fail CompileError,
      "nofile:1: BadStruct.__struct__/0 is undefined, cannot expand struct BadStruct",
      '%BadStruct{}'

    defmodule BadStruct do
      def __struct__ do
        []
      end
    end

    assert_compile_fail CompileError,
      "nofile:1: expected Kernel.ErrorsTest.BadStruct.__struct__/0 to return a map, got: []",
      '%#{BadStruct}{}'

    defmodule GoodStruct do
      def __struct__ do
        %{ name: "josé" }
      end
    end

    assert_compile_fail CompileError,
      "nofile:1: unknown key :age for struct Kernel.ErrorsTest.GoodStruct",
      '%#{GoodStruct}{ age: 27 }'
  end

  test :name_for_defmodule do
    assert_compile_fail CompileError,
      "nofile:1: invalid module name: 3",
      'defmodule 1 + 2, do: 3'
  end

  test :invalid_unquote do
    assert_compile_fail CompileError,
      "nofile:1: unquote called outside quote",
      'unquote 1'
  end

  test :invalid_quote_args do
    assert_compile_fail CompileError,
      "nofile:1: invalid arguments for quote",
      'quote 1'
  end

  test :invalid_calls do
    assert_compile_fail CompileError,
      "nofile:1: invalid call foo(1)(2)",
      'foo(1)(2)'

    assert_compile_fail CompileError,
      "nofile:1: invalid call 1.foo()",
      '1.foo'
  end

  test :unhandled_stab do
    assert_compile_fail CompileError,
      "nofile:3: unhandled operator ->",
      '''
      defmodule Mod do
        def fun do
          casea foo, do: (bar -> baz)
        end
      end
      '''
  end

  test :undefined_non_local_function do
    assert_compile_fail CompileError,
      "nofile:1: undefined function casea/2",
      'casea foo, do: 1'
  end

  test :invalid_fn_args do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: end (for \"fn\" starting at line 1)",
      'fn 1'
  end

  test :function_local_conflict do
    assert_compile_fail CompileError,
      "nofile:1: imported Kernel.&&/2 conflicts with local function",
      '''
      defmodule ErrorsTest do
        def other, do: 1 && 2
        def _ && _, do: :error
      end
      '''
  end

  test :macro_local_conflict do
    assert_compile_fail CompileError,
      "nofile:6: call to local macro &&/2 conflicts with imported Kernel.&&/2, " <>
      "please rename the local macro or remove the conflicting import",
      '''
      defmodule ErrorsTest do
        def hello, do: 1 || 2
        defmacro _ || _, do: :ok

        defmacro _ && _, do: :error
        def world, do: 1 && 2
      end
      '''
  end

  test :macro_with_undefined_local do
    assert_compile_fail UndefinedFunctionError,
      "undefined function: ErrorsTest.unknown/1",
      '''
      defmodule ErrorsTest do
        defmacrop bar, do: unknown(1)
        def baz, do: bar()
      end
      '''
  end

  test :private_macro do
    assert_compile_fail UndefinedFunctionError,
      "undefined function: ErrorsTest.foo/0",
      '''
      defmodule ErrorsTest do
        defmacrop foo, do: 1
        defmacro bar, do: __MODULE__.foo
        defmacro baz, do: bar
      end
      '''
  end

  test :function_definition_with_alias do
    assert_compile_fail CompileError,
      "nofile:2: function names should start with lowercase characters or underscore, invalid name Bar",
      '''
      defmodule ErrorsTest do
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
      defmodule ErrorsTest do
        import :erlang, warn: false
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
    assert_compile_fail SyntaxError,
      "nofile:2: you must require Kernel.ErrorsTest.UnproperMacro before invoking " <>
      "the macro Kernel.ErrorsTest.UnproperMacro.unproper/1 "
      '''
      defmodule ErrorsTest do
        Kernel.ErrorsTest.UnproperMacro.unproper([])
      end
      '''
  end

  test :def_defmacro_clause_change do
    assert_compile_fail CompileError,
      "nofile:3: defmacro foo/1 already defined as def",
      '''
      defmodule ErrorsTest do
        def foo(1), do: 1
        defmacro foo(x), do: x
      end
      '''
  end

  test :internal_function_overridden do
    assert_compile_fail CompileError,
      "nofile:1: function __info__/1 is internal and should not be overridden",
      '''
      defmodule ErrorsTest do
        def __info__(_), do: []
      end
      '''
  end

  test :no_macros do
    assert_compile_fail CompileError,
      "nofile:2: could not load macros from module :lists",
      '''
      defmodule ErrorsTest do
        import :lists, only: :macros
      end
      '''
  end

  test :invalid_macro do
    assert_compile_fail CompileError,
      "nofile: invalid quoted expression: {:foo, :bar, :baz, :bat}",
      '''
      defmodule ErrorsTest do
        defmacrop oops do
          { :foo, :bar, :baz, :bat }
        end

        def test, do: oops
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
      "nofile:4: module ErrorsTest.Hygiene is not loaded but was defined. " <>
      "This happens because you are trying to use a module in the same context it is defined. " <>
      "Try defining the module outside the context that requires it.",
      '''
      defmodule ErrorsTest do
        defmodule Hygiene do
        end
        import ErrorsTest.Hygiene
      end
      '''
  end

  test :already_compiled_module do
    assert_compile_fail ArgumentError,
      "could not call eval_quoted on module Record " <>
      "because it was already compiled",
      'Module.eval_quoted Record, quote(do: 1), [], file: __ENV__.file'
  end

  test :interpolation_error do
    assert_compile_fail SyntaxError,
      "nofile:1: \"do\" starting at line 1 is missing terminator \"end\". Unexpected token: )",
      '"foo\#{case 1 do )}bar"'
  end

  test :in_definition_module do
    assert_compile_fail CompileError,
      "nofile:1: cannot define module ErrorsTest because it is currently being defined",
      'defmodule ErrorsTest, do: (defmodule Elixir.ErrorsTest, do: true)'
  end

  test :invalid_definition do
    assert_compile_fail CompileError,
      "nofile:1: invalid syntax in def 1.(hello)",
      'defmodule ErrorsTest, do: (def 1.(hello), do: true)'
  end

  test :duplicated_bitstring_size do
    assert_compile_fail CompileError,
      "nofile:1: duplicated size definition in bitstring",
      '<<1 :: [size(12), size(13)]>>'
  end

  test :invalid_bitstring_specified do
    assert_compile_fail CompileError,
      "nofile:1: unknown bitstring specifier :atom",
      '<<1 :: :atom>>'

    assert_compile_fail CompileError,
      "nofile:1: unknown bitstring specifier unknown()",
      '<<1 :: unknown>>'

    assert_compile_fail CompileError,
      "nofile:1: unknown bitstring specifier another(12)",
      '<<1 :: another(12)>>'

    assert_compile_fail CompileError,
      "nofile:1: size in bitstring expects an integer or a variable as argument, got: :a",
      '<<1 :: size(:a)>>'

    assert_compile_fail CompileError,
      "nofile:1: unit in bitstring expects an integer as argument, got: :x",
      '<<1 :: unit(:x)>>'
  end

  test :invalid_var! do
    assert_compile_fail CompileError,
      "nofile:1: expected var x to expand to an existing variable or be a part of a match",
      'var!(x)'
  end

  test :invalid_alias do
    assert_compile_fail CompileError,
      "nofile:1: invalid value for keyword :as, expected an alias, got nested alias: Sample.Lists",
      'alias :lists, as: Sample.Lists'
  end

  test :invalid_import_option do
    assert_compile_fail CompileError,
      "nofile:1: unsupported option :ops given to import",
      'import :lists, [ops: 1]'
  end

  test :invalid_access_protocol_not_available do
    assert_compile_fail CompileError,
      "nofile:2: module Unknown is not loaded and could not be found",
      '''
      defmodule ErrorsTest do
        def sample(Unknown[integer: 0]), do: true
      end
      '''
  end

  test :invalid_access_protocol_not_alias do
    assert_raise ArgumentError, "dynamic access cannot be invoked inside match and guard clauses", fn ->
      defmodule ErrorsTest do
        def sample(config[integer: 0]), do: true
      end
    end
  end

  test :invalid_access_protocol_not_record do
    assert_raise ArgumentError, "cannot access module Kernel.ErrorsTest because it is not a record", fn ->
      defmodule ErrorsTest do
        def sample(Kernel.ErrorsTest[integer: 0]), do: true
      end
    end
  end

  test :invalid_access_protocol_not_keywords do
    assert_raise ArgumentError, "expected contents inside brackets to be a keyword list or an atom, got: [0]", fn ->
      defmodule ErrorsTest do
        def sample(Kernel.ErrorsTest.Config[0]), do: true
      end
    end
  end

  test :invalid_access_protocol_invalid_keywords do
    assert_raise ArgumentError, "record Kernel.ErrorsTest.Config does not have the key: :foo", fn ->
      defmodule ErrorsTest do
        def sample(Kernel.ErrorsTest.Config[foo: :bar]), do: true
      end
    end
  end

  test :invalid_rescue_clause do
    assert_compile_fail CompileError,
      "nofile:4: invalid rescue clause. The clause should match on an alias, a variable or be in the `var in [alias]` format",
      'try do\n1\nrescue\nUndefinedFunctionError[arity: 1] -> false\nend'
  end

  test :invalid_for_without_generators do
    assert_compile_fail CompileError,
      "nofile:1: for comprehensions must start with a generator",
      'for x, do: x'
  end

  test :invalid_for_bit_generator do
    assert_compile_fail CompileError,
      "nofile:1: bitstring fields without size are not allowed in bitstring generators",
      'for << x :: binary <- "123" >>, do: x'
  end

  test :unbound_cond do
    assert_compile_fail ArgumentError,
      "unbound variable _ inside cond. If you want the last clause to match, you probably meant to use true ->",
      'cond do _ -> true end'
  end

  test :fun_different_arities do
    assert_compile_fail CompileError,
      "nofile:1: cannot mix clauses with different arities in function definition",
      'fn x -> x; x, y -> x + y end'
  end

  test :new_line_error do
    assert_compile_fail SyntaxError,
      "nofile:3: syntax error before: newline",
      'if true do\n  foo = [],\n  baz\nend'
  end

  test :invalid_var_or_function_on_guard do
    assert_compile_fail CompileError,
      "nofile:2: unknown variable something_that_does_not_exist or " <>
      "cannot invoke function something_that_does_not_exist/0 inside guard",
      '''
      case [] do
        [] when something_that_does_not_exist == [] -> :ok
      end
      '''
  end

  test :bodyless_function_with_guard do
    assert_compile_fail CompileError,
      "nofile:2: missing do keyword in def",
      '''
      defmodule ErrorsTest do
        def foo(n) when is_number(n)
      end
      '''
  end

  test :invalid_function_on_match do
    assert_compile_fail CompileError,
      "nofile:1: cannot invoke function something_that_does_not_exist/0 inside match",
      'case [] do; something_that_does_not_exist() -> :ok; end'
  end

  test :invalid_remote_on_match do
    assert_compile_fail CompileError,
      "nofile:1: cannot invoke remote function Hello.something_that_does_not_exist/0 inside match",
      'case [] do; Hello.something_that_does_not_exist() -> :ok; end'
  end

  test :invalid_remote_on_guard do
    assert_compile_fail CompileError,
      "nofile:1: cannot invoke remote function Hello.something_that_does_not_exist/0 inside guard",
      'case [] do; [] when Hello.something_that_does_not_exist == [] -> :ok; end'
  end

  test :typespec_errors do
    assert_compile_fail CompileError,
      "nofile:2: type foo() undefined",
      '''
      defmodule ErrorsTest do
        @type omg :: foo
      end
      '''

    assert_compile_fail CompileError,
      "nofile:2: spec for undefined function ErrorsTest.omg/0",
      '''
      defmodule ErrorsTest do
        @spec omg :: atom
      end
      '''
  end

  test :bad_unquoting do
    assert_compile_fail CompileError,
      "nofile: invalid quoted expression: {Range, 1, 3}",
      '''
      defmodule ErrorsTest do
        def range(unquote(1..3)), do: :ok
      end
      '''
  end

  test :macros_error_stacktrace do
    assert [{:erlang, :+, [1, :foo], _}, {ErrorsTest, :sample, 1, _}|_] =
      rescue_stacktrace("""
      defmodule ErrorsTest do
        defmacro sample(num), do: num + :foo
        def other, do: sample(1)
      end
      """)
  end

  test :macros_function_clause_stacktrace do
    assert [{__MODULE__, :sample, 1, _}|_] =
      rescue_stacktrace("""
      defmodule ErrorsTest do
        import Kernel.ErrorsTest
        sample(1)
      end
      """)
  end

  test :macros_interpreted_function_clause_stacktrace do
    assert [{ErrorsTest, :sample, 1, _}|_] =
      rescue_stacktrace("""
      defmodule ErrorsTest do
        defmacro sample(0), do: 0
        def other, do: sample(1)
      end
      """)
  end

  test :macros_compiled_callback do
    assert [{Kernel.ErrorsTest, :__before_compile__, [Macro.Env[module: ErrorsTest]], _}|_] =
      rescue_stacktrace("""
      defmodule ErrorsTest do
        Module.put_attribute(__MODULE__, :before_compile, Kernel.ErrorsTest)
      end
      """)
  end

  defmacro sample(0), do: 0

  defmacro before_compile(_) do
    quote(do: _)
  end

  defp range do
    1..3
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
