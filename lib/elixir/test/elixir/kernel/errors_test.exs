Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true
  import CompileAssertion

  defmodule UnproperMacro do
    defmacro unproper(args), do: args
    defmacro exit(args), do: args
  end

  test :invalid_token do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid token: \end",
      '\end\nlol\nbarbecue'
  end

  test :invalid_quoted_token do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: \"world\"",
      '"hello" "world"'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: Foobar",
      '1 Foobar'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: foo",
      'Foo.:foo'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: \"foo\"",
      'Foo.:"foo\#{:bar}"'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: \"",
      'Foo.:"\#{:bar}"'
  end

  test :invalid_identifier do
    msg = fn char, name -> "nofile:1: invalid character '#{char}' in identifier: #{name}" end

    assert_compile_fail SyntaxError, msg.(:@, "foo@"), 'foo@'
    assert_compile_fail SyntaxError, msg.(:@, "foo@"), 'foo@ '
    assert_compile_fail SyntaxError, msg.(:@, "foo@bar"), 'foo@bar'
    assert_compile_fail SyntaxError, msg.(:!, "Foo!"), 'Foo!'
  end

  test :kw_missing_space do
    msg = "nofile:1: keyword argument must be followed by space after: foo:"

    assert_compile_fail SyntaxError, msg, "foo:bar"
    assert_compile_fail SyntaxError, msg, "foo:+"
    assert_compile_fail SyntaxError, msg, "foo:+1"
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
    assert_compile_fail SyntaxError,
      "nofile:1: heredoc start must be followed by a new line after \"\"\"",
      '"""bar\n"""'
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

  test :syntax_error_before_sigil do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: sigil ~s with content 'bar baz'",
      '~s(foo) ~s(bar baz)'
  end

  test :compile_error_on_op_ambiguity do
    msg = "nofile:1: \"a -1\" looks like a function call but there is a variable named \"a\", " <>
          "please use explicit parentheses or even spaces"
    assert_compile_fail CompileError, msg, 'a = 1; a -1'

    max = 1
    assert max == 1
    assert (max 1, 2) == 2
  end

  test :syntax_error_on_parens_call do
    msg = "nofile:1: unexpected parentheses. If you are making a function call, do not " <>
          "insert spaces between the function name and the opening parentheses. " <>
          "Syntax error before: '('"

    assert_compile_fail SyntaxError, msg, 'foo (hello, world)'
  end

  test :syntax_error_on_nested_no_parens_call do
    msg = "nofile:1: unexpected comma. Parentheses are required to solve ambiguity in " <>
          "nested calls. Syntax error before: ','"

    assert_compile_fail SyntaxError, msg, '[foo 1, 2]'
    assert_compile_fail SyntaxError, msg, '[foo bar 1, 2]'
    assert_compile_fail SyntaxError, msg, '[do: foo 1, 2]'
    assert_compile_fail SyntaxError, msg, 'foo(do: bar 1, 2)'
    assert_compile_fail SyntaxError, msg, '{foo 1, 2}'
    assert_compile_fail SyntaxError, msg, '{foo bar 1, 2}'
    assert_compile_fail SyntaxError, msg, 'foo 1, foo 2, 3'
    assert_compile_fail SyntaxError, msg, 'foo(1, foo 2, 3)'

    assert is_list List.flatten [1]
    assert is_list Enum.reverse [3, 2, 1], [4, 5, 6]
    assert is_list(Enum.reverse [3, 2, 1], [4, 5, 6])
    assert [List.flatten List.flatten [1]] == [[1]]

    interpret = fn x -> Macro.to_string Code.string_to_quoted! x end
    assert interpret.("f 1 + g h 2, 3") == "f(1 + g(h(2, 3)))"
    assert interpret.("assert [] = TestRepo.all from p in Post, where: p.title in ^[]") ==
           "assert([] = TestRepo.all(from(p in Post, where: p.title() in ^[])))"
  end

  test :syntax_error_on_atom_dot_alias do
    msg = "nofile:1: atom cannot be followed by an alias. If the '.' was meant to be " <>
          "part of the atom's name, the name must be quoted. Syntax error before: '.'"

    assert_compile_fail SyntaxError, msg, ':foo.Bar'
    assert_compile_fail SyntaxError, msg, ':"foo".Bar'
  end

  test :syntax_error_with_no_token do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: ) (for \"(\" starting at line 1)",
      'case 1 ('
  end

  test :clause_with_defaults do
    assert_compile_fail CompileError,
      "nofile:3: def hello/1 has default values and multiple clauses, " <>
      "define a function head with the defaults",
      ~C'''
      defmodule Kernel.ErrorsTest.ClauseWithDefaults1 do
        def hello(arg \\ 0), do: nil
        def hello(arg \\ 1), do: nil
      end
      '''

    assert_compile_fail CompileError,
      "nofile:6: def hello/1 has default values and multiple clauses, " <>
      "define a function head with the defaults",
      ~C'''
      defmodule Kernel.ErrorsTest.ClauseWithDefaults2 do
        def bye(arg \\ 0)
        def bye(arg), do: arg

        def hello(arg \\ 0), do: nil
        def hello(arg), do: arg
      end
      '''

    assert_compile_fail CompileError,
      "nofile:2: function foo/0 undefined",
      ~C'''
      defmodule Kernel.ErrorsTest.ClauseWithDefaults3 do
        def hello(foo, bar \\ foo)
        def hello(foo, bar), do: foo + bar
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
      defmodule Kernel.ErrorsTest.DifferentDefsWithDefaults1 do
        def hello(a, b \\ nil), do: a + b
        def hello(a, b \\ nil, c \\ nil), do: a + b + c
      end
      '''

    assert_compile_fail CompileError,
      "nofile:3: def hello/2 conflicts with defaults from def hello/3",
      ~C'''
      defmodule Kernel.ErrorsTest.DifferentDefsWithDefaults2 do
        def hello(a, b \\ nil, c \\ nil), do: a + b + c
        def hello(a, b \\ nil), do: a + b
      end
      '''
  end

  test :bad_form do
    assert_compile_fail CompileError,
      "nofile:2: function bar/0 undefined",
      '''
      defmodule Kernel.ErrorsTest.BadForm do
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
      "nofile:1: invalid argument for unary operator ^, expected an existing variable, got: ^is_atom(:foo)",
      '^is_atom(:foo) = true'
  end

  test :literal_on_map_and_struct do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: '}'",
      '%{{:a, :b}}'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: '{'",
      '%{:a, :b}{a: :b}'

    assert_compile_fail CompileError,
      "nofile:1: expected key-value pairs in a map, got: put_in(foo.bar().baz(), nil)",
      'foo = 1; %{put_in(foo.bar.baz, nil), :bar}'
  end

  test :struct_fields_on_defstruct do
    assert_compile_fail ArgumentError,
      "struct field names must be atoms, got: 1",
      '''
      defmodule Kernel.ErrorsTest.StructFieldsOnDefstruct do
        defstruct [1, 2, 3]
      end
      '''
  end

  test :struct_access_on_body do
    assert_compile_fail CompileError,
      "nofile:3: cannot access struct Kernel.ErrorsTest.StructAccessOnBody, " <>
      "the struct was not yet defined or the struct " <>
      "is being accessed in the same context that defines it",
      '''
      defmodule Kernel.ErrorsTest.StructAccessOnBody do
        defstruct %{name: "Brasilia"}
        %Kernel.ErrorsTest.StructAccessOnBody{}
      end
      '''
  end

  test :unbound_map_key_var do
    assert_compile_fail CompileError,
      "nofile:1: illegal use of variable x in map key",
      '%{x => 1} = %{}'

    assert_compile_fail CompileError,
      "nofile:1: illegal use of variable x in map key",
      '%{x = 1 => 1}'
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
        %{name: "john"}
      end
    end

    assert_compile_fail CompileError,
      "nofile:1: unknown key :age for struct Kernel.ErrorsTest.GoodStruct",
      '%#{GoodStruct}{age: 27}'
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

  test :invalid_unquote_splicing_in_oneliners do
    assert_compile_fail ArgumentError,
      "unquote_splicing only works inside arguments and block contexts, " <>
      "wrap it in parens if you want it to work with one-liners",
      '''
      defmodule Kernel.ErrorsTest.InvalidUnquoteSplicingInOneliners do
        defmacro oneliner2 do
          quote do: unquote_splicing 1
        end

        def callme do
          oneliner2
        end
      end
      '''
  end

  test :invalid_quote_args do
    assert_compile_fail CompileError,
      "nofile:1: invalid arguments for quote",
      'quote 1'
    assert_compile_fail CompileError,
      "nofile:1: invalid options for quote, expected a keyword list",
      '''
      quote :foo do
        foo
      end
      '''
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
      defmodule Kernel.ErrorsTest.UnhandledStab do
        def fun do
          casea foo, do: (bar -> baz)
        end
      end
      '''
  end

  test :undefined_non_local_function do
    assert_compile_fail CompileError,
      "nofile:1: undefined function casea/2",
      'casea foo, do: @hello :world'
  end

  test :invalid_attribute do
    msg = ~r"cannot inject attribute @foo into function/macro because cannot escape "
    assert_raise ArgumentError, msg, fn ->
      defmodule InvalidAttribute do
        @foo fn -> end
        def bar, do: @foo
      end
    end
  end

  test :invalid_struct_field_value do
    msg = ~r"invalid value for struct field baz, cannot escape "
    assert_raise ArgumentError, msg, fn ->
      defmodule InvaliadStructFieldValue do
        defstruct baz: fn -> end
      end
    end
  end

  test :match_attribute_in_module do
    msg = "invalid write attribute syntax, you probably meant to use: @foo expression"
    assert_raise ArgumentError, msg, fn ->
      defmodule MatchAttributeInModule do
        @foo = 42
      end
    end
  end

  test :invalid_fn_args do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: end (for \"fn\" starting at line 1)",
      'fn 1'
  end

  test :invalid_escape do
    assert_compile_fail TokenMissingError,
      "nofile:1: invalid escape \\ at end of file",
      '1 \\'
  end

  test :function_local_conflict do
    assert_compile_fail CompileError,
      "nofile:1: imported Kernel.&&/2 conflicts with local function",
      '''
      defmodule Kernel.ErrorsTest.FunctionLocalConflict do
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
      defmodule Kernel.ErrorsTest.MacroLocalConflict do
        def hello, do: 1 || 2
        defmacro _ || _, do: :ok

        defmacro _ && _, do: :error
        def world, do: 1 && 2
      end
      '''
  end

  test :macro_with_undefined_local do
    assert_compile_fail UndefinedFunctionError,
      "undefined function: Kernel.ErrorsTest.MacroWithUndefinedLocal.unknown/1 " <>
      "(function unknown/1 is not available)",
      '''
      defmodule Kernel.ErrorsTest.MacroWithUndefinedLocal do
        defmacrop bar, do: unknown(1)
        def baz, do: bar()
      end
      '''
  end

  test :private_macro do
    assert_compile_fail UndefinedFunctionError,
      "undefined function: Kernel.ErrorsTest.PrivateMacro.foo/0 (function foo/0 is not available)",
      '''
      defmodule Kernel.ErrorsTest.PrivateMacro do
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
      defmodule Kernel.ErrorsTest.FunctionDefinitionWithAlias do
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
      defmodule Kernel.ErrorsTest.FunctionImportConflict do
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
      defmodule Kernel.ErrorsTest.UnrequiredMacro do
        Kernel.ErrorsTest.UnproperMacro.unproper([])
      end
      '''
  end

  test :def_defmacro_clause_change do
    assert_compile_fail CompileError,
      "nofile:3: defmacro foo/1 already defined as def",
      '''
      defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
        def foo(1), do: 1
        defmacro foo(x), do: x
      end
      '''
  end

  test :internal_function_overridden do
    assert_compile_fail CompileError,
      "nofile:1: function __info__/1 is internal and should not be overridden",
      '''
      defmodule Kernel.ErrorsTest.InternalFunctionOverridden do
        def __info__(_), do: []
      end
      '''
  end

  test :no_macros do
    assert_compile_fail CompileError,
      "nofile:2: could not load macros from module :lists",
      '''
      defmodule Kernel.ErrorsTest.NoMacros do
        import :lists, only: :macros
      end
      '''
  end

  test :invalid_macro do
    assert_compile_fail CompileError,
      "nofile: invalid quoted expression: {:foo, :bar, :baz, :bat}",
      '''
      defmodule Kernel.ErrorsTest.InvalidMacro do
        defmacrop oops do
          {:foo, :bar, :baz, :bat}
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
      "nofile:4: module Kernel.ErrorsTest.ScheduledModule.Hygiene is not loaded but was defined. " <>
      "This happens because you are trying to use a module in the same context it is defined. " <>
      "Try defining the module outside the context that requires it.",
      '''
      defmodule Kernel.ErrorsTest.ScheduledModule do
        defmodule Hygiene do
        end
        import Kernel.ErrorsTest.ScheduledModule.Hygiene
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
      "nofile:1: unexpected token: \")\". \"do\" starting at line 1 is missing terminator \"end\"",
      '"foo\#{case 1 do )}bar"'
  end

  test :in_definition_module do
    assert_compile_fail CompileError,
      "nofile:2: cannot define module Kernel.ErrorsTest.InDefinitionModule " <>
      "because it is currently being defined in nofile:1",
      '''
      defmodule Kernel.ErrorsTest.InDefinitionModule do
        defmodule Elixir.Kernel.ErrorsTest.InDefinitionModule, do: true
      end
      '''
  end

  test :invalid_definition do
    assert_compile_fail CompileError,
      "nofile:1: invalid syntax in def 1.(hello)",
      'defmodule Kernel.ErrorsTest.InvalidDefinition, do: (def 1.(hello), do: true)'
  end

  test :duplicated_bitstring_size do
    assert_compile_fail CompileError,
      "nofile:1: duplicated size definition in bitstring",
      '<<1 :: size(12)-size(13)>>'
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

  test :invalid_alias do
    assert_compile_fail CompileError,
      "nofile:1: invalid value for keyword :as, expected a simple alias, got nested alias: Sample.Lists",
      'alias :lists, as: Sample.Lists'

    assert_compile_fail CompileError,
      "nofile:1: invalid argument for alias, expected a compile time atom or alias, got: 1 + 2",
      'alias 1 + 2'
  end

  test :invalid_alias_expansion do
    assert_compile_fail CompileError,
      "nofile:1: an alias must expand to an atom at compilation time, but did not in \"foo.Foo\". " <>
      "Use Module.concat/2 if you want to dynamically generate aliases",
      'foo = :foo; foo.Foo'
  end

  test :invalid_import_option do
    assert_compile_fail CompileError,
      "nofile:1: unsupported option :ops given to import",
      'import :lists, [ops: 1]'
  end

  test :invalid_rescue_clause do
    assert_compile_fail CompileError,
      "nofile:4: invalid rescue clause. The clause should match on an alias, a variable or be in the `var in [alias]` format",
      'try do\n1\nrescue\n%UndefinedFunctionError{arity: 1} -> false\nend'
  end

  test :invalid_for_without_generators do
    assert_compile_fail CompileError,
      "nofile:1: for comprehensions must start with a generator",
      'for is_atom(:foo), do: :foo'
  end

  test :invalid_for_bit_generator do
    assert_compile_fail CompileError,
      "nofile:1: bitstring fields without size are not allowed in bitstring generators",
      'for << x :: binary <- "123" >>, do: x'
  end

  test :unbound_cond do
    assert_compile_fail CompileError,
      "nofile:1: unbound variable _ inside cond. If you want the last clause to always match, " <>
      "you probably meant to use: true ->",
      'cond do _ -> true end'
  end

  test :fun_different_arities do
    assert_compile_fail CompileError,
      "nofile:1: cannot mix clauses with different arities in function definition",
      'fn x -> x; x, y -> x + y end'
  end

  test :end_of_expression do
    # All valid examples
    Code.eval_quoted '''
    1;
    2;
    3

    (;)
    (;1)
    (1;)
    (1; 2)

    fn -> 1; 2 end
    fn -> ; end

    if true do
      ;
    end

    try do
      ;
    catch
      _, _ -> ;
    after
      ;
    end
    '''

    # All invalid examples
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: ';'",
      '1+;\n2'

    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: ';'",
      'max(1, ;2)'
  end

  test :new_line_error do
    assert_compile_fail SyntaxError,
      "nofile:3: syntax error before: eol",
      'if true do\n  foo = [],\n  baz\nend'
  end

  test :invalid_var_or_function_on_guard do
    assert_compile_fail CompileError,
      "nofile:4: unknown variable something_that_does_not_exist or " <>
      "cannot invoke local something_that_does_not_exist/0 inside guard",
      '''
      defmodule Kernel.ErrorsTest.InvalidVarOrFunctionOnGuard do
        def bar do
          case [] do
            [] when something_that_does_not_exist == [] -> :ok
          end
        end
      end
      '''
  end

  test :bodyless_function_with_guard do
    assert_compile_fail CompileError,
      "nofile:2: missing do keyword in def",
      '''
      defmodule Kernel.ErrorsTest.BodyessFunctionWithGuard do
        def foo(n) when is_number(n)
      end
      '''
  end

  test :invalid_args_for_bodyless_clause do
    assert_compile_fail CompileError,
      "nofile:2: can use only variables and \\\\ as arguments of bodyless clause",
      '''
      defmodule Kernel.ErrorsTest.InvalidArgsForBodylessClause do
        def foo(arg // nil)
        def foo(_), do: :ok
      end
      '''
  end

  test :invalid_function_on_match do
    assert_compile_fail CompileError,
      "nofile:3: cannot invoke local something_that_does_not_exist/0 inside match",
      '''
      defmodule Kernel.ErrorsTest.InvalidFunctionOnMatch do
        def fun do
          case [] do; something_that_does_not_exist() -> :ok; end
        end
      end
      '''
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
      defmodule Kernel.ErrorsTest.TypespecErrors1 do
        @type omg :: foo
      end
      '''

    if :erlang.system_info(:otp_release) >= '18' do
      message = "nofile:2: spec for undefined function omg/0"
    else
      message = "nofile:2: spec for undefined function Kernel.ErrorsTest.TypespecErrors2.omg/0"
    end

    assert_compile_fail CompileError, message,
      '''
      defmodule Kernel.ErrorsTest.TypespecErrors2 do
        @spec omg :: atom
      end
      '''
  end

  test :bad_unquoting do
    assert_compile_fail CompileError,
      "nofile: invalid quoted expression: {:foo, 0, 1}",
      '''
      defmodule Kernel.ErrorsTest.BadUnquoting do
        def range(unquote({:foo, 0, 1})), do: :ok
      end
      '''
  end

  test :macros_error_stacktrace do
    assert [{:erlang, :+, [1, :foo], _},
            {Kernel.ErrorsTest.MacrosErrorStacktrace, :sample, 1, _}|_] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosErrorStacktrace do
        defmacro sample(num), do: num + :foo
        def other, do: sample(1)
      end
      """)
  end

  test :macros_function_clause_stacktrace do
    assert [{__MODULE__, :sample, 1, _}|_] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosFunctionClauseStacktrace do
        import Kernel.ErrorsTest
        sample(1)
      end
      """)
  end

  test :macros_interpreted_function_clause_stacktrace do
    assert [{Kernel.ErrorsTest.MacrosInterpretedFunctionClauseStacktrace, :sample, 1, _}|_] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosInterpretedFunctionClauseStacktrace do
        defmacro sample(0), do: 0
        def other, do: sample(1)
      end
      """)
  end

  test :macros_compiled_callback do
    assert [{Kernel.ErrorsTest, :__before_compile__, [%Macro.Env{module: Kernel.ErrorsTest.MacrosCompiledCallback}], _}|_] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosCompiledCallback do
        Module.put_attribute(__MODULE__, :before_compile, Kernel.ErrorsTest)
      end
      """)
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
