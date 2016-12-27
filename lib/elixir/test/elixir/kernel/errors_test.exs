Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true
  import CompileAssertion

  defmacro hello do
    quote location: :keep do
      def hello, do: :world
    end
  end

  test "invalid token" do
    assert_compile_fail SyntaxError,
      "nofile:1: unexpected token: \"\u200B\" (column 7, codepoint U+200B)",
      '[foo: \u200B]\noops'
  end

  test "invalid quoted token" do
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

  test "invalid identifier" do
    msg = fn name -> "nofile:1: invalid character \"@\" (codepoint U+0040) in token: #{name}" end

    assert_compile_fail SyntaxError, msg.("foo@"), 'foo@'
    assert_compile_fail SyntaxError, msg.("foo@"), 'foo@ '
    assert_compile_fail SyntaxError, msg.("foo@bar"), 'foo@bar'
    assert_compile_fail SyntaxError, msg.("Foo@"), 'Foo@'
  end

  test "invalid fn" do
    assert_compile_fail SyntaxError,
                        "nofile:1: expected clauses to be defined with -> inside: 'fn'",
                        'fn 1 end'
  end

  test "invalid Access" do
    msg = fn(val) ->
      "nofile:1: the Access syntax and calls to Access.get/2" <>
      " are not available for the value: " <> val
    end

    assert_compile_fail CompileError, msg.("1"), "1[:foo]"
    assert_compile_fail CompileError, msg.("1.1"), "1.1[:foo]"
    assert_compile_fail CompileError, msg.("{}"), "{}[:foo]"
    assert_compile_fail CompileError, msg.(":foo"), ":foo[:foo]"
    assert_compile_fail CompileError, msg.("\"\""), "\"\"[:foo]"
    assert_compile_fail CompileError, msg.("<<>>"), "<<>>[:foo]"
  end

  test "kw missing space" do
    msg = "nofile:1: keyword argument must be followed by space after: foo:"

    assert_compile_fail SyntaxError, msg, "foo:bar"
    assert_compile_fail SyntaxError, msg, "foo:+"
    assert_compile_fail SyntaxError, msg, "foo:+1"
  end

  test "sigil terminator" do
    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: \" (for sigil ~r\" starting at line 1)",
      '~r"foo\n\n'

    assert_compile_fail TokenMissingError,
      "nofile:3: missing terminator: } (for sigil ~r{ starting at line 1)",
      '~r{foo\n\n'
  end

  test "dot terminator" do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: \" (for function name starting at line 1)",
      'foo."bar'
  end

  test "string terminator" do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: \" (for string starting at line 1)",
      '"bar'
  end

  test "heredoc start" do
    assert_compile_fail SyntaxError,
      "nofile:1: heredoc start must be followed by a new line after \"\"\"",
      '"""bar\n"""'
  end

  test "heredoc terminator" do
    assert_compile_fail TokenMissingError,
      "nofile:2: missing terminator: \"\"\" (for heredoc starting at line 1)",
      '"""\nbar'
    assert_compile_fail SyntaxError,
      "nofile:2: invalid location for heredoc terminator, please escape token or move it to its own line: \"\"\"",
      '"""\nbar"""'
  end

  test "unexpected end" do
    assert_compile_fail SyntaxError,
      "nofile:1: unexpected token: end",
      '1 end'
  end

  test "syntax error" do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: '.'",
      '+.foo'
  end

  test "syntax error before sigil" do
    msg = fn x -> "nofile:1: syntax error before: sigil ~s starting with content '#{x}'" end

    assert_compile_fail SyntaxError, msg.("bar baz"), '~s(foo) ~s(bar baz)'
    assert_compile_fail SyntaxError, msg.(""), '~s(foo) ~s()'
    assert_compile_fail SyntaxError, msg.("bar "), '~s(foo) ~s(bar \#{:baz})'
    assert_compile_fail SyntaxError, msg.(""), '~s(foo) ~s(\#{:bar} baz)'
  end

  test "compile error on op ambiguity" do
    msg = "nofile:1: \"a -1\" looks like a function call but there is a variable named \"a\", " <>
          "please use explicit parentheses or even spaces"
    assert_compile_fail CompileError, msg, 'a = 1; a -1'

    max = 1
    assert max == 1
    assert (max 1, 2) == 2
  end

  test "syntax error with do" do
    assert_compile_fail SyntaxError,
                        ~r/nofile:1: unexpected token "do"./,
                        'if true, do\n'

    assert_compile_fail SyntaxError,
                        ~r/nofile:1: unexpected keyword "do:"./,
                        'if true do:\n'
  end

  test "syntax error on parens call" do
    msg = "nofile:1: unexpected parentheses. If you are making a function call, do not " <>
          "insert spaces between the function name and the opening parentheses. " <>
          "Syntax error before: '('"

    assert_compile_fail SyntaxError, msg, 'foo (hello, world)'
  end

  test "syntax error on nested no parens call" do
    msg = "nofile:1: unexpected comma. Parentheses are required to solve ambiguity"

    assert_compile_fail SyntaxError, msg, '[foo 1, 2]'
    assert_compile_fail SyntaxError, msg, '[foo bar 1, 2]'
    assert_compile_fail SyntaxError, msg, '[do: foo 1, 2]'
    assert_compile_fail SyntaxError, msg, 'foo(do: bar 1, 2)'
    assert_compile_fail SyntaxError, msg, '{foo 1, 2}'
    assert_compile_fail SyntaxError, msg, '{foo bar 1, 2}'
    assert_compile_fail SyntaxError, msg, 'foo 1, foo 2, 3'
    assert_compile_fail SyntaxError, msg, 'foo 1, @bar 3, 4'
    assert_compile_fail SyntaxError, msg, 'foo 1, 2 + bar 3, 4'
    assert_compile_fail SyntaxError, msg, 'foo(1, foo 2, 3)'

    assert is_list List.flatten [1]
    assert is_list Enum.reverse [3, 2, 1], [4, 5, 6]
    assert is_list(Enum.reverse [3, 2, 1], [4, 5, 6])
    assert false || is_list Enum.reverse [3, 2, 1], [4, 5, 6]
    assert [List.flatten List.flatten [1]] == [[1]]

    interpret = fn x -> Macro.to_string Code.string_to_quoted! x end
    assert interpret.("f 1 + g h 2, 3") == "f(1 + g(h(2, 3)))"
    assert interpret.("assert [] = TestRepo.all from p in Post, where: p.title in ^[]") ==
           "assert([] = TestRepo.all(from(p in Post, where: p.title() in ^[])))"
  end

  test "syntax error on atom dot alias" do
    msg = "nofile:1: atom cannot be followed by an alias. If the '.' was meant to be " <>
          "part of the atom's name, the atom name must be quoted. Syntax error before: '.'"

    assert_compile_fail SyntaxError, msg, ':foo.Bar'
    assert_compile_fail SyntaxError, msg, ':"foo".Bar'
  end

  test "syntax error with no token" do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: ) (for \"(\" starting at line 1)",
      'case 1 ('
  end

  test "clause with defaults" do
    assert_compile_fail CompileError,
      "nofile:3: definitions with multiple clauses and default values require a header",
      ~C'''
      defmodule Kernel.ErrorsTest.ClauseWithDefaults1 do
        def hello(arg \\ 0), do: nil
        def hello(arg \\ 1), do: nil
      end
      '''

    assert_compile_fail CompileError,
      "nofile:2: undefined function foo/0",
      ~C'''
      defmodule Kernel.ErrorsTest.ClauseWithDefaults3 do
        def hello(foo, bar \\ foo())
        def hello(foo, bar), do: foo + bar
      end
      '''
  end

  test "invalid match pattern" do
    assert_compile_fail CompileError,
    "nofile:2: invalid expression in match",
    '''
    case true do
      true && true -> true
    end
    '''
  end

  test "different defs with defaults" do
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

  test "bad form" do
    assert_compile_fail CompileError,
      "nofile:2: undefined function bar/0",
      '''
      defmodule Kernel.ErrorsTest.BadForm do
        def foo, do: bar()
      end
      '''
  end

  test "unbound var" do
    assert_compile_fail CompileError,
      "nofile:1: unbound variable ^x",
      '^x = 1'
  end

  test "unbound not match" do
    assert_compile_fail CompileError,
      "nofile:1: cannot use ^x outside of match clauses",
      '^x'
  end

  test "unbound expr" do
    assert_compile_fail CompileError,
      "nofile:1: invalid argument for unary operator ^, expected an existing variable, got: ^is_atom(:foo)",
      '^is_atom(:foo) = true'
  end

  test "literal on map and struct" do
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

  test "struct fields on defstruct" do
    assert_compile_fail ArgumentError,
      "struct field names must be atoms, got: 1",
      '''
      defmodule Kernel.ErrorsTest.StructFieldsOnDefstruct do
        defstruct [1, 2, 3]
      end
      '''
  end

  test "struct access on body" do
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

  test "unbound map key var" do
    assert_compile_fail CompileError,
      ~r"nofile:1: illegal use of variable x inside map key match,",
      '%{x => 1} = %{}'

    assert_compile_fail CompileError,
      ~r"nofile:1: illegal use of variable x inside map key match,",
      '%{x = 1 => 1}'
  end

  test "struct errors" do
    assert_compile_fail CompileError,
      "nofile:1: BadStruct.__struct__/1 is undefined, cannot expand struct BadStruct",
      '%BadStruct{}'

    assert_compile_fail CompileError,
      "nofile:1: BadStruct.__struct__/0 is undefined, cannot expand struct BadStruct",
      '%BadStruct{} = %{}'

    defmodule BadStruct do
      def __struct__ do
        []
      end
    end

    assert_compile_fail CompileError,
      "nofile:1: expected Kernel.ErrorsTest.BadStruct.__struct__/0 to return a map, got: []",
      '%#{BadStruct}{} = %{}'

    defmodule GoodStruct do
      defstruct name: "john"
    end

    assert_compile_fail KeyError,
      "key :age not found in: %Kernel.ErrorsTest.GoodStruct{name: \"john\"}",
      '%#{GoodStruct}{age: 27}'

    assert_compile_fail CompileError,
      "nofile:1: unknown key :age for struct Kernel.ErrorsTest.GoodStruct",
      '%#{GoodStruct}{age: 27} = %{}'

    assert_compile_fail CompileError,
      "nofile:1: unknown key ^field for struct Kernel.ErrorsTest.GoodStruct",
      '%#{GoodStruct}{^field => 27} = %{}'
  end

  test "name for defmodule" do
    assert_compile_fail CompileError,
      "nofile:1: invalid module name: 3",
      'defmodule 1 + 2, do: 3'
  end

  test "invalid unquote" do
    assert_compile_fail CompileError,
      "nofile:1: unquote called outside quote",
      'unquote 1'
  end

  test "invalid unquote splicing in oneliners" do
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

  test "invalid quote args" do
    assert_compile_fail CompileError,
      "nofile:1: invalid arguments for quote",
      'quote 1'
    assert_compile_fail CompileError,
      "nofile:1: invalid options for quote, expected a keyword list",
      'quote(:foo, do: foo)'
  end

  test "invalid calls" do
    assert_compile_fail CompileError,
      "nofile:1: invalid call foo(1)(2)",
      'foo(1)(2)'

    assert_compile_fail CompileError,
      "nofile:1: invalid call 1.foo()",
      '1.foo'
  end

  test "unhandled stab" do
    assert_compile_fail CompileError,
      "nofile:1: unhandled operator ->",
      '(bar -> baz)'
  end

  test "undefined non-local function" do
    assert_compile_fail CompileError,
      "nofile:1: undefined function call/2",
      'call foo, do: :foo'
  end

  test "invalid attribute" do
    msg = ~r"cannot inject attribute @foo into function/macro because cannot escape "
    assert_raise ArgumentError, msg, fn ->
      defmodule InvalidAttribute do
        @foo fn -> nil end
        def bar, do: @foo
      end
    end
  end

  test "invalid struct field value" do
    msg = ~r"invalid value for struct field baz, cannot escape "
    assert_raise ArgumentError, msg, fn ->
      defmodule InvaliadStructFieldValue do
        defstruct baz: fn -> nil end
      end
    end
  end

  test "match attribute in module" do
    msg = "invalid write attribute syntax, you probably meant to use: @foo expression"
    assert_raise ArgumentError, msg, fn ->
      defmodule MatchAttributeInModule do
        @foo = 42
      end
    end
  end

  test "invalid fn args" do
    assert_compile_fail TokenMissingError,
      "nofile:1: missing terminator: end (for \"fn\" starting at line 1)",
      'fn 1'
  end

  test "invalid escape" do
    assert_compile_fail TokenMissingError,
      "nofile:1: invalid escape \\ at end of file",
      '1 \\'
  end

  test "function local conflict" do
    assert_compile_fail CompileError,
      "nofile:1: imported Kernel.&&/2 conflicts with local function",
      '''
      defmodule Kernel.ErrorsTest.FunctionLocalConflict do
        def other, do: 1 && 2
        def _ && _, do: :error
      end
      '''
  end

  test "macro local conflict" do
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

  test "macro with undefined local" do
    assert_compile_fail UndefinedFunctionError,
      "function Kernel.ErrorsTest.MacroWithUndefinedLocal.unknown/1" <>
      " is undefined (function unknown/1 is not available)",
      '''
      defmodule Kernel.ErrorsTest.MacroWithUndefinedLocal do
        defmacrop bar, do: unknown(1)
        def baz, do: bar()
      end
      '''
  end

  test "private macro" do
    assert_compile_fail UndefinedFunctionError,
      "function Kernel.ErrorsTest.PrivateMacro.foo/0 is undefined (function foo/0 is not available)",
      '''
      defmodule Kernel.ErrorsTest.PrivateMacro do
        defmacrop foo, do: 1
        defmacro bar, do: __MODULE__.foo
        defmacro baz, do: bar()
      end
      '''
  end

  test "function definition with alias" do
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

  test "function import conflict" do
    assert_compile_fail CompileError,
      "nofile:3: function exit/1 imported from both :erlang and Kernel, call is ambiguous",
      '''
      defmodule Kernel.ErrorsTest.FunctionImportConflict do
        import :erlang, warn: false
        def foo, do: exit(:test)
      end
      '''
  end

  test "import invalid macro" do
    assert_compile_fail CompileError,
      "nofile:1: cannot import Kernel.invalid/1 because it is undefined or private",
      'import Kernel, only: [invalid: 1]'
  end

  test "import with invalid options" do
    assert_compile_fail CompileError,
      "nofile:1: invalid :only option for import, " <>
      "expected a keyword list with integer values",
      'import Kernel, only: [invalid: nil]'

    assert_compile_fail CompileError,
      "nofile:1: invalid :except option for import, " <>
      "expected a keyword list with integer values",
      'import Kernel, except: [invalid: nil]'
  end

  test "import with conflicting options" do
    assert_compile_fail CompileError,
      "nofile:1: :only and :except can only be given together to import" <>
      " when :only is either :functions or :macros",
      'import Kernel, only: [], except: []'
  end

  test "unrequired macro" do
    assert_compile_fail CompileError,
      "nofile:2: you must require Kernel.ErrorsTest before invoking " <>
      "the macro Kernel.ErrorsTest.hello/0",
      '''
      defmodule Kernel.ErrorsTest.UnrequiredMacro do
        Kernel.ErrorsTest.hello()
      end
      '''
  end

  test "def defmacro clause change" do
    assert_compile_fail CompileError,
      "nofile:3: defmacro foo/1 already defined as def",
      '''
      defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
        def foo(1), do: 1
        defmacro foo(x), do: x
      end
      '''
  end

  test "def defp clause change from another file" do
    assert_compile_fail CompileError,
      "nofile:4: def hello/0 already defined as defp",
      '''
      defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
        require Kernel.ErrorsTest
        defp hello, do: :world
        Kernel.ErrorsTest.hello()
      end
      '''
  end

  test "internal function overridden" do
    assert_compile_fail CompileError,
      "nofile:1: function __info__/1 is internal and should not be overridden",
      '''
      defmodule Kernel.ErrorsTest.InternalFunctionOverridden do
        def __info__(_), do: []
      end
      '''
  end

  test "no macros" do
    assert_compile_fail CompileError,
      "nofile:2: could not load macros from module :lists",
      '''
      defmodule Kernel.ErrorsTest.NoMacros do
        import :lists, only: :macros
      end
      '''
  end

  test "invalid macro" do
    assert_compile_fail CompileError,
      "nofile: invalid quoted expression: {:foo, :bar, :baz, :bat}",
      '''
      defmodule Kernel.ErrorsTest.InvalidMacro do
        defmacrop oops do
          {:foo, :bar, :baz, :bat}
        end

        def test, do: oops()
      end
      '''
  end

  test "unloaded module" do
    assert_compile_fail CompileError,
      "nofile:1: module Certainly.Doesnt.Exist is not loaded and could not be found",
      'import Certainly.Doesnt.Exist'
  end

  test "module imported from the context it was defined in" do
    assert_compile_fail CompileError,
      ~r"nofile:4: module Kernel.ErrorsTest.ScheduledModule.Hygiene is not loaded but was defined.",
      '''
      defmodule Kernel.ErrorsTest.ScheduledModule do
        defmodule Hygiene do
        end
        import Kernel.ErrorsTest.ScheduledModule.Hygiene
      end
      '''
  end

  test "module imported from the same module" do
    assert_compile_fail CompileError,
      ~r"nofile:3: you are trying to use the module Kernel.ErrorsTest.ScheduledModule.Hygiene which is currently being defined",
      '''
      defmodule Kernel.ErrorsTest.ScheduledModule do
        defmodule Hygiene do
          import Kernel.ErrorsTest.ScheduledModule.Hygiene
        end
      end
      '''
  end

  test "already compiled module" do
    assert_compile_fail ArgumentError,
      "could not call eval_quoted on module Record " <>
      "because it was already compiled",
      'Module.eval_quoted Record, quote(do: 1), [], file: __ENV__.file'
  end

  test "doc attributes format" do
    message =
      "expected the moduledoc attribute to be {line, doc} (where \"doc\" is " <>
      "a binary, a boolean, or nil), got: \"Other\""
    assert_raise ArgumentError, message, fn ->
      defmodule DocAttributesFormat do
        Module.put_attribute(__MODULE__, :moduledoc, "Other")
      end
    end

    message = "expected the moduledoc attribute to contain a binary, a boolean, or nil, got: :not_a_binary"
    assert_raise ArgumentError, message, fn ->
      defmodule AtSyntaxDocAttributesFormat do
        @moduledoc :not_a_binary
      end
    end
  end

  test "interpolation error" do
    assert_compile_fail SyntaxError,
      "nofile:1: \"do\" is missing terminator \"end\". unexpected token: \")\" at line 1",
      '"foo\#{case 1 do )}bar"'
  end

  test "in definition module" do
    assert_compile_fail CompileError,
      "nofile:2: cannot define module Kernel.ErrorsTest.InDefinitionModule " <>
      "because it is currently being defined in nofile:1",
      '''
      defmodule Kernel.ErrorsTest.InDefinitionModule do
        defmodule Elixir.Kernel.ErrorsTest.InDefinitionModule, do: true
      end
      '''
  end

  test "invalid definition" do
    assert_compile_fail CompileError,
      "nofile:1: invalid syntax in def 1.(hello)",
      'defmodule Kernel.ErrorsTest.InvalidDefinition, do: (def 1.(hello), do: true)'
  end

  test "duplicated bitstring size" do
    assert_compile_fail CompileError,
      "nofile:1: duplicated size definition in bitstring",
      '<<1::size(12)-size(13)>>'
  end

  test "invalid bitstring specified" do
    assert_compile_fail CompileError,
      "nofile:1: unknown bitstring specifier :atom",
      '<<1::(:atom)>>'

    assert_compile_fail CompileError,
      "nofile:1: unknown bitstring specifier unknown()",
      '<<1::unknown>>'

    assert_compile_fail CompileError,
      "nofile:1: unknown bitstring specifier another(12)",
      '<<1::another(12)>>'

    assert_compile_fail CompileError,
      "nofile:1: size in bitstring expects an integer or a variable as argument, got: :a",
      '<<1::size(:a)>>'

    assert_compile_fail CompileError,
      "nofile:1: unit in bitstring expects an integer as argument, got: :x",
      '<<1::unit(:x)>>'
  end

  test "invalid alias" do
    assert_compile_fail CompileError,
      "nofile:1: invalid value for keyword :as, expected a simple alias, got nested alias: Sample.Lists",
      'alias :lists, as: Sample.Lists'

    assert_compile_fail CompileError,
      "nofile:1: invalid argument for alias, expected a compile time atom or alias, got: 1 + 2",
      'alias 1 + 2'

    assert_compile_fail CompileError,
      "nofile:1: invalid value for keyword :as, expected an alias, got: :\"bar.baz\"",
      'alias :lists, as: :"bar.baz"'
  end

  test "invalid alias expansion" do
    assert_compile_fail CompileError,
      ~r"nofile:1: invalid alias: \"foo\.Foo\"",
      'foo = :foo; foo.Foo'
  end

  test "invalid import option" do
    assert_compile_fail CompileError,
      "nofile:1: unsupported option :ops given to import",
      'import :lists, [ops: 1]'
  end

  test "invalid rescue clause" do
    assert_compile_fail CompileError,
      "nofile:4: invalid rescue clause. The clause should match on an alias, a variable or be in the \"var in [alias]\" format",
      'try do\n1\nrescue\n%UndefinedFunctionError{arity: 1} -> false\nend'
  end

  test "invalid for without generators" do
    assert_compile_fail CompileError,
      "nofile:1: for comprehensions must start with a generator",
      'for is_atom(:foo), do: :foo'
  end

  test "invalid for bit generator" do
    assert_compile_fail CompileError,
      "nofile:1: bitstring fields without size are not allowed in bitstring generators",
      'for <<x::binary <- "123">>, do: x'
  end

  test "invalid size in bitstrings" do
    assert_compile_fail CompileError,
      "nofile:1: cannot use ^x outside of match clauses",
      'x = 8; <<a, b::size(^x)>> = <<?a, ?b>>'
  end


  test "unbound cond" do
    assert_compile_fail CompileError,
      "nofile:1: unbound variable _ inside cond. If you want the last clause to always match, " <>
      "you probably meant to use: true ->",
      'cond do _ -> true end'
  end

  test "fun different arities" do
    assert_compile_fail CompileError,
      "nofile:1: cannot mix clauses with different arities in function definition",
      'fn x -> x; x, y -> x + y end'
  end

  test "end of expression" do
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

  test "new line error" do
    assert_compile_fail SyntaxError,
      "nofile:3: unexpectedly reached end of line. The current expression is invalid or incomplete",
      'if true do\n  foo = [],\n  baz\nend'
  end

  # As reported and discussed in
  # https://github.com/elixir-lang/elixir/issues/4419.
  test "characters literal are printed correctly in syntax errors" do
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: ?a",
      ':ok ?a'
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: ?\\s",
      ':ok ?\\s'
    assert_compile_fail SyntaxError,
      "nofile:1: syntax error before: ?す"
      ':ok ?す'
  end

  test "good error message on \"fn do expr end\"" do
    assert_compile_fail SyntaxError,
      "nofile:1: unexpected token \"do\". Anonymous functions are written as:\n\n" <>
        "    fn pattern -> expression end\n\n" <>
        "Syntax error before: do",
      'fn do :ok end'
  end

  test "invalid var or function on guard" do
    assert_compile_fail CompileError,
      "nofile:4: unknown variable something_that_does_not_exist or " <>
      "cannot invoke local something_that_does_not_exist/0 inside guard",
      '''
      defmodule Kernel.ErrorsTest.InvalidVarOrFunctionOnGuard do
        def bar do
          case [] do
            [] when something_that_does_not_exist() == [] -> :ok
          end
        end
      end
      '''
  end

  test "bodyless function with guard" do
    assert_compile_fail CompileError,
      "nofile:2: missing do keyword in def",
      '''
      defmodule Kernel.ErrorsTest.BodyessFunctionWithGuard do
        def foo(n) when is_number(n)
      end
      '''
  end

  test "invalid args for bodyless clause" do
    assert_compile_fail CompileError,
      "nofile:2: can use only variables and \\\\ as arguments in definition header",
      '''
      defmodule Kernel.ErrorsTest.InvalidArgsForBodylessClause do
        def foo(arg // nil)
        def foo(_), do: :ok
      end
      '''
  end

  test "invalid function on match" do
    assert_compile_fail CompileError,
      "nofile:3: cannot invoke local something_that_does_not_exist/1 inside match," <>
      " called as: something_that_does_not_exist(:foo)",
      '''
      defmodule Kernel.ErrorsTest.InvalidFunctionOnMatch do
        def fun do
          case [] do; something_that_does_not_exist(:foo) -> :ok; end
        end
      end
      '''
  end

  test "invalid remote on match" do
    assert_compile_fail CompileError,
      "nofile:1: cannot invoke remote function Hello.something_that_does_not_exist/0 inside match",
      'case [] do; Hello.something_that_does_not_exist() -> :ok; end'
  end

  test "invalid remote on guard" do
    assert_compile_fail CompileError,
      "nofile:1: cannot invoke remote function Hello.something_that_does_not_exist/0 inside guard",
      'case [] do; [] when Hello.something_that_does_not_exist == [] -> :ok; end'
  end

  test "typespec errors" do
    assert_compile_fail CompileError,
      "nofile:2: type foo() undefined",
      '''
      defmodule Kernel.ErrorsTest.TypespecErrors1 do
        @type omg :: foo
      end
      '''

    message = "nofile:2: spec for undefined function omg/0"
    assert_compile_fail CompileError, message,
      '''
      defmodule Kernel.ErrorsTest.TypespecErrors2 do
        @spec omg :: atom
      end
      '''
  end

  test "bad unquoting" do
    assert_compile_fail CompileError,
      "nofile: invalid quoted expression: {:foo, 0, 1}",
      '''
      defmodule Kernel.ErrorsTest.BadUnquoting do
        def range(unquote({:foo, 0, 1})), do: :ok
      end
      '''
  end

  test "bad multi-call" do
    assert_compile_fail CompileError,
      "nofile:1: invalid argument for alias, expected a compile time atom or alias, got: 42",
      'alias IO.{ANSI, 42}'

    assert_compile_fail CompileError,
      "nofile:1: :as option is not supported by multi-alias call",
      'alias Elixir.{Map}, as: Dict'

    assert_compile_fail UndefinedFunctionError,
      "function List.{}/1 is undefined or private",
      '[List.{Chars}, "one"]'
  end

  test "macros error stacktrace" do
    assert [{:erlang, :+, [1, :foo], _},
            {Kernel.ErrorsTest.MacrosErrorStacktrace, :sample, 1, _} | _] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosErrorStacktrace do
        defmacro sample(num), do: num + :foo
        def other, do: sample(1)
      end
      """)
  end

  test "macros function clause stacktrace" do
    assert [{__MODULE__, :sample, 1, _} | _] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosFunctionClauseStacktrace do
        import Kernel.ErrorsTest
        sample(1)
      end
      """)
  end

  test "macros interpreted function clause stacktrace" do
    assert [{Kernel.ErrorsTest.MacrosInterpretedFunctionClauseStacktrace, :sample, 1, _} | _] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosInterpretedFunctionClauseStacktrace do
        defmacro sample(0), do: 0
        def other, do: sample(1)
      end
      """)
  end

  test "macros compiled callback" do
    assert [{Kernel.ErrorsTest, :__before_compile__, [%Macro.Env{module: Kernel.ErrorsTest.MacrosCompiledCallback}], _} | _] =
      rescue_stacktrace("""
      defmodule Kernel.ErrorsTest.MacrosCompiledCallback do
        Module.put_attribute(__MODULE__, :before_compile, Kernel.ErrorsTest)
      end
      """)
  end

  test "failed remote call stacktrace includes file/line info" do
    try do
      bad_remote_call(1)
    rescue
      ArgumentError ->
        stack = System.stacktrace
        assert [{:erlang, :apply, [1, :foo, []], []},
                {__MODULE__, :bad_remote_call, 1, [file: _, line: _]} | _] = stack
    end
  end

  defp bad_remote_call(x), do: x.foo

  defmacro sample(0), do: 0

  defmacro before_compile(_) do
    quote(do: _)
  end

  ## Helpers

  defp rescue_stacktrace(expr) do
    result = try do
      :elixir.eval(to_charlist(expr), [])
      nil
    rescue
      _ -> System.stacktrace
    end

    result || raise(ExUnit.AssertionError, message: "Expected function given to rescue_stacktrace to fail")
  end
end
