Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true

  defmacro hello do
    quote location: :keep do
      def hello, do: :world
    end
  end

  test "no optional arguments in fn" do
    assert_eval_raise CompileError,
                      "nofile:1: anonymous functions cannot have optional arguments",
                      'fn x \\\\ 1 -> x end'

    assert_eval_raise CompileError,
                      "nofile:1: anonymous functions cannot have optional arguments",
                      'fn x, y \\\\ 1 -> x + y end'
  end

  test "invalid fn" do
    assert_eval_raise SyntaxError,
                      "nofile:1: expected anonymous functions to be defined with -> inside: 'fn'",
                      'fn 1 end'

    assert_eval_raise SyntaxError,
                      ~r"nofile:2: unexpected operator ->. If you want to define multiple clauses, ",
                      'fn 1\n2 -> 3 end'
  end

  test "invalid token" do
    assert_eval_raise SyntaxError,
                      "nofile:1: unexpected token: \"\u200B\" (column 7, code point U+200B)",
                      '[foo: \u200B]\noops'
  end

  test "reserved tokens" do
    assert_eval_raise SyntaxError, "nofile:1: reserved token: __aliases__", '__aliases__'
    assert_eval_raise SyntaxError, "nofile:1: reserved token: __block__", '__block__'
  end

  test "invalid __CALLER__" do
    assert_eval_raise CompileError,
                      "nofile:1: __CALLER__ is available only inside defmacro and defmacrop",
                      'defmodule Sample do def hello do __CALLER__ end end'
  end

  test "invalid __STACKTRACE__" do
    assert_eval_raise CompileError,
                      "nofile:1: __STACKTRACE__ is available only inside catch and rescue clauses of try expressions",
                      'defmodule Sample do def hello do __STACKTRACE__ end end'

    assert_eval_raise CompileError,
                      "nofile:1: __STACKTRACE__ is available only inside catch and rescue clauses of try expressions",
                      'defmodule Sample do try do raise "oops" rescue _ -> def hello do __STACKTRACE__ end end end'
  end

  test "invalid quoted token" do
    assert_eval_raise SyntaxError,
                      "nofile:1: syntax error before: \"world\"",
                      '"hello" "world"'

    assert_eval_raise SyntaxError,
                      "nofile:1: syntax error before: 'Foobar'",
                      '1 Foobar'

    assert_eval_raise SyntaxError,
                      "nofile:1: syntax error before: foo",
                      'Foo.:foo'

    assert_eval_raise SyntaxError,
                      "nofile:1: syntax error before: \"foo\"",
                      'Foo.:"foo\#{:bar}"'

    assert_eval_raise SyntaxError,
                      "nofile:1: syntax error before: \"",
                      'Foo.:"\#{:bar}"'
  end

  test "invalid identifier" do
    message = fn name ->
      "nofile:1: invalid character \"@\" (code point U+0040) in identifier: #{name}"
    end

    assert_eval_raise SyntaxError, message.("foo@"), 'foo@'
    assert_eval_raise SyntaxError, message.("foo@"), 'foo@ '
    assert_eval_raise SyntaxError, message.("foo@bar"), 'foo@bar'

    message = fn name ->
      "nofile:1: invalid character \"@\" (code point U+0040) in alias: #{name}"
    end

    assert_eval_raise SyntaxError, message.("Foo@"), 'Foo@'
    assert_eval_raise SyntaxError, message.("Foo@bar"), 'Foo@bar'

    message = "nofile:1: invalid character \"!\" (code point U+0021) in alias: Foo!"
    assert_eval_raise SyntaxError, message, 'Foo!'

    message = "nofile:1: invalid character \"?\" (code point U+003F) in alias: Foo?"
    assert_eval_raise SyntaxError, message, 'Foo?'

    message =
      "nofile:1: invalid character \"ó\" (code point U+00F3) in alias (only ASCII characters are allowed): Foó"

    assert_eval_raise SyntaxError, message, 'Foó'

    message = ~r"""
    Elixir expects unquoted Unicode atoms, variables, and calls to be in NFC form.

    Got:

        "foó" \(code points 0x0066 0x006F 0x006F 0x0301\)

    Expected:

        "foó" \(code points 0x0066 0x006F 0x00F3\)

    """

    assert_eval_raise SyntaxError, message, :unicode.characters_to_nfd_list("foó")
  end

  test "kw missing space" do
    msg = "nofile:1: keyword argument must be followed by space after: foo:"

    assert_eval_raise SyntaxError, msg, "foo:bar"
    assert_eval_raise SyntaxError, msg, "foo:+"
    assert_eval_raise SyntaxError, msg, "foo:+1"
  end

  test "invalid map start" do
    assert_eval_raise SyntaxError,
                      "nofile:1: expected %{ to define a map, got: %[",
                      "{:ok, %[], %{}}"
  end

  test "sigil terminator" do
    assert_eval_raise TokenMissingError,
                      "nofile:3: missing terminator: \" (for sigil ~r\" starting at line 1)",
                      '~r"foo\n\n'

    assert_eval_raise TokenMissingError,
                      "nofile:3: missing terminator: } (for sigil ~r{ starting at line 1)",
                      '~r{foo\n\n'
  end

  test "dot terminator" do
    assert_eval_raise TokenMissingError,
                      "nofile:1: missing terminator: \" (for function name starting at line 1)",
                      'foo."bar'
  end

  test "string terminator" do
    assert_eval_raise TokenMissingError,
                      "nofile:1: missing terminator: \" (for string starting at line 1)",
                      '"bar'
  end

  test "heredoc start" do
    assert_eval_raise SyntaxError,
                      "nofile:1: heredoc start must be followed by a new line after \"\"\"",
                      '"""bar\n"""'
  end

  test "heredoc terminator" do
    assert_eval_raise TokenMissingError,
                      "nofile:2: missing terminator: \"\"\" (for heredoc starting at line 1)",
                      '"""\nbar'

    assert_eval_raise SyntaxError,
                      "nofile:2: invalid location for heredoc terminator, please escape token or move it to its own line: \"\"\"",
                      '"""\nbar"""'
  end

  test "unexpected end" do
    assert_eval_raise SyntaxError, "nofile:1: unexpected token: end", '1 end'

    assert_eval_raise SyntaxError,
                      ~r" HINT: it looks like the \"end\" on line 2 does not have a matching \"do\" defined before it",
                      '''
                      defmodule MyApp do
                        def one end
                        def two do end
                      end
                      '''

    assert_eval_raise SyntaxError,
                      ~r" HINT: it looks like the \"end\" on line 3 does not have a matching \"do\" defined before it",
                      '''
                      defmodule MyApp do
                        def one
                        end

                        def two do
                        end
                      end
                      '''

    assert_eval_raise SyntaxError,
                      ~r" HINT: it looks like the \"end\" on line 6 does not have a matching \"do\" defined before it",
                      '''
                      defmodule MyApp do
                        def one do
                        end

                        def two
                        end
                      end
                      '''
  end

  test "missing end" do
    assert_eval_raise TokenMissingError,
                      "nofile:1: missing terminator: end (for \"do\" starting at line 1)",
                      'foo do 1'

    assert_eval_raise TokenMissingError,
                      ~r"HINT: it looks like the \"do\" on line 2 does not have a matching \"end\"",
                      '''
                      defmodule MyApp do
                        def one do
                        # end

                        def two do
                        end
                      end
                      '''

    assert_eval_raise SyntaxError,
                      ~r"HINT: it looks like the \"do\" on line 3 does not have a matching \"end\"",
                      '''
                      defmodule MyApp do
                        (
                          def one do
                          # end

                          def two do
                          end
                        )
                      end
                      '''
  end

  test "syntax error" do
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: '.'", '+.foo'
  end

  test "syntax error before sigil" do
    msg = fn x -> "nofile:1: syntax error before: sigil ~s starting with content '#{x}'" end

    assert_eval_raise SyntaxError, msg.("bar baz"), '~s(foo) ~s(bar baz)'
    assert_eval_raise SyntaxError, msg.(""), '~s(foo) ~s()'
    assert_eval_raise SyntaxError, msg.("bar "), '~s(foo) ~s(bar \#{:baz})'
    assert_eval_raise SyntaxError, msg.(""), '~s(foo) ~s(\#{:bar} baz)'
  end

  test "op ambiguity" do
    max = 1
    assert max == 1
    assert max(1, 2) == 2
  end

  test "syntax error with do" do
    assert_eval_raise SyntaxError, ~r/nofile:1: unexpected token: do./, 'if true, do\n'

    assert_eval_raise SyntaxError, ~r/nofile:1: unexpected keyword: do:./, 'if true do:\n'
  end

  test "syntax error on parens call" do
    msg =
      "nofile:1: unexpected parentheses. If you are making a function call, do not " <>
        "insert spaces between the function name and the opening parentheses. " <>
        "Syntax error before: '('"

    assert_eval_raise SyntaxError, msg, 'foo (hello, world)'
  end

  test "syntax error on nested no parens call" do
    msg = ~r"nofile:1: unexpected comma. Parentheses are required to solve ambiguity"

    assert_eval_raise SyntaxError, msg, '[foo 1, 2]'
    assert_eval_raise SyntaxError, msg, '[foo bar 1, 2]'
    assert_eval_raise SyntaxError, msg, '[do: foo 1, 2]'
    assert_eval_raise SyntaxError, msg, 'foo(do: bar 1, 2)'
    assert_eval_raise SyntaxError, msg, '{foo 1, 2}'
    assert_eval_raise SyntaxError, msg, '{foo bar 1, 2}'
    assert_eval_raise SyntaxError, msg, 'foo 1, foo 2, 3'
    assert_eval_raise SyntaxError, msg, 'foo 1, @bar 3, 4'
    assert_eval_raise SyntaxError, msg, 'foo 1, 2 + bar 3, 4'
    assert_eval_raise SyntaxError, msg, 'foo(1, foo 2, 3)'

    assert is_list(List.flatten([1]))
    assert is_list(Enum.reverse([3, 2, 1], [4, 5, 6]))
    assert is_list(Enum.reverse([3, 2, 1], [4, 5, 6]))
    assert false || is_list(Enum.reverse([3, 2, 1], [4, 5, 6]))
    assert [List.flatten(List.flatten([1]))] == [[1]]

    interpret = fn x -> Macro.to_string(Code.string_to_quoted!(x)) end
    assert interpret.("f 1 + g h 2, 3") == "f(1 + g(h(2, 3)))"

    assert interpret.("assert [] = TestRepo.all from p in Post, where: p.title in ^[]") ==
             "assert([] = TestRepo.all(from(p in Post, where: p.title() in ^[])))"
  end

  test "syntax error on atom dot alias" do
    msg =
      "nofile:1: atom cannot be followed by an alias. If the '.' was meant to be " <>
        "part of the atom's name, the atom name must be quoted. Syntax error before: '.'"

    assert_eval_raise SyntaxError, msg, ':foo.Bar'
    assert_eval_raise SyntaxError, msg, ':"+".Bar'
  end

  test "syntax error with no token" do
    assert_eval_raise TokenMissingError,
                      "nofile:1: missing terminator: ) (for \"(\" starting at line 1)",
                      'case 1 ('
  end

  test "clause with defaults" do
    message = ~r"nofile:3: def hello/1 defines defaults multiple times"

    assert_eval_raise CompileError,
                      message,
                      ~C'''
                      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
                        def hello(_arg \\ 0)
                        def hello(_arg \\ 1)
                      end
                      '''

    assert_eval_raise CompileError,
                      message,
                      ~C'''
                      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
                        def hello(_arg \\ 0), do: nil
                        def hello(_arg \\ 1), do: nil
                      end
                      '''

    assert_eval_raise CompileError,
                      message,
                      ~C'''
                      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
                        def hello(_arg \\ 0)
                        def hello(_arg \\ 1), do: nil
                      end
                      '''

    assert_eval_raise CompileError,
                      message,
                      ~C'''
                      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
                        def hello(_arg \\ 0), do: nil
                        def hello(_arg \\ 1)
                      end
                      '''

    assert_eval_raise CompileError, ~r"nofile:4: undefined function foo/0", ~C'''
    defmodule Kernel.ErrorsTest.ClauseWithDefaults5 do
      def hello(
            foo,
            bar \\ foo()
          )

      def hello(foo, bar), do: foo + bar
    end
    '''
  end

  test "different defs with defaults" do
    assert_eval_raise CompileError, "nofile:3: def hello/3 defaults conflicts with hello/2", ~C'''
    defmodule Kernel.ErrorsTest.DifferentDefsWithDefaults1 do
      def hello(a, b \\ nil), do: a + b
      def hello(a, b \\ nil, c \\ nil), do: a + b + c
    end
    '''

    assert_eval_raise CompileError,
                      "nofile:3: def hello/2 conflicts with defaults from hello/3",
                      ~C'''
                      defmodule Kernel.ErrorsTest.DifferentDefsWithDefaults2 do
                        def hello(a, b \\ nil, c \\ nil), do: a + b + c
                        def hello(a, b \\ nil), do: a + b
                      end
                      '''
  end

  test "bad form" do
    assert_eval_raise CompileError, "nofile:3: undefined function bar/0", '''
    defmodule Kernel.ErrorsTest.BadForm do
      def foo do
        bar()
      end
    end
    '''

    assert_eval_raise CompileError, "nofile:8: undefined function baz/0", '''
    defmodule Sample do
      def foo do
        bar()
      end

      defoverridable [foo: 0]
      def foo do
        baz()
      end
    end
    '''
  end

  test "literal on map and struct" do
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: '}'", '%{:a}'
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: '}'", '%{{:a, :b}}'
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: '{'", '%{a, b}{a: :b}'

    assert_eval_raise CompileError,
                      "nofile:1: expected key-value pairs in a map, got: put_in(foo.bar().baz(), nil)",
                      'foo = 1; %{put_in(foo.bar.baz, nil), foo}'
  end

  test "struct fields on defstruct" do
    assert_eval_raise ArgumentError, "struct field names must be atoms, got: 1", '''
    defmodule Kernel.ErrorsTest.StructFieldsOnDefstruct do
      defstruct [1, 2, 3]
    end
    '''
  end

  test "struct access on body" do
    assert_eval_raise CompileError,
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

  test "struct errors" do
    assert_eval_raise CompileError,
                      "nofile:1: BadStruct.__struct__/1 is undefined, cannot expand struct BadStruct",
                      '%BadStruct{}'

    assert_eval_raise CompileError,
                      "nofile:1: BadStruct.__struct__/0 is undefined, cannot expand struct BadStruct",
                      '%BadStruct{} = %{}'

    bad_struct_type_error =
      ~r"expected Kernel.ErrorsTest.BadStructType.__struct__/(0|1) to return a map.*, got: :invalid"

    defmodule BadStructType do
      def __struct__, do: :invalid
      def __struct__(_), do: :invalid

      assert_raise CompileError, bad_struct_type_error, fn ->
        Macro.struct!(__MODULE__, __ENV__)
      end
    end

    assert_eval_raise CompileError,
                      bad_struct_type_error,
                      '%#{BadStructType}{} = %{}'

    assert_eval_raise CompileError,
                      bad_struct_type_error,
                      '%#{BadStructType}{}'

    assert_raise ArgumentError, bad_struct_type_error, fn ->
      struct(BadStructType)
    end

    assert_raise ArgumentError, bad_struct_type_error, fn ->
      struct(BadStructType, foo: 1)
    end

    missing_struct_key_error =
      ~r"expected Kernel.ErrorsTest.MissingStructKey.__struct__/(0|1) to return a map.*, got: %\{\}"

    defmodule MissingStructKey do
      def __struct__, do: %{}
      def __struct__(_), do: %{}

      assert_raise CompileError, missing_struct_key_error, fn ->
        Macro.struct!(__MODULE__, __ENV__)
      end
    end

    assert_eval_raise CompileError,
                      missing_struct_key_error,
                      '%#{MissingStructKey}{} = %{}'

    assert_eval_raise CompileError,
                      missing_struct_key_error,
                      '%#{MissingStructKey}{}'

    assert_raise ArgumentError, missing_struct_key_error, fn ->
      struct(MissingStructKey)
    end

    assert_raise ArgumentError, missing_struct_key_error, fn ->
      struct(MissingStructKey, foo: 1)
    end

    invalid_struct_key_error =
      ~r"expected Kernel.ErrorsTest.InvalidStructKey.__struct__/(0|1) to return a map.*, got: %\{__struct__: 1\}"

    defmodule InvalidStructKey do
      def __struct__, do: %{__struct__: 1}
      def __struct__(_), do: %{__struct__: 1}

      assert_raise CompileError, invalid_struct_key_error, fn ->
        Macro.struct!(__MODULE__, __ENV__)
      end
    end

    assert_eval_raise CompileError,
                      invalid_struct_key_error,
                      '%#{InvalidStructKey}{} = %{}'

    assert_eval_raise CompileError,
                      invalid_struct_key_error,
                      '%#{InvalidStructKey}{}'

    assert_raise ArgumentError, invalid_struct_key_error, fn ->
      struct(InvalidStructKey)
    end

    assert_raise ArgumentError, invalid_struct_key_error, fn ->
      struct(InvalidStructKey, foo: 1)
    end

    invalid_struct_name_error =
      ~r"expected struct name returned by Kernel.ErrorsTest.InvalidStructName.__struct__/(0|1) to be Kernel.ErrorsTest.InvalidStructName, got: InvalidName"

    defmodule InvalidStructName do
      def __struct__, do: %{__struct__: InvalidName}
      def __struct__(_), do: %{__struct__: InvalidName}

      assert_raise CompileError, invalid_struct_name_error, fn ->
        Macro.struct!(__MODULE__, __ENV__)
      end
    end

    assert_eval_raise CompileError,
                      invalid_struct_name_error,
                      '%#{InvalidStructName}{} = %{}'

    assert_eval_raise CompileError,
                      invalid_struct_name_error,
                      '%#{InvalidStructName}{}'

    assert_raise ArgumentError, invalid_struct_name_error, fn ->
      struct(InvalidStructName)
    end

    assert_raise ArgumentError, invalid_struct_name_error, fn ->
      struct(InvalidStructName, foo: 1)
    end

    defmodule GoodStruct do
      defstruct name: "john"
    end

    assert_eval_raise KeyError,
                      "key :age not found",
                      '%#{GoodStruct}{age: 27}'

    assert_eval_raise CompileError,
                      "nofile:1: unknown key :age for struct Kernel.ErrorsTest.GoodStruct",
                      '%#{GoodStruct}{age: 27} = %{}'
  end

  test "name for defmodule" do
    assert_eval_raise CompileError, "nofile:1: invalid module name: 3", 'defmodule 1 + 2, do: 3'
  end

  test "@compile inline with undefined function" do
    assert_eval_raise CompileError,
                      "nofile:1: inlined function foo/1 undefined",
                      'defmodule Test do @compile {:inline, foo: 1} end'
  end

  test "invalid unquote" do
    assert_eval_raise CompileError, "nofile:1: unquote called outside quote", 'unquote 1'
  end

  test "invalid unquote splicing in oneliners" do
    assert_eval_raise ArgumentError,
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

  test "undefined non-local function" do
    assert_eval_raise CompileError, "nofile:1: undefined function call/2", 'call foo, do: :foo'
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

  test "typespec attributes set via Module.put_attribute/4" do
    message =
      "attributes type, typep, opaque, spec, callback, and macrocallback " <>
        "must be set directly via the @ notation"

    for kind <- [:type, :typep, :opaque, :spec, :callback, :macrocallback] do
      assert_eval_raise ArgumentError,
                        message,
                        """
                        defmodule PutTypespecAttribute do
                          Module.put_attribute(__MODULE__, #{inspect(kind)}, {})
                        end
                        """
    end
  end

  test "invalid struct field value" do
    msg = ~r"invalid value for struct field baz, cannot escape "

    assert_raise ArgumentError, msg, fn ->
      defmodule InvalidStructFieldValue do
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
    assert_eval_raise TokenMissingError,
                      "nofile:1: missing terminator: end (for \"fn\" starting at line 1)",
                      'fn 1'
  end

  test "invalid escape" do
    assert_eval_raise TokenMissingError, "nofile:1: invalid escape \\ at end of file", '1 \\'
  end

  test "function local conflict" do
    assert_eval_raise CompileError,
                      "nofile:3: imported Kernel.&&/2 conflicts with local function",
                      '''
                      defmodule Kernel.ErrorsTest.FunctionLocalConflict do
                        def other, do: 1 && 2
                        def _ && _, do: :error
                      end
                      '''
  end

  test "macro local conflict" do
    assert_eval_raise CompileError,
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
    assert_eval_raise UndefinedFunctionError,
                      "function Kernel.ErrorsTest.MacroWithUndefinedLocal.unknown/1" <>
                        " is undefined (function not available)",
                      '''
                      defmodule Kernel.ErrorsTest.MacroWithUndefinedLocal do
                        defmacrop bar, do: unknown(1)
                        def baz, do: bar()
                      end
                      '''
  end

  test "private macro" do
    assert_eval_raise UndefinedFunctionError,
                      "function Kernel.ErrorsTest.PrivateMacro.foo/0 is undefined (function not available)",
                      '''
                      defmodule Kernel.ErrorsTest.PrivateMacro do
                        defmacrop foo, do: 1
                        defmacro bar, do: __MODULE__.foo
                        defmacro baz, do: bar()
                      end
                      '''
  end

  test "macro invoked before its definition" do
    assert_eval_raise CompileError,
                      "nofile:2: cannot invoke macro bar/0 before its definition",
                      '''
                      defmodule Kernel.ErrorsTest.IncorrectMacroDispatch do
                        def foo, do: bar()
                        defmacro bar, do: :bar
                      end
                      '''

    assert_eval_raise CompileError,
                      "nofile:2: cannot invoke macro bar/0 before its definition",
                      '''
                      defmodule Kernel.ErrorsTest.IncorrectMacropDispatch do
                        def foo, do: bar()
                        defmacrop bar, do: :ok
                      end
                      '''
  end

  test "macro captured before its definition" do
    assert_eval_raise CompileError,
                      "nofile:3: cannot invoke macro is_ok/1 before its definition",
                      '''
                      defmodule Kernel.ErrorsTest.IncorrectMacroDispatch.Capture do
                        def foo do
                          predicate = &is_ok/1
                          Enum.any?([:ok, :error, :foo], predicate)
                        end

                        defmacro is_ok(atom), do: atom == :ok
                      end
                      '''
  end

  test "function definition with alias" do
    assert_eval_raise CompileError,
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
    assert_eval_raise CompileError,
                      "nofile:3: function exit/1 imported from both :erlang and Kernel, call is ambiguous",
                      '''
                      defmodule Kernel.ErrorsTest.FunctionImportConflict do
                        import :erlang, warn: false
                        def foo, do: exit(:test)
                      end
                      '''
  end

  test "duplicated function on import options" do
    assert_eval_raise CompileError,
                      "nofile:2: invalid :only option for import, flatten/1 is duplicated",
                      '''
                      defmodule Kernel.ErrorsTest.DuplicatedFunctionOnImportOnly do
                        import List, only: [flatten: 1, keyfind: 4, flatten: 1]
                      end
                      '''

    assert_eval_raise CompileError,
                      "nofile:2: invalid :except option for import, flatten/1 is duplicated",
                      '''
                      defmodule Kernel.ErrorsTest.DuplicatedFunctionOnImportExcept do
                        import List, except: [flatten: 1, keyfind: 4, flatten: 1]
                      end
                      '''
  end

  test "unrequired macro" do
    assert_eval_raise CompileError,
                      "nofile:2: you must require Kernel.ErrorsTest before invoking " <>
                        "the macro Kernel.ErrorsTest.hello/0",
                      '''
                      defmodule Kernel.ErrorsTest.UnrequiredMacro do
                        Kernel.ErrorsTest.hello()
                      end
                      '''
  end

  test "def defmacro clause change" do
    assert_eval_raise CompileError, "nofile:3: defmacro foo/1 already defined as def", '''
    defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
      def foo(1), do: 1
      defmacro foo(x), do: x
    end
    '''
  end

  test "def defp clause change from another file" do
    assert_eval_raise CompileError, ~r"nofile:4: def hello/0 already defined as defp", '''
    defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
      require Kernel.ErrorsTest
      defp hello, do: :world
      Kernel.ErrorsTest.hello()
    end
    '''
  end

  test "internal function overridden" do
    assert_eval_raise CompileError,
                      "nofile:2: cannot define def __info__/1 as it is automatically defined by Elixir",
                      '''
                      defmodule Kernel.ErrorsTest.InternalFunctionOverridden do
                        def __info__(_), do: []
                      end
                      '''
  end

  test "no macros" do
    assert_eval_raise CompileError, "nofile:2: could not load macros from module :lists", '''
    defmodule Kernel.ErrorsTest.NoMacros do
      import :lists, only: :macros
    end
    '''
  end

  test "invalid macro" do
    assert_eval_raise CompileError,
                      ~r"nofile: invalid quoted expression: {:foo, :bar, :baz, :bat}",
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
    assert_eval_raise CompileError,
                      "nofile:1: module Certainly.Doesnt.Exist is not loaded and could not be found",
                      'import Certainly.Doesnt.Exist'
  end

  test "module imported from the context it was defined in" do
    assert_eval_raise CompileError,
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
    assert_eval_raise CompileError,
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
    assert_eval_raise ArgumentError,
                      "could not call Module.eval_quoted/4 because the module Record is already compiled",
                      'Module.eval_quoted Record, quote(do: 1), [], file: __ENV__.file'
  end

  test "@on_load attribute format" do
    assert_raise ArgumentError, ~r/should be an atom or a {atom, 0} tuple/, fn ->
      defmodule BadOnLoadAttribute do
        Module.put_attribute(__MODULE__, :on_load, "not an atom")
      end
    end
  end

  test "duplicated @on_load attribute" do
    assert_raise ArgumentError, "the @on_load attribute can only be set once per module", fn ->
      defmodule DuplicatedOnLoadAttribute do
        @on_load :foo
        @on_load :bar
      end
    end
  end

  test "@on_load attribute with undefined function" do
    assert_eval_raise CompileError,
                      "nofile:1: @on_load function foo/0 is undefined",
                      'defmodule UndefinedOnLoadFunction do @on_load :foo end'
  end

  test "wrong kind for @on_load attribute" do
    assert_eval_raise CompileError,
                      "nofile:1: expected @on_load function foo/0 to be defined as \"def\", " <>
                        "got \"defp\"",
                      '''
                      defmodule PrivateOnLoadFunction do
                        @on_load :foo


                        defp foo do
                          :ok
                        end

                        # To avoid warning: function foo/0 is unused
                        def bar do
                          foo()
                        end
                      end
                      '''
  end

  test "interpolation error" do
    assert_eval_raise SyntaxError,
                      "nofile:1: unexpected token: ). The \"do\" at line 1 is missing terminator \"end\"",
                      '"foo\#{case 1 do )}bar"'
  end

  test "in definition module" do
    assert_eval_raise CompileError,
                      "nofile:2: cannot define module Kernel.ErrorsTest.InDefinitionModule " <>
                        "because it is currently being defined in nofile:1",
                      '''
                      defmodule Kernel.ErrorsTest.InDefinitionModule do
                        defmodule Elixir.Kernel.ErrorsTest.InDefinitionModule, do: true
                      end
                      '''
  end

  test "invalid definition" do
    assert_eval_raise CompileError,
                      "nofile:1: invalid syntax in def 1.(hello)",
                      'defmodule Kernel.ErrorsTest.InvalidDefinition, do: (def 1.(hello), do: true)'
  end

  test "invalid size in bitstrings" do
    assert_eval_raise CompileError,
                      "nofile:1: cannot use ^x outside of match clauses",
                      'x = 8; <<a, b::size(^x)>> = <<?a, ?b>>'
  end

  test "end of expression" do
    # All valid examples
    Code.eval_quoted('''
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
    ''')

    # All invalid examples
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: ';'", '1+;\n2'

    assert_eval_raise SyntaxError, "nofile:1: syntax error before: ';'", 'max(1, ;2)'
  end

  test "new line error" do
    assert_eval_raise SyntaxError,
                      "nofile:3: unexpectedly reached end of line. The current expression is invalid or incomplete",
                      'if true do\n  foo = [],\n  baz\nend'
  end

  test "characters literal are printed correctly in syntax errors" do
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: ?a", ':ok ?a'
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: ?\\s", ':ok ?\\s'
    assert_eval_raise SyntaxError, "nofile:1: syntax error before: ?す", ':ok ?す'
  end

  test "invalid \"fn do expr end\"" do
    assert_eval_raise SyntaxError,
                      "nofile:1: unexpected token: do. Anonymous functions are written as:\n\n    fn pattern -> expression end",
                      'fn do :ok end'
  end

  test "bodyless function with guard" do
    assert_eval_raise CompileError, "nofile:2: missing :do option in \"def\"", '''
    defmodule Kernel.ErrorsTest.BodyessFunctionWithGuard do
      def foo(n) when is_number(n)
    end
    '''

    assert_eval_raise CompileError, "nofile:2: missing :do option in \"def\"", '''
    defmodule Kernel.ErrorsTest.BodyessFunctionWithGuard do
      def foo(n) when is_number(n), true
    end
    '''
  end

  test "invalid args for function head" do
    assert_eval_raise CompileError,
                      ~r"nofile:2: only variables and \\\\ are allowed as arguments in function head.",
                      '''
                      defmodule Kernel.ErrorsTest.InvalidArgsForBodylessClause do
                        def foo(nil)
                        def foo(_), do: :ok
                      end
                      '''
  end

  test "bad multi-call" do
    assert_eval_raise CompileError,
                      "nofile:1: invalid argument for alias, expected a compile time atom or alias, got: 42",
                      'alias IO.{ANSI, 42}'

    assert_eval_raise CompileError,
                      "nofile:1: :as option is not supported by multi-alias call",
                      'alias Elixir.{Map}, as: Dict'

    assert_eval_raise UndefinedFunctionError,
                      "function List.\"{}\"/1 is undefined or private",
                      '[List.{Chars}, "one"]'
  end

  test "macros error stacktrace" do
    assert [
             {:erlang, :+, [1, :foo], _},
             {Kernel.ErrorsTest.MacrosErrorStacktrace, :sample, 1, _} | _
           ] =
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
    assert [{Kernel.ErrorsTest, :__before_compile__, [env], _} | _] =
             rescue_stacktrace("""
             defmodule Kernel.ErrorsTest.MacrosCompiledCallback do
               Module.put_attribute(__MODULE__, :before_compile, Kernel.ErrorsTest)
             end
             """)

    assert %Macro.Env{module: Kernel.ErrorsTest.MacrosCompiledCallback} = env
  end

  test "failed remote call stacktrace includes file/line info" do
    try do
      bad_remote_call(1)
    rescue
      ArgumentError ->
        assert [
                 {:erlang, :apply, [1, :foo, []], []},
                 {__MODULE__, :bad_remote_call, 1, [file: _, line: _]} | _
               ] = __STACKTRACE__
    end
  end

  test "def fails when rescue, else or catch don't have clauses" do
    assert_eval_raise CompileError, ~r"expected -> clauses for :rescue in \"def\"", """
    defmodule Example do
      def foo do
        bar()
      rescue
        baz()
      end
    end
    """
  end

  defp bad_remote_call(x), do: x.foo

  defmacro sample(0), do: 0

  defmacro before_compile(_) do
    quote(do: _)
  end

  ## Helpers

  defp assert_eval_raise(given_exception, given_message, string) do
    assert_raise given_exception, given_message, fn ->
      Code.eval_string(string)
    end
  end

  defp rescue_stacktrace(string) do
    try do
      Code.eval_string(string)
      nil
    rescue
      _ -> __STACKTRACE__
    else
      _ -> flunk("Expected expression to fail")
    end
  end
end
