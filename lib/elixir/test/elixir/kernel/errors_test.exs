Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true

  defmacro hello do
    quote location: :keep do
      def hello, do: :world
    end
  end

  test "no default arguments in fn" do
    assert_compile_error(
      ["nofile:1:1", "anonymous functions cannot have optional arguments"],
      ~c"fn x \\\\ 1 -> x end"
    )

    assert_compile_error(
      ["nofile:1:1", "anonymous functions cannot have optional arguments"],
      ~c"fn x, y \\\\ 1 -> x + y end"
    )
  end

  test "invalid __CALLER__" do
    assert_compile_error(
      ["nofile:1: ", "__CALLER__ is available only inside defmacro and defmacrop"],
      ~c"defmodule Sample do def hello do __CALLER__ end end"
    )
  end

  test "invalid __STACKTRACE__" do
    assert_compile_error(
      [
        "nofile:1: ",
        "__STACKTRACE__ is available only inside catch and rescue clauses of try expressions"
      ],
      ~c"defmodule Sample do def hello do __STACKTRACE__ end end"
    )

    assert_compile_error(
      [
        "nofile:1: ",
        "__STACKTRACE__ is available only inside catch and rescue clauses of try expressions"
      ],
      ~c"defmodule Sample do try do raise \"oops\" rescue _ -> def hello do __STACKTRACE__ end end end"
    )
  end

  test "undefined function" do
    assert_compile_error(
      [
        "hello.ex:4: ",
        "undefined function bar/0 (expected Kernel.ErrorsTest.BadForm to define such a function or for it to be imported, but none are available)"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.BadForm do
        @file "hello.ex"
        def foo do
          bar()
        end
      end
      """
    )

    assert_compile_error(
      [
        "nofile:2: ",
        "undefined function module_info/0 (this function is auto-generated by the compiler and must always be called as a remote, as in __MODULE__.module_info/0)"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.Info do
        def foo, do: module_info()
      end
      """
    )

    assert_compile_error(
      [
        "nofile:3: ",
        "undefined function behaviour_info/1 (this function is auto-generated by the compiler and must always be called as a remote, as in __MODULE__.behaviour_info/1)"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.BehaviourInfo do
        @callback dummy() :: :ok
        def foo, do: behaviour_info(:callbacks)
      end
      """
    )

    assert_compile_error(
      [
        "nofile:3: ",
        "undefined function bar/1 (expected Kernel.ErrorsTest.BadForm to define such a function or for it to be imported, but none are available)"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.BadForm do
        def foo do
          bar(
            baz(1, 2)
          )
        end
      end
      """
    )

    assert_compile_error(
      [
        "nofile:8: ",
        "undefined function baz/0 (expected Sample to define such a function or for it to be imported, but none are available)"
      ],
      ~c"""
      defmodule Sample do
        def foo do
          bar()
        end

        defoverridable [foo: 0]
        def foo do
          baz()
        end
      end
      """
    )
  end

  test "undefined non-local function" do
    assert_compile_error(
      ["nofile:1:1", "undefined function call/2 (there is no such import)"],
      ~c"call foo, do: :foo"
    )
  end

  test "undefined variables" do
    assert_compile_error(
      ["nofile:3: ", "undefined variable \"bar\"", "nofile:4: ", "undefined variable \"baz\""],
      ~c"""
      defmodule Sample do
        def foo do
          IO.puts bar
          IO.puts baz
        end
      end
      """
    )
  end

  test "function without definition" do
    assert_compile_error(
      ["nofile:2: ", "implementation not provided for predefined def foo/0"],
      ~c"""
      defmodule Kernel.ErrorsTest.FunctionWithoutDefition do
        def foo
      end
      """
    )

    assert_compile_error(
      ["nofile:10: ", "implementation not provided for predefined def example/2"],
      ~c"""
      defmodule Kernel.ErrorsTest.FunctionTemplate do
        defmacro __using__(_) do
          quote do
            def example(foo, bar \\\\ [])
          end
        end
      end

      defmodule Kernel.ErrorsTest.UseFunctionTemplate do
        use Kernel.ErrorsTest.FunctionTemplate
      end
      """
    )
  end

  test "guard without definition" do
    assert_compile_error(
      ["nofile:2: ", "implementation not provided for predefined defmacro foo/1"],
      ~c"""
      defmodule Kernel.ErrorsTest.GuardWithoutDefition do
        defguard foo(bar)
      end
      """
    )
  end

  test "literal on map and struct" do
    assert_compile_error(
      ["nofile:1:11", "expected key-value pairs in a map, got: put_in(foo.bar.baz, nil)"],
      ~c"foo = 1; %{put_in(foo.bar.baz, nil), foo}"
    )
  end

  test "struct fields on defstruct" do
    assert_eval_raise ArgumentError, "struct field names must be atoms, got: 1", ~c"""
    defmodule Kernel.ErrorsTest.StructFieldsOnDefstruct do
      defstruct [1, 2, 3]
    end
    """
  end

  test "struct access on body" do
    assert_compile_error(
      [
        "nofile:3: ",
        "cannot access struct Kernel.ErrorsTest.StructAccessOnBody, " <>
          "the struct was not yet defined or the struct " <>
          "is being accessed in the same context that defines it"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.StructAccessOnBody do
        defstruct %{name: "Brasilia"}
        %Kernel.ErrorsTest.StructAccessOnBody{}
      end
      """
    )
  end

  describe "struct errors" do
    test "bad errors" do
      assert_compile_error(
        ["nofile:1:1", "BadStruct.__struct__/1 is undefined, cannot expand struct BadStruct"],
        ~c"%BadStruct{}"
      )

      assert_compile_error(
        ["nofile:1:1", "BadStruct.__struct__/0 is undefined, cannot expand struct BadStruct"],
        ~c"%BadStruct{} = %{}"
      )

      bad_struct_type_error =
        ~r"expected Kernel.ErrorsTest.BadStructType.__struct__/(0|1) to return a map.*, got: :invalid"

      defmodule BadStructType do
        def __struct__, do: :invalid
        def __struct__(_), do: :invalid

        assert_raise ArgumentError, bad_struct_type_error, fn ->
          Macro.struct!(__MODULE__, __ENV__)
        end
      end

      assert_compile_error(
        bad_struct_type_error,
        ~c"%#{BadStructType}{} = %{}"
      )

      assert_compile_error(
        bad_struct_type_error,
        ~c"%#{BadStructType}{}"
      )

      assert_raise ArgumentError, bad_struct_type_error, fn ->
        struct(BadStructType)
      end

      assert_raise ArgumentError, bad_struct_type_error, fn ->
        struct(BadStructType, foo: 1)
      end
    end

    test "bad struct on module conflict" do
      Code.put_compiler_option(:ignore_module_conflict, true)

      assert_compile_error(~r'MissingStructOnReload\.__struct__/1 is undefined', ~c'''
      defmodule MissingStructOnReload do
        defstruct [:title]
        def d(), do: %MissingStructOnReload{}
      end

      defmodule MissingStructOnReload do
        def d(), do: %MissingStructOnReload{}
      end
      ''')
    after
      Code.put_compiler_option(:ignore_module_conflict, false)
    end

    test "missing struct key" do
      missing_struct_key_error =
        ~r"expected Kernel.ErrorsTest.MissingStructKey.__struct__/(0|1) to return a map.*, got: %\{\}"

      defmodule MissingStructKey do
        def __struct__, do: %{}
        def __struct__(_), do: %{}

        assert_raise ArgumentError, missing_struct_key_error, fn ->
          Macro.struct!(__MODULE__, __ENV__)
        end
      end

      assert_compile_error(
        missing_struct_key_error,
        ~c"%#{MissingStructKey}{} = %{}"
      )

      assert_compile_error(
        missing_struct_key_error,
        ~c"%#{MissingStructKey}{}"
      )

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

        assert_raise ArgumentError, invalid_struct_key_error, fn ->
          Macro.struct!(__MODULE__, __ENV__)
        end
      end

      assert_compile_error(
        invalid_struct_key_error,
        ~c"%#{InvalidStructKey}{} = %{}"
      )

      assert_compile_error(
        invalid_struct_key_error,
        ~c"%#{InvalidStructKey}{}"
      )

      assert_raise ArgumentError, invalid_struct_key_error, fn ->
        struct(InvalidStructKey)
      end

      assert_raise ArgumentError, invalid_struct_key_error, fn ->
        struct(InvalidStructKey, foo: 1)
      end
    end

    test "invalid struct" do
      invalid_struct_name_error =
        ~r"expected struct name returned by Kernel.ErrorsTest.InvalidStructName.__struct__/(0|1) to be Kernel.ErrorsTest.InvalidStructName, got: InvalidName"

      defmodule InvalidStructName do
        def __struct__, do: %{__struct__: InvalidName}
        def __struct__(_), do: %{__struct__: InvalidName}

        assert_raise ArgumentError, invalid_struct_name_error, fn ->
          Macro.struct!(__MODULE__, __ENV__)
        end
      end

      assert_compile_error(
        invalid_struct_name_error,
        ~c"%#{InvalidStructName}{} = %{}"
      )

      assert_compile_error(
        invalid_struct_name_error,
        ~c"%#{InvalidStructName}{}"
      )

      assert_raise ArgumentError, invalid_struct_name_error, fn ->
        struct(InvalidStructName)
      end

      assert_raise ArgumentError, invalid_struct_name_error, fn ->
        struct(InvalidStructName, foo: 1)
      end
    end

    test "good struct" do
      defmodule GoodStruct do
        defstruct name: "john"
      end

      assert_eval_raise KeyError,
                        "key :age not found",
                        ~c"%#{GoodStruct}{age: 27}"

      assert_compile_error(
        ["nofile:1:1", "unknown key :age for struct Kernel.ErrorsTest.GoodStruct"],
        ~c"%#{GoodStruct}{age: 27} = %{}"
      )
    end

    test "enforce @enforce_keys" do
      defmodule EnforceKeys do
        @enforce_keys [:foo]
        defstruct(foo: nil)
      end

      assert_raise ArgumentError,
                   "@enforce_keys required keys ([:fo, :bar]) that are not defined in defstruct: [foo: nil]",
                   fn ->
                     defmodule EnforceKeysError do
                       @enforce_keys [:foo, :fo, :bar]
                       defstruct(foo: nil)
                     end
                   end
    end
  end

  test "invalid unquote" do
    assert_compile_error(["nofile:1:1", "unquote called outside quote"], ~c"unquote 1")
  end

  test "invalid unquote splicing in one-liners" do
    assert_eval_raise ArgumentError,
                      "unquote_splicing only works inside arguments and block contexts, " <>
                        "wrap it in parens if you want it to work with one-liners",
                      ~c"""
                      defmodule Kernel.ErrorsTest.InvalidUnquoteSplicingInOneliners do
                        defmacro oneliner2 do
                          quote do: unquote_splicing 1
                        end

                        def callme do
                          oneliner2
                        end
                      end
                      """
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

  test "invalid case clauses" do
    assert_compile_error(
      ["nofile:1:1", "expected one argument for :do clauses (->) in \"case\""],
      ~c"case nil do 0, z when not is_nil(z) -> z end"
    )
  end

  test "invalid fn args" do
    assert_eval_raise TokenMissingError,
                      ~r/nofile:1:5: missing terminator: end \(for "fn" starting at line 1\).*/,
                      ~c"fn 1"
  end

  test "invalid escape" do
    assert_eval_raise TokenMissingError,
                      ~r/nofile:1:3: invalid escape \\ at end of file/,
                      ~c"1 \\"
  end

  test "show snippet on missing tokens" do
    assert_eval_raise TokenMissingError,
                      "nofile:1:25: missing terminator: end (for \"do\" starting at line 1)\n" <>
                        "    |\n" <>
                        "  1 | defmodule ShowSnippet do\n" <>
                        "    |                         ^",
                      ~c"defmodule ShowSnippet do"
  end

  test "don't show snippet when error line is empty" do
    assert_eval_raise TokenMissingError,
                      "nofile:3:1: missing terminator: end (for \"do\" starting at line 1)",
                      ~c"defmodule ShowSnippet do\n\n"
  end

  test "function local conflict" do
    assert_compile_error(
      ["nofile:3: ", "imported Kernel.&&/2 conflicts with local function"],
      ~c"""
      defmodule Kernel.ErrorsTest.FunctionLocalConflict do
        def other, do: 1 && 2
        def _ && _, do: :error
      end
      """
    )
  end

  test "macro local conflict" do
    assert_compile_error(
      [
        "nofile:6: ",
        "call to local macro &&/2 conflicts with imported Kernel.&&/2, " <>
          "please rename the local macro or remove the conflicting import"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.MacroLocalConflict do
        def hello, do: 1 || 2
        defmacro _ || _, do: :ok

        defmacro _ && _, do: :error
        def world, do: 1 && 2
      end
      """
    )
  end

  test "macro with undefined local" do
    assert_eval_raise UndefinedFunctionError,
                      "function Kernel.ErrorsTest.MacroWithUndefinedLocal.unknown/1" <>
                        " is undefined (function not available)",
                      ~c"""
                      defmodule Kernel.ErrorsTest.MacroWithUndefinedLocal do
                        defmacrop bar, do: unknown(1)
                        def baz, do: bar()
                      end
                      """
  end

  test "private macro" do
    assert_eval_raise UndefinedFunctionError,
                      "function Kernel.ErrorsTest.PrivateMacro.foo/0 is undefined (function not available)",
                      ~c"""
                      defmodule Kernel.ErrorsTest.PrivateMacro do
                        defmacrop foo, do: 1
                        defmacro bar, do: __MODULE__.foo()
                        defmacro baz, do: bar()
                      end
                      """
  end

  test "macro invoked before its definition" do
    assert_compile_error(
      ["nofile:2: ", "cannot invoke macro bar/0 before its definition"],
      ~c"""
      defmodule Kernel.ErrorsTest.IncorrectMacroDispatch do
        def foo, do: bar()
        defmacro bar, do: :bar
      end
      """
    )

    assert_compile_error(
      ["nofile:2: ", "cannot invoke macro bar/0 before its definition"],
      ~c"""
      defmodule Kernel.ErrorsTest.IncorrectMacropDispatch do
        def foo, do: bar()
        defmacrop bar, do: :ok
      end
      """
    )

    assert_compile_error(
      ["nofile:2: ", "cannot invoke macro bar/1 before its definition"],
      ~c"""
      defmodule Kernel.ErrorsTest.IncorrectMacroDispatch do
        defmacro bar(a) when is_atom(a), do: bar([a])
      end
      """
    )
  end

  test "macro captured before its definition" do
    assert_compile_error(
      ["nofile:3: ", "cannot invoke macro is_ok/1 before its definition"],
      ~c"""
      defmodule Kernel.ErrorsTest.IncorrectMacroDispatch.Capture do
        def foo do
          predicate = &is_ok/1
          Enum.any?([:ok, :error, :foo], predicate)
        end

        defmacro is_ok(atom), do: atom == :ok
      end
      """
    )
  end

  test "function definition with alias" do
    assert_compile_error(
      [
        "nofile:2\n",
        "function names should start with lowercase characters or underscore, invalid name Bar"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.FunctionDefinitionWithAlias do
        def Bar do
          :baz
        end
      end
      """
    )
  end

  test "function import conflict" do
    assert_compile_error(
      ["nofile:3: ", "function exit/1 imported from both :erlang and Kernel, call is ambiguous"],
      ~c"""
      defmodule Kernel.ErrorsTest.FunctionImportConflict do
        import :erlang, only: [exit: 1], warn: false
        def foo, do: exit(:test)
      end
      """
    )
  end

  test "ensure valid import :only option" do
    assert_compile_error(
      [
        "nofile:3: ",
        "invalid :only option for import, expected value to be an atom " <>
          ":functions, :macros, or a list literal, got: x"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.Only do
        x = [flatten: 1]
        import List, only: x
      end
      """
    )
  end

  test "ensure valid import :except option" do
    assert_compile_error(
      [
        "nofile:3:3",
        "invalid :except option for import, expected value to be a list " <>
          "literal, got: Module.__get_attribute__(Kernel.ErrorsTest.Only, :x, 3, true)"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.Only do
        @x [flatten: 1]
        import List, except: @x
      end
      """
    )
  end

  test "def defmacro clause change" do
    assert_compile_error(
      ["nofile:3\n", "defmacro foo/1 already defined as def in nofile:2"],
      ~c"""
      defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
        def foo(1), do: 1
        defmacro foo(x), do: x
      end
      """
    )
  end

  test "def defp clause change from another file" do
    assert_compile_error(["nofile:4\n", "def hello/0 already defined as defp"], ~c"""
    defmodule Kernel.ErrorsTest.DefDefmacroClauseChange do
      require Kernel.ErrorsTest
      defp hello, do: :world
      Kernel.ErrorsTest.hello()
    end
    """)
  end

  test "internal function overridden" do
    assert_compile_error(
      ["nofile:2\n", "cannot define def __info__/1 as it is automatically defined by Elixir"],
      ~c"""
      defmodule Kernel.ErrorsTest.InternalFunctionOverridden do
        def __info__(_), do: []
      end
      """
    )
  end

  test "no macros" do
    assert_compile_error(["nofile:2: ", "could not load macros from module :lists"], ~c"""
    defmodule Kernel.ErrorsTest.NoMacros do
      import :lists, only: :macros
    end
    """)
  end

  test "invalid macro" do
    assert_compile_error(
      "invalid quoted expression: {:foo, :bar, :baz, :bat}",
      ~c"""
      defmodule Kernel.ErrorsTest.InvalidMacro do
        defmacrop oops do
          {:foo, :bar, :baz, :bat}
        end

        def test, do: oops()
      end
      """
    )
  end

  test "unloaded module" do
    assert_compile_error(
      ["nofile:1:1", "module Certainly.Doesnt.Exist is not loaded and could not be found"],
      ~c"import Certainly.Doesnt.Exist"
    )
  end

  test "module imported from the context it was defined in" do
    assert_compile_error(
      [
        "nofile:4: ",
        "module Kernel.ErrorsTest.ScheduledModule.Hygiene is not loaded but was defined."
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.ScheduledModule do
        defmodule Hygiene do
        end
        import Kernel.ErrorsTest.ScheduledModule.Hygiene
      end
      """
    )
  end

  test "module imported from the same module" do
    assert_compile_error(
      [
        "nofile:3: ",
        "you are trying to use/import/require the module Kernel.ErrorsTest.ScheduledModule.Hygiene which is currently being defined"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.ScheduledModule do
        defmodule Hygiene do
          import Kernel.ErrorsTest.ScheduledModule.Hygiene
        end
      end
      """
    )
  end

  test "already compiled module" do
    assert_eval_raise ArgumentError,
                      "could not call Module.eval_quoted/4 because the module Record is already compiled",
                      ~c"Module.eval_quoted Record, quote(do: 1), [], file: __ENV__.file"
  end

  test "@compile inline with undefined function" do
    assert_compile_error(
      ["nofile:1: ", "inlined function foo/1 undefined"],
      ~c"defmodule Test do @compile {:inline, foo: 1} end"
    )
  end

  test "invalid @dialyzer options" do
    assert_compile_error(
      ["nofile:1: ", "undefined function foo/1 given to @dialyzer :nowarn_function"],
      ~c"defmodule Test do @dialyzer {:nowarn_function, {:foo, 1}} end"
    )

    assert_compile_error(
      ["nofile:1: ", "macro foo/1 given to @dialyzer :nowarn_function"],
      ~c"defmodule Test do @dialyzer {:nowarn_function, {:foo, 1}}; defmacro foo(_), do: :ok end"
    )

    assert_compile_error(
      ["nofile:1: ", "undefined function foo/1 given to @dialyzer :no_opaque"],
      ~c"defmodule Test do @dialyzer {:no_opaque, {:foo, 1}} end"
    )

    assert_eval_raise ArgumentError,
                      "invalid value for @dialyzer attribute: :not_an_option",
                      ~c"defmodule Test do @dialyzer :not_an_option end"
  end

  test "@on_load attribute format" do
    assert_raise ArgumentError, ~r/should be an atom or an {atom, 0} tuple/, fn ->
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
    assert_compile_error(
      ["nofile:1: ", "undefined function foo/0 given to @on_load"],
      ~c"defmodule UndefinedOnLoadFunction do @on_load :foo end"
    )
  end

  test "wrong kind for @on_load attribute" do
    assert_compile_error(
      ["nofile:1: ", "expected @on_load function foo/0 to be a function, got \"defmacro\""],
      ~c"""
      defmodule PrivateOnLoadFunction do
        @on_load :foo
        defmacro foo, do: :ok
      end
      """
    )
  end

  test "in definition module" do
    assert_compile_error(
      [
        "nofile:2: ",
        "cannot define module Kernel.ErrorsTest.InDefinitionModule " <>
          "because it is currently being defined in nofile:1"
      ],
      ~c"""
      defmodule Kernel.ErrorsTest.InDefinitionModule do
        defmodule Elixir.Kernel.ErrorsTest.InDefinitionModule, do: true
      end
      """
    )
  end

  test "invalid definition" do
    assert_compile_error(
      ["nofile:1: ", "invalid syntax in def 1.(hello)"],
      ~c"defmodule Kernel.ErrorsTest.InvalidDefinition, do: (def 1.(hello), do: true)"
    )
  end

  test "function head with guard" do
    assert_compile_error(["nofile:2: ", "missing :do option in \"def\""], ~c"""
    defmodule Kernel.ErrorsTest.BodyessFunctionWithGuard do
      def foo(n) when is_number(n)
    end
    """)

    assert_compile_error(["nofile:2: ", "missing :do option in \"def\""], ~c"""
    defmodule Kernel.ErrorsTest.BodyessFunctionWithGuard do
      def foo(n) when is_number(n), true
    end
    """)
  end

  test "invalid args for function head" do
    assert_compile_error(
      ["nofile:2: ", "only variables and \\\\ are allowed as arguments in function head."],
      ~c"""
      defmodule Kernel.ErrorsTest.InvalidArgsForBodylessClause do
        def foo(nil)
        def foo(_), do: :ok
      end
      """
    )
  end

  test "bad multi-call" do
    assert_compile_error(
      [
        "nofile:1:1",
        "invalid argument for alias, expected a compile time atom or alias, got: 42"
      ],
      ~c"alias IO.{ANSI, 42}"
    )

    assert_compile_error(
      ["nofile:1:1", ":as option is not supported by multi-alias call"],
      ~c"alias Elixir.{Map}, as: Dict"
    )

    assert_eval_raise UndefinedFunctionError,
                      "function List.\"{}\"/1 is undefined or private",
                      ~c"[List.{Chars}, \"one\"]"
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
                 {:erlang, :apply, [1, :foo, []], _},
                 {__MODULE__, :bad_remote_call, 1, [file: _, line: _]} | _
               ] = __STACKTRACE__
    end
  end

  test "def fails when rescue, else or catch don't have clauses" do
    assert_compile_error(~r"expected -> clauses for :rescue in \"def\"", """
    defmodule Example do
      def foo do
        bar()
      rescue
        baz()
      end
    end
    """)
  end

  test "duplicate map keys" do
    assert_compile_error(["nofile:1:4", "key :a will be overridden in map"], """
      %{a: :b, a: :c} = %{a: :c}
    """)

    assert_compile_error(["nofile:1:4", "key :a will be overridden in map"], """
      %{a: :b, a: :c, a: :d} = %{a: :c}
    """)
  end

  test "| outside of cons" do
    assert_compile_error(["nofile:1:3", "misplaced operator |/2"], "1 | 2")

    assert_compile_error(
      ["nofile:1: ", "misplaced operator |/2"],
      "defmodule MisplacedOperator, do: (def bar(1 | 2), do: :ok)"
    )
  end

  defp bad_remote_call(x), do: x.foo()

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

  defp assert_compile_error(messages, string) do
    captured =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        ast = Code.string_to_quoted!(string, columns: true)
        assert_raise CompileError, fn -> Code.eval_quoted(ast) end
      end)

    for message <- List.wrap(messages) do
      assert captured =~ message
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
