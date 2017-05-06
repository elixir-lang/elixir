Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.WarningTest.FooBehaviour do
  @callback foo :: any
  @callback bar(any) :: any
end

defmodule Kernel.WarningTest.BarBehaviour do
  @callback bar(any, any) :: any
end

defmodule Kernel.WarningTest.BazBehaviour do
  @callback baz :: any
end

defmodule Kernel.WarningTest.EmptyModule do
end

defmodule Kernel.WarningTest.FooMacroBehaviour do
  @macrocallback foo :: any
end

defmodule Kernel.WarningTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  defp capture_err(fun) do
    capture_io(:stderr, fun)
  end

  test "unused variable" do
    output = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello(arg), do: nil

        if true do
          user = :warning
        else
          :nothing
        end
      end
      """
    end)
    assert output =~ "variable \"arg\" is unused"
    assert output =~ "variable \"user\" is unused"
  after
    purge Sample
  end

  test "unused variable in redefined function in different file" do
    output = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        defmacro __using__(_) do
          quote location: :keep do
            def function(arg)
          end
        end
      end
      """
      Code.eval_string("""
      defmodule RedefineSample do
        use Sample
        def function(var123), do: nil
      end
      """, [], file: "redefine_sample.ex")
    end)
    assert output =~ "redefine_sample.ex:3"
    assert output =~ "variable \"var123\" is unused"
  after
    purge Sample
    purge RedefineSample
  end

  test "useless literal" do
    message = "code block contains unused literal \"oops\""

    assert capture_err(fn ->
      Code.eval_string """
      "oops"
      :ok
      """
    end) =~ message

    assert capture_err(fn ->
      Code.eval_string """
      fn ->
        "oops"
        :ok
      end
      """
    end) =~ message

    assert capture_err(fn ->
      Code.eval_string """
      try do
        "oops"
        :ok
      after
        :ok
      end
      """
    end) =~ message
  end

  test "useless attr" do
    message = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @foo 1
        @bar 1
        @foo

        def bar do
          @bar
          :ok
        end
      end
      """
    end)

    assert message =~ "module attribute @foo in code block has no effect as it is never returned "
    assert message =~ "module attribute @bar in code block has no effect as it is never returned "
  after
    purge Sample
  end

  test "useless var" do
    message = "variable foo in code block has no effect as it is never returned "

    assert capture_err(fn ->
      Code.eval_string """
      foo = 1
      foo
      :ok
      """
    end) =~ message

    assert capture_err(fn ->
      Code.eval_string """
      fn ->
        foo = 1
        foo
        :ok
      end
      """
    end) =~ message

    assert capture_err(fn ->
      Code.eval_string """
      try do
        foo = 1
        foo
        :ok
      after
        :ok
      end
      """
    end) =~ message

    assert capture_err(fn ->
      Code.eval_string """
      node()
      :ok
      """
    end) == ""
  end

  test "underscored variable on match" do
    assert capture_err(fn ->
      Code.eval_string """
      {_arg, _arg} = {1, 1}
      """
    end) =~ "the underscored variable \"_arg\" appears more than once in a match"
  end

  test "underscored variable on assign" do
    assert capture_err(fn ->
      Code.eval_string """
       defmodule Sample do
        def fun(_var) do
          _var + 1
        end
      end
      """
    end) =~ "the underscored variable \"_var\" is used after being set"
  after
    purge Sample
  end

  test "unused function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        defp hello, do: nil
      end
      """
    end) =~ "function hello/0 is unused"

    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample2 do
        defp hello(0), do: hello(1)
        defp hello(1), do: :ok
      end
      """
    end) =~ "function hello/1 is unused"

    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample3 do
        def a, do: nil
        def b, do: d(10)
        defp c(x, y \\ 1), do: [x, y]
        defp d(x), do: x
      end
      """
    end) =~ "function c/2 is unused"
  after
    purge [Sample1, Sample2, Sample3]
  end

  test "unused cyclic functions" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        defp a, do: b()
        defp b, do: a()
      end
      """
    end) =~ "function a/0 is unused"
  after
    purge Sample
  end

  test "unused macro" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        defmacrop hello, do: nil
      end
      """
    end) =~ "macro hello/0 is unused"
  after
    purge Sample
  end

  test "shadowing" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def test(x) do
          case x do
            {:file, fid} -> fid
            {:path, _}   -> fn(fid) -> fid end
          end
        end
      end
      """
    end) == ""
  after
    purge Sample
  end

  test "unused default args" do
    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample1 do
        def a, do: b(1, 2, 3)
        defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ "default arguments in b/3 are never used"

    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample2 do
        def a, do: b(1, 2)
        defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ "the first 2 default arguments in b/3 are never used"

    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample3 do
        def a, do: b(1)
        defp b(arg1 \\ 1, arg2 \\ 2, arg3 \\ 3), do: [arg1, arg2, arg3]
      end
      """
    end) =~ "the first default argument in b/3 is never used"

    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample4 do
        def a, do: b(1)
        defp b(arg1 \\ 1, arg2, arg3 \\ 3), do: [arg1, arg2, arg3]
      end
      """
    end) == ""
  after
    purge [Sample1, Sample2, Sample3, Sample4]
  end

  test "unused import" do
    assert capture_err(fn ->
      Code.compile_string """
      defmodule Sample do
        import :lists
        def a, do: nil
      end
      """
    end) =~ "unused import :lists\n"

    assert capture_err(fn ->
      Code.compile_string """
      import :lists
      """
    end) =~ "unused import :lists\n"
  after
    purge Sample
  end

  test "unused import of one of the functions in :only" do
    output = capture_err(fn ->
      Code.compile_string """
      defmodule Sample do
        import String, only: [upcase: 1, downcase: 1, trim: 1]
        def a, do: upcase("hello")
      end
      """
    end)
    assert output =~ "unused import String.downcase/1"
    assert output =~ "unused import String.trim/1"
  after
    purge Sample
  end

  test "unused import of any of the functions in :only" do
    assert capture_err(fn ->
      Code.compile_string """
      defmodule Sample do
        import String, only: [upcase: 1, downcase: 1]
        def a, do: nil
      end
      """
    end) =~ "unused import String\n"
  after
    purge Sample
  end

  test "unused alias" do
    assert capture_err(fn ->
      Code.compile_string """
      defmodule Sample do
        alias :lists, as: List
        def a, do: nil
      end
      """
    end) =~ "unused alias List"
  after
    purge Sample
  end

  test "unused alias when also import" do
    assert capture_err(fn ->
      Code.compile_string """
      defmodule Sample do
        alias :lists, as: List
        import MapSet
        new()
      end
      """
    end) =~ "unused alias List"
  after
    purge Sample
  end

  test "unused inside dynamic module" do
    import List, only: [flatten: 1], warn: false

    assert capture_err(fn ->
      defmodule Sample do
        import String, only: [downcase: 1]

        def world do
          flatten([1, 2, 3])
        end
      end
    end) =~ "unused import String"
  after
    purge Sample
  end

  test "unused guard" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def atom_case do
          v = "bc"
          case v do
            _ when is_atom(v) -> :ok
            _ -> :fail
          end
        end
      end
      """
    end) =~ "this check/guard will always yield the same result"
  after
    purge Sample
  end

  test "previous clause always matches" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def binary_cond do
          v = "bc"
          cond do
            is_binary(v) -> :bin
            true -> :ok
          end
        end
      end
      """
    end) =~ "this clause cannot match because a previous clause at line 5 always matches"
  after
    purge Sample
  end

  test "empty clause" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        def hello
      end
      """
    end) =~ "implementation not provided for predefined def hello/0"
  after
    purge Sample1
  end

  test "used import via alias" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        import List, only: [flatten: 1]

        defmacro generate do
          List.duplicate(quote(do: flatten([1, 2, 3])), 100)
        end
      end

      defmodule Sample2 do
        import Sample1
        generate()
      end
      """
    end) == ""
  after
    purge [Sample1, Sample2]
  end

  test "clause not match" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: nil
        def hello, do: nil
      end
      """
    end) =~ "this clause cannot match because a previous clause at line 2 always matches"
  after
    purge Sample
  end

  test "clause with defaults should be first" do
    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample do
        def hello(arg), do: nil
        def hello(arg \\ 0), do: nil
      end
      """
    end) =~ "definitions with multiple clauses and default values require a header"
  after
    purge Sample
  end

  test "clauses with default should use fun head" do
    assert capture_err(fn ->
      Code.eval_string ~S"""
      defmodule Sample do
        def hello(arg \\ 0), do: nil
        def hello(arg), do: nil
      end
      """
    end) =~ "definitions with multiple clauses and default values require a header"
  after
    purge Sample
  end

  test "unused with local with overridable" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello, do: world()
        defp world, do: :ok
        defoverridable [hello: 0]
        def hello, do: :ok
      end
      """
    end) =~ "function world/0 is unused"
  after
    purge Sample
  end

  test "undefined module attribute" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @foo
      end
      """
    end) =~ "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
  after
    purge Sample
  end

  test "undefined module attribute in function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def hello do
          @foo
        end
      end
      """
    end) =~ "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
  after
    purge Sample
  end

  test "undefined module attribute with file" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @foo
      end
      """
    end) =~ "undefined module attribute @foo, please remove access to @foo or explicitly set it before access"
  after
    purge Sample
  end

  test "in guard empty list" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def a(x) when x in [], do: x
      end
      """
    end) =~ "this check/guard will always yield the same result"
  after
    purge Sample
  end

  test "no effect operator" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def a(x) do
          x != :foo
          :ok
        end
      end
      """
    end) =~ "use of operator != has no effect"
  after
    purge Sample
  end

  test "badarg warning" do
    assert capture_err(fn ->
      assert_raise ArgumentError, fn ->
        Code.eval_string """
        defmodule Sample do
          Atom.to_string "abc"
        end
        """
      end
    end) =~ "this expression will fail with ArgumentError"
  after
    purge [Sample]
  end

  test "undefined function for behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        @callback foo :: term
      end

      defmodule Sample2 do
        @behaviour Sample1
      end
      """
    end) =~ "undefined behaviour function foo/0 (for behaviour Sample1)"
  after
    purge [Sample1, Sample2]
  end

  test "undefined macro for behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample1 do
        @macrocallback foo :: Macro.t
      end

      defmodule Sample2 do
        @behaviour Sample1
      end
      """
    end) =~ "undefined behaviour macro foo/0 (for behaviour Sample1)"
  after
    purge [Sample1, Sample2]
  end

  test "undefined behavior" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behavior Hello
      end
      """
    end) =~ "@behavior attribute is not supported, please use @behaviour instead"
  after
    purge Sample
  end

  test "undefined macro for protocol" do
    assert capture_err(fn ->
      Code.eval_string """
      defprotocol Sample1 do
        def foo(subject)
      end

      defimpl Sample1, for: Atom do
      end
      """
    end) =~ "undefined protocol function foo/1 (for protocol Sample1)"
  after
    purge [Sample1, Sample1.Atom]
  end

  test "overridden def" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        def foo(x, 1), do: x + 1
        def bar(), do: nil
        def foo(x, 2), do: x * 2
      end
      """
    end) =~ "clauses for the same def should be grouped together, def foo/2 was previously defined (nofile:2)"
  after
    purge Sample
  end

  test "warning with overridden file" do
    output = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @file "sample"
        def foo(x), do: :ok
      end
      """
    end)
    assert output =~ "variable \"x\" is unused"
    assert output =~ "sample:3"
  after
    purge Sample
  end

  test "warning on codepoint escape" do
    assert capture_err(fn ->
      Code.eval_string "? "
    end) =~ "found ? followed by codepoint 0x20 (space), please use \\s instead"
  end

  test "duplicated docs" do
    output = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @doc "Something"
        @doc "Another"
        def foo, do: :ok

        @doc false
        @doc "Doc"
        def bar, do: :ok
      end
      """
    end)
    assert output =~ "redefining @doc attribute previously set at line 2"
    assert output =~ "nofile:3: Sample (module)"
    refute output =~ "nofile:7"
  after
    purge Sample
  end

  test "typedoc on typep" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @typedoc "Something"
        @typep priv :: any
        @spec foo() :: priv
        def foo(), do: nil
      end
      """
    end) =~ "type priv/0 is private, @typedoc's are always discarded for private types"
  after
    purge Sample
  end

  test "attribute with no use" do
    content = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @at "Something"
      end
      """
    end)
    assert content =~ "module attribute @at was set but never used"
    assert content =~ "nofile:2"
  after
    purge Sample
  end

  test "typedoc with no type" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @typedoc "Something"
      end
      """
    end) =~ "module attribute @typedoc was set but no type follows it"
  after
    purge Sample
  end

  test "doc with no function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @doc "Something"
      end
      """
    end) =~ "module attribute @doc was set but no definition follows it"
  after
    purge Sample
  end

  test "pipe without explicit parentheses" do
    assert capture_err(fn ->
      Code.eval_string """
      [5, 6, 7, 3]
      |> Enum.map_join "", &(Integer.to_string(&1))
      |> String.to_integer
      """
    end) =~ "parentheses are required when piping into a function call"
  end

  test "variable is being expanded to function call" do
    output = capture_err(fn ->
      Code.eval_string """
      self
      defmodule Sample do
        def my_node(), do: node
      end
      """
    end)
    assert output =~ "variable \"self\" does not exist and is being expanded to \"self()\""
    assert output =~ "variable \"node\" does not exist and is being expanded to \"node()\""
  after
    purge Sample
  end

  # ######################## impl warning tests ########################

  ### With invalid arg

  test "impl with undefined value" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour :abc

        @impl :abc
        def baz(), do: :ok
      end
      """
    end) =~ "got @impl :abc but :abc is not defined"
  after
    purge Sample
  end

  test "impl with no value" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @impl
        def baz(), do: :ok
      end
      """
    end) =~ "undefined module attribute @impl, please remove access to @impl or explicitly set it before access"
  after
    purge Sample
  end

  ### With nil arg or missing

  test "@impl nil is ignored" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        def baz, do: :ok

        @impl nil
        def baz(term), do: term
      end
      """

    end) == ""
  after
    purge Sample
  end

  test "no warnings when @impl not set" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        def foo(), do: :ok
        def bar(term), do: term
      end
      """
    end) == ""
  after
    purge Sample
  end

  ### With true arg

  test "impl true with no behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @impl true
        def baz(), do: :ok
      end
      """
    end) =~ "module attribute @impl was set but this module does not implement any behaviours"
  after
    purge Sample
  end

  test "impl true with callback name not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl true
        def baz(), do: :ok
      end
      """
    end) =~ "got @impl true for function baz/0 but no implemented behaviour specifies this callback"
  after
    purge Sample
  end

  test "impl true with macro callback name not in behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour

        @impl true
        defmacro baz() do
          unquote do
            :thats_not_a_callback
          end
        end

        @impl true
        defmacro foo() do
          unquote do
            :ok
          end
        end
      end
      """
    end) =~ "got @impl true for macro baz/0 but no implemented behaviour specifies this callback"
  after
    purge Sample
  end

  test "impl true with callback name not in multiple behaviours" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour
        @behaviour Kernel.WarningTest.BazBehaviour
        @behaviour Kernel.WarningTest.EmptyModule

        @impl true
        def qux(), do: :ok
      end
      """
    end) =~ "got @impl true for function qux/0 but no implemented behaviour specifies this callback"
  after
    purge Sample
  end

  test "impl true with correct callback name but incorrect callback arity" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl true
        def foo(term), do: term
      end
      """
    end) =~ "got @impl true for function foo/1 but no implemented behaviour specifies this callback"
  after
    purge Sample
  end

  test "impl true with correct callback name but incorrect callback arity and multiple behaviours" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour
        @behaviour Kernel.WarningTest.BarBehaviour

        @impl true
        def bar(), do: :ok
      end
      """
    end) =~ "got @impl true for function bar/0 but no implemented behaviour specifies this callback"
  after
    purge Sample
  end

  test "impl true set on private function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl true
        defp foo, do: :private
      end
      """

    end) =~ "function foo/0 is private, @impl is always ignored for private functions"
  after
    purge Sample
  end

  test "impl true with no function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl true
      end
      """
    end) =~ "module attribute @impl was set but no definition follows it"
  after
    purge Sample
  end

  test "warnings for functions without impl when @impl true set before" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl true
        def foo(), do: :ok

        def bar(term), do: term
      end
      """
    end) =~ "module attribute @impl was not set for implemented callback function bar/1"
  after
    purge Sample
  end

  test "impl true with correct callback" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl true
        def baz(), do: :ok
      end
      """
    end) == ""
  after
    purge Sample
  end

  test "impl true with correct macro callback" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour

        @impl true
        defmacro foo() do
          unquote do
            :ok
          end
        end
      end
      """
    end) == ""
  after
    purge Sample
  end

  test "impl true with function with the same name as macro" do
    warnings = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour

        @impl true
        def foo(), do: :ok
      end
      """
    end)

    assert warnings =~
      "got @impl true for function foo/0 but no implemented behaviour specifies this callback"

    assert warnings =~
      "undefined behaviour macro foo/0 (for behaviour Kernel.WarningTest.FooMacroBehaviour)"
  after
    purge Sample
  end

  test "impl true with macro the same name as function" do
    warnings = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl true
        defmacro baz() do
          unquote do
            :nope
          end
        end
      end
      """
    end)

    assert warnings =~
      "got @impl true for macro baz/0 but no implemented behaviour specifies this callback"

    assert warnings =~
      "undefined behaviour function baz/0 (for behaviour Kernel.WarningTest.BazBehaviour)"
  after
    purge Sample
  end

  test "impl true with multiple behaviours" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl true
        def foo(), do: :ok

        @impl true
        def bar(term), do: term

        @impl true
        def baz(), do: :ok
      end
      """
    end) == ""
  after
    purge Sample
  end

  ### With false arg

  test "@impl false is ignored for non-callback function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        def baz, do: :ok

        @impl false
        def baz(term), do: term
      end
      """

    end) == ""
  after
    purge Sample
  end

  test "@impl false warns for callback function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl false
        def baz, do: :ok
      end
      """

    end) =~ "got @impl false for function baz/0 but it implements a " <>
            "callback defined by Kernel.WarningTest.BazBehaviour"
  after
    purge Sample
  end

  test "@impl false warns for callback macro" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour

        @impl false
        defmacro foo() do
          unquote do
            :implemented_callback
          end
        end
      end
      """

    end) =~ "got @impl false for macro foo/0 but it implements a " <>
            "callback defined by Kernel.WarningTest.FooMacroBehaviour"
  after
    purge Sample
  end

  test "impl false with no function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl false
      end
      """
    end) =~ "module attribute @impl was set but no definition follows it"
  after
    purge Sample
  end

  ### With module arg

  test "impl behaviour with no @behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @impl Kernel.WarningTest.FooBehaviour
        def baz(), do: :ok
      end
      """
    end) =~ "module attribute @impl was set but this module does not implement any behaviours"
  after
    purge Sample
  end

  test "function implements behaviour but @behaviour is not specified" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        def foo(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.WarningTest.FooBehaviour for function foo/0 " <>
            "but that behaviour is not implemented by the module (@behaviour " <>
            "Kernel.WarningTest.FooBehaviour was not specified)"
  after
    purge Sample
  end

  test "function implements some behaviour from the module but it isn't the one specified in @impl behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        def baz(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.WarningTest.FooBehaviour for function baz/0 " <>
            "but that behaviour is not implemented by the module (@behaviour " <>
            "Kernel.WarningTest.FooBehaviour was not specified)"
  after
    purge Sample
  end


  test "macro implements some behaviour from the module but it isn't the one specified in @impl behaviour" do
    warnings = capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        defmacro baz() do
          unquote do
            :thats_not_a_callback
          end
        end

        @impl true
        def foo(), do: :ok

        @impl true
        def bar(term), do: term
      end
      """
    end)
    assert warnings =~
      "got @impl Kernel.WarningTest.FooBehaviour for macro baz/0 but " <>
      "behaviour Kernel.WarningTest.FooBehaviour does not define this callback"

    assert warnings =~
      "undefined behaviour macro foo/0 (for behaviour Kernel.WarningTest.FooMacroBehaviour)"
  after
    purge Sample
  end

  test "function does not implement specified behaviour and specified behaviour is not declared as @behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.BazBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        def qux(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.WarningTest.FooBehaviour for function qux/0 " <>
            "but that behaviour is not implemented by the module (@behaviour " <>
            "Kernel.WarningTest.FooBehaviour was not specified)"
  after
    purge Sample
  end

  test "function does not implement any behaviours from this module including the @impl behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        def baz(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.WarningTest.FooBehaviour for function baz/0 but behaviour" <>
            " Kernel.WarningTest.FooBehaviour does not define this callback"
  after
    purge Sample
  end

  test "macro does not implement any behaviours from this module including the @impl behaviour" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour

        @impl Kernel.WarningTest.FooMacroBehaviour
        defmacro baz() do
          unquote do
            :oops
          end
        end
      end
      """
    end) =~ "got @impl Kernel.WarningTest.FooMacroBehaviour for macro baz/0 " <>
            "but behaviour Kernel.WarningTest.FooMacroBehaviour does not define this callback"
  after
    purge Sample
  end

  test "impl behaviour with correct callback name but incorrect callback arity" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        def foo(term), do: term
      end
      """
    end) =~ "got @impl Kernel.WarningTest.FooBehaviour for function foo/1 but behaviour" <>
            " Kernel.WarningTest.FooBehaviour does not define this callback"
  after
    purge Sample
  end

  test "impl behaviour set on private function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        defp foo, do: :private
      end
      """

    end) =~ "function foo/0 is private, @impl is always ignored for private functions"
  after
    purge Sample
  end

  test "impl behaviour set on private macro" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooMacroBehaviour

        @impl Kernel.WarningTest.FooMacroBehaviour
        defmacrop foo do
          unquote do
            :private
          end
        end
      end
      """

    end) =~ "macro foo/0 is private, @impl is always ignored for private macros"
  after
    purge Sample
  end

  test "impl behaviour with no function" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl Kernel.WarningTest.FooBehaviour
      end
      """
    end) =~ "module attribute @impl was set but no definition follows it"
  after
    purge Sample
  end

  test "warnings for functions without impl when @impl behaviour set before" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.FooBehaviour

        @impl Kernel.WarningTest.FooBehaviour
        def foo(), do: :ok

        def bar(term), do: term
      end
      """
    end) =~ "module attribute @impl was not set for implemented callback function bar/1"
  after
    purge Sample
  end

  test "impl behaviour when behaviour doesn't define any callbacks" do
    assert capture_err(fn ->
      Code.eval_string """
      defmodule Sample do
        @behaviour Kernel.WarningTest.EmptyModule

        @impl Kernel.WarningTest.EmptyModule
        def baz(), do: :ok
      end
      """
    end) =~ "got @impl Kernel.WarningTest.EmptyModule but " <>
            "Kernel.WarningTest.EmptyModule does not define any callbacks"
  after
    purge Sample
  end

  # TODO: This is a tricky one
  # test "warnings for functions without impl when @impl set after" do
  #   assert capture_err(fn ->
  #     Code.eval_string """
  #     defmodule Sample do
  #       @behaviour Kernel.WarningTest.FooBehaviour

  #       def foo(), do: :ok

  #       @impl Kernel.WarningTest.FooBehaviour
  #       def bar(term), do: term
  #     end
  #     """
  #   end) =~ "module attribute @impl was not set for function foo/0 (callback specified in Kernel.WarningTest.FooBehaviour)"
  # after
  #   purge Sample
  # end

  defp purge(list) when is_list(list) do
    Enum.each list, &purge/1
  end

  defp purge(module) when is_atom(module) do
    :code.delete module
    :code.purge module
  end
end
