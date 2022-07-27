Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DefaultsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  def fun_with_fn_defaults(
        x,
        fun1 \\ & &1,
        fun2 \\ & &1,
        y
      ) do
    {fun1.(x), fun2.(y)}
  end

  def fun_with_block_defaults(
        x,
        y \\ (
          default = "y"
          default
        ),
        z \\ (
          default = "z"
          default
        )
      ) do
    {x, y, z}
  end

  test "with anonymous function defaults" do
    assert {1, 2} = fun_with_fn_defaults(1, 2)
    assert {100, 2} = fun_with_fn_defaults(1, &(&1 * 100), 2)
    assert {100, 12} = fun_with_fn_defaults(1, &(&1 * 100), &(&1 + 10), 2)
  end

  test "with block defaults" do
    assert {1, "y", "z"} = fun_with_block_defaults(1)
    assert {1, 2, "z"} = fun_with_block_defaults(1, 2)
    assert {1, 2, 3} = fun_with_block_defaults(1, 2, 3)
  end

  test "errors on accessing variable from default block" do
    message = "variable \"default\" does not exist"

    assert capture_io(:stderr, fn ->
             assert_raise CompileError, ~r/undefined function default\/0/, fn ->
               defmodule VarDefaultScope do
                 def test(_ \\ default = 1),
                   do: default
               end
             end
           end) =~ message
  end

  test "errors on multiple defaults" do
    message = ~r"def hello/1 defines defaults multiple times"

    assert_raise CompileError, message, fn ->
      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
        def hello(_arg \\ 0)
        def hello(_arg \\ 1)
      end
    end

    assert_raise CompileError, message, fn ->
      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
        def hello(_arg \\ 0), do: nil
        def hello(_arg \\ 1), do: nil
      end
    end

    assert_raise CompileError, message, fn ->
      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
        def hello(_arg \\ 0)
        def hello(_arg \\ 1), do: nil
      end
    end

    assert_raise CompileError, message, fn ->
      defmodule Kernel.ErrorsTest.ClauseWithDefaults do
        def hello(_arg \\ 0), do: nil
        def hello(_arg \\ 1)
      end
    end

    assert capture_io(:stderr, fn ->
             assert_raise CompileError, ~r"undefined function foo/0", fn ->
               defmodule Kernel.ErrorsTest.ClauseWithDefaults5 do
                 def hello(foo, bar \\ foo)
                 def hello(foo, bar), do: foo + bar
               end
             end
           end) =~
             "variable \"foo\" does not exist and is being expanded to \"foo()\", " <>
               "please use parentheses to remove the ambiguity or change the variable name"
  end

  test "errors on conflicting defaults" do
    assert_raise CompileError, ~r"def hello/3 defaults conflicts with hello/2", fn ->
      defmodule Kernel.ErrorsTest.DifferentDefsWithDefaults1 do
        def hello(a, b \\ nil), do: a + b
        def hello(a, b \\ nil, c \\ nil), do: a + b + c
      end
    end

    assert_raise CompileError, ~r"def hello/2 conflicts with defaults from hello/3", fn ->
      defmodule Kernel.ErrorsTest.DifferentDefsWithDefaults2 do
        def hello(a, b \\ nil, c \\ nil), do: a + b + c
        def hello(a, b \\ nil), do: a + b
      end
    end
  end
end
