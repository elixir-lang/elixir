Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.MacrosTest.Nested do
  defmacro value, do: 1

  defmacro do_identity!(do: x) do
    x
  end
end

defmodule Kernel.MacrosTest do
  use ExUnit.Case, async: true

  Kernel.MacrosTest.Nested = require Kernel.MacrosTest.Nested, as: Nested

  @spec my_macro :: Macro.t()
  defmacro my_macro do
    quote(do: 1 + 1)
  end

  @spec my_private_macro :: Macro.t()
  defmacrop my_private_macro do
    quote(do: 1 + 3)
  end

  defmacro my_macro_with_default(value \\ 5) do
    quote(do: 1 + unquote(value))
  end

  defp by_two(x), do: x * 2

  defmacro my_macro_with_local(value) do
    value = by_two(by_two(value))
    quote(do: 1 + unquote(value))
  end

  defmacro my_macro_with_capture(value) do
    Enum.map(value, &by_two/1)
  end

  test "require" do
    assert Kernel.MacrosTest.Nested.value() == 1
  end

  test "require with alias" do
    assert Nested.value() == 1
  end

  test "local with private macro" do
    assert my_private_macro() == 4
  end

  test "local with defaults macro" do
    assert my_macro_with_default() == 6
  end

  test "local with local call" do
    assert my_macro_with_local(4) == 17
  end

  test "local with capture" do
    assert my_macro_with_capture([1, 2, 3]) == [2, 4, 6]
  end

  test "macros cannot be called dynamically" do
    x = Nested
    assert_raise UndefinedFunctionError, fn -> x.value end
  end

  test "macros with bang and do block have proper precedence" do
    import Kernel.MacrosTest.Nested

    assert (do_identity! do
              1
            end) == 1

    assert (Kernel.MacrosTest.Nested.do_identity! do
              1
            end) == 1
  end
end
