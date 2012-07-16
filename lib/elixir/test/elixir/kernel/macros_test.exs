Code.require_file "../../test_helper", __FILE__

defmodule Kernel.MacrosTest.Nested do
  defmacro value, do: 1
end

defmodule Kernel.MacrosTest do
  require Kernel.MacrosTest.Nested, as: Nested

  use ExUnit.Case, async: true

  defmacro my_macro do
    quote do: 1 + 1
  end

  defmacrop my_private_macro do
    quote do: 1 + 3
  end

  defmacro my_macro_with_default(value // 5) do
    quote do: 1 + unquote(value)
  end

  test :require do
    assert Kernel.MacrosTest.Nested.value == 1
  end

  test :require_with_alias do
    assert Nested.value == 1
  end

  test :default_required do
    result = Elixir.Builtin.case 1 do
      1 -> true
      _ -> false
    end

    assert result
  end

  test :locals_macros do
    assert __MODULE__.my_macro == 2
  end

  test :local_but_private_macro do
    assert my_private_macro == 4
  end

  test :local_with_defaults_macro do
    assert my_macro_with_default == 6
  end

  test :macros_cannot_be_called_dynamically do
    x = Nested
    assert_raise UndefinedFunctionError, fn -> x.value end
  end
end