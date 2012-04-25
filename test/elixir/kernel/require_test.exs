Code.require_file "../../test_helper", __FILE__

defmodule Kernel.RequireTest.Nested do
  defmacro value, do: 1
end

defmodule Kernel.RequireTest do
  require Kernel.RequireTest.Nested, as: Nested

  use ExUnit.Case

  defmacro my_macro do
    quote do: 1 + 1
  end

  defmacrop my_private_macro do
    quote do: 1 + 3
  end

  defmacro my_macro_with_default(value // 5) do
    quote do: 1 + unquote(value)
  end

  test :require_erlang do
    require Erlang.lists, as: MyList
    assert_equal [1,2,3], MyList.flatten([1,[2],3])
    assert_equal :"__MAIN__.MyList.Bar", __MAIN__.MyList.Bar
    assert_equal :"__MAIN__.lists.Bar", MyList.Bar
  end

  test :double_named_require do
    require Kernel.RequireTest.Nested, as: Nested2
    assert_equal 1, Nested.value
    assert_equal 1, Nested2.value
  end

  test :default_required do
    result = Elixir.Builtin.case 1 do
    match: 1
      true
    else:
      false
    end

    assert result
  end

  test :locals_are_always_required do
    assert_equal 2, __MODULE__.my_macro
  end

  test :locals_and_private_are_always_required do
    assert_equal 4, my_private_macro
  end

  test :locals_with_default_are_always_required do
    assert_equal 6, my_macro_with_default
  end

  test :cannot_be_called_dynamically_even_if_required do
    x = Nested
    assert_raise UndefinedFunctionError, fn -> x.value end
  end
end