Code.require_file "../../test_helper", __FILE__

defmodule Kernel.RequireTest.Nested do
  def value, do: 1
end

refer Kernel.RequireTest.Nested

defmodule Kernel.RequireTest do
  use ExUnit.Case

  defmacro my_macro do
    quote do: 1 + 1
  end

  defmacrop my_private_macro do
    quote do: 1 + 3
  end

  test :require_erlang do
    require Erlang.lists, as: MyList
    assert_equal [1,2,3], MyList.flatten([1,[2],3])
    assert_equal :"__MAIN__.MyList.Bar", __MAIN__.MyList.Bar
    assert_equal :"__MAIN__.lists.Bar", MyList.Bar
  end

  test :automatic_require do
    assert_equal 1, Nested.value
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
    assert_equal 2, my_macro
  end

  test :locals_and_private_are_always_required do
    assert_equal 4, my_private_macro
  end
end
