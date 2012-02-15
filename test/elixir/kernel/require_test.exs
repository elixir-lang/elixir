Code.require_file "../../test_helper", __FILE__

defmodule Kernel::RequireTest::Nested do
  def value, do: 1
end

defmodule Kernel::RequireTest do
  use ExUnit::Case

  test :require_erlang do
    require Erlang.lists, as: MyList
    assert_equal [1,2,3], MyList.flatten([1,[2],3])
    assert_equal :"::MyList::Bar", ::MyList::Bar
    assert_equal :"::lists::Bar", MyList::Bar
  end

  test :require_with_as_true do
    require Kernel::RequireTest::Nested, as: true
    assert_equal 1, Nested.value
  end

  test :default_required do
    result = Elixir::Builtin.case 1 do
    match: 1
      true
    else:
      false
    end

    assert result
  end
end
