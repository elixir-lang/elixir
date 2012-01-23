Code.require_file "../../test_helper", __FILE__

defmodule Kernel::RequireTest::Nested do
  def value, do: 1
end

defmodule Kernel::RequireTest do
  use ExUnit::Case

  def test_require_erlang do
    require Erlang.lists, as: MyList
    [1,2,3] = MyList.flatten([1,[2],3])
    :"::MyList::Bar" = ::MyList::Bar
    :"::lists::Bar" = MyList::Bar
  end

  def test_require_with_one_arg do
    require Kernel::RequireTest::Nested
    1 = Nested.value
  end

  def test_default_required do
    true = Elixir::Macros.case 1 do
    match: 1
      true
    else:
      false
    end
  end
end