Code.require_file "test_helper.exs", __DIR__

defmodule CollectableTest do
  use ExUnit.Case, async: true

  doctest Collectable

  test "is implemented for tuples" do
    fun =  fn
      list, {:cont, x} -> [x | list]
      list, :done -> :lists.reverse(list)
      _, :halt -> :ok
    end
    assert Enum.into(1..3, {[], fun}) == [1, 2, 3]
  end
end
