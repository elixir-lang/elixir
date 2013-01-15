Code.require_file "../test_helper.exs", __FILE__

defmodule HashDictTest do
  use ExUnit.Case, async: true

  defp smoke_test(range) do
    dict = Enum.reduce 1..range, HashDict.new, fn(x, acc) ->
      acc = HashDict.put(acc, x, x)
      assert HashDict.size(acc) == x
      acc
    end

    Enum.each 1..range, fn(x) ->
      assert HashDict.get(dict, x) == x
    end

    Enum.reduce range..1, dict, fn(x, acc) ->
      assert HashDict.size(acc) == x
      acc = HashDict.delete(acc, x)
      assert HashDict.size(acc) == x - 1
      assert HashDict.get(acc, x) == nil
      acc
    end
  end

  test :smoke_small_range_test do
    smoke_test(8)
  end

  test :smoke_medium_range_test do
    smoke_test(80)
  end

  test :smoke_large_range_test do
    smoke_test(200)
  end
end
