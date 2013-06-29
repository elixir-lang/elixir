Code.require_file "test_helper.exs", __DIR__

defmodule StreamTest do
  use ExUnit.Case, async: true

  test "streams as enumerables" do
    stream = Stream.map([1,2,3], &1 * 2)

    # Reduce
    assert Enum.map(stream, &1 + 1) == [3,5,7]
    # Member
    assert Enum.member?(stream, 4)
    refute Enum.member?(stream, 1)
    # Count
    assert Enum.count(stream) == 3
  end

  test "streams are composable" do
    stream = Stream.map([1,2,3], &1 * 2)
    assert is_lazy(stream)

    stream = Stream.map(stream, &1 + 1)
    assert is_lazy(stream)

    assert Enum.to_list(stream) == [3,5,7]
  end

  test :map do
    stream = Stream.map([1,2,3], &1 * 2)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2,4,6]
  end

  test :filter do
    stream = Stream.filter([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2]
  end

  test :with_index do
    stream = Stream.with_index([1,2,3])
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [{1,0},{2,1},{3,2}]
  end

  defp is_lazy(stream) do
    assert is_record(stream, Stream.Lazy)
  end
end
