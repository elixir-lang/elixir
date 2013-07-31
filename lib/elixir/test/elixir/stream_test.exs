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

  test :cycle do
    stream = Stream.cycle([1,2,3])
    assert is_function(stream)

    assert Stream.cycle([1,2,3]) |> Stream.take(5) |> Enum.to_list == [1,2,3,1,2]
    assert Enum.take(stream, 5) == [1,2,3,1,2]
  end

  test :drop do
    stream = Stream.drop(1..10, 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [6,7,8,9,10]

    assert Enum.to_list(Stream.drop(1..5, 0)) == [1,2,3,4,5]
    assert Enum.to_list(Stream.drop(1..3, 5)) == []

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.drop(nats, 2) |> Enum.take(5) == [3,4,5,6,7]
  end

  test :drop_while do
    stream = Stream.drop_while(1..10, &1 <= 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [6,7,8,9,10]

    assert Enum.to_list(Stream.drop_while(1..5, &1 <= 0)) == [1,2,3,4,5]
    assert Enum.to_list(Stream.drop_while(1..3, &1 <= 5)) == []

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.drop_while(nats, &1 <= 5) |> Enum.take(5) == [6,7,8,9,10]
  end

  test :filter do
    stream = Stream.filter([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.filter(nats, &(rem(&1, 2) == 0)) |> Enum.take(5) == [2,4,6,8,10]
  end

  test :iterate do
    stream = Stream.iterate(0, &1+2)
    assert Enum.take(stream, 5) == [0,2,4,6,8]
    stream = Stream.iterate(5, &1+2)
    assert Enum.take(stream, 5) == [5,7,9,11,13]

    # Only calculate values if needed
    stream = Stream.iterate("HELLO", raise(&1))
    assert Enum.take(stream, 1) == ["HELLO"]
  end

  test :map do
    stream = Stream.map([1,2,3], &1 * 2)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2,4,6]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.map(nats, &(&1 * 2)) |> Enum.take(5) == [2,4,6,8,10]
  end

  test :reject do
    stream = Stream.reject([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.reject(nats, &(rem(&1, 2) == 0)) |> Enum.take(5) == [1,3,5,7,9]
  end

  test :repeatedly do
    stream = Stream.repeatedly(fn -> 1 end)
    assert Enum.take(stream, 5) == [1,1,1,1,1]
    stream = Stream.repeatedly(&:random.uniform/0)
    [r1,r2] = Enum.take(stream, 2)
    assert r1 != r2
  end

  test :take do
    stream = Stream.take(1..1000, 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,2,3,4,5]

    assert Enum.to_list(Stream.take(1..1000, 0)) == []
    assert Enum.to_list(Stream.take(1..3, 5)) == [1,2,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Enum.to_list(Stream.take(nats, 5)) == [1,2,3,4,5]
  end

  test :take_while do
    stream = Stream.take_while(1..1000, &1 <= 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,2,3,4,5]

    assert Enum.to_list(Stream.take_while(1..1000, &1 <= 0)) == []
    assert Enum.to_list(Stream.take_while(1..3, &1 <= 5)) == [1,2,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Enum.to_list(Stream.take_while(nats, &(&1 <= 5))) == [1,2,3,4,5]
  end

  test :with_index do
    stream = Stream.with_index([1,2,3])
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [{1,0},{2,1},{3,2}]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.with_index(nats) |> Enum.take(3) == [{1,0},{2,1},{3,2}]
  end

  defp is_lazy(stream) do
    assert is_record(stream, Stream.Lazy)
  end
end
