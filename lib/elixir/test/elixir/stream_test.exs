Code.require_file "test_helper.exs", __DIR__

defmodule StreamTest do
  use ExUnit.Case, async: true

  test "streams as enumerables" do
    stream = Stream.map([1,2,3], &(&1 * 2))

    # Reduce
    assert Enum.map(stream, &(&1 + 1)) == [3,5,7]
    # Member
    assert Enum.member?(stream, 4)
    refute Enum.member?(stream, 1)
    # Count
    assert Enum.count(stream) == 3
  end

  test "streams are composable" do
    stream = Stream.map([1,2,3], &(&1 * 2))
    assert is_lazy(stream)

    stream = Stream.map(stream, &(&1 + 1))
    assert is_lazy(stream)

    assert Enum.to_list(stream) == [3,5,7]
  end

  test "chunk/2, chunk/3 and chunk/4" do
    assert Stream.chunk([1, 2, 3, 4, 5], 2) |> Enum.to_list ==
           [[1, 2], [3, 4]]
    assert Stream.chunk([1, 2, 3, 4, 5], 2, 2, [6]) |> Enum.to_list ==
           [[1, 2], [3, 4], [5, 6]]
    assert Stream.chunk([1, 2, 3, 4, 5, 6], 3, 2) |> Enum.to_list ==
           [[1, 2, 3], [3, 4, 5]]
    assert Stream.chunk([1, 2, 3, 4, 5, 6], 2, 3) |> Enum.to_list ==
           [[1, 2], [4, 5]]
    assert Stream.chunk([1, 2, 3, 4, 5, 6], 3, 2, []) |> Enum.to_list ==
           [[1, 2, 3], [3, 4, 5], [5, 6]]
    assert Stream.chunk([1, 2, 3, 4, 5, 6], 3, 3, []) |> Enum.to_list ==
           [[1, 2, 3], [4, 5, 6]]
    assert Stream.chunk([1, 2, 3, 4, 5], 4, 4, 6..10) |> Enum.to_list ==
           [[1, 2, 3, 4], [5, 6, 7, 8]]
  end

  test "chunk/4 is zippable" do
    stream = Stream.chunk([1, 2, 3, 4, 5, 6], 3, 2, [])
    list   = Enum.to_list(stream)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
  end

  test "chunk_by/2" do
    stream = Stream.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))

    assert is_lazy(stream)
    assert Enum.to_list(stream) ==
           [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
    assert stream |> Stream.take(3) |> Enum.to_list ==
           [[1], [2, 2], [3]]
  end

  test "chunk_by/2 is zippable" do
    stream = Stream.chunk_by([1, 2, 2, 3], &(rem(&1, 2) == 1))
    list   = Enum.to_list(stream)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
  end

  test "concat/1" do
    stream = Stream.concat([1..3, [], [4, 5, 6], [], 7..9])
    assert is_function(stream)

    assert Enum.to_list(stream) == [1,2,3,4,5,6,7,8,9]
    assert Enum.take(stream, 5) == [1,2,3,4,5]

    stream = Stream.concat([1..3, [4, 5, 6], Stream.cycle(7..100)])
    assert is_function(stream)

    assert Enum.take(stream, 13) == [1,2,3,4,5,6,7,8,9,10,11,12,13]
  end

  test "concat/2" do
    stream = Stream.concat(1..3, 4..6)
    assert is_function(stream)
    assert Stream.cycle(stream) |> Enum.take(16) == [1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4]

    stream = Stream.concat(1..3, [])
    assert is_function(stream)
    assert Stream.cycle(stream) |> Enum.take(5) == [1,2,3,1,2]

    stream = Stream.concat(1..6, Stream.cycle(7..9))
    assert is_function(stream)
    assert Stream.drop(stream, 3) |> Enum.take(13) == [4,5,6,7,8,9,7,8,9,7,8,9,7]

    stream = Stream.concat(Stream.cycle(1..3), Stream.cycle(4..6))
    assert is_function(stream)
    assert Enum.take(stream, 13) == [1,2,3,1,2,3,1,2,3,1,2,3,1]
  end

  test "concat/2 does not intercept wrapped lazy enumeration" do
    # concat returns a lazy enumeration that does not halt
    assert Stream.concat([[0], Stream.map([1, 2, 3], & &1), [4]])
           |> Stream.take_while(fn x -> x <= 4 end)
           |> Enum.to_list == [0, 1, 2, 3, 4]

    # concat returns a lazy enumeration that does halts
    assert Stream.concat([[0], Stream.take_while(1..6, &(&1 <= 3)), [4]])
           |> Stream.take_while(fn x -> x <= 4 end)
           |> Enum.to_list == [0, 1, 2, 3, 4]
  end

  test "cycle/1" do
    stream = Stream.cycle([1,2,3])
    assert is_function(stream)

    assert Stream.cycle([1,2,3]) |> Stream.take(5) |> Enum.to_list == [1,2,3,1,2]
    assert Enum.take(stream, 5) == [1,2,3,1,2]
  end

  test "cycle/1 is zippable" do
    stream = Stream.cycle([1,2,3])
    assert Enum.zip(1..6, [1,2,3,1,2,3]) == Enum.zip(1..6, stream)
  end

  test "drop/2" do
    stream = Stream.drop(1..10, 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [6,7,8,9,10]

    assert Enum.to_list(Stream.drop(1..5, 0)) == [1,2,3,4,5]
    assert Enum.to_list(Stream.drop(1..3, 5)) == []

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.drop(nats, 2) |> Enum.take(5) == [3,4,5,6,7]
  end

  test "drop/2 with negative count" do
    stream = Stream.drop(1..10, -5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,2,3,4,5]

    stream = Stream.drop(1..10, -5)
    list   = Enum.to_list(stream)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
  end

  test "drop/2 with negative count stream entries" do
    par = self
    pid = spawn_link fn ->
      Enum.each Stream.drop(&inbox_stream/2, -3),
                fn x -> send par, { :stream, x } end
    end

    send pid, { :stream, 1 }
    send pid, { :stream, 2 }
    send pid, { :stream, 3 }
    refute_receive { :stream, 1 }

    send pid, { :stream, 4 }
    assert_receive { :stream, 1 }

    send pid, { :stream, 5 }
    assert_receive { :stream, 2 }
    refute_receive { :stream, 3 }
  end

  test "drop_while/2" do
    stream = Stream.drop_while(1..10, &(&1 <= 5))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [6,7,8,9,10]

    assert Enum.to_list(Stream.drop_while(1..5, &(&1 <= 0))) == [1,2,3,4,5]
    assert Enum.to_list(Stream.drop_while(1..3, &(&1 <= 5))) == []

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.drop_while(nats, &(&1 <= 5)) |> Enum.take(5) == [6,7,8,9,10]
  end

  test "each/2" do
    Process.put(:stream_each, [])

    stream = Stream.each([1,2,3], fn x ->
      Process.put(:stream_each, [x|Process.get(:stream_each)])
    end)

    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,2,3]
    assert Process.get(:stream_each) == [3,2,1]
  end

  test "filter/2" do
    stream = Stream.filter([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.filter(nats, &(rem(&1, 2) == 0)) |> Enum.take(5) == [2,4,6,8,10]
  end

  test "filter_map/3" do
    stream = Stream.filter_map([1,2,3], fn(x) -> rem(x, 2) == 0 end, &(&1 * 2))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [4]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.filter_map(nats, &(rem(&1, 2) == 0), &(&1 * 2))
           |> Enum.take(5) == [4,8,12,16,20]
  end

  test "flat_map/2" do
    stream = Stream.flat_map([1, 2, 3], &[&1, &1 * 2])
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1, 2, 2, 4, 3, 6]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.flat_map(nats, &[&1, &1 * 2]) |> Enum.take(6) == [1, 2, 2, 4, 3, 6]
  end

  test "flat_map/2 does not intercept wrapped lazy enumeration" do
    # flat_map returns a lazy enumeration that does not halt
    assert [1, 2, 3, -1, -2]
           |> Stream.flat_map(fn x -> Stream.map([x, x+1], & &1) end)
           |> Stream.take_while(fn x -> x >= 0 end)
           |> Enum.to_list == [1, 2, 2, 3, 3, 4]

    # flat_map returns a lazy enumeration that does halts
    assert [1, 2, 3, -1, -2]
           |> Stream.flat_map(fn x -> Stream.take_while([x, x+1, x+2], &(&1 <= x + 1)) end)
           |> Stream.take_while(fn x -> x >= 0 end)
           |> Enum.to_list == [1, 2, 2, 3, 3, 4]

    # flat_map returns a lazy enumeration that does halts wrapped in an enumerable
    assert [1, 2, 3, -1, -2]
           |> Stream.flat_map(fn x -> Stream.concat([x], Stream.take_while([x+1, x+2], &(&1 <= x + 1))) end)
           |> Stream.take_while(fn x -> x >= 0 end)
           |> Enum.to_list == [1, 2, 2, 3, 3, 4]
  end

  test "flat_map/2 is zippable" do
    stream = [1, 2, 3, -1, -2]
             |> Stream.flat_map(fn x -> Stream.map([x, x+1], & &1) end)
             |> Stream.take_while(fn x -> x >= 0 end)
    list   = Enum.to_list(stream)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
  end

  test "flat_map/2 does not leave inner stream suspended" do
    stream = Stream.flat_map [1,2,3],
      fn i ->
        Stream.resource(fn -> i end,
                        fn acc -> { acc, acc + 1 } end,
                        fn _ -> Process.put(:stream_flat_map, true) end)
      end

    Process.put(:stream_flat_map, false)
    assert stream |> Enum.take(3) == [1,2,3]
    assert Process.get(:stream_flat_map)
  end

  test "flat_map/2 does not leave outer stream suspended" do
    stream = Stream.resource(fn -> 1 end,
                             fn acc -> { acc, acc + 1 } end,
                             fn _ -> Process.put(:stream_flat_map, true) end)
    stream = Stream.flat_map(stream, fn i -> [i, i + 1, i + 2] end)

    Process.put(:stream_flat_map, false)
    assert stream |> Enum.take(3) == [1,2,3]
    assert Process.get(:stream_flat_map)
  end

  test "flat_map/2 closes on error" do
    stream = Stream.resource(fn -> 1 end,
                             fn acc -> { acc, acc + 1 } end,
                             fn _ -> Process.put(:stream_flat_map, true) end)
    stream = Stream.flat_map(stream, fn _ -> throw(:error) end)

    Process.put(:stream_flat_map, false)
    assert catch_throw(Enum.to_list(stream)) == :error
    assert Process.get(:stream_flat_map)
  end

  test "into/2 and run/1" do
    Process.put(:stream_cont, [])
    Process.put(:stream_done, false)
    Process.put(:stream_halt, false)

    stream = Stream.into([1, 2, 3], collectable_pdict)

    assert is_lazy(stream)
    assert Stream.run(stream) == :ok
    assert Process.get(:stream_cont) == [3,2,1]
    assert Process.get(:stream_done)
    refute Process.get(:stream_halt)

    stream = Stream.into(fn _, _ -> raise "error" end, collectable_pdict)
    catch_error(Stream.run(stream))
    assert Process.get(:stream_halt)
  end

  test "into/3" do
    Process.put(:stream_cont, [])
    Process.put(:stream_done, false)
    Process.put(:stream_halt, false)

    stream = Stream.into([1, 2, 3], collectable_pdict, fn x -> x*2 end)

    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1, 2, 3]
    assert Process.get(:stream_cont) == [6, 4, 2]
    assert Process.get(:stream_done)
    refute Process.get(:stream_halt)
  end

  test "into/2 with halting" do
    Process.put(:stream_cont, [])
    Process.put(:stream_done, false)
    Process.put(:stream_halt, false)

    stream = Stream.into([1, 2, 3], collectable_pdict)

    assert is_lazy(stream)
    assert Enum.take(stream, 1) == [1]
    assert Process.get(:stream_cont) == [1]
    assert Process.get(:stream_done)
    refute Process.get(:stream_halt)
  end

  test "transform/3" do
    stream = Stream.transform([1, 2, 3], 0, &{ [&1, &2], &1 + &2 })
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1, 0, 2, 1, 3, 3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.transform(nats, 0, &{ [&1, &2], &1 + &2 }) |> Enum.take(6) == [1, 0, 2, 1, 3, 3]
  end

  test "transform/3 with halt" do
    stream = Stream.resource(fn -> 1 end,
                             fn acc -> { acc, acc + 1 } end,
                             fn _ -> Process.put(:stream_transform, true) end)
    stream = Stream.transform(stream, 0, fn i, acc -> if acc < 3, do: { [i], acc + 1 }, else: { :halt, acc } end)

    Process.put(:stream_transform, false)
    assert Enum.to_list(stream) == [1,2,3]
    assert Process.get(:stream_transform)
  end

  test "iterate/2" do
    stream = Stream.iterate(0, &(&1+2))
    assert Enum.take(stream, 5) == [0,2,4,6,8]
    stream = Stream.iterate(5, &(&1+2))
    assert Enum.take(stream, 5) == [5,7,9,11,13]

    # Only calculate values if needed
    stream = Stream.iterate("HELLO", &raise/1)
    assert Enum.take(stream, 1) == ["HELLO"]
  end

  test "map/2" do
    stream = Stream.map([1,2,3], &(&1 * 2))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2,4,6]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.map(nats, &(&1 * 2)) |> Enum.take(5) == [2,4,6,8,10]
    assert Stream.map(nats, &(&1 - 2)) |> Stream.map(&(&1 * 2)) |> Enum.take(3) == [-2, 0, 2]
  end

  test "reject/2" do
    stream = Stream.reject([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.reject(nats, &(rem(&1, 2) == 0)) |> Enum.take(5) == [1,3,5,7,9]
  end

  test "repeatedly/1" do
    stream = Stream.repeatedly(fn -> 1 end)
    assert Enum.take(stream, 5) == [1,1,1,1,1]
    stream = Stream.repeatedly(&:random.uniform/0)
    [r1,r2] = Enum.take(stream, 2)
    assert r1 != r2
  end

  test "resource/3 closes on errors" do
    stream = Stream.resource(fn -> 1 end,
                             fn acc -> { acc, acc + 1 } end,
                             fn _ -> Process.put(:stream_resource, true) end)

    Process.put(:stream_resource, false)
    stream = Stream.map(stream, fn x -> if x > 2, do: throw(:error), else: x end)
    assert catch_throw(Enum.to_list(stream)) == :error
    assert Process.get(:stream_resource)
  end

  test "resource/3 is zippable" do
    stream = Stream.resource(fn -> 1 end,
                             fn 10 -> nil
                                acc -> { acc, acc + 1 }
                             end,
                             fn _ -> Process.put(:stream_resource, true) end)

    list = Enum.to_list(stream)
    Process.put(:stream_resource, false)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
    assert Process.get(:stream_resource)
  end

  test "scan/2" do
    stream = Stream.scan(1..5, &(&1 + &2))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,3,6,10,15]
    assert Stream.scan([], &(&1 + &2)) |> Enum.to_list == []
  end

  test "scan/3" do
    stream = Stream.scan(1..5, 0, &(&1 + &2))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,3,6,10,15]
    assert Stream.scan([], 0, &(&1 + &2)) |> Enum.to_list == []
  end

  test "take/2" do
    stream = Stream.take(1..1000, 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,2,3,4,5]

    assert Enum.to_list(Stream.take(1..1000, 0)) == []
    assert Enum.to_list(Stream.take(1..3, 5)) == [1,2,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Enum.to_list(Stream.take(nats, 5)) == [1,2,3,4,5]

    stream = Stream.drop(1..100, 5)
    assert Stream.take(stream, 5) |> Enum.to_list == [6,7,8,9,10]

    stream = 1..5 |> Stream.take(10) |> Stream.drop(15)
    assert { [], [] } = Enum.split(stream, 5)

    stream = 1..20 |> Stream.take(10 + 5) |> Stream.drop(4)
    assert Enum.to_list(stream) == [5,6,7,8,9,10,11,12,13,14,15]
  end

  test "take/2 with negative count" do
    Process.put(:stream_each, [])

    stream = Stream.take(1..100, -5)
    assert is_lazy(stream)

    stream = Stream.each(stream, &Process.put(:stream_each, [&1|Process.get(:stream_each)]))
    assert Enum.to_list(stream) == [96,97,98,99,100]
    assert Process.get(:stream_each) == [100,99,98,97,96]
  end

  test "take/2 is zippable" do
    stream = Stream.take(1..1000, 5)
    list   = Enum.to_list(stream)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
  end

  test "take_every/2" do
    assert 1..10
           |> Stream.take_every(2)
           |> Enum.to_list == [1, 3, 5, 7, 9]

    assert 1..10
           |> Stream.drop(2)
           |> Stream.take_every(2)
           |> Stream.drop(1)
           |> Enum.to_list == [5, 7, 9]
  end

  test "take_while/2" do
    stream = Stream.take_while(1..1000, &(&1 <= 5))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,2,3,4,5]

    assert Enum.to_list(Stream.take_while(1..1000, &(&1 <= 0))) == []
    assert Enum.to_list(Stream.take_while(1..3, &(&1 <= 5))) == [1,2,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Enum.to_list(Stream.take_while(nats, &(&1 <= 5))) == [1,2,3,4,5]

    stream = Stream.drop(1..100, 5)
    assert Stream.take_while(stream, &(&1 < 11)) |> Enum.to_list == [6,7,8,9,10]
  end

  test "unfold/2" do
    stream = Stream.unfold(10, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    assert Enum.take(stream, 5) == [10, 9, 8, 7, 6]
    stream = Stream.unfold(5, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    assert Enum.to_list(stream) == [5, 4, 3, 2, 1]
  end

  test "unfold/2 only calculates values if needed" do
    stream = Stream.unfold(1, fn x -> if x > 0, do: {x, x-1}, else: throw(:boom) end)
    assert Enum.take(stream, 1) == [1]

    stream = Stream.unfold(5, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    assert Enum.to_list(Stream.take(stream, 2)) == [5, 4]
  end

  test "unfold/2 is zippable" do
    stream = Stream.unfold(10, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    list   = Enum.to_list(stream)
    assert Enum.zip(list, list) == Enum.zip(stream, stream)
  end

  test "uniq/1" do
    assert Stream.uniq([1, 2, 3, 2, 1]) |> Enum.to_list ==
           [1, 2, 3]

    assert Stream.uniq([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end) |> Enum.to_list ==
           [{1,:x}, {2,:y}]
  end

  test "zip/2" do
    concat = Stream.concat(1..3, 4..6)
    cycle  = Stream.cycle([:a, :b, :c])
    assert Stream.zip(concat, cycle) |> Enum.to_list ==
           [{1,:a},{2,:b},{3,:c},{4,:a},{5,:b},{6,:c}]
  end

  test "zip/2 does not leave streams suspended" do
    stream = Stream.resource(fn -> 1 end,
                             fn acc -> { acc, acc + 1 } end,
                             fn _ -> Process.put(:stream_zip, true) end)

    Process.put(:stream_zip, false)
    assert Stream.zip([:a, :b, :c], stream) |> Enum.to_list == [a: 1, b: 2, c: 3]
    assert Process.get(:stream_zip)

    Process.put(:stream_zip, false)
    assert Stream.zip(stream, [:a, :b, :c]) |> Enum.to_list == [{ 1, :a }, { 2, :b }, { 3, :c }]
    assert Process.get(:stream_zip)
  end

  test "zip/2 does not leave streams suspended on halt" do
    stream = Stream.resource(fn -> 1 end,
                             fn acc -> { acc, acc + 1 } end,
                             fn _ -> Process.put(:stream_zip, :done) end)

    assert Stream.zip([:a, :b, :c, :d, :e], stream) |> Enum.take(3) ==
           [a: 1, b: 2, c: 3]

    assert Process.get(:stream_zip) == :done
  end

  test "zip/2 closes on inner error" do
    stream = Stream.into([1, 2, 3], collectable_pdict)
    stream = Stream.zip(stream, Stream.map([:a, :b, :c], fn _ -> throw(:error) end))

    Process.put(:stream_done, false)
    assert catch_throw(Enum.to_list(stream)) == :error
    assert Process.get(:stream_done)
  end

  test "zip/2 closes on outer error" do
    stream = Stream.into([1, 2, 3], collectable_pdict)
             |> Stream.zip([:a, :b, :c])
             |> Stream.map(fn _ -> throw(:error) end)

    Process.put(:stream_done, false)
    assert catch_throw(Enum.to_list(stream)) == :error
    assert Process.get(:stream_done)
  end

  test "with_index/2" do
    stream = Stream.with_index([1,2,3])
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [{1,0},{2,1},{3,2}]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.with_index(nats) |> Enum.take(3) == [{1,0},{2,1},{3,2}]
  end

  defp is_lazy(stream) do
    is_record(stream, Stream.Lazy) or is_function(stream, 2)
  end

  defp collectable_pdict do
    fn
      _, { :cont, x } -> Process.put(:stream_cont, [x|Process.get(:stream_cont)])
      _, :done -> Process.put(:stream_done, true)
      _, :halt -> Process.put(:stream_halt, true)
    end
  end

  defp inbox_stream({ :suspend, acc }, f) do
    { :suspended, acc, &inbox_stream(&1, f) }
  end

  defp inbox_stream({ :halt, acc }, _f) do
    { :halted, acc }
  end

  defp inbox_stream({ :cont, acc }, f) do
    receive do
      { :stream, item } ->
        inbox_stream(f.(item, acc), f)
    end
  end
end
