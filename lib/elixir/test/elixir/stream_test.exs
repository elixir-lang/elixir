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

  test "concat_1" do
    stream = Stream.concat([1..3, [], [4, 5, 6], [], 7..9])
    assert is_function(stream)

    assert Enum.to_list(stream) == [1,2,3,4,5,6,7,8,9]
    assert Enum.take(stream, 5) == [1,2,3,4,5]

    stream = Stream.concat([1..3, [4, 5, 6], Stream.cycle(7..100)])
    assert is_function(stream)

    assert Enum.take(stream, 13) == [1,2,3,4,5,6,7,8,9,10,11,12,13]
  end

  test "concat_2" do
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

  test "cycle" do
    stream = Stream.cycle([1,2,3])
    assert is_function(stream)

    assert Stream.cycle([1,2,3]) |> Stream.take(5) |> Enum.to_list == [1,2,3,1,2]
    assert Enum.take(stream, 5) == [1,2,3,1,2]
  end

  test "drop" do
    stream = Stream.drop(1..10, 5)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [6,7,8,9,10]

    assert Enum.to_list(Stream.drop(1..5, 0)) == [1,2,3,4,5]
    assert Enum.to_list(Stream.drop(1..3, 5)) == []

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.drop(nats, 2) |> Enum.take(5) == [3,4,5,6,7]
  end

  test "drop_while" do
    stream = Stream.drop_while(1..10, &(&1 <= 5))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [6,7,8,9,10]

    assert Enum.to_list(Stream.drop_while(1..5, &(&1 <= 0))) == [1,2,3,4,5]
    assert Enum.to_list(Stream.drop_while(1..3, &(&1 <= 5))) == []

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.drop_while(nats, &(&1 <= 5)) |> Enum.take(5) == [6,7,8,9,10]
  end

  test "filter" do
    stream = Stream.filter([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.filter(nats, &(rem(&1, 2) == 0)) |> Enum.take(5) == [2,4,6,8,10]
  end

  test "iterate" do
    stream = Stream.iterate(0, &(&1+2))
    assert Enum.take(stream, 5) == [0,2,4,6,8]
    stream = Stream.iterate(5, &(&1+2))
    assert Enum.take(stream, 5) == [5,7,9,11,13]

    # Only calculate values if needed
    stream = Stream.iterate("HELLO", &raise/1)
    assert Enum.take(stream, 1) == ["HELLO"]
  end

  test "map" do
    stream = Stream.map([1,2,3], &(&1 * 2))
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [2,4,6]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.map(nats, &(&1 * 2)) |> Enum.take(5) == [2,4,6,8,10]
    assert Stream.map(nats, &(&1 - 2)) |> Stream.map(&(&1 * 2)) |> Enum.take(3) == [-2, 0, 2]
  end

  test "flat_map" do
    stream = Stream.flat_map([1, 2, 3], &[&1, &1 * 2])
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1, 2, 2, 4, 3, 6]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.flat_map(nats, &[&1, &1 * 2]) |> Enum.take(6) == [1, 2, 2, 4, 3, 6]
  end

  test "flat_map does not intercept wrapped lazy enumeration" do
    # flat_map returns a lazy enumeration that does not throw
    assert [1, 2, 3, -1, -2]
           |> Stream.flat_map(fn x -> Stream.map([x, x+1], & &1) end)
           |> Stream.take_while(fn x -> x >= 0 end)
           |> Enum.to_list == [1, 2, 2, 3, 3, 4]

    # flat_map returns a lazy enumeration that does throws
    assert [1, 2, 3, -1, -2]
           |> Stream.flat_map(fn x -> Stream.take_while([x, x+1, x+2], &(&1 <= x + 1)) end)
           |> Stream.take_while(fn x -> x >= 0 end)
           |> Enum.to_list == [1, 2, 2, 3, 3, 4]

    # flat_map returns a lazy enumeration that does throws wrapped in an enumerable
    assert [1, 2, 3, -1, -2]
           |> Stream.flat_map(fn x -> Stream.concat([x], Stream.take_while([x+1, x+2], &(&1 <= x + 1))) end)
           |> Stream.take_while(fn x -> x >= 0 end)
           |> Enum.to_list == [1, 2, 2, 3, 3, 4]
  end

  test "pmap" do
    # The sleep should prevent processes from completing before new ones can be
    # started.
    f = fn ms -> fn x -> :timer.sleep(ms); x * 2 end end
   
    f_bad_mw = fn mw -> fn -> Stream.pmap([], [max_workers: mw], f.(10)) end end
    assert_raise ArgumentError, f_bad_mw.(nil)
    assert_raise ArgumentError, f_bad_mw.(0)
    assert_raise ArgumentError, f_bad_mw.(-1)
    
    stream = Stream.pmap([], f.(10))
    assert is_function(stream)
    assert Enum.to_list(stream) == [] 

    stream = Stream.pmap(1..5, [max_workers: 1], f.(10))
    assert is_function(stream)
    assert Enum.to_list(stream) == [2,4,6,8,10]

    # Less workers than elements, but more than one.
    stream = Stream.pmap(1..5, [max_workers: 2], f.(10))
    assert is_function(stream)
    assert Enum.to_list(stream) == [2,4,6,8,10]
    
    # Default max_workers, which are more than five.
    stream = Stream.pmap(1..5, f.(10))
    assert is_function(stream)
    assert Enum.to_list(stream) == [2,4,6,8,10]
   
    # Very high chance of only one process at a time here because the process
    # will complete very quickly.
    stream = Stream.pmap(1..5, &(&1*2))
    assert is_function(stream)
    assert Enum.to_list(stream) == [2,4,6,8,10]
  end
  
  test "pmap fails if a worker fails" do
    old_te = Process.flag(:trap_exit, true)
    assert (catch_throw (Stream.pmap([1], fn _, _ -> throw :foo end |> Enum.to_list())))
           == :foo
    assert (catch_error (Stream.pmap([1], 
                            fn _, _ -> raise ArgumentError, message: "foo" end |> Enum.to_list())))
           == ArgumentError[message: "foo"]
    assert (catch_exit (Stream.pmap([1], fn _, _ -> exit(:foo) end |> Enum.to_list())))
           == :foo
    # Ensure no leftover messages
    assert :erlang.process_info(self(), :messages) == {:messages, []}
    Process.flag(:trap_exit, old_te)
  end

  test "reject" do
    stream = Stream.reject([1,2,3], fn(x) -> rem(x, 2) == 0 end)
    assert is_lazy(stream)
    assert Enum.to_list(stream) == [1,3]

    nats = Stream.iterate(1, &(&1 + 1))
    assert Stream.reject(nats, &(rem(&1, 2) == 0)) |> Enum.take(5) == [1,3,5,7,9]
  end

  test "repeatedly" do
    stream = Stream.repeatedly(fn -> 1 end)
    assert Enum.take(stream, 5) == [1,1,1,1,1]
    stream = Stream.repeatedly(&:random.uniform/0)
    [r1,r2] = Enum.take(stream, 2)
    assert r1 != r2
  end

  test "take" do
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

  test "take_while" do
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

  test "unfold" do
    stream = Stream.unfold(10, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    assert Enum.take(stream, 5) == [10, 9, 8, 7, 6]
    stream = Stream.unfold(5, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    assert Enum.to_list(stream) == [5, 4, 3, 2, 1]
  end

  test "unfold only calculate values if needed" do
    stream = Stream.unfold(1, fn x -> if x > 0, do: {x, x-1}, else: throw(:boom) end)
    assert Enum.take(stream, 1) == [1]

    stream = Stream.unfold(5, fn x -> if x > 0, do: {x, x-1}, else: nil end)
    assert Enum.to_list(Stream.take(stream, 2)) == [5, 4]
  end
  
  test "upmap" do
    # The sleep should prevent processes from completing before new ones can be
    # started.
    f = fn ms -> fn k, x -> :timer.sleep(ms); x * 10 + k end end
    
    f_bad_mw = fn mw -> fn -> Stream.upmap([], [max_workers: mw], f.(10)) end end
    assert_raise ArgumentError, f_bad_mw.(nil)
    assert_raise ArgumentError, f_bad_mw.(0)
    assert_raise ArgumentError, f_bad_mw.(-1)
    
    input = Enum.zip(1..5, 6..10) |> Stream.map(&(&1)) # Turn into Stream.Lazy
    output = [{1, 61}, {2, 72}, {3, 83}, {4, 94}, {5, 105}] 
    
    stream = Stream.upmap([], f.(10))
    assert is_function(stream)
    assert Enum.to_list(stream) == [] 

    stream = Stream.upmap(input, [max_workers: 1], f.(10))
    assert is_function(stream)
    assert Enum.sort(stream) == output

    # Less workers than elements, but more than one.
    stream = Stream.upmap(input, [max_workers: 2], f.(10))
    assert is_function(stream)
    assert Enum.sort(stream) == output
    
    # Default max_workers, which are more than five.
    stream = Stream.upmap(input, f.(10))
    assert is_function(stream)
    assert Enum.sort(stream) == output
   
    # Very high chance of only one process at a time here because the process
    # will complete very quickly.
    stream = Stream.upmap(input, f.(10))
    assert is_function(stream)
    assert Enum.sort(stream) == output
  end

  test "upmap fails if a worker fails" do
    old_te = Process.flag(:trap_exit, true)
    assert (catch_throw (Stream.upmap([{1,1}], fn _, _ -> throw :foo end |> Enum.to_list())))
           == :foo
    assert (catch_error (Stream.upmap([{1,1}], 
                            fn _, _ -> raise ArgumentError, message: "foo" end |> Enum.to_list())))
           == ArgumentError[message: "foo"]
    assert (catch_exit (Stream.upmap([{1,1}], fn _, _ -> exit(:foo) end |> Enum.to_list())))
           == :foo
    # Ensure no leftover messages
    assert :erlang.process_info(self(), :messages) == {:messages, []}
    Process.flag(:trap_exit, old_te)
  end

  test "with_index" do
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
