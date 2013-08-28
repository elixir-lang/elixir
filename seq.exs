defmodule Streq do
  defrecord Lazy, [:enumerable, :fun, :acc]

  defimpl Enumerable, for: Lazy do
    def reduce(Lazy[] = lazy, acc, fun) do
      { enumerable, merged } = collect_funs(lazy, fn
        { :yield, _, acc } -> acc
        { :skip, acc }     -> acc
        { :done, acc }     -> throw({ :stream_done, acc })
      end)

      try do
        Enumerable.reduce(enumerable, acc, fn(x, acc) ->
          merged.({ :yield, x, acc })
        end)
      catch
        { :stream_done, acc } -> acc
      end
    end

    def count(lazy) do
      reduce(lazy, 0, fn _, acc -> acc + 1 end)
    end

    defp collect_funs(Lazy[enumerable: enumerable, fun: fun], merged) do
      collect_funs(enumerable, &(&1 |> fun.() |> merged.()))
    end

    defp collect_funs(enumerable, merged) do
      { enumerable, merged }
    end
  end

  def map(enumerable, mapper) do
    Lazy[enumerable: enumerable,
         fun: &map_s(&1, mapper)]
  end

  defp map_s({ :yield, x, acc }, mapper), do: { :yield, mapper.(x), acc }
  defp map_s(other, _mapper), do: other

  def filter(enumerable, filter) do
    Lazy[enumerable: enumerable,
         fun: &filter_s(&1, filter)]
  end

  defp filter_s({ :yield, x, acc }, filter) do
    if filter.(x) do
      { :yield, x, acc }
    else
      { :skip, acc }
    end
  end

  defp filter_s(other, _filter), do: other
end

stream = 1..10 |> Streq.filter(&Integer.even?/1) |> Streq.map(&(&1 * 2))
IO.inspect Enum.to_list(stream)

defmodule Bench do  
  def run() do
    streq  = 1..10 |> Streq.filter(&Integer.even?/1) |> Streq.map(&(&1 * 2))
    stream = 1..10 |> Stream.filter(&Integer.even?/1) |> Stream.map(&(&1 * 2))

    IO.puts "Warm up"
    bench fn -> Enum.count(streq) end
    bench fn -> Enum.count(stream) end

    IO.puts "10 items" 
    bench fn -> Enum.count(streq) end
    bench fn -> Enum.count(stream) end

    streq  = 1..100000 |> Streq.filter(&Integer.even?/1) |> Streq.map(&(&1 * 2))
    stream = 1..100000 |> Stream.filter(&Integer.even?/1) |> Stream.map(&(&1 * 2))

    IO.puts "1000 items" 
    bench fn -> Enum.count(streq) end
    bench fn -> Enum.count(stream) end
  end
 
  defp bench(fun) do
    pid = spawn_link(fn ->
      :timer.tc(fun, []) |> elem(0) |> IO.inspect
    end)
 
    receive do: ({ :EXIT, ^pid, :normal } -> :ok)
  end
end
 
Bench.run