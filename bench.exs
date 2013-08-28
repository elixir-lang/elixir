noise1 = Enum.map 1..50, fn(x) -> Module.concat Hello, "W#{x}" end
noise2 = Enum.map 51..100, fn(x) -> Module.concat Hello, "W#{x}" end

{ :module, module, binary, _ } =
  defprotocol Sample do
    @fallback_to_any true
    def blank(thing)
  end

:code.purge(module)
:code.delete(module)
File.write!("#{module}.beam", binary)

defrecord Yes, field: nil do
  defimpl Sample do
    def blank(_) do
      false
    end
  end
end

defrecord No, field: nil

defmodule Bench do 
  defrecord R, a: nil
 
  def run do
    IO.puts "Warmup"
    bench fn -> Enum.each 1..10, fn _ -> Sample.impl_for(Yes[]) end end
    bench fn -> Enum.each 1..10, fn _ -> Sample.impl_for(No[]) end end

    IO.puts "Normal"
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for(Yes[]) end end
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for(No[]) end end

    { :ok, binary } = Protocol.Consolidation.apply_to(Sample, unquote(noise1) ++ unquote(noise2) ++ [Yes, Tuple])
    :code.load_binary(Sample, 'sample.exs', binary)

    IO.puts "Consolidated"
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for(Yes[]) end end
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for(No[]) end end

    IO.puts "List"
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for([]) end end
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for([]) end end

    IO.puts "Empty Tuple"
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for({ }) end end
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for({ }) end end

    IO.puts "Fake Record"
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for({ :trololol }) end end
    bench fn -> Enum.each 1..1000, fn _ -> Sample.impl_for({ :trololol }) end end
  end
 
  defp bench(fun) do
    pid = spawn_link(fn ->
      :timer.tc(fun, []) |> elem(0) |> IO.inspect
    end)
 
    receive do: ({ :EXIT, ^pid, :normal } -> :ok)
  end
end
 
Bench.run
