defmodule IEx.History do
  @moduledoc false

  def init do
    Process.put(:iex_history_start_counter, 1)
    Process.put(:iex_history_counter, 1)
  end

  ### append ###

  def append(entry, counter) do
    Process.put({:iex_history, counter}, entry)
    Process.put(:iex_history_counter, counter+1)

    limit = IEx.Options.get(:history_size)
    start_counter = Process.get(:iex_history_start_counter)
    should_collect = limit_history(start_counter, counter, limit)
    if should_collect do
      IO.puts "...collecting garbage"
      :erlang.garbage_collect
    end
  end

  defp limit_history(_, _, limit) when limit < 0 do
    false
  end

  defp limit_history(counter, max_counter, limit) when max_counter - counter < limit do
    Process.put(:iex_history_start_counter, counter)
    true
  end

  defp limit_history(counter, max_counter, limit) do
    Process.delete({:iex_history, counter})
    limit_history(counter+1, max_counter, limit)
  end

  ### each ###

  def each(fun) do
    each(Process.get(:iex_history_start_counter),
         Process.get(:iex_history_counter),
         fun)
  end

  defp each(counter, max_counter, fun) when counter < max_counter do
    entry = Process.get({:iex_history, counter})
    fun.(entry)
    each(counter+1, max_counter, fun)
  end

  defp each(_, _, _) do
    :ok
  end

  ### nth ###

  def nth(n) do
    entry = case n do
      n when n >= 0 ->
        Process.get({:iex_history, n})
      n when n < 0 ->
        counter = Process.get(:iex_history_counter)
        Process.get({:iex_history, counter + n})
    end
    if nil?(entry) do
      raise "Out of bounds"
    end
    entry
  end
end
