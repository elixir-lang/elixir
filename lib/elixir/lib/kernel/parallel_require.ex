defmodule Kernel.ParallelRequire do
  @moduledoc """
  A module responsible for requiring files in parallel.
  """

  defmacrop default_callback, do: quote(do: fn x -> x end)

  @doc """
  Requires the given files.

  A callback that is invoked every time a file is required
  can be optionally given as argument.
  """
  def files(files, callback // default_callback) do
    spawn_requires(files, [], callback, [])
  end

  defp spawn_requires([], [], _callback, result),     do: result
  defp spawn_requires([], waiting, callback, result), do: wait_for_messages([], waiting, callback, result)

  defp spawn_requires(files, waiting, callback, result) when length(waiting) >= 4 do
    wait_for_messages(files, waiting, callback, result)
  end

  defp spawn_requires([h|t], waiting, callback, result) do
    parent = self

    child  = spawn_link fn ->
      try do
        new    = Code.require_file(h)
        result = new ++ result
        callback.(h)
        parent <- { :required, self }
      catch
        kind, reason ->
          parent <- { :failure, self, kind, reason, System.stacktrace }
      end
    end

    spawn_requires(t, [child|waiting], callback, result)
  end

  defp wait_for_messages(files, waiting, callback, result) do
    receive do
      { :required, child } ->
        spawn_requires(files, List.delete(waiting, child), callback, result)
      { :failure, _child, kind, reason, stacktrace } ->
        Erlang.erlang.raise(kind, reason, stacktrace)
    end
  end
end