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
    schedulers = :erlang.system_info(:schedulers_online)
    spawn_requires(files, [], callback, schedulers, [])
  end

  defp spawn_requires([], [], _callback, _schedulers, result), do: result

  defp spawn_requires([], waiting, callback, schedulers, result) do
    wait_for_messages([], waiting, callback, schedulers, result)
  end

  defp spawn_requires(files, waiting, callback, schedulers, result) when length(waiting) >= schedulers do
    wait_for_messages(files, waiting, callback, schedulers, result)
  end

  defp spawn_requires([h|t], waiting, callback, schedulers, result) do
    parent = self

    child  = spawn_link fn ->
      try do
        new = Code.require_file(h)
        callback.(h)
        parent <- { :required, self, new }
      catch
        kind, reason ->
          parent <- { :failure, self, kind, reason, System.stacktrace }
      end
    end

    spawn_requires(t, [child|waiting], callback, schedulers, result)
  end

  defp wait_for_messages(files, waiting, callback, schedulers, result) do
    receive do
      { :required, child, new } ->
        spawn_requires(files, List.delete(waiting, child), callback, schedulers, (new || []) ++ result)
      { :failure, _child, kind, reason, stacktrace } ->
        :erlang.raise(kind, reason, stacktrace)
    end
  end
end