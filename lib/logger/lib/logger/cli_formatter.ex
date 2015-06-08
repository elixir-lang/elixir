defmodule Logger.CLIFormatter do
  @moduledoc """
  An ExUnit CLI Formatter. Captures logs per test and prints as batch
  on failure.

  ## Options

  Logger.CLIFormatter supports the follow options:

    * `capture_log_blacklist` - list of backends to remove during tests

  """

  use GenEvent

  @doc false
  def init(opts) do
    colors = Keyword.put_new(opts[:colors], :enabled, IO.ANSI.enabled?)
    captured =
      Keyword.get(opts, :capture_log_blacklist, [:console])
      |> Enum.reduce([], &[{&1, Logger.remove_backend(&1)} | &2])
    {:ok, {captured, colors: colors}}
  end

  @doc false
  def terminate(_reason, {captured, opts}) do
    :ok = add_capture(%{group_leader: nil}, opts)

    Enum.map(captured, fn
      {_, {:error, :not_found}} -> nil
      {backend, :ok} ->
        Logger.add_backend(backend)
    end)

    case remove_capture(%{group_leader: nil}, :get) do
      {:ok, []} -> nil
      {:ok, output} ->
        IO.puts(["The following output was logged:\n" | output])
    end
    :ok
  end

  @doc false
  def handle_event({:test_started, %ExUnit.Test{} = test}, {_, opts} = state) do
    :ok = add_capture(test, opts)
    {:ok, state}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:failed, _reason}} = test}, state) do
    case remove_capture(test, :get) do
      {:ok, []} -> nil
      {:ok, output} ->
        IO.puts(["The following output was logged:\n" | output])
    end
    {:ok, state}
  end

  def handle_event({:test_finished, %ExUnit.Test{} = test}, state) do
    _ = remove_capture(test, nil)
    {:ok, state}
  end

  def handle_event(_event, map) do
    {:ok, map}
  end

  defp add_capture(%{group_leader: pid}, opts) do
    GenEvent.add_handler(Logger, {Logger.Backends.Capture, pid}, {pid, opts})
  end

  defp remove_capture(%{group_leader: pid}, flag) do
    GenEvent.remove_handler(Logger, {Logger.Backends.Capture, pid}, flag)
  end
end
