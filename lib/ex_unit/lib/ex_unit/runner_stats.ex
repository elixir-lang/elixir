# Small event consumer to handle runner statistics.
defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenEvent

  def init(_opts) do
    {:ok, %{total: 0, failures: 0, skipped: 0}}
  end

  def handle_call(:stop, map) do
    {:remove_handler, map}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {tag, _}}},
                   %{total: total, failures: failures} = map) when tag in [:failed, :invalid] do
    {:ok, %{map | total: total + 1, failures: failures + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:skip, _}}},
                   %{total: total, skipped: skipped} = map) do
    {:ok, %{map | total: total + 1, skipped: skipped + 1}}
  end

  def handle_event({:test_finished, _}, %{total: total} = map) do
    {:ok, %{map | total: total + 1}}
  end

  def handle_event(_, map) do
    {:ok, map}
  end
end
