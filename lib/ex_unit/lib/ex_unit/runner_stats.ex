defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenServer
  alias ExUnit.{FailuresManifest, Test, TestModule}

  def stats(pid) do
    GenServer.call(pid, :stats, :infinity)
  end

  # Callbacks

  def init(opts) do
    state = %{
      total: 0,
      failures: 0,
      skipped: 0,
      excluded: 0,
      max_failures: opts[:max_failures],
      failures_manifest_file: opts[:failures_manifest_file],
      failures_manifest: FailuresManifest.new()
    }

    {:ok, state}
  end

  def handle_call(:stats, _from, state) do
    stats = Map.take(state, [:total, :failures, :skipped, :excluded])
    {:reply, stats, state}
  end

  def handle_cast({:test_finished, %Test{} = test}, state) do
    state =
      state
      |> Map.update!(:failures_manifest, &FailuresManifest.put_test(&1, test))
      |> Map.update!(:total, &(&1 + 1))
      |> increment_status_counter(test.state)
      |> handle_max_failures

    {:noreply, state}
  end

  def handle_cast({:module_finished, %TestModule{state: {:failed, _}} = test_module}, state) do
    %{failures: failures, total: total} = state
    test_count = length(test_module.tests)
    {:noreply, %{state | failures: failures + test_count, total: total + test_count}}
  end

  def handle_cast({:suite_started, _opts}, %{failures_manifest_file: file} = state)
      when is_binary(file) do
    state = %{state | failures_manifest: FailuresManifest.read(file)}
    {:noreply, state}
  end

  def handle_cast({:suite_finished, _, _}, %{failures_manifest_file: file} = state)
      when is_binary(file) do
    FailuresManifest.write!(state.failures_manifest, file)
    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  defp increment_status_counter(state, {:skipped, _}) do
    Map.update!(state, :skipped, &(&1 + 1))
  end

  defp increment_status_counter(state, {:excluded, _}) do
    Map.update!(state, :excluded, &(&1 + 1))
  end

  defp increment_status_counter(state, {tag, _}) when tag in [:failed, :invalid] do
    Map.update!(state, :failures, &(&1 + 1))
  end

  defp increment_status_counter(state, _), do: state

  defp handle_max_failures(state) do
    if(state.failures >= state.max_failures) do
      IO.puts("STOP TESTING")
    end
    state
  end
end
