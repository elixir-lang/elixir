defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenServer
  alias ExUnit.{FailuresManifest, Test}

  @typep counter :: non_neg_integer

  @spec stats(pid) :: %{
          excluded: counter,
          failures: counter,
          skipped: counter,
          total: counter
        }
  def stats(pid) when is_pid(pid) do
    GenServer.call(pid, :stats, :infinity)
  end

  @spec get_failure_counter(pid) :: counter
  def get_failure_counter(sup) when is_pid(sup) do
    GenServer.call(sup, :get_failure_counter)
  end

  @spec increment_failure_counter(pid) :: pos_integer
  def increment_failure_counter(sup, increment \\ 1)
      when is_pid(sup) and is_integer(increment) and increment >= 1 do
    GenServer.call(sup, {:increment_failure_counter, increment})
  end

  # Callbacks

  def init(opts) do
    state = %{
      total: 0,
      passed: 0,
      failures: 0,
      skipped: 0,
      excluded: 0,
      failures_manifest_file: opts[:failures_manifest_file],
      failures_manifest: FailuresManifest.new(),
      failure_counter: 0,
      pids: []
    }

    {:ok, state}
  end

  def handle_call(:stats, _from, state) do
    stats = Map.take(state, [:total, :failures, :skipped, :excluded])
    {:reply, stats, state}
  end

  def handle_call(:get_failure_counter, _from, state) do
    {:reply, state.failure_counter, state}
  end

  def handle_call({:increment_failure_counter, increment}, _from, state) do
    %{failure_counter: failure_counter} = state
    {:reply, failure_counter, %{state | failure_counter: failure_counter + increment}}
  end

  def handle_cast({:test_finished, %Test{} = test}, state) do
    state =
      state
      |> Map.update!(:failures_manifest, &FailuresManifest.put_test(&1, test))
      |> Map.update!(:total, &(&1 + 1))
      |> increment_status_counter(test.state)

    {:noreply, state}
  end

  def handle_cast({:suite_started, _opts}, %{failures_manifest_file: file} = state)
      when is_binary(file) do
    state = %{state | failures_manifest: FailuresManifest.read(file)}
    {:noreply, state}
  end

  def handle_cast({:suite_finished, _}, %{failures_manifest_file: file} = state)
      when is_binary(file) do
    FailuresManifest.write!(state.failures_manifest, file)
    {:noreply, state}
  end

  def handle_cast(
        {:module_finished, %ExUnit.TestModule{state: {:failed, _}} = test_module},
        state
      ) do
    # The failed tests have already been added to the manifest,
    # so we should only invalidate the successful tests
    successful_tests = Enum.filter(test_module.tests, &is_nil(&1.state))

    state =
      Enum.reduce(successful_tests, state, fn test, acc ->
        acc
        |> Map.update!(:failures_manifest, &FailuresManifest.put_test(&1, test))
        |> Map.update!(:failures, &(&1 + 1))
        |> Map.update!(:passed, &(&1 - 1))
      end)

    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  defp increment_status_counter(state, tag) when tag in [nil, :passed] do
    Map.update!(state, :passed, &(&1 + 1))
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
end
