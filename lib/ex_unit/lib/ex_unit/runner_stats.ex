defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenServer
  alias ExUnit.{Manifest, Test, TestModule}

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
      manifest_file: opts[:manifest_file],
      old_manifest: nil,
      new_manifest: Manifest.new()
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
      |> Map.update!(:new_manifest, &Manifest.add_test(&1, test))
      |> Map.update!(:total, &(&1 + 1))
      |> increment_status_counter(test.state)

    {:noreply, state}
  end

  def handle_cast({:module_finished, %TestModule{state: {:failed, _}} = test_module}, state) do
    %{failures: failures, total: total} = state
    test_count = length(test_module.tests)
    {:noreply, %{state | failures: failures + test_count, total: total + test_count}}
  end

  def handle_cast({:suite_started, _opts}, %{old_manifest: nil, manifest_file: file} = state)
      when is_binary(file) do
    state = %{state | old_manifest: Manifest.read(file)}
    {:noreply, state}
  end

  def handle_cast({:suite_finished, _, _}, %{manifest_file: file} = state) when is_binary(file) do
    state.old_manifest
    |> Manifest.merge(state.new_manifest)
    |> Manifest.write!(file)

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
end
