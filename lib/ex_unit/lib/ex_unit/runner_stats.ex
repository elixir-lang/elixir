defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenServer
  alias ExUnit.{Test, TestModule}

  def init(_opts) do
    {:ok, %{total: 0, failures: 0, skipped: 0, failure_details: []}}
  end

  def stats(pid) do
    GenServer.call(pid, :stats, :infinity)
  end

  def handle_call(:stats, _from, map) do
    {:reply, map, map}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {tag, _}} = test}, map)
      when tag in [:failed, :invalid] do
    %{total: total, failures: failures, failure_details: failure_details} = map

    failure_details =
      case tag do
        :failed -> failure_details ++ [failure_details(test, failures + 1)]
        :invalid -> failure_details
      end

    {:noreply, %{map | total: total + 1, failures: failures + 1, failure_details: failure_details}}
  end

  def handle_cast({:test_finished, %Test{state: {:skip, _}}}, map) do
    %{total: total, skipped: skipped} = map
    {:noreply, %{map | total: total + 1, skipped: skipped + 1}}
  end

  def handle_cast({:test_finished, _}, %{total: total} = map) do
    {:noreply, %{map | total: total + 1}}
  end

  def handle_cast({:module_finished, %TestModule{state: {:failed, _}} = test_module}, map) do
    %{failures: failures, total: total} = map
    test_count = length(test_module.tests)
    {:noreply, %{map | failures: failures + test_count, total: total + test_count}}
  end

  def handle_cast(_, map) do
    {:noreply, map}
  end

  defp failure_details(%ExUnit.Test{state: {:failed, failures}} = test, counter) do
    message =
      ExUnit.Formatter.format_test_failure(test, failures, counter, :infinity, &formatter/2)
      |> String.trim()

    {file, line} =
      case failure_location(test) do
        nil -> {test.tags[:file], test.tags[:line]}
        res -> res
      end

    {file, line, message}
  end

  # If the stack is only one frame, we can assume the line number accurately reflects the assertion
  # that failed
  defp failure_location(%ExUnit.Test{
         tags: %{type: :test},
         state: {:failed, [{:error, _, [stack_frame]}]}
       }) do
    stack_frame_location(stack_frame)
  end

  defp failure_location(%ExUnit.Test{
         tags: %{type: :doctest},
         state: {:failed, [{:error, _, stacktrace}]}
       })
       when is_list(stacktrace) do
    stack_frame_location(List.last(stacktrace))
  end

  defp failure_location(_) do
    nil
  end

  defp stack_frame_location({_, _, _, meta}) when is_list(meta) do
    case {Keyword.get(meta, :file), Keyword.get(meta, :line)} do
      {'(for doctest at) ' ++ file, line} when is_integer(line) ->
        {Path.absname(file), line}

      {file, line} when (is_binary(file) or is_list(file)) and is_integer(line) ->
        {Path.absname(file), line}

      _ ->
        nil
    end
  end

  defp stack_frame_location(_) do
    nil
  end

  defp formatter(_, msg), do: msg
end
