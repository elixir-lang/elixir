defmodule ExUnit.EventManager do
  @moduledoc false
  @timeout 30_000

  # TODO: Remove support for GenEvent formatters on 2.0

  @doc """
  Starts an event manager that publishes events during the suite run.

  This is what power formatters as well as the
  internal statistics server for ExUnit.
  """
  def start_link() do
    import Supervisor.Spec
    child = worker(GenServer, [], restart: :temporary)
    {:ok, sup} = Supervisor.start_link([child], strategy: :simple_one_for_one)
    {:ok, event} = :gen_event.start_link()
    {:ok, {sup, event}}
  end

  def stop({sup, event}) do
    for {_, pid, _, _} <- Supervisor.which_children(sup) do
      GenServer.stop(pid, :normal, @timeout)
    end
    Supervisor.stop(sup)
    :gen_event.stop(event)
  end

  def add_handler({sup, event}, handler, opts) do
    if Code.ensure_loaded?(handler) and function_exported?(handler, :handle_call, 2) do
      IO.warn "passing GenEvent handlers (#{inspect(handler)} in this case) in " <>
              "the :formatters option of ExUnit is deprecated, please pass a " <>
              "GenServer instead. Check the documentation for the ExUnit.Formatter " <>
              "module for more information"
      :gen_event.add_handler(event, handler, opts)
    else
      Supervisor.start_child(sup, [handler, opts])
    end
  end

  def suite_started(ref, opts) do
    notify(ref, {:suite_started, opts})
  end

  def suite_finished(ref, run_us, load_us) do
    notify(ref, {:suite_finished, run_us, load_us})
  end

  def case_started(ref, test_case) do
    notify(ref, {:case_started, test_case})
  end

  def case_finished(ref, test_case) do
    notify(ref, {:case_finished, test_case})
  end

  def test_started(ref, test) do
    notify(ref, {:test_started, test})
  end

  def test_finished(ref, test) do
    notify(ref, {:test_finished, test})
  end

  defp notify({sup, event}, msg) do
    :gen_event.notify(event, msg)
    for {_, pid, _, _} <- Supervisor.which_children(sup) do
      GenServer.cast(pid, msg)
    end
    :ok
  end
end
