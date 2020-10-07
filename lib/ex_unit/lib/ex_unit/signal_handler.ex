defmodule ExUnit.SignalHandler do
  @moduledoc false
  @behaviour :gen_event

  @doc false
  def install(runner) do
    :gen_event.swap_handler(:erl_signal_server, {:erl_signal_handler, []}, {__MODULE__, runner})
  end

  @doc false
  def uninstall(runner) do
    :gen_event.swap_handler(:erl_signal_server, {__MODULE__, runner}, {:erl_signal_handler, []})
  end

  @impl true
  def init({runner, _}) do
    {:ok, runner}
  end

  @impl true
  def handle_call(_message, runner) do
    {:ok, :ok, runner}
  end

  @impl true
  def handle_event(:sigusr1, runner) do
    :erlang.halt('Received SIGUSR1')
    {:ok, runner}
  end

  def handle_event(:sigquit, runner) do
    ref = Process.monitor(runner)
    send(runner, {ref, self(), :sigquit})

    receive do
      ^ref -> :ok
      {:DOWN, ^ref, _, _, _} -> :ok
    after
      5_000 -> :ok
    end

    Process.demonitor(ref, [:flush])
    :erlang.halt()
    {:ok, runner}
  end

  def handle_event(:sigterm, runner) do
    :error_logger.info_msg('SIGTERM received - shutting down~n')
    :ok = System.stop()
    {:ok, runner}
  end
end
