ExUnit.start [trace: "--trace" in System.argv]

defmodule ExUnit.TestsLoggingFormatter do
  @timeout 30_000
  @behaviour ExUnit.Formatter

  use GenServer.Behaviour

  def suite_started(opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, opts, [])
    pid
  end

  def suite_finished(id, _run_us, _load_us) do
    :gen_server.call(id, { :suite_finished }, @timeout)
  end

  def case_started(id, test_case) do
    :gen_server.cast(id, { :log, { :case_started, test_case } })
    :ok
  end

  def case_finished(id, test_case) do
    :gen_server.cast(id, { :log, { :case_finished, test_case } })
    :ok
  end

  def test_started(id, test) do
    :gen_server.cast(id, { :log, { :test_started, test } })
    :ok
  end

  def test_finished(id, test) do
    :gen_server.cast(id, { :log, { :test_finished, test } })
  end

  def init(_opts) do
    { :ok, [] }
  end

  def handle_call({ :suite_finished }, _from, logs) do
    { :stop, :normal, :lists.reverse(logs), logs }
  end

  def handle_cast({ :log, entry }, logs) do
    { :noreply, [ entry | logs ] }
  end
end
