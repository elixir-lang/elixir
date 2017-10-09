Logger.configure_backend(:console, colors: [enabled: false])
ExUnit.start()

defmodule Logger.Case do
  use ExUnit.CaseTemplate
  import ExUnit.CaptureIO

  using _ do
    quote do
      import Logger.Case
    end
  end

  def msg(msg) do
    ~r/\d\d\:\d\d\:\d\d\.\d\d\d #{Regex.escape(msg)}/
  end

  def wait_for_handler(manager, handler) do
    unless handler in :gen_event.which_handlers(manager) do
      Process.sleep(10)
      wait_for_handler(manager, handler)
    end
  end

  def wait_for_logger() do
    try do
      :gen_event.which_handlers(Logger)
    catch
      :exit, _ ->
        Process.sleep(10)
        wait_for_logger()
    else
      _ ->
        :ok
    end
  end

  def capture_log(level \\ :debug, fun) do
    Logger.configure(level: level)

    capture_io(:user, fn ->
      fun.()
      Logger.flush()
    end)
  after
    Logger.configure(level: :debug)
  end
end
