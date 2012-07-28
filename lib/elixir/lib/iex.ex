defrecord IEx.Config, io: nil, binding: nil, cache: '', counter: 1, scope: nil, result: nil

defmodule IEx do
  @moduledoc """
  This module implements interactive Elixir. It provides a main
  function, `start` which will either delegate to `tty` or `simple`.
  The former is meant for systems where tty is available and relies
  on it in order to work properly. This makes all control commands
  available in tty available to the developer.

  In case `tty` is not available (for example, Windows), a
  developer may invoke `simple` which starts a stripped
  down version.
  """

  import Exception, only: [format_stacktrace: 1]

  @doc """
  Starts IEx checking if tty is available or not.
  If so, invoke tty, otherwise go with the simple iex.
  """
  def start(binding // [], io // IEx.UnicodeIO) do
    type = elem(:os.type, 1)

    if type == :unix && System.find_executable("tty") do
      tty(binding, io)
    else
      simple(binding, io)
    end
  end

  @doc """
  Starts IEx using a tty server.
  """
  def tty(binding // [], io // IEx.UnicodeIO) do
    config   = boot_config(binding, io)

    function = fn ->
      # We are inside the new tty and in a new process,
      # reattach it the error logger.
      attach_error_logger
      :io.setopts :erlang.group_leader,
                  [{:expand_fun, IEx.Autocomplete.expand &1}]
      start_loop(config)
    end

    # Dettach the error logger because we are going to unregister
    # the user process and start a new tty which will get control
    # over the standardio. Dettaching it here allows us to get rid
    # of warnings. We reattach it again when we get the new tty.
    dettach_error_logger

    # Unregister the user process, user_drv command below
    # will register the new one.
    unregister_user_process

    # Close the default io port, user_drv start command below
    # will take control over the io.
    close_io_port

    Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
  end

  @doc """
  Starts IEx simply using stdio. It requires the initial
  binding an the IO mechanism as argument.
  """
  def simple(binding // [], io // IEx.UnicodeIO) do
    config = boot_config(binding, io)
    start_loop(config)
  end

  ## Boot Helpers

  defp boot_config(binding, io) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit"

    scope  = Erlang.elixir.scope_for_eval(
      file: 'iex',
      delegate_locals_to: IEx.Helpers
    )

    IEx.Config.new(io: io, binding: binding, scope: scope)
  end

  defp dettach_error_logger do
    :error_logger.delete_report_handler(:error_logger_tty_h)
  end

  defp attach_error_logger do
    :error_logger.add_report_handler(:error_logger_tty_h)
  end

  defp unregister_user_process do
    if is_pid(Process.whereis(:user)), do: Process.unregister :user
  end

  defp close_io_port do
    if port = Enum.find(Port.list, io_port?(&1)) do
      Port.close(port)
    end
  end

  defp io_port?(port) do
    Port.info(port, :name) == {:name,'0/1'} && port
  end

  ## Loop helpers

  defp start_loop(config) do
    Process.put :iex_history, []
    { _, _, scope } = Erlang.elixir.eval('import IEx.Helpers', [], 0, config.scope)
    do_loop(config.scope(scope))
  end

  defp do_loop(config) do
    io = config.io

    counter = config.counter
    cache   = config.cache
    code    = cache ++ io.get(config)

    new_config =
      try do
        { result, new_binding, scope } =
          Erlang.elixir.eval(code, config.binding, counter, config.scope)

        io.put result

        config = config.result(result)
        update_history(config.cache(code).scope(nil))
        config.increment_counter.cache('').binding(new_binding).scope(scope)
      rescue
        TokenMissingError ->
          config.cache(code)
        exception ->
          stacktrace = System.stacktrace
          io.error "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          print_stacktrace io, stacktrace
          config.cache('')
      catch
        kind, error ->
          stacktrace = System.stacktrace
          io.error "** (#{kind}) #{inspect(error)}"
          print_stacktrace io, stacktrace
          config.cache('')
      end

    do_loop(new_config)
  end

  defp update_history(config) do
    current = Process.get :iex_history
    Process.put :iex_history, [config|current]
  end

  defp print_stacktrace(io, stacktrace) do
    Enum.each stacktrace, fn s -> io.error "    #{format_stacktrace(s)}" end
  end
end

