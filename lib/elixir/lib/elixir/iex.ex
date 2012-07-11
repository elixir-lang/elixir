defmodule Elixir.IEx.UnicodeIO do
  @moduledoc """
  This module implements the API used by IEX to
  interact with the console. This API may change
  in the future without warnings.
  """

  @doc """
  Implements the get IO API used by IEx. It receives the
  code cache, the instructions counter and needs to
  return a list with the new characters inserted.
  """
  def get(cache, count) do
    prompt = case cache do
      [] -> "iex(#{count})> "
      _ -> "...> "
    end
    :unicode.characters_to_list(IO.gets(prompt))
  end

  @doc """
  Implements the put IO API used by IEx. It receives the
  result and prints it.
  """
  def put(result) do
    IO.inspect result
  end

  @doc """
  Implements the error IO API used by IEx. It prints error
  messages.
  """
  def error(result) do
    IO.puts :standard_error, result
  end
end

defrecord Elixir.IEx.Config, io: nil, binding: nil, cache: '', counter: 0, scope: nil

defmodule Elixir.IEx do
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
  def start(binding // [], io // Elixir.IEx.UnicodeIO) do
    if System.find_executable("tty") do
      tty(binding, io)
    else
      simple(binding, io)
    end
  end

  @doc """
  Starts IEx using a tty server.
  """
  def tty(binding // [], io // Elixir.IEx.UnicodeIO) do
    config   = boot_config(binding, io)

    function = fn ->
      # We are inside the new tty and in a new process,
      # reattach it the error logger.
      attach_error_logger
      do_loop(config)
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
  def simple(binding // [], io // Elixir.IEx.UnicodeIO) do
    config = boot_config(binding, io)
    do_loop(config)
  end

  ## Helpers

  defp boot_config(binding, io) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit"

    scope  = Erlang.elixir.scope_for_eval(
      file: 'iex',
      delegate_locals_to: Elixir.IEx.Helpers
    )

    Elixir.IEx.Config.new(io: io, binding: binding, scope: scope)
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

  defp do_loop(config, history // []) do
    io = config.io

    config  = config.increment_counter
    counter = config.counter
    cache   = config.cache
    code    = cache ++ io.get(cache, counter)

    {new_config, history} =
      try do
        Process.put :__history__, history
        { result, new_binding, scope } =
          Erlang.elixir.eval(code, config.binding, counter, config.scope)
        io.put result
        {config.binding(new_binding).cache('').scope(scope), 
         [[code: code, result: result, binding: new_binding, scope: scope]|history]}
      rescue
        TokenMissingError ->
          config.cache(code)
        exception ->
          stacktrace = System.stacktrace
          io.error "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          print_stacktrace io, stacktrace
          {config.cache(''), history}
      catch
        kind, error ->
          stacktrace = System.stacktrace
          io.error "** (#{kind}) #{inspect(error)}"
          print_stacktrace io, stacktrace
          {config.cache(''), history}
      end

    do_loop(new_config, history)
  end

  defp print_stacktrace(io, stacktrace) do
    Enum.each stacktrace, fn s -> io.error "    #{format_stacktrace(s)}" end
  end
end

defmodule Elixir.IEx.Helpers do
  @moduledoc """
  A bunch of helpers available in IEx console.
  """

  @doc """
  Expects a list of files to compile and a path
  to write their object code to. It returns the name
  of the compiled modules.

  ## Examples

      c ["foo.ex"], "ebin"
      #=> Foo

  """
  def c(files, path // ".") do
    tuples = Elixir.ParallelCompiler.files_to_path List.wrap(files), path
    Enum.map tuples, elem(&1, 1)
  end

  @doc """
  Returns the name and module of all modules loaded.
  """
  def m do
    lc {mod, file} inlist List.sort(:code.all_loaded) do
      :io.format("~-20s ~s~n",[inspect(mod), file])
    end
    :ok
  end

  @doc """
  Prints the module information for the given module.
  """
  def m(mod) do
    IO.inspect mod.module_info
  end

  @doc """
  Prints the history
  """
  def h do
    history = List.reverse(Process.get(:__history__))
    lc {item, index} inlist List.zip(history,
                            :lists.seq(1,length(history))) do
      IO.puts "## #{index}:\n#{item[:code]}#=> #{inspect item[:result]}"
    end
    nil
  end

  @doc """
  Retrieves Nth query's value from the history
  """
  def v(n) when n < 0 do
    history_length = length(Process.get(:__history__))
    v(history_length + n + 1)
  end
  def v(n) do
    history = List.reverse(Process.get(:__history__))
    item = :lists.nth(n, history)
    item[:result]
  end
end