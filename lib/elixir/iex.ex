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
  def get(cache, _count) do
    prompt = case cache do
      [] -> "iex> "
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
  This module implements interactive Elixir. It provides two
  main functions, `start` and `simple_start`. `start` was
  meant for systems where tty is available and relies on
  it in order to work properly. This makes all control commands
  available in tty available to the developer.

  In case `tty` is not available (for example, Windows), a
  developer may invoke `simple_start` which starts a stripped
  down version.
  """

  import Exception, only: [format_stacktrace: 1]

  @doc """
  Starts IEx using a tty server. It requires the initial
  binding an the IO mechanism as argument.
  """
  def start(binding // [], io // Elixir.IEx.UnicodeIO) do
    config = boot_config(binding, io)
    function = fn ->
      :error_logger.delete_report_handler(:error_logger_tty_h)
      :error_logger.add_report_handler(:error_logger_tty_h)
      do_loop(config)
    end
    if is_pid(Process.whereis(:user)), do: Process.unregister :user
    Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
  end

  @doc """
  Starts IEx simply using stdio. It requires the initial
  binding an the IO mechanism as argument.
  """
  def simple_start(binding // [], io // Elixir.IEx.UnicodeIO) do
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

  defp do_loop(config) do
    io = config.io

    config  = config.increment_counter
    counter = config.counter
    cache   = config.cache
    code    = cache ++ io.get(cache, counter)

    new_config =
      try do
        { result, new_binding, scope } =
          Erlang.elixir.eval(code, config.binding, counter, config.scope)
        io.put result
        config.binding(new_binding).cache('').scope(scope)
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
end