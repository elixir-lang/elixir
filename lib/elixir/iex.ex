defmodule Elixir.IEx.UnicodeIO do
  @moduledoc false

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
    :unicode.characters_to_list(Erlang.io.get_line(prompt))
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
  @moduledoc false

  import Exception, only: [format_stacktrace: 1]

  def start(binding // [], io // Elixir.IEx.UnicodeIO) do
    config = boot_config(binding, io)
    function = fn -> do_loop(config) end
    Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
  end

  def simple_start(binding // [], io // Elixir.IEx.UnicodeIO) do
    config = boot_config(binding, io)
    do_loop(config)
  end

  ## Helpers

  defp boot_config(binding, io) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit"

    scope  = Erlang.elixir.scope_for_eval(
      file: 'iex',
      delegate_locals_to: __MODULE__
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