defmodule Elixir.IEx.UnicodeIO do
  @moduledoc false

  @doc """
  Implements the get IO API used by IEX. It receives the
  code cache, the instructions counter and needs to
  return a list with the new characters inserted.
  """
  def get(cache, _count) do
    prompt = case cache do
    match: []
      "iex> "
    match: _
      "...> "
    end
    binary_to_list(:unicode.characters_to_binary(Erlang.io.get_line(prompt)))
  end

  @doc """
  Implements the put IO API used by IEX. It receives the
  result and prints it.
  """
  def put(result) do
    IO.inspect result
  end
end

defrecord Elixir.IEx.Config, io: nil, binding: nil, cache: '', counter: 0, scope: nil

defmodule Elixir.IEx do
  import Exception, only: [format_stacktrace: 1]

  def start(binding // [], io // Elixir.IEx.UnicodeIO) do
    IO.puts "Interactive Elixir (#{Code.version}) - press Ctrl+C to exit"
    config = Elixir.IEx.Config.new(io: io, binding: binding, scope: Erlang.elixir.scope_for_eval)
    function = fn -> do_loop(config) end
    Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
  end

  defp do_loop(config) do
    config  = config.increment_counter
    counter = config.counter
    cache   = config.cache
    code    = cache ++ config.io.get(cache, counter)

    new_config =
      try do
        { result, new_binding, scope } =
          Erlang.elixir.eval(code, config.binding, "iex", counter, config.scope)
        IO.puts inspect(result)
        config.binding(new_binding).cache('').scope(scope)
      rescue: TokenMissingError
        config.cache(code)
      rescue: exception
        stacktrace = Code.stacktrace
        IO.puts :standard_error, "** (#{exception.__record__(:name)}) #{exception.message}"
        print_stacktrace stacktrace
        config.cache('')
      catch: kind, error
        stacktrace = Code.stacktrace
        IO.puts :standard_error, "** (#{kind}) #{inspect(error)}"
        print_stacktrace stacktrace
        config.cache('')
      end

    do_loop(new_config)
  end

  defp print_stacktrace(stacktrace) do
    Enum.each stacktrace, fn(s, do: IO.puts :standard_error, "    #{format_stacktrace(s)}")
  end
end