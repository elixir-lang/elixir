defmodule IEx.Loop do
  @moduledoc false

  def start(config) do
    Process.put :iex_history, []
    { _, _, scope } = :elixir.eval('import IEx.Helpers', [], 0, config.scope)
    do_loop(config.scope(scope))
  end

  defp do_loop(config) do
    counter = config.counter
    cache   = config.cache
    code    = cache ++ io_get(config)

    new_config =
      try do
        { result, new_binding, scope } =
          :elixir.eval(code, config.binding, counter, config.scope)

        io_put result

        config = config.result(result)
        update_history(config.cache(code).scope(nil))
        config.increment_counter.cache('').binding(new_binding).scope(scope)
      rescue
        TokenMissingError ->
          config.cache(code)
        exception ->
          trace = System.stacktrace
          io_error "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          io_error Exception.formatted_stacktrace(trace)
          config.cache('')
      catch
        kind, error ->
          trace = System.stacktrace
          io_error "** (#{kind}) #{inspect(error)}"
          io_error Exception.formatted_stacktrace(trace)
          config.cache('')
      end

    do_loop(new_config)
  end

  defp update_history(config) do
    current = Process.get :iex_history
    Process.put :iex_history, [config|current]
  end

  defp io_get(config) do
    prefix = if config.cache != [], do: "..."

    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{config.counter}> "
      else
        "#{prefix || "iex"}(#{config.counter})> "
      end

    case IO.gets(:stdio, prompt) do
      { :error, _ } -> ''
      data -> :unicode.characters_to_list(data)
    end
  end

  defp io_put(result) do
    IO.inspect :stdio, result, IEx.inspect_opts
  end

  defp io_error(result) do
    IO.puts :stdio, result
  end

  defp remote_prefix do
    if node == node(:erlang.group_leader), do: "iex", else: "rem"
  end
end