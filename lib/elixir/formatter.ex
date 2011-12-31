module Elixir::Formatter do
  def format_stacktrace({module, fun, arity}) do
    if is_list(arity) do
      "#{_inspect(module)}##{_inspect(fun)}(#{_inspect(arity)})"
    else:
      "#{_inspect(module)}##{_inspect(fun)}/#{_inspect(arity)}"
    end
  end

  def format_catch(:error, {:badsyntax, {line, filename, error, token}}) do
    "#{list_to_binary(filename)}:#{_inspect(line)}: #{list_to_binary(error)}#{format_token token}"
  end

  def format_catch(:error, {:badform, {line, filename, module, desc}}) do
    formatted = list_to_binary Erlang.elixir_errors.format_error(module, desc)
    "#{list_to_binary(filename)}:#{_inspect(line)}: #{formatted}"
  end

  def format_catch(_, reason) do
    _inspect(reason)
  end

  def _inspect(data) when is_atom(data) do
    atom_to_binary(data, :utf8)
  end

  def _inspect(data) when is_binary(data) do
    "\"#{data}\""
  end

  def _inspect(data) when is_integer(data) do
    list_to_binary integer_to_list(data)
  end

  def _inspect(data) when is_float(data) do
    list_to_binary float_to_list(data)
  end

  def _inspect([]) do
    "[]"
  end

  def _inspect(data) when is_list(data) do
    if Erlang.io_lib.printable_list(data) do
      "'#{list_to_binary List.flatten(data)}'"
    else:
      list_to_binary :io_lib.format("~p", [data])
    end
  end

  def _inspect(data) do
    list_to_binary :io_lib.format("~p", [data])
  end

  ## Private

  defp format_token([]),    do: ""
  defp format_token(token), do: list_to_binary(token)
end