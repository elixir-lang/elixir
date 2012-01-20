defmodule Elixir::Formatter do
  # Handle stacktrace for Erlang < R15
  def format_stacktrace({module, fun, arity}) do
    format_module_fun_arity(module, fun, arity)
  end

  # Handle stacktrace for Erlang >= R15
  def format_stacktrace({module, fun, arity, file_line}) do
    "#{format_file_line(file_line)}#{format_module_fun_arity(module, fun, arity)}"
  end

  def format_catch(:error, {:badsyntax, {line, filename, error, token}}) do
    "#{list_to_binary(filename)}:#{line}: #{list_to_binary(error)}#{format_token(token)}"
  end

  def format_catch(:error, {:badform, {line, filename, module, desc}}) do
    formatted = list_to_binary Erlang.elixir_errors.format_error(module, desc)
    "#{list_to_binary(filename)}:#{line}: #{formatted}"
  end

  def format_catch(_, reason) do
    inspect(reason)
  end

  ## Private
  @visibility :private

  def format_token([]),    do: ""
  def format_token(token), do: list_to_binary(token)

  def format_file_line(file_line) do
    if file = Orddict.get(file_line, :file) do
      file = list_to_binary(file)
      if line = Orddict.get(file_line, :line) do
        "#{file}:#{line}: "
      else:
        "#{file}: "
      end
    else:
      ""
    end
  end

  def format_module_fun_arity(module, fun, arity) do
    separator =
      case atom_to_list(module) do
      match: '::' ++ _
        "."
      else:
        ":"
      end

    if is_list(arity) do
      inspected = lc x in arity, do: inspect(x)
      "#{module}#{separator}#{fun}(#{Enum.join(inspected, ", ")})"
    else:
      "#{module}#{separator}#{fun}/#{arity}"
    end
  end
end