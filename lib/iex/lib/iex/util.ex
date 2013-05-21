defmodule IEx.Util do
  @moduledoc false

  # Private functions used by several IEx.* modules.

  @doc false
  def print_exception(exception, stacktrace) do
    print_stacktrace stacktrace, fn ->
      "** (#{inspect exception.__record__(:name)}) #{exception.message}"
    end
  end

  @doc false
  def print_error(kind, reason, stacktrace) do
    print_stacktrace stacktrace, fn ->
      "** (#{kind}) #{inspect(reason)}"
    end
  end

  defp print_stacktrace(trace, callback) do
    try do
      io_error callback.()
      io_error Exception.format_stacktrace(trace)
    catch
      _, _ ->
        io_error "** (IEx.Error) error when printing exception message and stacktrace"
    end
  end

  defp io_error(result) do
    IO.puts :stdio, result
  end
end
