# Implement error_handler pattern for Erlang
# which is integrated with Kernel.ParallelCompiler
defmodule Kernel.ErrorHandler do
  @moduledoc false

  def undefined_function(module, fun, args) do
    ensure_loaded(module)
    :error_handler.undefined_function(module, fun, args)
  end

  def undefined_lambda(module, fun, args) do
    ensure_loaded(module)
    :error_handler.undefined_lambda(module, fun, args)
  end

  defp ensure_loaded(module) do
    case Code.ensure_loaded(module) do
      { :module, _ } -> []
      { :error, _ } ->
        parent = Process.get(:elixir_compiler_pid)
        parent <- { :waiting, self(), module }
        receive do
          { :release, ^parent } -> :ok
        end
    end
  end
end
