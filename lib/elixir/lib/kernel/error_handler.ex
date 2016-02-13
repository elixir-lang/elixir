# Implement error_handler pattern for Erlang
# which is integrated with Kernel.ParallelCompiler
defmodule Kernel.ErrorHandler do
  @moduledoc false

  def undefined_function(module, fun, args) do
    ensure_loaded(module) or ensure_compiled(module, :module)
    :error_handler.undefined_function(module, fun, args)
  end

  def undefined_lambda(module, fun, args) do
    ensure_loaded(module) or ensure_compiled(module, :module)
    :error_handler.undefined_lambda(module, fun, args)
  end

  def ensure_loaded(module) do
    case :code.ensure_loaded(module) do
      {:module, _} -> true
      {:error, _} -> false
    end
  end

  # Never wait on nil because it should never be defined.
  def ensure_compiled(nil, _kind) do
    false
  end
  def ensure_compiled(module, kind) do
    parent = :erlang.get(:elixir_compiler_pid)
    ref    = :erlang.make_ref
    send parent, {:waiting, kind, self(), ref, module, current_module()}
    :erlang.garbage_collect(self)
    receive do
      {^ref, :found}     -> true
      {^ref, :not_found} -> false
    end
  end

  defp current_module do
    case :erlang.get(:elixir_compiler_module) do
      :undefined -> nil
      other -> other
    end
  end
end
