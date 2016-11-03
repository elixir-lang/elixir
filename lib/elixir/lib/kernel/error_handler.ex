# Implement error_handler pattern for Erlang
# which is integrated with Kernel.ParallelCompiler
defmodule Kernel.ErrorHandler do
  @moduledoc false

  @spec undefined_function(module, atom, list) :: term
  def undefined_function(module, fun, args) do
    ensure_loaded(module) or ensure_compiled(module, :module)
    :error_handler.undefined_function(module, fun, args)
  end

  @spec undefined_lambda(module, fun, list) :: term
  def undefined_lambda(module, fun, args) do
    ensure_loaded(module) or ensure_compiled(module, :module)
    :error_handler.undefined_lambda(module, fun, args)
  end

  @spec ensure_loaded(module) :: boolean
  def ensure_loaded(module) do
    case :code.ensure_loaded(module) do
      {:module, _} -> true
      {:error, _} -> false
    end
  end

  @spec ensure_compiled(module, atom) :: boolean
  # Never wait on nil because it should never be defined.
  def ensure_compiled(nil, _kind) do
    false
  end

  def ensure_compiled(module, kind) do
    parent = :erlang.get(:elixir_compiler_pid)
    ref    = :erlang.make_ref
    send parent, {:waiting, kind, self(), ref, module, :elixir_module.compiler_modules()}
    :erlang.garbage_collect(self())
    receive do
      {^ref, :found}     -> true
      {^ref, :not_found} -> false
    end
  end
end
