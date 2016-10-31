# Implement error_handler pattern for Erlang
# which is integrated with Kernel.ParallelCompiler
defmodule Kernel.ErrorHandler do
  @moduledoc false

  @spec undefined_function(module, atom, list) :: term
  def undefined_function(module, fun, args)
      when is_atom(module) and is_atom(fun) and is_list(args) do
    ensure_loaded(module) or ensure_compiled(module, :module)
    :error_handler.undefined_function(module, fun, args)
  end

  @spec undefined_function(module, fun, list) :: term
  def undefined_lambda(module, fun, args)
      when is_atom(module) and is_function(fun) and is_list(args) do
    ensure_loaded(module) or ensure_compiled(module, :module)
    :error_handler.undefined_lambda(module, fun, args)
  end

  @spec ensure_loaded(module) :: boolean
  def ensure_loaded(module) when is_atom(module) do
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

  def ensure_compiled(module, kind) when is_atom(module) and is_atom(kind) do
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
