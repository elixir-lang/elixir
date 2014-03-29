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

  def release() do
    # On release, no further allow elixir_ensure_compiled
    # directives and revert to the original error handler.
    # Note we should not delete the elixir_compiler_pid though,
    # as we still want to send notifications to the compiler.
    :erlang.erase(:elixir_ensure_compiled)
    :erlang.process_flag(:error_handler, :error_handler)
    :ok
  end

  defp ensure_loaded(module) do
    case Code.ensure_loaded(module) do
      { :module, _ } -> []
      { :error, _ } ->
        parent = :erlang.get(:elixir_compiler_pid)
        ref    = :erlang.make_ref
        send parent, { :waiting, module, self(), ref, module }
        :erlang.garbage_collect(self)
        receive do
          { ^ref, :ready }   -> :ok
          { ^ref, :release } -> release()
        end
    end
  end
end
