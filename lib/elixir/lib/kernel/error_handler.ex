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
        parent = :erlang.get(:elixir_compiler_pid)
        ref    = :erlang.make_ref
        parent <- { :waiting, self(), ref, module }
        :erlang.garbage_collect(self)
        receive do
          { ^ref, :ready } ->
            :ok
          { ^ref, :release } ->
            # On release, get rid of the compiler pid,
            # no further allow elixir_ensure_compiled directives
            # and revert to the original error handler.
            :erlang.erase(:elixir_compiler_pid)
            :erlang.erase(:elixir_ensure_compiled)
            :erlang.process_flag(:error_handler, :error_handler)
        end
    end
  end
end
