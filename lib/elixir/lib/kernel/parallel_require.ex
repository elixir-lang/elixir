defmodule Kernel.ParallelRequire do
  # TODO: Deprecate on Elixir v1.8
  @moduledoc false

  def files(files, callbacks \\ [])

  def files(files, callback) when is_function(callback, 1) do
    files(files, [each_file: callback])
  end

  def files(files, options) when is_list(options) do
    case Kernel.ParallelCompiler.require(files, options) do
      {:ok, modules, _} -> modules
      {:error, _, _} -> exit({:shutdown, 1})
    end
  end
end
