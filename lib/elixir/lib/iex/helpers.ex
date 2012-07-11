defmodule IEx.Helpers do
  @moduledoc """
  A bunch of helpers available in IEx console.
  """

  @doc """
  Expects a list of files to compile and a path
  to write their object code to. It returns the name
  of the compiled modules.

  ## Examples

      c ["foo.ex"], "ebin"
      #=> Foo

  """
  def c(files, path // ".") do
    tuples = Elixir.ParallelCompiler.files_to_path List.wrap(files), path
    Enum.map tuples, elem(&1, 1)
  end

  @doc """
  Returns the name and module of all modules loaded.
  """
  def m do
    lc {mod, file} inlist List.sort(:code.all_loaded) do
      :io.format("~-20s ~s~n",[inspect(mod), file])
    end
    :ok
  end

  @doc """
  Prints the module information for the given module.
  """
  def m(mod) do
    IO.inspect mod.module_info
  end

  @doc """
  Prints the history
  """
  def h do
    history = List.reverse(Process.get(:__history__))
    lc {item, index} inlist List.zip(history,
                            :lists.seq(1,length(history))) do
      IO.puts "## #{index}:\n#{item[:code]}#=> #{inspect item[:result]}"
    end
    nil
  end

  @doc """
  Retrieves nth query's value from the history. Use negative
  values to lookup query's value from latest to earliest.
  For instance, v(-1) returns the latest result.
  """
  def v(n) when n < 0 do
    history = Process.get(:iex_history)
    config  = :lists.nth(abs(n), history)
    config.result
  end

  def v(n) do
    history = Process.get(:iex_history) /> List.reverse
    config  = :lists.nth(n, history)
    config.result
  end
end