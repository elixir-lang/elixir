defmodule IEx.Helpers do
  @moduledoc """
  A bunch of helpers available in IEx.

  Documentation for functions in this module can be
  consulted directly from the command line, example:

      d(:c, 2)

  Will print the documentation for the function `c`
  in this module with arity 2.
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
  Prints commands history and their result.
  """
  def h do
    history = List.reverse(Process.get(:iex_history))
    Enum.each(history, print_history(&1))
  end

  defp print_history(config) do
    IO.puts "#{config.counter}: #{config.cache}#=> #{inspect config.result}\n"
  end

  @doc """
  Shows the documentation for the given module.
  Defaults to print documentation for `IEx.Helpers`.

  ## Examples

      d(Enum)
      #=> Prints documentation for Enum

  """
  def d(module // IEx.Helpers) do
    d(module, true)
  end

  @doc """
  Prints the documentation for the given function and arity.

  The function may either be a function defined inside `IEx.Helpers`
  or in `Elixir.Builtin`. To see functions from other module, use
  `d/3` instead.

  ## Examples

      d(:d, 2)
      #=> Prints documentation for this function

  """
  def d(function, arity) when is_atom(function) and is_integer(arity) do
    if :erlang.function_exported(__MODULE__, function, arity) do
      d(__MODULE__, function, arity)
    else
      d(Elixir.Builtin, function, arity)
    end
  end

  def d(module, print_functions) when is_atom(module) and is_boolean(print_functions) do
    case Code.ensure_loaded(module) do
      { :module, _ } ->
        case module.__info__(:moduledoc) do
          { _, binary } when is_binary(binary) ->
            IO.puts "# #{inspect module}\n"
            IO.write binary
            if print_functions do
              IO.puts "\n## Functions and Macros\n"
              Enum.each module.__info__(:docs), print_signature(&1)
            end
          { _, _ } ->
            IO.puts :stderr, "No docs for #{inspect module}"
          _ ->
            IO.puts :stderr, "#{inspect module} was not compiled with docs"
        end
      { :error, reason } ->
        IO.puts :stderr, "Could not load module #{inspect module}: #{reason}"
    end
  end

  @doc """
  Shows the documentation for the `function/arity` in `module`.
  """
  def d(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    if docs = module.__info__(:docs) do
      doc =
        if tuple = List.keyfind(docs, { function, arity }, 1) do
          print_signature(tuple)
        end

      if doc do
        IO.write "\n" <> doc
      else
        IO.puts :stderr, "No docs for #{function}/#{arity}"
      end
    else
      IO.puts :stderr, "#{inspect module} was not compiled with docs"
    end
  end

  # Get the full signature from a function.
  defp print_signature({ _info, _line, _kind, _args, false }) do
    false
  end

  defp print_signature({ { name, _arity }, _line, kind, args, docs }) do
    args = Enum.map_join(args, ", ", signature_arg(&1))
    IO.puts "* #{kind} #{name}(#{args})"
    docs
  end

  defp signature_arg({ ://, _, [left, right] }) do
    signature_arg(left) <> " // " <> Macro.to_binary(right)
  end

  defp signature_arg({ var, _, _ }) do
    atom_to_binary(var)
  end

  @doc """
  Retrieves nth query's value from the history. Use negative
  values to lookup query's value from latest to earliest.
  For instance, v(-1) returns the latest result.
  """
  def v(n) when n < 0 do
    history = Process.get(:iex_history)
    Enum.nth!(history, abs(n)).result
  end

  def v(n) do
    history = Process.get(:iex_history) /> List.reverse
    Enum.nth!(history, n).result
  end
end