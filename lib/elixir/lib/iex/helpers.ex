defmodule IEx.Helpers do
  @moduledoc """
  A bunch of helpers available in IEx.

  * `c` - compiles a file in the given path
  * `d` - prints documentation
  * `h` - prints history
  * `m` - prints loaded modules
  * `r` - recompiles and reloads the given module's source file
  * `v` - retrieves nth value from console

  Documentation for functions in this module can be consulted
  directly from the command line, as an example, try:

    d(:c, 1)

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
    tuples = Kernel.ParallelCompiler.files_to_path List.wrap(files), path
    Enum.map tuples, elem(&1, 1)
  end

  @doc """
  Returns the name and module of all modules loaded.
  """
  def m do
    all    = Enum.map :code.all_loaded, fn { mod, file } -> { inspect(mod), file } end
    sorted = List.sort(all)
    size   = Enum.reduce sorted, 0, fn({ mod, _ }, acc) -> max(byte_size(mod), acc) end
    format = "~-#{size}s ~s~n"

    Enum.each sorted, fn({ mod, file }) ->
      :io.format(format, [mod, file])
    end
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
  Shows the documentation for IEx.Helpers.
  """
  def d() do
    d(IEx.Helpers, :all)
  end

  @doc """
  Shows the documentation for the given module
  or for the given function/arity pair.

  ## Examples

      d(Enum)
      #=> Prints documentation for Enum

  It also accepts functions in the format `fun/arity`
  and `module.fun/arity`, for example:

      d receive/1
      d Enum.all?/2

  """
  defmacro d({ :/, _, [{ fun, _, nil }, arity] }) do
    quote do
      d(unquote(fun), unquote(arity))
    end
  end

  defmacro d({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      d(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro d(other) do
    quote do
      d(unquote(other), :all)
    end
  end

  @doc """
  Prints the documentation for the given function and arity.

  The function may either be a function defined inside `IEx.Helpers`
  or in `Kernel`. To see functions from other module, use
  `d/3` instead.

  ## Examples

      d(:d, 2)
      #=> Prints documentation for this function

  """
  def d(:d, 1) do
    d(__MODULE__, :d, 1)
  end

  def d(function, arity) when is_atom(function) and is_integer(arity) do
    if function_exported?(__MODULE__, function, arity) do
      d(__MODULE__, function, arity)
    else
      d(Kernel, function, arity)
    end
  end

  def d(module, :all) when is_atom(module) do
    case Code.ensure_loaded(module) do
      { :module, _ } ->
        case module.__info__(:moduledoc) do
          { _, binary } when is_binary(binary) ->
            IO.puts "# #{inspect module}\n"
            IO.write binary
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

  @doc """
  Reloads all modules that were already reloaded
  at some point with `r/1`.
  """
  def r do
    Enum.map iex_reloaded, r(&1)
  end

  @doc """
  Recompiles and reloads the specified module's source file.

  Please note that all the modules defined in the specified
  files are recompiled and reloaded.
  """
  def r(module) do
    if source = source(module) do
      Process.put(:iex_reloaded, :ordsets.add_element(module, iex_reloaded))
      { module, Code.load_file source }
    else
      :nosource
    end
  end

  defp iex_reloaded do
    Process.get(:iex_reloaded) || :ordsets.new
  end

  defp source(module) do
    compile = module.module_info(:compile)

    # Get the source of the compiled module. Due to a bug in Erlang
    # R15 and before, we need to look for the source first in the
    # options and then into the real source.
    options =
      case List.keyfind(compile, :options, 1) do
        { :options, opts } -> opts
        _ -> []
      end

    source = List.keyfind(options, :source, 1)  || List.keyfind(compile, :source, 1)

    case source do
      { :source, source } -> list_to_binary(source)
      _ -> nil
    end
  end
end
