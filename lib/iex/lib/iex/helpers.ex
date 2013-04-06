defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module IEx.Helpers
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper
  `h()`, usually referred as `h/0` (since it expects 0
  arguments).

  There are many other helpers available:

  * `c/2` - compiles a file in the given path
  * `cd/1` - changes the current directory
  * `flush/0` — flush all messages sent to the shell
  * `h/0`, `h/1` - prints help/documentation
  * `l/1` - loads given module beam code by purging the current version
  * `m/0` - prints loaded modules
  * `pwd/0` - prints the current working directory
  * `r/0`, `r/1` - recompiles and reloads the given module's source file
  * `s/1` — prints spec information
  * `t/1` — prints type information
  * `v/0` - prints all commands and values
  * `v/1` - retrieves nth value from console

  Help for functions in this module can be consulted
  directly from the command line, as an example, try:

      h(c/2)

  You can also retrieve the documentation for any module
  or function. Try these:

      h(Enum)
      h(Enum.reverse/1)

  """

  @doc """
  Expects a list of files to compile and a path
  to write their object code to. It returns the name
  of the compiled modules.

  ## Examples

      c ["foo.ex"], "ebin"
      #=> [Foo]

  """
  def c(files, path // ".") do
    tuples = Kernel.ParallelCompiler.files_to_path List.wrap(files), path
    Enum.map tuples, elem(&1, 0)
  end

  @doc """
  Returns the name and module of all modules loaded.
  """
  def m do
    all    = Enum.map :code.all_loaded, fn { mod, file } -> { inspect(mod), file } end
    sorted = Enum.sort all
    size   = Enum.reduce sorted, 0, fn({ mod, _ }, acc) -> max(byte_size(mod), acc) end
    format = "~-#{size}s ~ts~n"

    Enum.each sorted, fn({ mod, file }) ->
      :io.format(format, [mod, file])
    end
  end

  @doc """
  Prints commands history and their result.
  """
  def v do
    history = Enum.reverse(Process.get(:iex_history))
    Enum.each(history, print_history(&1))
  end

  defp print_history(config) do
    IO.puts IO.ANSI.escape("%{yellow}#{config.counter}: #{config.cache}#=> #{inspect config.result}\n")
  end

  @doc """
  Shows the documentation for IEx.Helpers.
  """
  def h() do
    IEx.Introspection.h(IEx.Helpers)
  end

  @doc """
  Shows the documentation for the given module
  or for the given function/arity pair.

  ## Examples

      h(Enum)
      #=> Prints documentation for Enum

  It also accepts functions in the format `fun/arity`
  and `module.fun/arity`, for example:

      h receive/1
      h Enum.all?/2
      h Enum.all?

  """
  defmacro h({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      IEx.Introspection.h(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro h({ { :., _, [mod, fun] }, _, [] }) do
    quote do
      IEx.Introspection.h(unquote(mod), unquote(fun))
    end
  end

  defmacro h({ :/, _, [{ fun, _, args }, arity] }) when args == [] or is_atom(args) do
    quote do
      IEx.Introspection.h(unquote(fun), unquote(arity))
    end
  end

  defmacro h({ name, _, args }) when args == [] or is_atom(args) do
    quote do
      IEx.Introspection.h([unquote(__MODULE__), Kernel, Kernel.SpecialForms], unquote(name))
    end
  end

  defmacro h(other) do
    quote do
      IEx.Introspection.h(unquote(other))
    end
  end

  @doc """
  Prints all types for the given module or prints out a specified type's
  specification

  ## Examples

      t(Enum)
      t(Enum.t/0)
      t(Enum.t)

  """
  defmacro t({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      IEx.Introspection.t(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro t({ { :., _, [mod, fun] }, _, [] }) do
    quote do
      IEx.Introspection.t(unquote(mod), unquote(fun))
    end
  end

  defmacro t(module) do
    quote do
      IEx.Introspection.t(unquote(module))
    end
  end

  @doc """
  Prints all specs from a given module.

  ## Examples

      s(Enum)
      s(Enum.all?)
      s(Enum.all?/2)
      s(list_to_atom)
      s(list_to_atom/1)

  """
  defmacro s({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      IEx.Introspection.s(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro s({ { :., _, [mod, fun] }, _, [] }) do
    quote do
      IEx.Introspection.s(unquote(mod), unquote(fun))
    end
  end

  defmacro s({ fun, _, args }) when args == [] or is_atom(args) do
    quote do
      IEx.Introspection.s(Kernel, unquote(fun))
    end
  end

  defmacro s({ :/, _, [{ fun, _, args }, arity] }) when args == [] or is_atom(args) do
    quote do
      IEx.Introspection.s(Kernel, unquote(fun), unquote(arity))
    end
  end

  defmacro s(module) do
    quote do
      IEx.Introspection.s(unquote(module))
    end
  end

  @doc """
  Retrieves nth query's value from the history. Use negative
  values to lookup query's value from latest to earliest.
  For instance, v(-1) returns the latest result.
  """
  def v(n) when n < 0 do
    history = Process.get(:iex_history)
    Enum.at!(history, abs(n) - 1).result
  end

  def v(n) when n > 0 do
    history = Process.get(:iex_history) |> Enum.reverse
    Enum.at!(history, n - 1).result
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

  @doc """
  Purges and reloads specified module
  """
  def l(module) do
    :code.purge(module)
    :code.load_file(module)
  end

  @doc """
  Flushes all messages sent to the shell and prints them out
  """
  def flush do
    receive do
      msg ->
        IO.inspect(msg)
        flush
    after
      0 -> :ok
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
      case List.keyfind(compile, :options, 0) do
        { :options, opts } -> opts
        _ -> []
      end

    source = List.keyfind(options, :source, 0)  || List.keyfind(compile, :source, 0)

    case source do
      { :source, source } -> list_to_binary(source)
      _ -> nil
    end
  end

  @doc """
  Prints the current working directory.
  """
  def pwd do
    IO.puts IO.ANSI.escape("%{yellow}#{System.cwd!}")
  end

  @doc """
  Changes the shell directory to the given path.
  """
  def cd(directory) do
    case File.cd(directory) do
      :ok -> pwd
      { :error, :enoent } ->
        IO.puts IO.ANSI.escape("%{red}No directory #{directory}")
    end
  end
end
