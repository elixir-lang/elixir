defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module IEx.Helpers
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper
  `h()`, usually referred to as `h/0` (since it expects 0
  arguments).

  There are many other helpers available:

  * `c/2`     — compiles a file at the given path
  * `cd/1`    — changes the current directory
  * `clear/0` — clears the screen
  * `flush/0` — flushes all messages sent to the shell
  * `h/0`     — prints this help
  * `h/1`     — prints help for the given module, function or macro
  * `l/1`     — loads the given module's beam code and purges the current version
  * `ls/0`    — lists the contents of the current directory
  * `ls/1`    — lists the contents of the specified directory
  * `m/0`     — prints loaded modules
  * `pwd/0`   — prints the current working directory
  * `r/0`     — recompile and reload all modules that were previously reloaded
  *  r/1`     — recompiles and reloads the given module's source file
  * `s/1`     — prints spec information
  * `t/1`     — prints type information
  * `v/0`     — prints the history of commands evaluated in the session
  * `v/1`     — retrieves the nth value from the history
  * `import_file/1`
              — evaluates the given file in the shell's context

  Help for functions in this module can be consulted
  directly from the command line, as an example, try:

      h(c/2)

  You can also retrieve the documentation for any module
  or function. Try these:

      h(Enum)
      h(Enum.reverse/1)

  To learn more about IEx as a whole, just type `h(IEx)`.
  """

  import IEx, only: [dont_display_result: 0]

  @doc """
  Expects a list of files to compile and a path
  to write their object code to. It returns the name
  of the compiled modules.

  When compiling one file, there is no need to wrap it in a list.

  ## Examples

      c ["foo.ex", "bar.ex"], "ebin"
      #=> [Foo,Bar]

      c "baz.ex"
      #=> [Baz]
  """
  def c(files, path // ".") do
    tuples = Kernel.ParallelCompiler.files_to_path List.wrap(files), path
    Enum.map tuples, elem(&1, 0)
  end

  @doc """
  Clear the console screen.
  """
  def clear do
    IO.write [ IO.ANSI.home, IO.ANSI.clear ]
    dont_display_result
  end

  @doc """
  Prints the list of all loaded modules with paths to their corresponding .beam
  files.
  """
  def m do
    all    = Enum.map :code.all_loaded, fn { mod, file } -> { inspect(mod), file } end
    sorted = Enum.sort all
    size   = Enum.reduce sorted, 0, fn({ mod, _ }, acc) -> max(byte_size(mod), acc) end
    format = "~-#{size}s ~ts~n"

    Enum.each sorted, fn({ mod, file }) ->
      :io.format(format, [mod, file])
    end
    dont_display_result
  end

  @doc """
  Prints the documentation for `IEx.Helpers`.
  """
  def h() do
    IEx.Introspection.h(IEx.Helpers)
    dont_display_result
  end

  @doc """
  Prints the documentation for the given module
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
  # Special case for `h AnyModule.__info__/1`
  defmacro h({ :/, _, [{ { :., _, [_mod, :__info__] }, _, [] }, 1] }) do
    quote do
      IEx.Introspection.h(Module, :__info__, 1)
    end
  end

  defmacro h({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      IEx.Introspection.h(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  # Special case for `h AnyModule.__info__`
  defmacro h({ { :., _, [_mod, :__info__] }, _, [] }) do
    quote do
      IEx.Introspection.h(Module, :__info__, 1)
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
  When given a module, prints specifications (or simply specs) for all the
  types defined in it.

  When given a particular type name, prints its spec.

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
  Similar to `t/1`, only for specs.

  When given a module, prints the list of all specs defined in the module.

  When given a particular spec name (with optional arity), prints its spec.

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
  Prints the history of expressions evaluated during the session along with
  their results.
  """
  def v do
    inspect_opts = IEx.Options.get(:inspect)
    IEx.History.each(print_history_entry(&1, inspect_opts))
  end

  defp print_history_entry(config, inspect_opts) do
    IO.write IEx.color(:info, "#{config.counter}: #{config.cache}#=> ")
    IO.puts  IEx.color(:eval_result, "#{inspect config.result, inspect_opts}\n")
  end

  @doc """
  Retrieves the nth expression's value from the history.

  Use negative values to lookup expression values relative to the current one.
  For instance, v(-1) returns the result of the last evaluated expression.
  """
  def v(n) do
    IEx.History.nth(n).result
  end

  @doc """
  Reloads all modules that have already been reloaded with `r/1` at any point
  in the current IEx session.
  """
  def r do
    List.flatten(Enum.map(iex_reloaded, do_r(&1)))
  end

  @doc """
  Recompiles and reloads the specified module's source file.

  Please note that all the modules defined in the same file as `module`
  are recompiled and reloaded.
  """
  def r(module) do
    case do_r(module) do
      mods when is_list(mods) -> { module, mods }
      other -> other
    end
  end

  defp do_r(module) do
    if source = source(module) do
      Process.put(:iex_reloaded, :ordsets.add_element(module, iex_reloaded))
      Enum.map(Code.load_file(source), fn {name, _} -> name end)
    else
      :nosource
    end
  end

  @doc """
  Load the given module's beam code (and ensures any previous
  old version was properly purged before).
  """
  def l(module) do
    :code.purge(module)
    :code.load_file(module)
  end

  @doc """
  Flushes all messages sent to the shell and prints them out.
  """
  def flush do
    inspect_opts = IEx.Options.get(:inspect)
    do_flush(inspect_opts)
  end

  defp do_flush(inspect_opts) do
    receive do
      msg ->
        IO.inspect(msg, inspect_opts)
        do_flush(inspect_opts)
    after
      0 -> :ok
    end
  end

  defp iex_reloaded do
    Process.get(:iex_reloaded) || :ordsets.new
  end

  defp source(module) do
    source = module.module_info(:compile)[:source]

    case source do
      nil -> nil
      source -> list_to_binary(source)
    end
  end

  @doc """
  Prints the current working directory.
  """
  def pwd do
    IO.puts IEx.color(:info, System.cwd!)
  end

  @doc """
  Changes the current working directory to the given path.
  """
  def cd(directory) do
    case File.cd(expand_home(directory)) do
      :ok -> pwd
      { :error, :enoent } ->
        IO.puts IEx.color(:error, "No directory #{directory}")
    end
  end

  @doc """
  Produces a simple list of a directory's contents.
  If `path` points to a file, prints its full path.
  """
  def ls(path // ".") do
    path = expand_home(path)
    case File.ls(path) do
      { :ok, items } ->
        sorted_items = Enum.sort(items)
        ls_print(path, sorted_items)

      { :error, :enoent } ->
        IO.puts IEx.color(:error, "No such file or directory #{path}")

      { :error, :enotdir } ->
        IO.puts IEx.color(:info, Path.absname(path))
    end
  end

  defp expand_home(<<?~, rest :: binary>>) do
    System.user_home! <> rest
  end

  defp expand_home(other), do: other

  defp ls_print(_, []) do
    :ok
  end

  defp ls_print(path, list) do
    # print items in multiple columns (2 columns in the worst case)
    lengths = Enum.map(list, String.length(&1))
    maxlen = maxlength(lengths)
    width = min(maxlen, 30) + 5
    ls_print(path, list, width)
  end

  defp ls_print(path, list, width) do
    Enum.reduce(list, 0, fn(item, len) ->
      if len >= 80 do
        IO.puts ""
        len = 0
      end
      IO.write format_item(Path.join(path, item),
                           iolist_to_binary(:io_lib.format('~-*ts', [width, item])))
      len+width
    end)
    IO.puts ""
  end

  defp maxlength(list) do
    Enum.reduce(list, 0, max(&1, &2))
  end

  defp format_item(path, representation) do
    case File.stat(path) do
      { :ok, File.Stat[type: :device] } ->
        IEx.color(:device, representation)
      { :ok, File.Stat[type: :directory] } ->
        IEx.color(:directory, representation)
      _ ->
        representation
    end
  end

  @doc """
  Evaluates the contents of the file at `path` as if it were directly typed into
  the shell. `path` has to be a literal binary.

  A leading `~` in `path` is automatically expanded.

  ## Examples

      # ~/file.exs
      value = 13

      # in the shell
      iex(1)> import_file "~/file.exs"
      13
      iex(2)> value
      13
  """
  defmacro import_file(path) when is_binary(path) do
    path = Path.expand(path)
    Code.string_to_quoted! File.read!(path), file: path
  end

  defmacro import_file(_) do
    raise ArgumentError, message: "import_file/1 expects a literal binary as its argument"
  end
end
