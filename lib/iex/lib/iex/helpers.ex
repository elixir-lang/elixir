defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module `IEx.Helpers`
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper `h()`,
  usually referred to as `h/0` (since it expects 0 arguments).

  You can use the `h` function to invoke the documentation
  for any Elixir module or function:

      h Enum
      h Enum.map
      h Enum.reverse/1

  You can also use the `i` function to introspect any value
  you have in the shell:

      i "hello"

  There are many other helpers available:

    * `b/1`           - prints callbacks info and docs for a given module
    * `c/2`           - compiles a file at the given path
    * `cd/1`          - changes the current directory
    * `clear/0`       - clears the screen
    * `flush/0`       - flushes all messages sent to the shell
    * `h/0`           - prints this help message
    * `h/1`           - prints help for the given module, function or macro
    * `i/1`           - prints information about the given data type
    * `import_file/1` - evaluates the given file in the shell's context
    * `l/1`           - loads the given module's beam code
    * `ls/0`          - lists the contents of the current directory
    * `ls/1`          - lists the contents of the specified directory
    * `nl/2`          - deploys local beam code to a list of nodes
    * `pid/1`         - creates a PID from a string
    * `pid/3`         - creates a PID with the 3 integer arguments passed
    * `pwd/0`         - prints the current working directory
    * `r/1`           - recompiles and reloads the given module's source file
    * `respawn/0`     - respawns the current shell
    * `s/1`           - prints spec information
    * `t/1`           - prints type information
    * `v/0`           - retrieves the last value from the history
    * `v/1`           - retrieves the nth value from the history

  Help for all of those functions can be consulted directly from
  the command line using the `h` helper itself. Try:

      h(v/0)

  To learn more about IEx as a whole, just type `h(IEx)`.
  """

  import IEx, only: [dont_display_result: 0]

  @doc """
  Recompiles the current Mix application.

  This helper only works when IEx is started with a Mix
  project, for example, `iex -S mix`. Before compiling
  the code, it will stop the current application, and
  start it again afterwards. Stopping applications are
  required so processes in the supervision tree won't
  crash when code is upgraded multiple times without
  going through the proper hot-code swapping mechanism.

  Changes to `mix.exs` or configuration files won't be
  picked up by this helper, only changes to sources.
  Restarting the shell and Mix is required in such cases.

  If you want to reload a single module, consider using
  `r ModuleName` instead.

  NOTE: This feature is experimental and may be removed
  in upcoming releases.
  """
  def recompile do
    if mix_started? do
      config = Mix.Project.config
      reenable_tasks(config)
      case stop_apps(config) do
        {true, apps} ->
          Mix.Task.run("app.start")
          {:restarted, apps}
        {false, apps} ->
          Mix.Task.run("app.start", ["--no-start"])
          {:recompiled, apps}
      end
    else
      IO.puts IEx.color(:eval_error, "Mix is not running. Please start IEx with: iex -S mix")
      :error
    end
  end

  defp mix_started? do
    List.keyfind(Application.started_applications, :mix, 0) != nil
  end

  defp reenable_tasks(config) do
    Mix.Task.reenable("app.start")
    Mix.Task.reenable("compile")
    Mix.Task.reenable("compile.all")
    compilers = config[:compilers] || Mix.compilers
    Enum.each compilers, &Mix.Task.reenable("compile.#{&1}")
  end

  defp stop_apps(config) do
    apps =
      cond do
        Mix.Project.umbrella?(config) ->
          for %Mix.Dep{app: app} <- Mix.Dep.Umbrella.loaded, do: app
        app = config[:app] ->
          [app]
        true ->
          []
      end
    stopped? =
      Enum.reverse(apps)
      |> Enum.all?(&match?({:error, {:not_started, &1}}, Application.stop(&1)))
      |> Kernel.not
    {stopped?, apps}
  end

  @doc """
  Compiles the given files.

  It expects a list of files to compile and an optional path to write
  the compiled code to (defaults to the current directory). When compiling
  one file, there is no need to wrap it in a list.

  It returns the name of the compiled modules.

  If you want to recompile an existing module, check `r/1` instead.

  ## Examples

      c ["foo.ex", "bar.ex"], "ebin"
      #=> [Foo, Bar]

      c "baz.ex"
      #=> [Baz]
  """
  def c(files, path \\ ".") when is_binary(path) do
    files = List.wrap(files)

    unless Enum.all?(files, &is_binary/1) do
      raise ArgumentError, "expected a binary or a list of binaries as argument"
    end

    {found, not_found} =
      files
      |> Enum.map(&Path.expand(&1, path))
      |> Enum.partition(&File.exists?/1)

    unless Enum.empty?(not_found) do
      raise ArgumentError, "could not find files #{Enum.join(not_found, ", ")}"
    end

    {erls, exs} = Enum.partition(found, &String.ends_with?(&1, ".erl"))

    modules = Enum.map(erls, fn(source) ->
      {module, binary} = compile_erlang(source)
      base = source |> Path.basename |> Path.rootname
      File.write!(Path.join(path, base <> ".beam"), binary)
      module
    end)

    modules ++ Kernel.ParallelCompiler.files_to_path(exs, path)
  end

  @doc """
  Clears the console screen.

  This function only works if ANSI escape codes are enabled
  on the shell, which means this function is by default
  unavailable on Windows machines.
  """
  def clear do
    if IO.ANSI.enabled? do
      IO.write [IO.ANSI.home, IO.ANSI.clear]
    else
      IO.puts "Cannot clear the screen because ANSI escape codes are not enabled on this shell"
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
  @h_modules [__MODULE__, Kernel, Kernel.SpecialForms]

  defmacro h(term)
  defmacro h({:/, _, [call, arity]} = term) do
    args =
      case Macro.decompose_call(call) do
        {_mod, :__info__, []} when arity == 1 ->
          [Module, :__info__, 1]
        {mod, fun, []} ->
          [mod, fun, arity]
        {fun, []} ->
          [@h_modules, fun, arity]
        _ ->
          [term]
      end

    quote do
      IEx.Introspection.h(unquote_splicing(args))
    end
  end

  defmacro h(call) do
    args =
      case Macro.decompose_call(call) do
        {_mod, :__info__, []} ->
          [Module, :__info__, 1]
        {mod, fun, []} ->
          [mod, fun]
        {fun, []} ->
          [@h_modules, fun]
        _ ->
          [call]
      end

    quote do
      IEx.Introspection.h(unquote_splicing(args))
    end
  end

  @doc """
  Prints the documentation for the given callback function.

  It also accepts single module argument to list
  all available behaviour callbacks.

  ## Examples

      b(Mix.Task.run/1)
      b(Mix.Task.run)
      b(Dict)

  """
  defmacro b(term)
  defmacro b({:/, _, [{{:., _, [mod, fun]}, _, []}, arity]}) do
    quote do
      IEx.Introspection.b(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro b({{:., _, [mod, fun]}, _, []}) do
    quote do
      IEx.Introspection.b(unquote(mod), unquote(fun))
    end
  end

  defmacro b(module) do
    quote do
      IEx.Introspection.b(unquote(module))
    end
  end

  @doc """
  Prints the types for the given module or for the given function/arity pair.

  ## Examples

      t(Enum)
      t(Enum.t/0)
      t(Enum.t)
  """
  defmacro t(term)
  defmacro t({:/, _, [{{:., _, [mod, fun]}, _, []}, arity]}) do
    quote do
      IEx.Introspection.t(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro t({{:., _, [mod, fun]}, _, []}) do
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
  Prints the specs for the given module or for the given function/arity pair.

  ## Examples

      s(Enum)
      s(Enum.all?)
      s(Enum.all?/2)
      s(is_atom)
      s(is_atom/1)

  """
  defmacro s(term)
  defmacro s({:/, _, [call, arity]} = term) do
    args =
      case Macro.decompose_call(call) do
        {mod, fun, []} -> [mod, fun, arity]
        {fun, []} -> [Kernel, fun, arity]
        _ -> [term]
      end

    quote do
      IEx.Introspection.s(unquote_splicing(args))
    end
  end

  defmacro s(call) do
    args =
      case Macro.decompose_call(call) do
        {mod, fun, []} -> [mod, fun]
        {fun, []} -> [Kernel, fun]
        _ -> [call]
      end

    quote do
      IEx.Introspection.s(unquote_splicing(args))
    end
  end

  @doc """
  Retrieves the nth expression's value from the history.

  Use negative values to look up expression values relative to the current one.
  For instance, v(-1) returns the result of the last evaluated expression.
  """
  def v(n \\ -1) do
    IEx.History.nth(history, n) |> elem(2)
  end

  @doc """
  Recompiles and reloads the given `module`.

  Please note that all the modules defined in the same
  file as `module` are recompiled and reloaded.

  ## In-memory reloading

  When we reload the module in IEx, we recompile the module source code,
  updating its contents in memory. The original `.beam` file in disk,
  probably the one where the first definition of the module came from,
  does not change at all.

  Since typespecs and docs are loaded from the .beam file (they are not
  loaded in memory with the module because there is no need for them to
  be in memory), they are not reloaded when you reload the module.
  """
  def r(module) when is_atom(module) do
    {:reloaded, module, do_r(module)}
  end

  defp do_r(module) do
    unless Code.ensure_loaded?(module) do
      raise ArgumentError, "could not load nor find module: #{inspect module}"
    end

    source = source(module)
    cond do
      source == nil ->
        raise ArgumentError, "could not find source for module: #{inspect module}"

      not File.exists?(source) ->
        raise ArgumentError, "could not find source (#{source}) for module: #{inspect module}"

      String.ends_with?(source, ".erl") ->
        [compile_erlang(source) |> elem(0)]

      true ->
        Enum.map(Code.load_file(source), fn {name, _} -> name end)
    end
  end

  @doc """
  Loads the given module's beam code (and ensures any previous
  old version was properly purged before).

  This function is useful when you know the bytecode for module
  has been updated in the filesystem and you want to tell the VM
  to load it.
  """
  def l(module) when is_atom(module) do
    :code.purge(module)
    :code.load_file(module)
  end

  @doc """
  Prints information about the given data type.
  """
  def i(term) do
    info = ["Term": inspect(term)] ++ IEx.Info.info(term)

    for {subject, info} <- info do
      info = info |> to_string() |> String.strip() |> String.replace("\n", "\n  ")
      IO.puts IEx.color(:eval_result, to_string(subject))
      IO.puts IEx.color(:eval_info, "  #{info}")
    end

    dont_display_result
  end

  @doc """
  Flushes all messages sent to the shell and prints them out.
  """
  def flush do
    do_flush(IEx.inspect_opts)
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

  defp source(module) do
    source = module.module_info(:compile)[:source]

    case source do
      nil -> nil
      source -> List.to_string(source)
    end
  end

  @doc """
  Prints the current working directory.
  """
  def pwd do
    IO.puts IEx.color(:eval_info, System.cwd!)
    dont_display_result
  end

  @doc """
  Changes the current working directory to the given path.
  """
  def cd(directory) when is_binary(directory) do
    case File.cd(expand_home(directory)) do
      :ok -> pwd
      {:error, :enoent} ->
        IO.puts IEx.color(:eval_error, "No directory #{directory}")
    end
    dont_display_result()
  end

  @doc """
  Produces a simple list of a directory's contents.

  If `path` points to a file, prints its full path.
  """
  def ls(path \\ ".") when is_binary(path) do
    path = expand_home(path)
    case File.ls(path) do
      {:ok, items} ->
        sorted_items = Enum.sort(items)
        ls_print(path, sorted_items)

      {:error, :enoent} ->
        IO.puts IEx.color(:eval_error, "No such file or directory #{path}")

      {:error, :enotdir} ->
        IO.puts IEx.color(:eval_info, Path.absname(path))
    end
    dont_display_result()
  end

  defp expand_home(<<?~, rest::binary>>) do
    System.user_home! <> rest
  end

  defp expand_home(other), do: other

  defp ls_print(_, []) do
    :ok
  end

  defp ls_print(path, list) do
    # print items in multiple columns (2 columns in the worst case)
    lengths = Enum.map(list, &String.length(&1))
    maxlen = maxlength(lengths)
    width = min(maxlen, 30) + 5
    ls_print(path, list, width)
  end

  defp ls_print(path, list, width) do
    Enum.reduce(list, 0, fn(item, len) ->
      len =
        if len >= 80 do
          IO.puts ""
          0
        else
          len
        end
      IO.write format_item(Path.join(path, item), String.ljust(item, width))
      len + width
    end)

    IO.puts ""
  end

  defp maxlength(list) do
    Enum.reduce(list, 0, &max(&1, &2))
  end

  defp format_item(path, representation) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :device}} ->
        IEx.color(:ls_device, representation)
      {:ok, %File.Stat{type: :directory}} ->
        IEx.color(:ls_directory, representation)
      _ ->
        representation
    end
  end

  @doc """
  Respawns the current shell by starting a new shell process.

  Returns `true` if it worked.
  """
  def respawn do
    if whereis = IEx.Server.whereis do
      send whereis, {:respawn, self}
      dont_display_result
    end
  end

  @doc """
  Evaluates the contents of the file at `path` as if it were directly typed into
  the shell.

  `path` has to be a literal string. `path` is automatically expanded via
  `Path.expand/1`.

  ## Non-existent files

  By default, `import_file/1` fails when the given file does not exist. However,
  since this macro is expanded at compile-time, it's not possible to
  conditionally import a file since the macro is always expanded:

      # This raises a File.Error if ~/.iex.exs doesn't exist.
      if ("~/.iex.exs" |> Path.expand |> File.exists?) do
        import_file "~/.iex.exs"
      end

  This is why an `:optional` option can be passed to `import_file/1`. The
  default value of this option is `false`, meaning that an exception will be
  raised if the given file is missing. If `:optional` is set to `true`, missing
  files will be ignored and `import_file/1` will just compile to `nil`.

  ## Examples

      # ~/file.exs
      value = 13

      # in the shell
      iex(1)> import_file "~/file.exs"
      13
      iex(2)> value
      13
      iex(3)> import_file "nonexisting.file.ex", optional: true
      nil

  """
  defmacro import_file(path, opts \\ [])

  defmacro import_file(path, opts) when is_binary(path) do
    optional? = Keyword.get(opts, :optional, false)
    path = Path.expand(path)

    if not optional? or File.exists?(path) do
      path |> File.read! |> Code.string_to_quoted!(file: path)
    end
  end

  defmacro import_file(_path, _opts) do
    raise ArgumentError, "import_file/1 expects a literal binary as its argument"
  end

  # Compiles and loads an Erlang source file, returns {module, binary}
  defp compile_erlang(source) do
    source = Path.relative_to_cwd(source) |> String.to_char_list
    case :compile.file(source, [:binary, :report]) do
      {:ok, module, binary} ->
        :code.purge(module)
        {:module, module} = :code.load_binary(module, source, binary)
        {module, binary}
      _ ->
        raise CompileError
    end
  end

  defp history, do: Process.get(:iex_history)

  @doc """
  Creates a PID from `string`.

  ## Examples

      iex> pid("0.21.32")
      #PID<0.21.32>

  """
  def pid(string) when is_binary(string) do
    :erlang.list_to_pid('<#{string}>')
  end

  @doc """
  Creates a PID with 3 non negative integers passed as arguments
  to the function.

  ## Examples

      iex> pid(0, 21, 32)
      #PID<0.21.32>
      iex> pid(0, 64, 2048)
      #PID<0.64.2048>

  """
  def pid(x, y, z) when is_integer(x) and x >= 0 and
                        is_integer(y) and y >= 0 and
                        is_integer(z) and z >= 0 do
    :erlang.list_to_pid(
      '<' ++ Integer.to_char_list(x) ++ '.' ++
             Integer.to_char_list(y) ++ '.' ++
             Integer.to_char_list(z) ++ '>'
    )
  end

  @doc """
  Deloys a given module's beam code to a list of nodes.

  This function is useful for development and debugging when you have code that
  has been compiled or updated locally that you want to run on other nodes.
  
  The node list defaults to a list of all connected nodes.

  Returns `{:error, :nofile}` if the object code (i.e. ".beam" file) for the module
  could not be found locally.

  ## Examples

      nl(HelloWorld)
      #=> {:ok, [{:node1@easthost, :loaded, HelloWorld},
                 {:node1@westhost, :loaded, HelloWorld}]}

      nl(NoSuchModuleExists)
      #=> {:error, :nofile}
      
  """
  def nl(nodes \\ Node.list, module) when is_list(nodes) and is_atom(module) do
    case :code.get_object_code(module) do
      {^module, bin, beam_path} ->
        results = 
          for node <- nodes do
            case :rpc.call(node, :code, :load_binary, [module, beam_path, bin]) do
              {:module, _} -> {node, :loaded, module}
              {:badrpc, message} -> {node, :badrpc, message}
              {:error, message} -> {node, :error, message}
              unexpected -> {node, :error, unexpected}
            end
          end
        {:ok, results}
      _otherwise -> {:error, :nofile}
    end
  end
end
