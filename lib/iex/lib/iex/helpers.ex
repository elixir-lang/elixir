defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module `IEx.Helpers`
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper `h()`,
  usually referred to as `h/0` (since it expects 0 arguments).

  You can use the `h/1` function to invoke the documentation
  for any Elixir module or function:

      iex> h(Enum)
      iex> h(Enum.map)
      iex> h(Enum.reverse/1)

  You can also use the `i/1` function to introspect any value
  you have in the shell:

      iex> i("hello")

  There are many other helpers available, here are some examples:

    * `b/1`            - prints callbacks info and docs for a given module
    * `c/1`            - compiles a file
    * `c/2`            - compiles a file and writes bytecode to the given path
    * `cd/1`           - changes the current directory
    * `clear/0`        - clears the screen
    * `exports/1`      - shows all exports (functions + macros) in a module
    * `flush/0`        - flushes all messages sent to the shell
    * `h/0`            - prints this help message
    * `h/1`            - prints help for the given module, function or macro
    * `i/0`            - prints information about the last value
    * `i/1`            - prints information about the given term
    * `ls/0`           - lists the contents of the current directory
    * `ls/1`           - lists the contents of the specified directory
    * `open/1`         - opens the source for the given module or function in your editor
    * `pid/1`          - creates a PID from a string
    * `pid/3`          - creates a PID with the 3 integer arguments passed
    * `port/1`         - creates a port from a string
    * `port/2`         - creates a port with the 2 non-negative integers passed
    * `ref/1`          - creates a reference from a string
    * `ref/4`          - creates a reference with the 4 integer arguments passed
    * `pwd/0`          - prints the current working directory
    * `r/1`            - recompiles the given module's source file
    * `recompile/0`    - recompiles the current project
    * `runtime_info/0` - prints runtime info (versions, memory usage, stats)
    * `v/0`            - retrieves the last value from the history
    * `v/1`            - retrieves the nth value from the history

  Help for all of those functions can be consulted directly from
  the command line using the `h/1` helper itself. Try:

      iex> h(v/0)

  To list all IEx helpers available, which is effectively all
  exports (functions and macros) in the `IEx.Helpers` module:

      iex> exports(IEx.Helpers)

  This module also includes helpers for debugging purposes, see
  `IEx.break!/4` for more information.

  To learn more about IEx as a whole, type `h(IEx)`.
  """

  import IEx, only: [dont_display_result: 0]

  @doc """
  Recompiles the current Mix application.

  This helper only works when IEx is started with a Mix
  project, for example, `iex -S mix`. The application is
  not restarted after compilation, which means any long
  running process may crash as any changed module will be
  temporarily removed and recompiled, without going through
  the proper code changes callback.

  If you want to reload a single module, consider using
  `r(ModuleName)` instead.

  This function is meant to be used for development and
  debugging purposes. Do not depend on it in production code.

  ## Options

    * `:force` - when `true`, forces the application to recompile

  """
  def recompile(options \\ []) do
    if mix_started?() do
      config = Mix.Project.config()
      consolidation = Mix.Project.consolidation_path(config)
      reenable_tasks(config)

      # No longer allow consolidations to be accessed.
      Code.delete_path(consolidation)
      purge_protocols(consolidation)

      force? = Keyword.get(options, :force, false)
      arguments = if force?, do: ["--force"], else: []

      {result, _} = Mix.Task.run("compile", arguments)

      # Reenable consolidation and allow them to be loaded.
      Code.prepend_path(consolidation)
      purge_protocols(consolidation)

      result
    else
      IO.puts(IEx.color(:eval_error, "Mix is not running. Please start IEx with: iex -S mix"))
      :error
    end
  end

  defp mix_started? do
    List.keyfind(Application.started_applications(), :mix, 0) != nil
  end

  defp reenable_tasks(config) do
    Mix.Task.reenable("compile")
    Mix.Task.reenable("compile.all")
    Mix.Task.reenable("compile.protocols")
    compilers = config[:compilers] || Mix.compilers()
    Enum.each(compilers, &Mix.Task.reenable("compile.#{&1}"))
  end

  defp purge_protocols(path) do
    case File.ls(path) do
      {:ok, beams} ->
        Enum.each(beams, fn beam ->
          module = beam |> Path.rootname() |> String.to_atom()
          :code.purge(module)
          :code.delete(module)
        end)

      {:error, _} ->
        :ok
    end
  end

  @doc """
  Compiles the given files.

  It expects a list of files to compile and an optional path to write
  the compiled code to. By default files are in-memory compiled.
  To write compiled files to the current directory, an empty string
  can be given.

  It returns the names of the compiled modules.

  If you want to recompile an existing module, check `r/1` instead.

  ## Examples

  In the example below, we pass a directory to where the `c/2` function will
  write the compiled `.beam` files to. This directory is typically named "ebin"
  in Erlang/Elixir systems:

      iex> c(["foo.ex", "bar.ex"], "ebin")
      [Foo, Bar]

  When compiling one file, there is no need to wrap it in a list:

      iex> c("baz.ex")
      [Baz]

  """
  def c(files, path \\ :in_memory) when is_binary(path) or path == :in_memory do
    files = List.wrap(files)

    unless Enum.all?(files, &is_binary/1) do
      raise ArgumentError, "expected a binary or a list of binaries as argument"
    end

    {found, not_found} = Enum.split_with(files, &File.exists?/1)

    unless Enum.empty?(not_found) do
      raise ArgumentError, "could not find files #{Enum.join(not_found, ", ")}"
    end

    {erls, exs} = Enum.split_with(found, &String.ends_with?(&1, ".erl"))

    erl_modules =
      Enum.map(erls, fn source ->
        {module, binary} = compile_erlang(source)

        if path != :in_memory do
          base = source |> Path.basename() |> Path.rootname()
          File.write!(Path.join(path, base <> ".beam"), binary)
        end

        module
      end)

    ex_modules =
      case compile_elixir(exs, path) do
        {:ok, modules, _} -> modules
        {:error, _, _} -> raise CompileError
      end

    erl_modules ++ ex_modules
  end

  @doc """
  Clears the console screen.

  This function only works if ANSI escape codes are enabled
  on the shell, which means this function is by default
  unavailable on Windows machines.
  """
  def clear() do
    if IO.ANSI.enabled?() do
      IO.write([IO.ANSI.home(), IO.ANSI.clear()])
    else
      IO.puts("Cannot clear the screen because ANSI escape codes are not enabled on this shell")
    end

    dont_display_result()
  end

  @doc """
  Opens the current prying location.

  This command only works inside a pry session started manually
  via `IEx.pry/0` or a breakpoint set via `IEx.break!/4`. Calling
  this function during a regular `IEx` session will print an error.

  Keep in mind the `open/0` location may not exist when prying
  precompiled source code, such as Elixir itself.

  For more information and to open any module or function, see
  `open/1`.
  """
  def open() do
    case Process.get(:iex_whereami) do
      {file, line, _} ->
        IEx.Introspection.open({file, line})

      _ ->
        IO.puts(IEx.color(:eval_error, "Pry session is not currently enabled"))
    end

    dont_display_result()
  end

  @doc """
  Opens the given `module`, `module.function/arity`, or `{file, line}`.

  This function uses the `ELIXIR_EDITOR` environment variable
  and falls back to `EDITOR` if the former is not available.

  By default, it attempts to open the file and line using the
  `file:line` notation. For example, if your editor is called
  `subl`, it will open the file as:

      subl path/to/file:line

  It is important that you choose an editor command that does
  not block nor that attempts to run an editor directly in the
  terminal. Command-line based editors likely need extra
  configuration so they open up the given file and line in a
  separate window.

  Custom editors are supported by using the `__FILE__` and
  `__LINE__` notations, for example:

      ELIXIR_EDITOR="my_editor +__LINE__ __FILE__"

  and Elixir will properly interpolate values.

  Since this function prints the result returned by the editor,
  `ELIXIR_EDITOR` can be set "echo" if you prefer to display the
  location rather than opening it.

  Keep in mind the location may not exist when opening precompiled
  source code.

  ## Examples

      iex> open(MyApp)
      iex> open(MyApp.fun/2)
      iex> open({"path/to/file", 1})

  """
  defmacro open(term) do
    quote do
      IEx.Introspection.open(unquote(IEx.Introspection.decompose(term, __CALLER__)))
    end
  end

  @doc """
  Prints the documentation for `IEx.Helpers`.
  """
  def h() do
    IEx.Introspection.h(IEx.Helpers)
  end

  @doc """
  Prints the documentation for the given module
  or for the given `function/arity` pair.

  ## Examples

      iex> h(Enum)

  It also accepts functions in the format `function/arity`
  and `module.function/arity`, for example:

      iex> h(receive/1)
      iex> h(Enum.all?/2)
      iex> h(Enum.all?)

  """
  defmacro h(term) do
    quote do
      IEx.Introspection.h(unquote(IEx.Introspection.decompose(term, __CALLER__)))
    end
  end

  @doc """
  Prints the documentation for the given callback function.

  It also accepts single module argument to list
  all available behaviour callbacks.

  ## Examples

      iex> b(Mix.Task.run/1)
      iex> b(Mix.Task.run)
      iex> b(GenServer)

  """
  defmacro b(term) do
    quote do
      IEx.Introspection.b(unquote(IEx.Introspection.decompose(term, __CALLER__)))
    end
  end

  @doc """
  Prints the types for the given module or for the given function/arity pair.

  ## Examples

      iex> t(Enum)
      @type t() :: Enumerable.t()
      @type acc() :: any()
      @type element() :: any()
      @type index() :: integer()
      @type default() :: any()

      iex> t(Enum.t/0)
      @type t() :: Enumerable.t()

      iex> t(Enum.t)
      @type t() :: Enumerable.t()

  """
  defmacro t(term) do
    quote do
      IEx.Introspection.t(unquote(IEx.Introspection.decompose(term, __CALLER__)))
    end
  end

  @doc """
  Returns the value of the `n`th expression in the history.

  `n` can be a negative value: if it is, the corresponding expression value
  relative to the current one is returned. For example, `v(-2)` returns the
  value of the expression evaluated before the last evaluated expression. In
  particular, `v(-1)` returns the result of the last evaluated expression and
  `v()` does the same.

  ## Examples

      iex(1)> "hello" <> " world"
      "hello world"
      iex(2)> 40 + 2
      42
      iex(3)> v(-2)
      "hello world"
      iex(4)> v(2)
      42
      iex(5)> v()
      42

  """
  def v(n \\ -1) do
    IEx.History.nth(history(), n) |> elem(1)
  end

  @doc """
  Recompiles and reloads the given `module`.

  Please note that all the modules defined in the same
  file as `module` are recompiled and reloaded.

  This function is meant to be used for development and
  debugging purposes. Do not depend on it in production code.

  ## In-memory reloading

  When we reload the module in IEx, we recompile the module source
  code, updating its contents in memory. The original `.beam` file
  in disk, probably the one where the first definition of the module
  came from, does not change at all.

  Since typespecs and docs are loaded from the .beam file (they
  are not loaded in memory with the module because there is no need
  for them to be in memory), they are not reloaded when you reload
  the module.
  """
  def r(module) when is_atom(module) do
    {:reloaded, module, do_r(module)}
  end

  defp do_r(module) do
    unless Code.ensure_loaded?(module) do
      raise ArgumentError, "could not load nor find module: #{inspect(module)}"
    end

    source = source(module)

    cond do
      source == nil ->
        raise ArgumentError, "could not find source for module: #{inspect(module)}"

      not File.exists?(source) ->
        raise ArgumentError, "could not find source (#{source}) for module: #{inspect(module)}"

      String.ends_with?(source, ".erl") ->
        [compile_erlang(source) |> elem(0)]

      true ->
        Enum.map(Code.compile_file(source), fn {name, _} -> name end)
    end
  end

  @doc """
  Loads the given module's BEAM code (and ensures any previous
  old version was properly purged before).

  This function is useful when you know the bytecode for module
  has been updated in the file system and you want to tell the VM
  to load it.
  """
  def l(module) when is_atom(module) do
    :code.purge(module)
    :code.load_file(module)
  end

  @doc """
  Prints information about the data type of any given term.

  If no argument is given, the value of the previous expression
  is used.

  ## Examples

      iex> i(1..5)

  Will print:

      Term
        1..5
      Data type
        Range
      Description
        This is a struct. Structs are maps with a __struct__ key.
      Reference modules
        Range, Map

  """
  def i(term \\ v(-1)) do
    implemented_protocols = [{"Implemented protocols", all_implemented_protocols_for_term(term)}]
    info = [{"Term", inspect(term)}] ++ IEx.Info.info(term) ++ implemented_protocols

    for {subject, info} <- info do
      info = info |> to_string() |> String.trim() |> String.replace("\n", "\n  ")
      IO.puts(IEx.color(:eval_result, to_string(subject)))
      IO.puts(IEx.color(:eval_info, "  #{info}"))
    end

    dont_display_result()
  end

  # Given any "term", this function returns all the protocols in
  # :code.get_path() implemented by the data structure of such term, in the form
  # of a binary like "Protocol1, Protocol2, Protocol3".
  defp all_implemented_protocols_for_term(term) do
    :code.get_path()
    |> Protocol.extract_protocols()
    |> Enum.uniq()
    |> Enum.reject(fn protocol -> is_nil(protocol.impl_for(term)) end)
    |> Enum.sort()
    |> Enum.map_join(", ", &inspect/1)
  end

  @runtime_info_topics [:system, :memory, :allocators, :limits, :applications]
  @doc """
  Prints vm/runtime information such as versions, memory usage and statistics.
  Additional topics are available via `runtime_info/1`.
  """
  @doc since: "1.5.0"
  def runtime_info(), do: runtime_info([:system, :memory, :limits])

  @doc """
  Just like `runtime_info/0`, except accepts topic or a list of topics.
  E.g. topic `:applications` will list the applications loaded.
  """
  def runtime_info(topic) when is_atom(topic) and topic in @runtime_info_topics do
    topic
    |> List.wrap()
    |> runtime_info
  end

  def runtime_info(topics) when is_list(topics) do
    topics
    |> Enum.uniq()
    |> print_runtime_info
  end

  defp print_runtime_info(topics) do
    Enum.each(topics, &print_runtime_info_topic/1)
    IO.puts("")
    print_topic_info(topics)
    IO.puts("")
    dont_display_result()
  end

  defp print_topic_info(topics) when is_list(topics) do
    IO.write(pad_key("Showing topics"))
    IO.puts(inspect(topics))
    IO.write(pad_key("Additional topics"))
    IO.puts(inspect(@runtime_info_topics -- topics))
    IO.puts("")
    IO.puts("To view a specific topic call runtime_info(topic)")
  end

  defp print_runtime_info_topic(:system) do
    print_pane("System and architecture")

    print_entry("Elixir version", System.version())
    print_entry("Erlang/OTP version", :erlang.system_info(:otp_release))
    print_entry("ERTS version", :erlang.system_info(:version))
    print_entry("Compiled for", :erlang.system_info(:system_architecture))
    print_entry("Schedulers", :erlang.system_info(:schedulers))
    print_entry("Schedulers online", :erlang.system_info(:schedulers_online))
  end

  defp print_runtime_info_topic(:memory) do
    print_pane("Memory")
    print_memory("Total", :total)
    print_memory("Atoms", :atom)
    print_memory("Binaries", :binary)
    print_memory("Code", :code)
    print_memory("ETS", :ets)
    print_memory("Processes", :processes)
  end

  defp print_runtime_info_topic(:allocators) do
    print_pane("Allocators")

    areas = :erlang.system_info(:allocated_areas)

    print_allocator(areas, "Atom Space", :atom_space)
    print_allocator(areas, "Atom Table", :atom_table)
    print_allocator(areas, "BIF Timer", :bif_timer)
    print_allocator(areas, "Bits Bufs Size", :bits_bufs_size)
    print_allocator(areas, "Dist Table", :dist_table)
    print_allocator(areas, "ETS Misc", :ets_misc)
    print_allocator(areas, "Export List", :export_list)
    print_allocator(areas, "Export Table", :export_table)
    print_allocator(areas, "Function Table", :fun_table)
    print_allocator(areas, "Loaded Code", :loaded_code)
    print_allocator(areas, "Module Refs", :module_refs)
    print_allocator(areas, "Module Table", :module_table)
    print_allocator(areas, "Node Table", :node_table)
    print_allocator(areas, "Port Table", :port_table)
    print_allocator(areas, "Process Table", :process_table)
    print_allocator(areas, "Register Table", :register_table)
    print_allocator(areas, "Static", :static)
    print_allocator(areas, "System Misc", :sys_misc)
  end

  defp print_runtime_info_topic(:limits) do
    print_pane("Statistics / limits")
    print_uptime()
    print_entry("Run queue", :erlang.statistics(:run_queue))

    print_percentage("Atoms", :atom_count, :atom_limit)
    print_percentage("ETS", :ets_count, :ets_limit)
    print_percentage("Ports", :port_count, :port_limit)
    print_percentage("Processes", :process_count, :process_limit)
  end

  defp print_runtime_info_topic(:applications) do
    print_pane("Loaded OTP Applications")
    started = Application.started_applications()
    loaded = Application.loaded_applications()

    for {app, _, version} = entry <- Enum.sort(loaded) do
      IO.write(pad_key(app))
      IO.write(String.pad_trailing("#{version}", 20))

      if entry in started do
        IO.write("(started)")
      end

      IO.puts("")
    end

    :ok
  end

  defp print_pane(msg) do
    IO.puts(IEx.color(:eval_result, ["\n## ", msg, " \n"]))
  end

  defp print_entry(_key, nil), do: :ok
  defp print_entry(key, value), do: IO.puts("#{pad_key(key)}#{value}")

  defp print_uptime() do
    IO.write(pad_key("Uptime"))
    :c.uptime()
  end

  defp print_percentage(key, min, max) do
    min = get_stat(min)
    max = get_stat(max)
    percentage = trunc(min / max * 100)
    IO.puts("#{pad_key(key)}#{min} / #{max} (#{percentage}% used)")
  end

  defp get_stat(:ets_count), do: length(:ets.all())
  defp get_stat(other), do: :erlang.system_info(other)

  defp print_memory(key, memory) do
    value = :erlang.memory(memory)
    IO.puts("#{pad_key(key)}#{format_bytes(value)}")
  end

  defp print_allocator(allocated_areas, key, probe) do
    case List.keyfind(allocated_areas, probe, 0) do
      {_, allocated, used} ->
        IO.puts("#{pad_key(key)}#{format_bytes(allocated)} (#{format_bytes(used)} used)")

      {_, allocated} ->
        IO.puts("#{pad_key(key)}#{format_bytes(allocated)}")

      _ ->
        IO.puts("#{pad_key(key)}N/A")
    end
  end

  defp format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= memory_unit(:GB) -> format_bytes(bytes, :GB)
      bytes >= memory_unit(:MB) -> format_bytes(bytes, :MB)
      bytes >= memory_unit(:KB) -> format_bytes(bytes, :KB)
      true -> format_bytes(bytes, :B)
    end
  end

  defp format_bytes(bytes, unit) when is_integer(bytes) and unit in [:GB, :MB, :KB] do
    value =
      bytes
      |> div(memory_unit(unit))
      |> round()

    "#{value} #{unit}"
  end

  defp format_bytes(bytes, :B) when is_integer(bytes), do: "#{bytes} B"

  defp memory_unit(:GB), do: 1024 * 1024 * 1024
  defp memory_unit(:MB), do: 1024 * 1024
  defp memory_unit(:KB), do: 1024

  defp pad_key(key), do: String.pad_trailing("#{key}:", 20, " ")

  @doc """
  Clears out all messages sent to the shell's inbox and prints them out.
  """
  def flush do
    do_flush(IEx.inspect_opts())
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
    IO.puts(IEx.color(:eval_info, File.cwd!()))
    dont_display_result()
  end

  @doc """
  Changes the current working directory to the given path.
  """
  def cd(directory) when is_binary(directory) do
    case File.cd(expand_home(directory)) do
      :ok ->
        pwd()

      {:error, :enoent} ->
        IO.puts(IEx.color(:eval_error, "No directory #{directory}"))
    end

    dont_display_result()
  end

  @doc """
  Prints a list of all the functions and macros exported by the given module.
  """
  @doc since: "1.5.0"
  def exports(module \\ Kernel) do
    exports = IEx.Autocomplete.exports(module)

    list =
      Enum.map(exports, fn {name, arity} ->
        Atom.to_string(name) <> "/" <> Integer.to_string(arity)
      end)

    print_table(list)
    dont_display_result()
  end

  @doc """
  Prints a list of the given directory's contents.

  If `path` points to a file, prints its full path.
  """
  def ls(path \\ ".") when is_binary(path) do
    path = expand_home(path)

    case File.ls(path) do
      {:ok, items} ->
        sorted_items = Enum.sort(items)

        printer = fn item, width ->
          format_item(Path.join(path, item), String.pad_trailing(item, width))
        end

        print_table(sorted_items, printer)

      {:error, :enoent} ->
        IO.puts(IEx.color(:eval_error, "No such file or directory #{path}"))

      {:error, :enotdir} ->
        IO.puts(IEx.color(:eval_info, Path.absname(path)))
    end

    dont_display_result()
  end

  defp expand_home(<<?~, rest::binary>>) do
    System.user_home!() <> rest
  end

  defp expand_home(other), do: other

  defp print_table(list, printer \\ &String.pad_trailing/2)

  defp print_table([], _printer) do
    :ok
  end

  defp print_table(list, printer) do
    # print items in multiple columns (2 columns in the worst case)
    lengths = Enum.map(list, &String.length(&1))
    maxlen = maxlength(lengths)
    offset = min(maxlen, 30) + 5
    print_table(list, printer, offset)
  end

  defp print_table(list, printer, offset) do
    Enum.reduce(list, 0, fn item, len ->
      len =
        if len >= 80 do
          IO.puts("")
          0
        else
          len
        end

      IO.write(printer.(item, offset))
      len + offset
    end)

    IO.puts("")
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
  """
  def respawn do
    if iex_server = Process.get(:iex_server) do
      send(iex_server, {:respawn, self()})
    end

    dont_display_result()
  end

  @doc """
  Continues execution of the current process.

  This is usually called by sessions started with `IEx.pry/0`
  or `IEx.break!/4`. This allows the current process to execute
  until the next breakpoint, which will automatically yield control
  back to IEx without requesting permission to pry.

  If the running process terminates, a new IEx session is
  started.

  While the process executes, the user will no longer have
  control of the shell. If you would rather start a new shell,
  use `respawn/0` instead.
  """
  @doc since: "1.5.0"
  def continue do
    if iex_server = Process.get(:iex_server) do
      send(iex_server, {:continue, self()})
    end

    dont_display_result()
  end

  @doc """
  Macro-based shortcut for `IEx.break!/4`.
  """
  @doc since: "1.5.0"
  defmacro break!(ast, stops \\ 1) do
    quote do
      require IEx
      IEx.break!(unquote(ast), unquote(stops))
    end
  end

  @doc """
  Sets up a breakpoint in `module`, `function` and `arity`
  with the given number of `stops`.

  See `IEx.break!/4` for a complete description of breakpoints
  in IEx.
  """
  @doc since: "1.5.0"
  defdelegate break!(module, function, arity, stops \\ 1), to: IEx

  @doc """
  Prints all breakpoints to the terminal.
  """
  @doc since: "1.5.0"
  def breaks do
    breaks(IEx.Pry.breaks())
  end

  defp breaks([]) do
    IO.puts(IEx.color(:eval_info, "No breakpoints set"))
    dont_display_result()
  end

  defp breaks(breaks) do
    entries =
      for {id, module, {function, arity}, stops} <- breaks do
        {
          Integer.to_string(id),
          Exception.format_mfa(module, function, arity),
          Integer.to_string(stops)
        }
      end

    entries = [{"ID", "Module.function/arity", "Pending stops"} | entries]

    {id_max, mfa_max, stops_max} =
      Enum.reduce(entries, {0, 0, 0}, fn {id, mfa, stops}, {id_max, mfa_max, stops_max} ->
        {
          max(byte_size(id), id_max),
          max(byte_size(mfa), mfa_max),
          max(byte_size(stops), stops_max)
        }
      end)

    [header | entries] = entries

    IO.puts("")
    print_break(header, id_max, mfa_max)

    IO.puts([
      String.duplicate("-", id_max + 2),
      ?\s,
      String.duplicate("-", mfa_max + 2),
      ?\s,
      String.duplicate("-", stops_max + 2)
    ])

    Enum.each(entries, &print_break(&1, id_max, mfa_max))
    IO.puts("")

    dont_display_result()
  end

  defp print_break({id, mfa, stops}, id_max, mfa_max) do
    IO.puts([
      ?\s,
      String.pad_trailing(id, id_max + 2),
      ?\s,
      String.pad_trailing(mfa, mfa_max + 2),
      ?\s,
      stops
    ])
  end

  @doc """
  Sets the number of pending stops in the breakpoint
  with the given `id` to zero.

  Returns `:ok` if there is such breakpoint ID. `:not_found`
  otherwise.

  Note the module remains "instrumented" on reset. If you would
  like to effectively remove all breakpoints and instrumentation
  code from a module, use `remove_breaks/1` instead.
  """
  @doc since: "1.5.0"
  defdelegate reset_break(id), to: IEx.Pry

  @doc """
  Sets the number of pending stops in the given module,
  function and arity to zero.

  If the module is not instrumented or if the given function
  does not have a breakpoint, it is a no-op and it returns
  `:not_found`. Otherwise it returns `:ok`.

  Note the module remains "instrumented" on reset. If you would
  like to effectively remove all breakpoints and instrumentation
  code from a module, use `remove_breaks/1` instead.
  """
  @doc since: "1.5.0"
  defdelegate reset_break(module, function, arity), to: IEx.Pry

  @doc """
  Removes all breakpoints and instrumentation from `module`.
  """
  @doc since: "1.5.0"
  defdelegate remove_breaks(module), to: IEx.Pry

  @doc """
  Removes all breakpoints and instrumentation from all modules.
  """
  @doc since: "1.5.0"
  defdelegate remove_breaks(), to: IEx.Pry

  @doc """
  Prints the current location and stacktrace in a pry session.

  It expects a `radius` which chooses how many lines before and after
  the current line we should print. By default the `radius` is of two
  lines:

      Location: lib/iex/lib/iex/helpers.ex:79

      77:
      78:   def recompile do
      79:     require IEx; IEx.pry()
      80:     if mix_started?() do
      81:       config = Mix.Project.config

      (IEx.Helpers) lib/iex/lib/iex/helpers.ex:78: IEx.Helpers.recompile/0

  This command only works inside a pry session started manually
  via `IEx.pry/0` or a breakpoint set via `IEx.break!/4`. Calling
  this function during a regular `IEx` session will print an error.

  Keep in mind the `whereami/1` location may not exist when prying
  precompiled source code, such as Elixir itself.
  """
  @doc since: "1.5.0"
  def whereami(radius \\ 2) do
    case Process.get(:iex_whereami) do
      {file, line, stacktrace} ->
        msg = ["Location: ", Path.relative_to_cwd(file), ":", Integer.to_string(line)]
        IO.puts(IEx.color(:eval_info, msg))

        case IEx.Pry.whereami(file, line, radius) do
          {:ok, lines} ->
            IO.write([?\n, lines, ?\n])

          :error ->
            msg = "Could not extract source snippet. Location is not available."
            IO.puts(IEx.color(:eval_error, msg))
        end

        case stacktrace do
          nil -> :ok
          stacktrace -> IO.write([Exception.format_stacktrace(stacktrace), ?\n])
        end

      _ ->
        IO.puts(IEx.color(:eval_error, "Pry session is not currently enabled"))
    end

    dont_display_result()
  end

  @doc """
  Similar to `import_file` but only imports the file it if it is available.

  By default, `import_file/1` fails when the given file does not exist.
  However, since `import_file/1` is expanded at compile-time, it's not
  possible to conditionally import a file since the macro is always
  expanded:

      # This raises a File.Error if ~/.iex.exs doesn't exist.
      if ("~/.iex.exs" |> Path.expand |> File.exists?) do
        import_file "~/.iex.exs"
      end

  This macro addresses this issue by checking if the file exists or not
  in behalf of the user.
  """
  defmacro import_file_if_available(path) when is_binary(path) do
    import_file_if_available(path, true)
  end

  defmacro import_file_if_available(_) do
    raise ArgumentError, "import_file_if_available/1 expects a literal binary as its argument"
  end

  defp import_file_if_available(path, optional?) when is_binary(path) do
    path = Path.expand(path)

    if not optional? or File.exists?(path) do
      path |> File.read!() |> Code.string_to_quoted!(file: path)
    end
  end

  @doc """
  Injects the contents of the file at `path` as if it was typed into
  the shell.

  This would be the equivalent of getting all of the file contents and
  packing it all into a single line in IEx and executing it.

  By default, the contents of a `.iex.exs` file in the same directory
  as you are starting IEx are automatically imported. See the section
  for ".iex.exs" in the `IEx` module docs for more information.

  `path` has to be a literal string and is automatically expanded via
  `Path.expand/1`.

  ## Examples

      # ~/file.exs
      value = 13

      # in the shell
      iex(1)> import_file "~/file.exs"
      13
      iex(2)> value
      13

  """
  @doc since: "1.4.0"
  defmacro import_file(path) when is_binary(path) do
    import_file_if_available(path, false)
  end

  defmacro import_file(_) do
    raise ArgumentError, "import_file/1 expects a literal binary as its argument"
  end

  @doc false
  @deprecated "Use import_file_if_available/1 instead"
  defmacro import_file(path, opts) when is_binary(path) and is_list(opts) do
    import_file_if_available(path, Keyword.get(opts, :optional, false))
  end

  @doc """
  Calls `import/2` with the given arguments, but only if the module is available.

  This lets you put imports in `.iex.exs` files (including `~/.iex.exs`) without
  getting compile errors if you open a console where the module is not available.

  ## Example

      # In ~/.iex.exs
      import_if_available Ecto.Query

  """
  defmacro import_if_available(quoted_module, opts \\ []) do
    module = Macro.expand(quoted_module, __CALLER__)

    if Code.ensure_loaded?(module) do
      quote do
        import unquote(quoted_module), unquote(opts)
      end
    end
  end

  @doc """
  Calls `use/2` with the given arguments, but only if the module is available.

  This lets you use the module in `.iex.exs` files (including `~/.iex.exs`) without
  getting compile errors if you open a console where the module is not available.

  ## Example

      # In ~/.iex.exs
      use_if_available Phoenix.HTML

  """
  @doc since: "1.7.0"
  defmacro use_if_available(quoted_module, opts \\ []) do
    module = Macro.expand(quoted_module, __CALLER__)

    if Code.ensure_loaded?(module) do
      quote do
        use unquote(quoted_module), unquote(opts)
      end
    end
  end

  defp compile_elixir(exs, :in_memory), do: Kernel.ParallelCompiler.compile(exs)
  defp compile_elixir(exs, path), do: Kernel.ParallelCompiler.compile_to_path(exs, path)

  # Compiles and loads an Erlang source file, returns {module, binary}
  defp compile_erlang(source) do
    source = Path.relative_to_cwd(source) |> String.to_charlist()

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
  Creates a PID with 3 non-negative integers passed as arguments
  to the function.

  ## Examples

      iex> pid(0, 21, 32)
      #PID<0.21.32>
      iex> pid(0, 64, 2048)
      #PID<0.64.2048>

  """
  def pid(x, y, z)
      when is_integer(x) and x >= 0 and is_integer(y) and y >= 0 and is_integer(z) and z >= 0 do
    :erlang.list_to_pid(
      '<' ++
        Integer.to_charlist(x) ++
        '.' ++ Integer.to_charlist(y) ++ '.' ++ Integer.to_charlist(z) ++ '>'
    )
  end

  @doc """
  Creates a Port from `string`.

  ## Examples

      iex> port("0.4")
      #Port<0.4>

  """
  @doc since: "1.8.0"
  def port(string) when is_binary(string) do
    :erlang.list_to_port('#Port<#{string}>')
  end

  @doc """
  Creates a Port from two non-negative integers.

  ## Examples

      iex> port(0, 8080)
      #Port<0.8080>
      iex> port(0, 443)
      #Port<0.443>

  """
  @doc since: "1.8.0"
  def port(major, minor)
      when is_integer(major) and major >= 0 and is_integer(minor) and minor >= 0 do
    :erlang.list_to_port(
      '#Port<' ++ Integer.to_charlist(major) ++ '.' ++ Integer.to_charlist(minor) ++ '>'
    )
  end

  @doc """
  Creates a Reference from `string`.

  ## Examples

      iex> ref("0.1.2.3")
      #Reference<0.1.2.3>

  """
  @doc since: "1.6.0"
  def ref(string) when is_binary(string) do
    :erlang.list_to_ref('#Ref<#{string}>')
  end

  @doc """
  Creates a Reference from its 4 non-negative integers components.

  ## Examples

      iex> ref(0, 1, 2, 3)
      #Reference<0.1.2.3>

  """
  @doc since: "1.6.0"
  def ref(w, x, y, z)
      when is_integer(w) and w >= 0 and is_integer(x) and x >= 0 and is_integer(y) and y >= 0 and
             is_integer(z) and z >= 0 do
    :erlang.list_to_ref(
      '#Ref<' ++
        Integer.to_charlist(w) ++
        '.' ++
        Integer.to_charlist(x) ++
        '.' ++ Integer.to_charlist(y) ++ '.' ++ Integer.to_charlist(z) ++ '>'
    )
  end

  @doc """
  Deploys a given module's BEAM code to a list of nodes.

  This function is useful for development and debugging when you have code that
  has been compiled or updated locally that you want to run on other nodes.

  The node list defaults to a list of all connected nodes.

  Returns `{:error, :nofile}` if the object code (i.e. ".beam" file) for the module
  could not be found locally.

  ## Examples

      iex> nl(HelloWorld)
      {:ok, [{:node1@easthost, :loaded, HelloWorld},
             {:node1@westhost, :loaded, HelloWorld}]}

      iex> nl(NoSuchModuleExists)
      {:error, :nofile}

  """
  def nl(nodes \\ Node.list(), module) when is_list(nodes) and is_atom(module) do
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

      _otherwise ->
        {:error, :nofile}
    end
  end
end
