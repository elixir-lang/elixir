defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module `IEx.Helpers`
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper
  `h()`, usually referred to as `h/0` (since it expects 0
  arguments).

  There are many other helpers available:

    * `c/2`           — compiles a file at the given path
    * `cd/1`          — changes the current directory
    * `clear/0`       — clears the screen
    * `flush/0`       — flushes all messages sent to the shell
    * `h/0`           — prints this help message
    * `h/1`           — prints help for the given module, function or macro
    * `i/0`           — information about the system
    * `l/1`           — loads the given module's beam code
    * `ls/0`          — lists the contents of the current directory
    * `ls/1`          — lists the contents of the specified directory
    * `pwd/0`         — prints the current working directory
    * `r/1`           — recompiles and reloads the given module's source file
    * `respawn/0`     — respawns the current shell
    * `s/1`           — prints spec information
    * `t/1`           — prints type information
    * `v/0`           — prints the history of commands evaluated in the session
    * `v/1`           — retrieves the nth value from the history
    * `import_file/1` — evaluates the given file in the shell's context

  Help for functions in this module can be consulted
  directly from the command line, as an example, try:

      h(c/2)

  You can also retrieve the documentation for any module
  or function. Try these:

      h(Enum)
      h(Enum.reverse/1)

  To discover all available functions for a module, type the module name
  followed by a dot, then press tab to trigger autocomplete. For example:

      Enum.

  To learn more about IEx as a whole, just type `h(IEx)`.
  """

  import IEx, only: [dont_display_result: 0]

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
  Prints information about the system.
  """
  def i(), do: i(:erlang.processes)

  def i(processes), do: i(processes, length(processes))

  def i(processes, num) when num <= 100 do
    i_header()
    {r,m,h,s} = List.foldl(processes, {0,0,0,0}, fn(pid, {r0,m0,h0,s0}) ->
                        {a,b,c,d} = display_info(pid)
                        {r0+a,m0+b,h0+c,s0+d}
                      end)
    iformat_ansi('Total', '', w(h), w(r), w(m), [:bright, :yellow])
    iformat('', '', w(s), '', '')
  end
  def i(processes, num) do
    i_header()
    paged_i(processes, {0,0,0,0}, num, 50)
  end

  defp i_header() do
    iformat_ansi('Pid', 'Initial Call', 'Heap', 'Reds', 'Msgs', [:bright, :yellow])
    iformat_ansi('Registered', 'Current Function', 'Stack', '', '', [:bright, :yellow])
  end

  def paged_i([], {r,m,h,s}, _, _) do
    iformat('Total', '', w(h), w(r), w(m))
    iformat('', '', w(s), '', '')
  end
  def paged_i(processes, acc, num, page) do
    {pids, rest, n1} =
      if num > page do
        {l1,l2} = :lists.split(page, processes)
        {l1,l2,num-page}
      else
        {processes, [], 0}
      end
    new_acc = List.foldl(pids, acc, fn(pid, {r,m,h,s}) ->
         {a,b,c,d} = display_info(pid)
         {r+a,m+b,h+c,s+d}
       end)
    case rest do
      [_|_] ->
          choice(fn() -> paged_i(rest, new_acc, n1, page) end)
      [] ->
          paged_i([], new_acc, 0, page)
    end
  end

  def choice(f) do
    case get_line('(c)ontinue (q)uit -->', "c\n") do
      'c\n' -> f.()
      'q\n' -> :quit
      other ->
        choice(f)
    end
  end

  def pinfo(pid) do
    case :erlang.is_alive() do
      true -> :rpc.call(node(pid), :erlang, :process_info, [pid])
      false -> :erlang.process_info(pid)
    end
  end

  def get_line(p, default) do
    case line_string(:io.get_line(p)) do
      '\n' -> default
      l    -> l
    end
  end

  # If the standard input is set to binary mode
  # convert it to a list so we can properly match.
  def line_string(binary) when is_binary(binary), do: :unicode.characters_to_list(binary)
  def line_string(other), do: other

  def mfa_string(fun) when is_function(fun) do
    {:module,m} = :erlang.fun_info(fun, :module)
    {:name,f} = :erlang.fun_info(fun, :name)
    {:arity,a} = :erlang.fun_info(fun, :arity)
    mfa_string({m,f,a})
  end
  def mfa_string({m,f,a}) do
    :io_lib.format('~w:~w/~w', [m,f,a])
  end
  def mfa_string(x), do: w(x)

  def display_info(pid) do
    case pinfo(pid) do
      :undefined -> {0,0,0,0}
      info ->
        call = initial_call(info)
        curr = case fetch(:current_function, info) do
                {mod,f,args} when is_list(args) ->
                   {mod,f,length(args)}
                 other -> other
               end
        reds = fetch(:reductions, info)
        lm = length(fetch(:messages, info))
        hs = fetch(:heap_size, info)
        ss = fetch(:stack_size, info)
        iformat(w(pid),
          mfa_string(call),
          w(hs),
          w(reds),
          w(lm))
        iformat(case fetch(:registered_name, info) do
                  0 -> ''
                  x -> w(x)
                end,
                mfa_string(curr),
                w(ss),
                "",
                "")
        {reds, lm, hs, ss}
    end
  end

  def fetch(key, info) do
    case :lists.keyfind(key, 1, info) do
      {_, val} -> val
      false -> 0
    end
  end

  # We have to do some assumptions about the initial call.
  # If the initial call is proc_lib:init_p/3,5 we can find more information
  # calling the function proc_lib:initial_call/1.
  def initial_call(info) do
    case fetch(:initial_call, info) do
      {proc_lib, init_p, _} ->
        :proc_lib.translate_initial_call(info)
      icall -> icall
    end
  end

  def iformat(a1, a2, a3, a4, a5) do
    :io.format('~-21s ~-33s ~-8s ~-4s ~-4s~n', [a1, a2, a3, a4, a5])
  end
  def iformat_ansi(a1, a2, a3, a4, a5, ansi_options) when is_list(ansi_options) do
    formatted = :io_lib.format('~-21s ~-33s ~-8s ~-4s ~-4s~n', [a1, a2, a3, a4, a5])
    IO.puts IO.ANSI.format(ansi_options ++ formatted)
  end

  def w(x) when is_pid(x), do: inspect(x)
  def w(x), do: :io_lib.write(x)

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

  defmacro h({:/, _, [call, arity]} = other) do
    args =
      case Macro.decompose_call(call) do
        {_mod, :__info__, []} when arity == 1 ->
          [Module, :__info__, 1]
        {mod, fun, []} ->
          [mod, fun, arity]
        {fun, []} ->
          [@h_modules, fun, arity]
        _ ->
          [other]
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
  defmacro s({:/, _, [call, arity]} = other) do
    args =
      case Macro.decompose_call(call) do
        {mod, fun, []} -> [mod, fun, arity]
        {fun, []} -> [Kernel, fun, arity]
        _ -> [other]
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
  Prints the history of expressions evaluated during the session along with
  their results.
  """
  def v do
    inspect_opts = IEx.inspect_opts
    IEx.History.each(&print_history_entry(&1, inspect_opts))
  end

  defp print_history_entry({counter, cache, result}, inspect_opts) do
    IO.write IEx.color(:eval_info, "#{counter}: #{cache}#=> ")
    IO.puts  IEx.color(:eval_result, "#{inspect result, inspect_opts}\n")
  end

  @doc """
  Retrieves the nth expression's value from the history.

  Use negative values to lookup expression values relative to the current one.
  For instance, v(-1) returns the result of the last evaluated expression.
  """
  def v(n) do
    IEx.History.nth(n) |> elem(2)
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
    lengths = Enum.map(list, &String.length(&1))
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
      IO.write format_item(Path.join(path, item), String.ljust(item, width))
      len+width
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

  Returns true if it worked.
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

  # Compiles and loads an erlang source file, returns {module, binary}
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
end
