defmodule Kernel.ParallelCompiler do
  @moduledoc """
  A module responsible for compiling and requiring files in parallel.
  """

  @doc """
  Starts a task for parallel compilation.

  If you have a file that needs to compile other modules in parallel,
  the spawned processes need to be aware of the compiler environment.
  This function allows a developer to create a task that is aware of
  those environments.

  See `Task.async/1` for more information. The task spawned must be
  always awaited on by calling `Task.await/1`
  """
  @doc since: "1.6.0"
  def async(fun) when is_function(fun) do
    if parent = :erlang.get(:elixir_compiler_pid) do
      file = :erlang.get(:elixir_compiler_file)
      dest = :erlang.get(:elixir_compiler_dest)
      {:error_handler, error_handler} = :erlang.process_info(self(), :error_handler)

      Task.async(fn ->
        send(parent, {:async, self()})
        :erlang.put(:elixir_compiler_pid, parent)
        :erlang.put(:elixir_compiler_file, file)
        dest != :undefined and :erlang.put(:elixir_compiler_dest, dest)
        :erlang.process_flag(:error_handler, error_handler)
        fun.()
      end)
    else
      raise ArgumentError,
            "cannot spawn parallel compiler task because " <>
              "the current file is not being compiled/required"
    end
  end

  @doc """
  Compiles the given files.

  Those files are compiled in parallel and can automatically
  detect dependencies between them. Once a dependency is found,
  the current file stops being compiled until the dependency is
  resolved.

  It returns `{:ok, modules, warnings}` or `{:error, errors, warnings}`.

  Both errors and warnings are a list of three-element tuples containing
  the file, line and the formatted error/warning.

  ## Options

    * `:each_file` - for each file compiled, invokes the callback passing the
      file

    * `:each_long_compilation` - for each file that takes more than a given
      timeout (see the `:long_compilation_threshold` option) to compile, invoke
      this callback passing the file as its argument

    * `:each_module` - for each module compiled, invokes the callback passing
      the file, module and the module bytecode

    * `:each_cycle` - after the given files are compiled, invokes this function
      that should return the following values:
      * `{:compile, modules, warnings}` - to continue compilation with a list of
        further modules to compile
      * `{:runtime, modules, warnings}` - to stop compilation and verify the list
        of modules because dependent modules have changed

    * `:long_compilation_threshold` - the timeout (in seconds) after the
      `:each_long_compilation` callback is invoked; defaults to `15`

    * `:profile` - if set to `:time` measure the compilation time of each compilation cycle
       and group pass checker

    * `:dest` - the destination directory for the BEAM files. When using `compile/2`,
      this information is only used to properly annotate the BEAM files before
      they are loaded into memory. If you want a file to actually be written to
      `dest`, use `compile_to_path/3` instead.

    * `:beam_timestamp` - the modification timestamp to give all BEAM files

  """
  @doc since: "1.6.0"
  def compile(files, options \\ []) when is_list(options) do
    spawn_workers(files, :compile, options)
  end

  @doc since: "1.6.0"
  def compile_to_path(files, path, options \\ []) when is_binary(path) and is_list(options) do
    spawn_workers(files, {:compile, path}, options)
  end

  @doc """
  Requires the given files in parallel.

  Opposite to compile, dependencies are not attempted to be
  automatically solved between files.

  It returns `{:ok, modules, warnings}` or `{:error, errors, warnings}`.

  Both errors and warnings are a list of three-element tuples containing
  the file, line and the formatted error/warning.

  ## Options

    * `:each_file` - for each file compiled, invokes the callback passing the
      file

    * `:each_module` - for each module compiled, invokes the callback passing
      the file, module and the module bytecode

  """
  @doc since: "1.6.0"
  def require(files, options \\ []) when is_list(options) do
    spawn_workers(files, :require, options)
  end

  @doc false
  @deprecated "Use Kernel.ParallelCompiler.compile/2 instead"
  def files(files, options \\ []) when is_list(options) do
    case spawn_workers(files, :compile, options) do
      {:ok, modules, _} -> modules
      {:error, _, _} -> exit({:shutdown, 1})
    end
  end

  @doc false
  @deprecated "Use Kernel.ParallelCompiler.compile_to_path/2 instead"
  def files_to_path(files, path, options \\ []) when is_binary(path) and is_list(options) do
    case spawn_workers(files, {:compile, path}, options) do
      {:ok, modules, _} -> modules
      {:error, _, _} -> exit({:shutdown, 1})
    end
  end

  defp spawn_workers(files, output, options) do
    {:module, _} = :code.ensure_loaded(Kernel.ErrorHandler)
    schedulers = max(:erlang.system_info(:schedulers_online), 2)
    beam_timestamp = Keyword.get(options, :beam_timestamp)

    outcome =
      spawn_workers(files, 0, [], [], %{}, [], %{
        dest: Keyword.get(options, :dest),
        each_cycle: Keyword.get(options, :each_cycle, fn -> {:runtime, [], []} end),
        each_file: Keyword.get(options, :each_file, fn _, _ -> :ok end) |> each_file(),
        each_long_compilation: Keyword.get(options, :each_long_compilation, fn _file -> :ok end),
        each_module: Keyword.get(options, :each_module, fn _file, _module, _binary -> :ok end),
        long_compilation_threshold: Keyword.get(options, :long_compilation_threshold, 15),
        profile: profile_init(Keyword.get(options, :profile)),
        output: output,
        schedulers: schedulers
      })

    case {outcome, Code.get_compiler_option(:warnings_as_errors)} do
      {{:ok, _, [_ | _] = warnings}, true} ->
        message = "Compilation failed due to warnings while using the --warnings-as-errors option"
        IO.puts(:stderr, message)
        {:error, warnings, []}

      {{:ok, outcome, warnings}, _} ->
        {:ok, write_module_binaries(outcome, output, beam_timestamp), warnings}

      {{:error, errors, warnings}, true} ->
        {:error, errors ++ warnings, []}

      {{:error, errors, warnings}, _} ->
        {:error, errors, warnings}
    end
  end

  defp each_file(fun) when is_function(fun, 1), do: fn file, _ -> fun.(file) end
  defp each_file(fun) when is_function(fun, 2), do: fun

  defp each_file(file, lexical, parent) do
    ref = Process.monitor(parent)
    send(parent, {:file_ok, self(), ref, file, lexical})

    receive do
      ^ref -> :ok
      {:DOWN, ^ref, _, _, _} -> :ok
    end
  end

  defp write_module_binaries(result, {:compile, path}, timestamp) do
    Enum.flat_map(result, fn
      {{:module, module}, {binary, _map}} ->
        full_path = Path.join(path, Atom.to_string(module) <> ".beam")
        File.write!(full_path, binary)
        if timestamp, do: File.touch!(full_path, timestamp)
        [module]

      _ ->
        []
    end)
  end

  defp write_module_binaries(result, _output, _timestamp) do
    for {{:module, module}, _} <- result, do: module
  end

  ## Verification

  defp verify_modules(result, warnings, dependent_modules, state) do
    checker_warnings = maybe_check_modules(result, dependent_modules, state)
    warnings = Enum.reverse(warnings, checker_warnings)
    {:ok, result, warnings}
  end

  defp maybe_check_modules(result, runtime_modules, state) do
    %{schedulers: schedulers, profile: profile} = state

    if :elixir_config.get(:bootstrap) do
      []
    else
      compiled_modules = checker_compiled_modules(result)
      runtime_modules = checker_runtime_modules(runtime_modules)

      profile_checker(profile, compiled_modules, runtime_modules, fn ->
        Module.ParallelChecker.verify(compiled_modules, runtime_modules, schedulers)
      end)
    end
  end

  defp checker_compiled_modules(result) do
    for {{:module, _module}, {binary, module_map}} <- result do
      {module_map, binary}
    end
  end

  defp checker_runtime_modules(modules) do
    for module <- modules,
        path = :code.which(module),
        is_list(path) and path != [] do
      {module, File.read!(path)}
    end
  end

  defp profile_init(:time), do: {:time, System.monotonic_time(), 0}
  defp profile_init(nil), do: :none

  defp profile_checker({:time, _, _}, compiled_modules, runtime_modules, fun) do
    {time, result} = :timer.tc(fun)
    time = div(time, 1000)
    num_modules = length(compiled_modules) + length(runtime_modules)
    IO.puts(:stderr, "[profile] Finished group pass check of #{num_modules} modules in #{time}ms")
    result
  end

  defp profile_checker(:none, _compiled_modules, _runtime_modules, fun) do
    fun.()
  end

  ## Compiler worker spawning

  # We already have n=schedulers currently running, don't spawn new ones
  defp spawn_workers(
         queue,
         spawned,
         waiting,
         files,
         result,
         warnings,
         %{schedulers: schedulers} = state
       )
       when spawned - length(waiting) >= schedulers do
    wait_for_messages(queue, spawned, waiting, files, result, warnings, state)
  end

  # Release waiting processes
  defp spawn_workers([{ref, found} | t], spawned, waiting, files, result, warnings, state) do
    waiting =
      case List.keytake(waiting, ref, 2) do
        {{_kind, pid, ^ref, _on, _defining, _deadlock}, waiting} ->
          send(pid, {ref, found})
          waiting

        nil ->
          # In case the waiting process died (for example, it was an async process),
          # it will no longer be on the list. So we need to take it into account here.
          waiting
      end

    spawn_workers(t, spawned, waiting, files, result, warnings, state)
  end

  defp spawn_workers([file | queue], spawned, waiting, files, result, warnings, state) do
    %{output: output, long_compilation_threshold: threshold, dest: dest} = state
    parent = self()
    file = Path.expand(file)

    {pid, ref} =
      :erlang.spawn_monitor(fn ->
        :erlang.put(:elixir_compiler_pid, parent)
        :erlang.put(:elixir_compiler_file, file)

        try do
          case output do
            {:compile, path} -> compile_file(file, path, parent)
            :compile -> compile_file(file, dest, parent)
            :require -> require_file(file, parent)
          end
        catch
          kind, reason ->
            send(parent, {:file_error, self(), file, {kind, reason, __STACKTRACE__}})
        end

        exit(:shutdown)
      end)

    timer_ref = Process.send_after(self(), {:timed_out, pid}, threshold * 1000)
    files = [{pid, ref, file, timer_ref} | files]
    spawn_workers(queue, spawned + 1, waiting, files, result, warnings, state)
  end

  # No more queue, nothing waiting, this cycle is done
  defp spawn_workers([], 0, [], [], result, warnings, state) do
    cycle_return = each_cycle_return(state.each_cycle.())
    state = cycle_timing(result, state)

    case cycle_return do
      {:runtime, dependent_modules, extra_warnings} ->
        verify_modules(result, extra_warnings ++ warnings, dependent_modules, state)

      {:compile, [], extra_warnings} ->
        verify_modules(result, extra_warnings ++ warnings, [], state)

      {:compile, more, extra_warnings} ->
        spawn_workers(more, 0, [], [], result, extra_warnings ++ warnings, state)
    end
  end

  # files x, waiting for x: POSSIBLE ERROR! Release processes so we get the failures

  # Single entry, just release it.
  defp spawn_workers(
         [],
         1,
         [{_, pid, ref, _, _, _}] = waiting,
         [{pid, _, _, _}] = files,
         result,
         warnings,
         state
       ) do
    spawn_workers([{ref, :not_found}], 1, waiting, files, result, warnings, state)
  end

  # Multiple entries, try to release modules.
  defp spawn_workers([], spawned, waiting, files, result, warnings, state)
       when length(waiting) == spawned do
    # There is potentially a deadlock. We will release modules with
    # the following order:
    #
    #   1. Code.ensure_compiled/1 checks (deadlock = soft)
    #   2. Struct checks (deadlock = hard)
    #   3. Modules without a known definition
    #   4. Code invocation (deadlock = raise)
    #
    # In theory there is no difference between hard and raise, the
    # difference is where the raise is happening, inside the compiler
    # or in the caller.
    cond do
      deadlocked = deadlocked(waiting, :soft) || deadlocked(waiting, :hard) ->
        spawn_workers(deadlocked, spawned, waiting, files, result, warnings, state)

      without_definition = without_definition(waiting, files) ->
        spawn_workers(without_definition, spawned, waiting, files, result, warnings, state)

      true ->
        errors = handle_deadlock(waiting, files)
        {:error, errors, warnings}
    end
  end

  # No more queue, but spawned and length(waiting) do not match
  defp spawn_workers([], spawned, waiting, files, result, warnings, state) do
    wait_for_messages([], spawned, waiting, files, result, warnings, state)
  end

  defp compile_file(file, path, parent) do
    :erlang.process_flag(:error_handler, Kernel.ErrorHandler)
    :erlang.put(:elixir_compiler_dest, path)
    :elixir_compiler.file(file, &each_file(&1, &2, parent))
  end

  defp require_file(file, parent) do
    case :elixir_code_server.call({:acquire, file}) do
      :required ->
        send(parent, {:file_cancel, self()})

      :proceed ->
        :elixir_compiler.file(file, &each_file(&1, &2, parent))
        :elixir_code_server.cast({:required, file})
    end
  end

  defp cycle_timing(_result, %{profile: :none} = state) do
    state
  end

  defp cycle_timing(result, %{profile: {:time, cycle_start, module_counter}} = state) do
    num_modules = count_modules(result)
    diff_modules = num_modules - module_counter
    now = System.monotonic_time()
    time = System.convert_time_unit(now - cycle_start, :native, :millisecond)

    IO.puts(
      :stderr,
      "[profile] Finished compilation cycle of #{diff_modules} modules in #{time}ms"
    )

    %{state | profile: {:time, now, num_modules}}
  end

  defp count_modules(result) do
    Enum.count(result, &match?({{:module, _}, _}, &1))
  end

  # TODO: Deprecate other returns on v1.14
  defp each_cycle_return({kind, modules, warnings}), do: {kind, modules, warnings}
  defp each_cycle_return({kind, modules}), do: {kind, modules, []}
  defp each_cycle_return(modules) when is_list(modules), do: {:compile, modules, []}

  # The goal of this function is to find leaves in the dependency graph,
  # i.e. to find code that depends on code that we know is not being defined.
  # Note that not all files have been compile yet, so they may not be in waiting.
  defp without_definition(waiting, files) do
    nillify_empty(
      for {pid, _, _, _} <- files,
          {_, ^pid, ref, on, _, _} <- List.wrap(List.keyfind(waiting, pid, 1)),
          not Enum.any?(waiting, fn {_, _, _, _, defining, _} -> on in defining end),
          do: {ref, :not_found}
    )
  end

  defp deadlocked(waiting, type) do
    nillify_empty(for {_, _, ref, _, _, ^type} <- waiting, do: {ref, :deadlock})
  end

  defp nillify_empty([]), do: nil
  defp nillify_empty([_ | _] = list), do: list

  # Wait for messages from child processes
  defp wait_for_messages(queue, spawned, waiting, files, result, warnings, state) do
    %{output: output} = state

    receive do
      {:async, process} ->
        Process.monitor(process)
        wait_for_messages(queue, spawned + 1, waiting, files, result, warnings, state)

      {:available, kind, module} ->
        available =
          for {^kind, _, ref, ^module, _defining, _deadlock} <- waiting,
              do: {ref, :found}

        result = Map.put(result, {kind, module}, true)
        spawn_workers(available ++ queue, spawned, waiting, files, result, warnings, state)

      {:module_available, child, ref, file, module, binary, module_map} ->
        state.each_module.(file, module, binary)

        # Release the module loader which is waiting for an ack
        send(child, {ref, :ack})

        available =
          for {:module, _, ref, ^module, _defining, _deadlock} <- waiting,
              do: {ref, :found}

        cancel_waiting_timer(files, child)
        result = Map.put(result, {:module, module}, {binary, module_map})
        spawn_workers(available ++ queue, spawned, waiting, files, result, warnings, state)

      # If we are simply requiring files, we do not add to waiting.
      {:waiting, _kind, child, ref, _on, _defining, _deadlock} when output == :require ->
        send(child, {ref, :not_found})
        spawn_workers(queue, spawned, waiting, files, result, warnings, state)

      {:waiting, kind, child, ref, on, defining, deadlock?} ->
        # If we already got what we were waiting for, do not put it on waiting.
        # Alternatively, we're waiting on ourselves,
        # send :found so that we can crash with a better error.
        waiting =
          if Map.has_key?(result, {kind, on}) or on in defining do
            send(child, {ref, :found})
            waiting
          else
            [{kind, child, ref, on, defining, deadlock?} | waiting]
          end

        spawn_workers(queue, spawned, waiting, files, result, warnings, state)

      {:timed_out, child} ->
        case List.keyfind(files, child, 0) do
          {^child, _, file, _} -> state.each_long_compilation.(file)
          _ -> :ok
        end

        spawn_workers(queue, spawned, waiting, files, result, warnings, state)

      {:warning, file, line, message} ->
        file = file && Path.absname(file)
        message = :unicode.characters_to_binary(message)
        warning = {file, line, message}
        wait_for_messages(queue, spawned, waiting, files, result, [warning | warnings], state)

      {:file_ok, child_pid, ref, file, lexical} ->
        state.each_file.(file, lexical)
        send(child_pid, ref)
        cancel_waiting_timer(files, child_pid)

        discard_down(child_pid)
        new_files = List.keydelete(files, child_pid, 0)

        # Sometimes we may have spurious entries in the waiting list
        # because someone invoked try/rescue UndefinedFunctionError
        new_waiting = List.keydelete(waiting, child_pid, 1)
        spawn_workers(queue, spawned - 1, new_waiting, new_files, result, warnings, state)

      {:file_cancel, child_pid} ->
        cancel_waiting_timer(files, child_pid)
        discard_down(child_pid)
        new_files = List.keydelete(files, child_pid, 0)
        spawn_workers(queue, spawned - 1, waiting, new_files, result, warnings, state)

      {:file_error, child_pid, file, {kind, reason, stack}} ->
        print_error(file, kind, reason, stack)
        cancel_waiting_timer(files, child_pid)
        discard_down(child_pid)
        files |> List.keydelete(child_pid, 0) |> terminate()
        {:error, [to_error(file, kind, reason, stack)], warnings}

      {:DOWN, ref, :process, pid, reason} ->
        waiting = List.keydelete(waiting, pid, 1)

        case handle_down(files, ref, reason) do
          :ok -> wait_for_messages(queue, spawned - 1, waiting, files, result, warnings, state)
          {:error, errors} -> {:error, errors, warnings}
        end
    end
  end

  defp discard_down(pid) do
    receive do
      {:DOWN, _, :process, ^pid, _} -> :ok
    end
  end

  defp handle_down(_files, _ref, :normal) do
    :ok
  end

  defp handle_down(files, ref, reason) do
    case List.keyfind(files, ref, 1) do
      {child_pid, ^ref, file, _timer_ref} ->
        print_error(file, :exit, reason, [])

        files
        |> List.keydelete(child_pid, 0)
        |> terminate()

        {:error, [to_error(file, :exit, reason, [])]}

      _ ->
        :ok
    end
  end

  defp handle_deadlock(waiting, files) do
    deadlock =
      for {pid, _, file, _} <- files do
        {:current_stacktrace, stacktrace} = Process.info(pid, :current_stacktrace)
        Process.exit(pid, :kill)

        {kind, ^pid, _, on, _, _} = List.keyfind(waiting, pid, 1)
        description = "deadlocked waiting on #{kind} #{inspect(on)}"
        error = CompileError.exception(description: description, file: nil, line: nil)
        print_error(file, :error, error, stacktrace)
        {Path.relative_to_cwd(file), on, description}
      end

    IO.puts("""

    Compilation failed because of a deadlock between files.
    The following files depended on the following modules:
    """)

    max =
      deadlock
      |> Enum.map(&(&1 |> elem(0) |> String.length()))
      |> Enum.max()

    for {file, mod, _} <- deadlock do
      IO.puts(["  ", String.pad_leading(file, max), " => " | inspect(mod)])
    end

    IO.puts(
      "\nEnsure there are no compile-time dependencies between those files " <>
        "and that the modules they reference exist and are correctly named\n"
    )

    for {file, _, description} <- deadlock, do: {Path.absname(file), nil, description}
  end

  defp terminate(files) do
    for {pid, _, _, _} <- files, do: Process.exit(pid, :kill)
    for {pid, _, _, _} <- files, do: discard_down(pid)
    :ok
  end

  defp print_error(file, kind, reason, stack) do
    IO.write([
      "\n== Compilation error in file #{Path.relative_to_cwd(file)} ==\n",
      Kernel.CLI.format_error(kind, reason, stack)
    ])
  end

  defp cancel_waiting_timer(files, child_pid) do
    case List.keyfind(files, child_pid, 0) do
      {^child_pid, _ref, _file, timer_ref} ->
        Process.cancel_timer(timer_ref)
        # Let's flush the message in case it arrived before we canceled the timeout.
        receive do
          {:timed_out, ^child_pid} -> :ok
        after
          0 -> :ok
        end

      nil ->
        :ok
    end
  end

  defp to_error(file, kind, reason, stack) do
    line = get_line(file, reason, stack)
    file = Path.absname(file)
    message = :unicode.characters_to_binary(Kernel.CLI.format_error(kind, reason, stack))
    {file, line, message}
  end

  defp get_line(_file, %{line: line}, _stack) when is_integer(line) and line > 0 do
    line
  end

  defp get_line(file, :undef, [{_, _, _, []}, {_, _, _, info} | _]) do
    if Keyword.get(info, :file) == to_charlist(Path.relative_to_cwd(file)) do
      Keyword.get(info, :line)
    end
  end

  defp get_line(file, _reason, [{_, _, _, [file: 'expanding macro']}, {_, _, _, info} | _]) do
    if Keyword.get(info, :file) == to_charlist(Path.relative_to_cwd(file)) do
      Keyword.get(info, :line)
    end
  end

  defp get_line(file, _reason, [{_, _, _, info} | _]) do
    if Keyword.get(info, :file) == to_charlist(Path.relative_to_cwd(file)) do
      Keyword.get(info, :line)
    end
  end

  defp get_line(_, _, _) do
    nil
  end
end
