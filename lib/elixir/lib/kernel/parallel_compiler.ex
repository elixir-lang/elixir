defmodule Kernel.ParallelCompiler do
  @moduledoc """
  A module responsible for compiling and requiring files in parallel.
  """

  @typedoc "The line. 0 indicates no line."
  @type line() :: non_neg_integer()
  @type location() :: line() | {pos_integer(), column :: non_neg_integer}
  @type warning() :: {file :: Path.t(), location(), message :: String.t()}
  @type error() :: {file :: Path.t(), location(), message :: String.t()}

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
  def async(fun) when is_function(fun, 0) do
    case :erlang.get(:elixir_compiler_info) do
      {compiler, _} ->
        file = :erlang.get(:elixir_compiler_file)
        dest = :erlang.get(:elixir_compiler_dest)

        {:error_handler, error_handler} = :erlang.process_info(self(), :error_handler)
        {_parent, checker} = Module.ParallelChecker.get()

        Task.async(fn ->
          send(compiler, {:async, self()})
          Module.ParallelChecker.put(compiler, checker)
          :erlang.put(:elixir_compiler_info, {compiler, self()})
          :erlang.put(:elixir_compiler_file, file)
          dest != :undefined and :erlang.put(:elixir_compiler_dest, dest)
          :erlang.process_flag(:error_handler, error_handler)
          fun.()
        end)

      :undefined ->
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

    * `:long_compilation_threshold` - the timeout (in seconds) to check for modules
      taking too long to compile. For each file that exceeds the threshold, the
      `:each_long_compilation` callback is invoked. From Elixir v1.11, only the time
      spent compiling the actual module is taken into account by the threshold, the
      time spent waiting is not considered. Defaults to `10` seconds.

    * `:profile` - if set to `:time` measure the compilation time of each compilation cycle
       and group pass checker

    * `:dest` - the destination directory for the BEAM files. When using `compile/2`,
      this information is only used to properly annotate the BEAM files before
      they are loaded into memory. If you want a file to actually be written to
      `dest`, use `compile_to_path/3` instead.

    * `:beam_timestamp` - the modification timestamp to give all BEAM files

  """
  @doc since: "1.6.0"
  @spec compile([Path.t()], keyword()) :: {:ok, [atom], [warning]} | {:error, [error], [warning]}
  def compile(files, options \\ []) when is_list(options) do
    spawn_workers(files, :compile, options)
  end

  @doc """
  Compiles the given files and writes resulting BEAM files into path.

  See `compile/2` for more information.
  """
  @doc since: "1.6.0"
  @spec compile_to_path([Path.t()], Path.t(), keyword()) ::
          {:ok, [atom], [warning]} | {:error, [error], [warning]}
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
  @spec require([Path.t()], keyword()) ::
          {:ok, [atom], [warning]} | {:error, [error], [warning]}
  def require(files, options \\ []) when is_list(options) do
    spawn_workers(files, :require, options)
  end

  @doc """
  Prints a warning returned by the compiler.
  """
  @doc since: "1.13.0"
  @spec print_warning(warning) :: :ok
  def print_warning({file, location, warning}) do
    :elixir_errors.print_warning_no_diagnostic(location, file, warning)
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
    {:ok, checker} = Module.ParallelChecker.start_link(schedulers)

    try do
      outcome = spawn_workers(schedulers, checker, files, output, options)
      {outcome, Code.get_compiler_option(:warnings_as_errors)}
    else
      {{:ok, _, [_ | _] = warnings}, true} ->
        message = "Compilation failed due to warnings while using the --warnings-as-errors option"
        IO.puts(:stderr, message)
        {:error, warnings, []}

      {{:ok, outcome, warnings}, _} ->
        beam_timestamp = Keyword.get(options, :beam_timestamp)
        {:ok, write_module_binaries(outcome, output, beam_timestamp), warnings}

      {{:error, errors, warnings}, true} ->
        {:error, warnings ++ errors, []}

      {{:error, errors, warnings}, _} ->
        {:error, errors, warnings}
    after
      Module.ParallelChecker.stop(checker)
    end
  end

  defp spawn_workers(schedulers, checker, files, output, options) do
    threshold = Keyword.get(options, :long_compilation_threshold, 10) * 1000
    timer_ref = Process.send_after(self(), :threshold_check, threshold)

    {outcome, state} =
      spawn_workers(files, %{}, %{}, [], %{}, [], [], %{
        dest: Keyword.get(options, :dest),
        each_cycle: Keyword.get(options, :each_cycle, fn -> {:runtime, [], []} end),
        each_file: Keyword.get(options, :each_file, fn _, _ -> :ok end) |> each_file(),
        each_long_compilation: Keyword.get(options, :each_long_compilation, fn _file -> :ok end),
        each_module: Keyword.get(options, :each_module, fn _file, _module, _binary -> :ok end),
        profile: profile_init(Keyword.get(options, :profile)),
        output: output,
        timer_ref: timer_ref,
        long_compilation_threshold: threshold,
        schedulers: schedulers,
        checker: checker
      })

    Process.cancel_timer(state.timer_ref)

    receive do
      :threshold_check -> :ok
    after
      0 -> :ok
    end

    outcome
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
      {{:module, module}, binary} when is_binary(binary) ->
        full_path = Path.join(path, Atom.to_string(module) <> ".beam")
        File.write!(full_path, binary)
        if timestamp, do: File.touch!(full_path, timestamp)
        [module]

      _ ->
        []
    end)
  end

  defp write_module_binaries(result, _output, _timestamp) do
    for {{:module, module}, binary} when is_binary(binary) <- result, do: module
  end

  ## Verification

  defp verify_modules(result, warnings, dependent_modules, state) do
    checker_warnings = maybe_check_modules(result, dependent_modules, state)
    warnings = Enum.reverse(warnings, checker_warnings)
    {{:ok, result, warnings}, state}
  end

  defp maybe_check_modules(result, runtime_modules, state) do
    %{profile: profile, checker: checker} = state

    compiled_modules =
      for {{:module, module}, binary} when is_binary(binary) <- result,
          do: module

    profile_checker(profile, compiled_modules, runtime_modules, fn ->
      Module.ParallelChecker.verify(checker, runtime_modules)
    end)
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
         errors,
         %{schedulers: schedulers} = state
       )
       when map_size(spawned) - map_size(waiting) >= schedulers do
    wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)
  end

  # Release waiting processes
  defp spawn_workers([{pid, found} | t], spawned, waiting, files, result, warnings, errors, state) do
    {files, waiting} =
      case Map.pop(waiting, pid) do
        {{kind, ref, file_pid, on, _defining, _deadlock}, waiting} ->
          send(pid, {ref, found})
          {update_timing(files, file_pid, {:waiting, kind, on}), waiting}

        {nil, waiting} ->
          # In case the waiting process died (for example, it was an async process),
          # it will no longer be on the list. So we need to take it into account here.
          {files, waiting}
      end

    spawn_workers(t, spawned, waiting, files, result, warnings, errors, state)
  end

  defp spawn_workers([file | queue], spawned, waiting, files, result, warnings, errors, state) do
    %{output: output, dest: dest, checker: checker} = state
    parent = self()
    file = Path.expand(file)

    {pid, ref} =
      :erlang.spawn_monitor(fn ->
        Module.ParallelChecker.put(parent, checker)
        :erlang.put(:elixir_compiler_info, {parent, self()})
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

    file_data = %{
      pid: pid,
      ref: ref,
      file: file,
      timestamp: System.monotonic_time(),
      compiling: 0,
      waiting: [],
      warned: false
    }

    new_files = [file_data | files]
    new_spawned = Map.put(spawned, ref, pid)
    spawn_workers(queue, new_spawned, waiting, new_files, result, warnings, errors, state)
  end

  # No more queue, nothing waiting, this cycle is done
  defp spawn_workers([], spawned, waiting, files, result, warnings, errors, state)
       when map_size(spawned) == 0 and map_size(waiting) == 0 do
    [] = errors
    [] = files
    cycle_return = each_cycle_return(state.each_cycle.())
    state = cycle_timing(result, state)

    case cycle_return do
      {:runtime, dependent_modules, extra_warnings} ->
        verify_modules(result, extra_warnings ++ warnings, dependent_modules, state)

      {:compile, [], extra_warnings} ->
        verify_modules(result, extra_warnings ++ warnings, [], state)

      {:compile, more, extra_warnings} ->
        spawn_workers(more, %{}, %{}, [], result, extra_warnings ++ warnings, errors, state)
    end
  end

  # spawned 1, waiting for 1: Release it!
  defp spawn_workers([], spawned, waiting, files, result, warnings, errors, state)
       when map_size(waiting) == map_size(spawned) and map_size(waiting) == 1 do
    {pid, _, _iterator} = :maps.next(:maps.iterator(waiting))
    spawn_workers([{pid, :not_found}], spawned, waiting, files, result, warnings, errors, state)
  end

  # spawned x, waiting for x: POSSIBLE ERROR! Release processes so we get the failures
  defp spawn_workers([], spawned, waiting, files, result, warnings, errors, state)
       when map_size(waiting) == map_size(spawned) do
    # There is potentially a deadlock. We will release modules with
    # the following order:
    #
    #   1. Code.ensure_compiled/1 checks without a known definition (deadlock = soft)
    #   2. Code.ensure_compiled/1 checks with a known definition (deadlock = soft)
    #   3. Struct/import/require/ensure_compiled! checks without a known definition (deadlock = hard)
    #   4. Modules without a known definition
    #   5. Code invocation (deadlock = raise)
    #
    # The reason for step 3 and 4 is to not treat typos as deadlocks and
    # help developers handle those sooner. However, this can have false
    # positives in case multiple modules are defined in the same file
    # and the module we are waiting for is defined later on.
    #
    # Finally, note there is no difference between hard and raise, the
    # difference is where the raise is happening, inside the compiler
    # or in the caller.
    waiting_list = Map.to_list(waiting)

    deadlocked =
      deadlocked(waiting_list, :soft, false) ||
        deadlocked(waiting_list, :soft, true) ||
        deadlocked(waiting_list, :hard, false) ||
        without_definition(waiting_list, files)

    if deadlocked do
      spawn_workers(deadlocked, spawned, waiting, files, result, warnings, errors, state)
    else
      deadlock_errors = handle_deadlock(waiting, files)
      {return_error(deadlock_errors ++ errors, warnings), state}
    end
  end

  # No more queue, but spawned and map_size(waiting) do not match
  defp spawn_workers([], spawned, waiting, files, result, warnings, errors, state) do
    wait_for_messages([], spawned, waiting, files, result, warnings, errors, state)
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
    Enum.count(result, &match?({{:module, _}, binary} when is_binary(binary), &1))
  end

  defp each_cycle_return({kind, modules, warnings}), do: {kind, modules, warnings}

  defp each_cycle_return(other) do
    IO.warn(
      "the :each_cycle callback must return a tuple of format {:compile | :runtime, modules, warnings}"
    )

    case other do
      {kind, modules} -> {kind, modules, []}
      modules when is_list(modules) -> {:compile, modules, []}
    end
  end

  # The goal of this function is to find leaves in the dependency graph,
  # i.e. to find code that depends on code that we know is not being defined.
  # Note that not all files have been compiled yet, so they may not be in waiting.
  defp without_definition(waiting_list, files) do
    nilify_empty_or_sort(
      for %{pid: file_pid} <- files,
          {pid, {_, _, ^file_pid, on, _, _}} <- waiting_list,
          not defining?(on, waiting_list),
          do: {pid, :not_found}
    )
  end

  defp deadlocked(waiting_list, type, defining?) do
    nilify_empty_or_sort(
      for {pid, {_, _, _, on, _, ^type}} <- waiting_list,
          defining?(on, waiting_list) == defining?,
          do: {pid, :deadlock}
    )
  end

  defp defining?(on, waiting_list) do
    Enum.any?(waiting_list, fn {_, {_, _, _, _, defining, _}} -> on in defining end)
  end

  defp nilify_empty_or_sort([]), do: nil
  defp nilify_empty_or_sort([_ | _] = list), do: Enum.sort(list)

  # Wait for messages from child processes
  defp wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state) do
    %{output: output} = state

    receive do
      {:async, pid} ->
        ref = Process.monitor(pid)
        new_spawned = Map.put(spawned, ref, pid)
        wait_for_messages(queue, new_spawned, waiting, files, result, warnings, errors, state)

      {:available, kind, module} ->
        {available, result} = update_result(result, kind, module, true)

        spawn_workers(
          available ++ queue,
          spawned,
          waiting,
          files,
          result,
          warnings,
          errors,
          state
        )

      {:module_available, child, ref, file, module, binary} ->
        state.each_module.(file, module, binary)

        # Release the module loader which is waiting for an ack
        send(child, {ref, :ack})

        {available, result} = update_result(result, :module, module, binary)

        spawn_workers(
          available ++ queue,
          spawned,
          waiting,
          files,
          result,
          warnings,
          errors,
          state
        )

      # If we are simply requiring files, we do not add to waiting.
      {:waiting, _kind, child, ref, _file_pid, _on, _defining, _deadlock} when output == :require ->
        send(child, {ref, :not_found})
        spawn_workers(queue, spawned, waiting, files, result, warnings, errors, state)

      {:waiting, kind, child_pid, ref, file_pid, on, defining, deadlock?} ->
        # If we already got what we were waiting for, do not put it on waiting.
        # If we're waiting on ourselves, send :found so that we can crash with
        # a better error.
        available_or_pending = Map.get(result, {kind, on}, [])

        {waiting, files, result} =
          if not is_list(available_or_pending) or on in defining do
            send(child_pid, {ref, :found})
            {waiting, files, result}
          else
            waiting = Map.put(waiting, child_pid, {kind, ref, file_pid, on, defining, deadlock?})
            files = update_timing(files, file_pid, :compiling)
            result = Map.put(result, {kind, on}, [child_pid | available_or_pending])
            {waiting, files, result}
          end

        spawn_workers(queue, spawned, waiting, files, result, warnings, errors, state)

      :threshold_check ->
        files =
          for data <- files do
            if data.warned or Map.has_key?(waiting, data.pid) do
              data
            else
              data = update_timing(data, :compiling)
              data = maybe_warn_long_compilation(data, state)
              data
            end
          end

        timer_ref = Process.send_after(self(), :threshold_check, state.long_compilation_threshold)
        state = %{state | timer_ref: timer_ref}
        spawn_workers(queue, spawned, waiting, files, result, warnings, errors, state)

      {:diagnostic, type, file, location, message} ->
        file = file && Path.absname(file)
        message = :unicode.characters_to_binary(message)

        case type do
          :warning ->
            warnings = [{file, location, message} | warnings]
            wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)

          :error ->
            errors = [{file, location, message} | errors]
            wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)
        end

      {:file_ok, child_pid, ref, file, lexical} ->
        state.each_file.(file, lexical)
        send(child_pid, ref)

        {file, new_spawned, new_files} = discard_file_pid(spawned, files, child_pid)
        file && maybe_log_file_profile(file, state)

        # We may have spurious entries in the waiting list
        # if someone invoked try/rescue UndefinedFunctionError
        new_waiting = Map.delete(waiting, child_pid)
        spawn_workers(queue, new_spawned, new_waiting, new_files, result, warnings, errors, state)

      {:file_cancel, child_pid} ->
        {_file, new_spawned, new_files} = discard_file_pid(spawned, files, child_pid)
        spawn_workers(queue, new_spawned, waiting, new_files, result, warnings, errors, state)

      {:file_error, child_pid, file, {kind, reason, stack}} ->
        print_error(file, kind, reason, stack)
        {_file, _new_spawned, new_files} = discard_file_pid(spawned, files, child_pid)
        terminate(new_files)
        {return_error([to_error(file, kind, reason, stack) | errors], warnings), state}

      {:DOWN, ref, :process, pid, reason} when is_map_key(spawned, ref) ->
        # async spawned processes have no file, so we always have to delete the ref directly
        spawned = Map.delete(spawned, ref)
        waiting = Map.delete(waiting, pid)
        {file, spawned, files} = discard_file_pid(spawned, files, pid)

        if file do
          print_error(file.file, :exit, reason, [])
          terminate(files)
          {return_error([to_error(file.file, :exit, reason, []) | errors], warnings), state}
        else
          wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)
        end
    end
  end

  defp return_error(errors, warnings) do
    {:error, Enum.reverse(errors), Enum.reverse(warnings)}
  end

  defp update_result(result, kind, module, value) do
    available =
      case Map.get(result, {kind, module}) do
        [_ | _] = pids -> Enum.map(pids, &{&1, :found})
        _ -> []
      end

    {available, Map.put(result, {kind, module}, value)}
  end

  defp update_timing(files, pid, key) do
    Enum.map(files, fn data ->
      if data.pid == pid, do: update_timing(data, key), else: data
    end)
  end

  defp update_timing(data, :compiling) do
    time = System.monotonic_time()
    %{data | compiling: data.compiling + time - data.timestamp, timestamp: time}
  end

  defp update_timing(data, {:waiting, kind, on}) do
    time = System.monotonic_time()
    %{data | waiting: [{kind, on, time - data.timestamp} | data.waiting], timestamp: time}
  end

  defp maybe_warn_long_compilation(data, state) do
    compiling = System.convert_time_unit(data.compiling, :native, :millisecond)

    if not data.warned and compiling >= state.long_compilation_threshold do
      state.each_long_compilation.(data.file)
      %{data | warned: true}
    else
      data
    end
  end

  defp discard_file_pid(spawned, files, pid) do
    case Enum.split_with(files, &(&1.pid == pid)) do
      {[file], files} ->
        Process.demonitor(file.ref, [:flush])
        {file, Map.delete(spawned, file.ref), files}

      {[], files} ->
        {nil, spawned, files}
    end
  end

  defp maybe_log_file_profile(data, state) do
    data = update_timing(data, :compiling)
    data = maybe_warn_long_compilation(data, state)

    if state.profile != :none do
      compiling = to_padded_ms(data.compiling)
      relative = Path.relative_to_cwd(data.file)

      messages =
        case List.pop_at(data.waiting, 0) do
          {nil, []} ->
            "[profile] #{compiling}ms compiling +      0ms waiting while compiling #{relative}"

          {{kind, on, time}, rest} ->
            initial_message = [
              "[profile] #{compiling}ms compiling + ",
              format_waiting_message(time, kind, on, relative)
            ]

            waiting_details =
              Enum.map(rest, fn {kind, on, time} ->
                [
                  "\n[profile]                    | ",
                  format_waiting_message(time, kind, on, relative)
                ]
              end)

            [initial_message | waiting_details]
        end

      IO.puts(:stderr, messages)
    end
  end

  defp format_waiting_message(time, kind, on, relative),
    do: "#{to_padded_ms(time)}ms waiting for #{kind} #{inspect(on)} while compiling #{relative}"

  defp to_padded_ms(time) do
    time
    |> System.convert_time_unit(:native, :millisecond)
    |> Integer.to_string()
    |> String.pad_leading(6, " ")
  end

  defp handle_deadlock(waiting, files) do
    deadlock =
      for %{pid: pid, file: file} <- files do
        {:current_stacktrace, stacktrace} = Process.info(pid, :current_stacktrace)
        Process.exit(pid, :kill)

        {kind, _, _, on, _, _} = Map.fetch!(waiting, pid)
        description = "deadlocked waiting on #{kind} #{inspect(on)}"
        error = CompileError.exception(description: description, file: nil, line: nil)
        print_error(file, :error, error, stacktrace)
        {Path.relative_to_cwd(file), on, description}
      end

    IO.puts(:stderr, """

    Compilation failed because of a deadlock between files.
    The following files depended on the following modules:
    """)

    max =
      deadlock
      |> Enum.map(&(&1 |> elem(0) |> String.length()))
      |> Enum.max()

    for {file, mod, _} <- deadlock do
      IO.puts(:stderr, ["  ", String.pad_leading(file, max), " => " | inspect(mod)])
    end

    IO.puts(
      :stderr,
      "\nEnsure there are no compile-time dependencies between those files " <>
        "and that the modules they reference exist and are correctly named\n"
    )

    for {file, _, description} <- deadlock, do: {Path.absname(file), nil, description}
  end

  defp terminate(files) do
    for %{pid: pid} <- files, do: Process.exit(pid, :kill)

    for %{pid: pid} <- files do
      receive do
        {:DOWN, _, :process, ^pid, _} -> :ok
      end
    end

    :ok
  end

  defp print_error(file, kind, reason, stack) do
    IO.write(:stderr, [
      "\n== Compilation error in file #{Path.relative_to_cwd(file)} ==\n",
      Kernel.CLI.format_error(kind, reason, stack)
    ])
  end

  defp to_error(file, kind, reason, stack) do
    line = get_line(file, reason, stack)
    file = Path.absname(file)
    message = :unicode.characters_to_binary(Kernel.CLI.format_error(kind, reason, stack))
    {file, line || 0, message}
  end

  defp get_line(_file, %{line: line, column: column}, _stack)
       when is_integer(line) and line > 0 and is_integer(column) and column >= 0 do
    {line, column}
  end

  defp get_line(_file, %{line: line}, _stack) when is_integer(line) and line > 0 do
    line
  end

  defp get_line(file, :undef, [{_, _, _, []}, {_, _, _, info} | _]) do
    if Keyword.get(info, :file) == to_charlist(Path.relative_to_cwd(file)) do
      Keyword.get(info, :line)
    end
  end

  defp get_line(file, _reason, [{_, _, _, [file: expanding]}, {_, _, _, info} | _])
       when expanding in [~c"expanding macro", ~c"expanding struct"] do
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
