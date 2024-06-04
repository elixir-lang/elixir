defmodule Kernel.ParallelCompiler do
  @moduledoc """
  A module responsible for compiling and requiring files in parallel.
  """

  @type info :: %{
          runtime_warnings: [Code.diagnostic(:warning)],
          compile_warnings: [Code.diagnostic(:warning)]
        }

  # Deprecated types
  @type warning() :: {file :: Path.t(), Code.position(), message :: String.t()}
  @type error() :: {file :: Path.t(), Code.position(), message :: String.t()}

  @doc """
  Starts a task for parallel compilation.
  """
  # TODO: Deprecate this on Elixir v1.20.
  @doc deprecated: "Use `pmap/2` instead"
  def async(fun) when is_function(fun, 0) do
    {ref, task} = inner_async(fun)
    send(task.pid, ref)
    task
  end

  defp inner_async(fun) do
    case :erlang.get(:elixir_compiler_info) do
      {compiler_pid, file_pid} ->
        ref = make_ref()
        file = :erlang.get(:elixir_compiler_file)
        dest = :erlang.get(:elixir_compiler_dest)

        {:error_handler, error_handler} = :erlang.process_info(self(), :error_handler)
        {_parent, checker} = Module.ParallelChecker.get()

        task =
          Task.async(fn ->
            Module.ParallelChecker.put(compiler_pid, checker)
            :erlang.put(:elixir_compiler_info, {compiler_pid, file_pid})
            :erlang.put(:elixir_compiler_file, file)
            dest != :undefined and :erlang.put(:elixir_compiler_dest, dest)
            :erlang.process_flag(:error_handler, error_handler)

            receive do
              ^ref -> fun.()
            end
          end)

        send(compiler_pid, {:async, task.pid})
        {ref, task}

      :undefined ->
        raise ArgumentError,
              "cannot spawn parallel compiler task because " <>
                "the current file is not being compiled/required"
    end
  end

  @doc """
  Perform parallel compilation of `collection` with `fun`.

  If you have a file that needs to compile other modules in parallel,
  the spawned processes need to be aware of the compiler environment.
  This function allows a developer to perform such tasks.
  """
  @doc since: "1.16.0"
  def pmap(collection, fun) when is_function(fun, 1) do
    ref = make_ref()

    # We spawn a series of tasks for parallel processing.
    # The tasks are waiting until we give the go ahead.
    refs_tasks =
      Enum.map(collection, fn item ->
        inner_async(fn -> fun.(item) end)
      end)

    # Notify the compiler we are waiting on the tasks.
    {compiler_pid, file_pid} = :erlang.get(:elixir_compiler_info)
    defining = :elixir_module.compiler_modules()
    on = Enum.map(refs_tasks, fn {_ref, %{pid: pid}} -> pid end)
    send(compiler_pid, {:waiting, :pmap, self(), ref, file_pid, on, defining, :raise})

    # Now we allow the tasks to run. This step is not strictly
    # necessary but it makes compilation more deterministic by
    # only allowing tasks to run once we are waiting.
    tasks =
      Enum.map(refs_tasks, fn {ref, task} ->
        send(task.pid, ref)
        task
      end)

    # Await tasks and notify the compiler they are done. We could
    # have the tasks report directly to the compiler, which in turn
    # would notify us, but that would require reimplementing await_many,
    # and copying the results across boundaries, so we don't.
    res = Task.await_many(tasks, :infinity)
    send(compiler_pid, {:available, :pmap, on})

    # Only run once the compiler lets us, to avoid unbounded parallelism.
    receive do
      {^ref, _result} -> res
    end
  end

  @doc """
  Compiles the given files.

  Those files are compiled in parallel and can automatically
  detect dependencies between them. Once a dependency is found,
  the current file stops being compiled until the dependency is
  resolved.

  It returns `{:ok, modules, warnings}` or `{:error, errors, warnings}`
  by default but we recommend using `return_diagnostics: true` so it returns
  diagnostics as maps as well as a map of compilation information.
  The map has the shape of:

      %{
        runtime_warnings: [warning],
        compile_warnings: [warning]
      }

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

    * `:return_diagnostics` (since v1.15.0) - returns maps with information instead of
      a list of warnings and returns diagnostics as maps instead of tuples

    * `:max_concurrency` - the maximum number of files to compile in parallel.
      Setting this option to 1 will compile files sequentially.
      Defaults to the number of schedulers online, or at least 2.

  """
  @doc since: "1.6.0"
  @spec compile([Path.t()], keyword()) ::
          {:ok, [atom], [warning] | info()}
          | {:error, [error] | [Code.diagnostic(:error)], [warning] | info()}
  def compile(files, options \\ []) when is_list(options) do
    spawn_workers(files, :compile, options)
  end

  @doc """
  Compiles the given files and writes resulting BEAM files into path.

  See `compile/2` for more information.
  """
  @doc since: "1.6.0"
  @spec compile_to_path([Path.t()], Path.t(), keyword()) ::
          {:ok, [atom], [warning] | info()}
          | {:error, [error] | [Code.diagnostic(:error)], [warning] | info()}
  def compile_to_path(files, path, options \\ []) when is_binary(path) and is_list(options) do
    spawn_workers(files, {:compile, path}, options)
  end

  @doc """
  Requires the given files in parallel.

  Opposite to compile, dependencies are not attempted to be
  automatically solved between files.

  It returns `{:ok, modules, warnings}` or `{:error, errors, warnings}`
  by default but we recommend using `return_diagnostics: true` so it returns
  diagnostics as maps as well as a map of compilation information.
  The map has the shape of:

      %{
        runtime_warnings: [warning],
        compile_warnings: [warning]
      }

  ## Options

    * `:each_file` - for each file compiled, invokes the callback passing the
      file

    * `:each_module` - for each module compiled, invokes the callback passing
      the file, module and the module bytecode

    * `:max_concurrency` - the maximum number of files to compile in parallel.
      Setting this option to 1 will compile files sequentially.
      Defaults to the number of schedulers online, or at least 2.

  """
  @doc since: "1.6.0"
  @spec require([Path.t()], keyword()) ::
          {:ok, [atom], [warning] | info()}
          | {:error, [error] | [Code.diagnostic(:error)], [warning] | info()}
  def require(files, options \\ []) when is_list(options) do
    spawn_workers(files, :require, options)
  end

  @doc false
  # TODO: Deprecate me on Elixir v1.19
  def print_warning({file, location, warning}) do
    :elixir_errors.print_warning(location, file, warning)
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

    schedulers =
      Keyword.get_lazy(options, :max_concurrency, fn ->
        max(:erlang.system_info(:schedulers_online), 2)
      end)

    {:ok, checker} = Module.ParallelChecker.start_link(schedulers)

    {status, modules_or_errors, info} =
      try do
        outcome = spawn_workers(schedulers, checker, files, output, options)
        {outcome, Code.get_compiler_option(:warnings_as_errors)}
      else
        {{:ok, _, %{runtime_warnings: r_warnings, compile_warnings: c_warnings} = info}, true}
        when r_warnings != [] or c_warnings != [] ->
          message =
            "Compilation failed due to warnings while using the --warnings-as-errors option"

          IO.puts(:stderr, message)
          errors = Enum.map(r_warnings ++ c_warnings, &Map.replace!(&1, :severity, :error))
          {:error, errors, %{info | runtime_warnings: [], compile_warnings: []}}

        {{:ok, outcome, info}, _} ->
          beam_timestamp = Keyword.get(options, :beam_timestamp)
          {:ok, write_module_binaries(outcome, output, beam_timestamp), info}

        {{:error, errors, info}, true} ->
          %{runtime_warnings: r_warnings, compile_warnings: c_warnings} = info
          info = %{info | runtime_warnings: [], compile_warnings: []}
          {:error, c_warnings ++ r_warnings ++ errors, info}

        {{:error, errors, info}, _} ->
          {:error, errors, info}
      after
        Module.ParallelChecker.stop(checker)
      end

    # TODO: Require this to be set from Elixir v1.19
    if Keyword.get(options, :return_diagnostics, false) do
      {status, modules_or_errors, info}
    else
      to_tuples = &Enum.map(&1, fn diag -> {diag.file, diag.position, diag.message} end)

      modules_or_errors =
        if status == :ok, do: modules_or_errors, else: to_tuples.(modules_or_errors)

      {status, modules_or_errors, to_tuples.(info.runtime_warnings ++ info.compile_warnings)}
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

  defp verify_modules(result, compile_warnings, dependent_modules, state) do
    runtime_warnings = maybe_check_modules(result, dependent_modules, state)
    info = %{compile_warnings: Enum.reverse(compile_warnings), runtime_warnings: runtime_warnings}
    {{:ok, result, info}, state}
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
    # Print any spurious error that we may have found
    Enum.map(errors, fn {diagnostic, read_snippet} ->
      :elixir_errors.print_diagnostic(diagnostic, read_snippet)
    end)

    [] = files
    cycle_return = each_cycle_return(state.each_cycle.())
    state = cycle_timing(result, state)

    case cycle_return do
      {:runtime, dependent_modules, extra_warnings} ->
        :elixir_code_server.cast(:purge_compiler_modules)
        verify_modules(result, extra_warnings ++ warnings, dependent_modules, state)

      {:compile, [], extra_warnings} ->
        :elixir_code_server.cast(:purge_compiler_modules)
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
      return_error(warnings, errors, state, fn ->
        handle_deadlock(waiting, files)
      end)
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
          is_atom(on) and not defining?(on, waiting_list),
          do: {pid, :not_found}
    )
  end

  defp deadlocked(waiting_list, type, defining?) do
    nilify_empty_or_sort(
      for {pid, {_, _, _, on, _, ^type}} <- waiting_list,
          is_atom(on) and defining?(on, waiting_list) == defining?,
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

      {:available, kind, on} ->
        {available, result} = update_result(result, kind, on, :done)

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

      {:waiting, kind, child_pid, ref, file_pid, on, defining, deadlock} ->
        # If we already got what we were waiting for, do not put it on waiting.
        # If we're waiting on ourselves, send :found so that we can crash with
        # a better error.
        available_or_pending = Map.get(result, {kind, on}, [])

        {waiting, files, result} =
          if not is_list(available_or_pending) or on in defining do
            send(child_pid, {ref, :found})
            {waiting, files, result}
          else
            waiting = Map.put(waiting, child_pid, {kind, ref, file_pid, on, defining, deadlock})
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

      {:diagnostic, %{severity: :warning, file: file} = diagnostic, read_snippet} ->
        :elixir_errors.print_diagnostic(diagnostic, read_snippet)
        warnings = [%{diagnostic | file: file && Path.absname(file)} | warnings]
        wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)

      {:diagnostic, %{severity: :error} = diagnostic, read_snippet} ->
        errors = [{diagnostic, read_snippet} | errors]
        wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)

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
        {_file, _new_spawned, new_files} = discard_file_pid(spawned, files, child_pid)
        terminate(new_files)

        return_error(warnings, errors, state, fn ->
          print_error(file, kind, reason, stack)
          [to_error(file, kind, reason, stack)]
        end)

      {:DOWN, ref, :process, pid, reason} when is_map_key(spawned, ref) ->
        # async spawned processes have no file, so we always have to delete the ref directly
        spawned = Map.delete(spawned, ref)
        waiting = Map.delete(waiting, pid)
        {file, spawned, files} = discard_file_pid(spawned, files, pid)

        if file do
          terminate(files)

          return_error(warnings, errors, state, fn ->
            print_error(file.file, :exit, reason, [])
            [to_error(file.file, :exit, reason, [])]
          end)
        else
          wait_for_messages(queue, spawned, waiting, files, result, warnings, errors, state)
        end
    end
  end

  defp return_error(warnings, errors, state, fun) do
    # Also prune compiler modules in case of errors
    :elixir_code_server.cast(:purge_compiler_modules)

    errors =
      Enum.map(errors, fn {%{file: file} = diagnostic, read_snippet} ->
        :elixir_errors.print_diagnostic(diagnostic, read_snippet)
        %{diagnostic | file: file && Path.absname(file)}
      end)

    info = %{compile_warnings: Enum.reverse(warnings), runtime_warnings: []}
    {{:error, Enum.reverse(errors, fun.()), info}, state}
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
        {Path.relative_to_cwd(file), on, description, stacktrace}
      end

    IO.puts(:stderr, """

    Compilation failed because of a deadlock between files.
    The following files depended on the following modules:
    """)

    max =
      deadlock
      |> Enum.map(&(&1 |> elem(0) |> String.length()))
      |> Enum.max()

    for {file, mod, _, _} <- deadlock do
      IO.puts(:stderr, ["  ", String.pad_leading(file, max), " => " | inspect(mod)])
    end

    IO.puts(
      :stderr,
      "\nEnsure there are no compile-time dependencies between those files " <>
        "and that the modules they reference exist and are correctly named\n"
    )

    for {file, _, description, stacktrace} <- deadlock do
      file = Path.absname(file)

      %{
        severity: :error,
        file: file,
        source: file,
        position: nil,
        message: description,
        stacktrace: stacktrace,
        span: nil
      }
    end
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

  defp to_error(source, kind, reason, stack) do
    {file, line, span} = get_snippet_info(source, reason, stack)
    source = Path.absname(source)
    message = :unicode.characters_to_binary(Kernel.CLI.format_error(kind, reason, stack))

    %{
      file: file || source,
      source: source,
      position: line || 0,
      message: message,
      severity: :error,
      stacktrace: stack,
      span: span,
      details: {kind, reason}
    }
  end

  defp get_snippet_info(
         _file,
         %{file: file, line: line, column: column, end_line: end_line, end_column: end_column},
         _stack
       )
       when is_integer(line) and line > 0 and is_integer(column) and column >= 0 and
              is_integer(end_line) and end_line > 0 and is_integer(end_column) and end_column >= 0 do
    {Path.absname(file), {line, column}, {end_line, end_column}}
  end

  defp get_snippet_info(_file, %{file: file, line: line, column: column}, _stack)
       when is_integer(line) and line > 0 and is_integer(column) and column >= 0 do
    {Path.absname(file), {line, column}, nil}
  end

  defp get_snippet_info(_file, %{line: line}, _stack) when is_integer(line) and line > 0 do
    {nil, line, nil}
  end

  defp get_snippet_info(file, :undef, [{_, _, _, []}, {_, _, _, info} | _]) do
    get_snippet_info_from_stacktrace_info(info, file)
  end

  defp get_snippet_info(file, _reason, [{_, _, _, [file: expanding]}, {_, _, _, info} | _])
       when expanding in [~c"expanding macro", ~c"expanding struct"] do
    get_snippet_info_from_stacktrace_info(info, file)
  end

  defp get_snippet_info(file, _reason, [{_, _, _, info} | _]) do
    get_snippet_info_from_stacktrace_info(info, file)
  end

  defp get_snippet_info(_, _, _) do
    {nil, nil, nil}
  end

  defp get_snippet_info_from_stacktrace_info(info, file) do
    if Keyword.get(info, :file) == to_charlist(Path.relative_to_cwd(file)) do
      {nil, Keyword.get(info, :line), nil}
    else
      {nil, nil, nil}
    end
  end
end
