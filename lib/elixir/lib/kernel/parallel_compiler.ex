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

  Both errors and warnings are a list of three element tuples containing
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
      that return a list with potentially more files to compile

    * `:long_compilation_threshold` - the timeout (in seconds) after the
      `:each_long_compilation` callback is invoked; defaults to `15`

    * `:dest` - the destination directory for the BEAM files. When using `files/2`,
      this information is only used to properly annotate the BEAM files before
      they are loaded into memory. If you want a file to actually be written to
      `dest`, use `compile_to_path/3` instead.

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

  Both errors and warnings are a list of three element tuples containing
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

  # TODO: Remove on 2.0
  @doc false
  @deprecated "Use Kernel.ParallelCompiler.compile/2 instead"
  def files(files, options \\ []) when is_list(options) do
    case spawn_workers(files, :compile, options) do
      {:ok, modules, _} -> modules
      {:error, _, _} -> exit({:shutdown, 1})
    end
  end

  # TODO: Remove on 2.0
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
    compiler_pid = self()
    :elixir_code_server.cast({:reset_warnings, compiler_pid})
    schedulers = max(:erlang.system_info(:schedulers_online), 2)

    result =
      spawn_workers(files, [], [], [], [], %{
        dest: Keyword.get(options, :dest),
        each_cycle: Keyword.get(options, :each_cycle, fn -> [] end),
        each_file: Keyword.get(options, :each_file, fn _file -> :ok end),
        each_long_compilation: Keyword.get(options, :each_long_compilation, fn _file -> :ok end),
        each_module: Keyword.get(options, :each_module, fn _file, _module, _binary -> :ok end),
        output: output,
        long_compilation_threshold: Keyword.get(options, :long_compilation_threshold, 15),
        schedulers: schedulers
      })

    # In case --warning-as-errors is enabled and there was a warning,
    # compilation status will be set to error.
    compilation_status = :elixir_code_server.call({:compilation_status, compiler_pid})

    case {result, compilation_status} do
      {{:ok, _, warnings}, :error} ->
        message = "Compilation failed due to warnings while using the --warnings-as-errors option"
        IO.puts(:stderr, message)
        {:error, warnings, []}

      {{:error, errors, warnings}, :error} ->
        {:error, errors ++ warnings, []}

      _ ->
        result
    end
  end

  # We already have n=schedulers currently running, don't spawn new ones
  defp spawn_workers(files, waiting, queued, result, warnings, %{schedulers: schedulers} = state)
       when length(queued) - length(waiting) >= schedulers do
    wait_for_messages(files, waiting, queued, result, warnings, state)
  end

  # Release waiting processes
  defp spawn_workers([{ref, found} | t], waiting, queued, result, warnings, state) do
    waiting =
      case List.keytake(waiting, ref, 2) do
        {{_kind, pid, ^ref, _on, _defining}, waiting} ->
          send(pid, {ref, found})
          waiting

        nil ->
          waiting
      end

    spawn_workers(t, waiting, queued, result, warnings, state)
  end

  defp spawn_workers([file | files], waiting, queued, result, warnings, state) do
    %{output: output, long_compilation_threshold: threshold, dest: dest} = state
    parent = self()

    {pid, ref} =
      :erlang.spawn_monitor(fn ->
        :erlang.put(:elixir_compiler_pid, parent)
        :erlang.put(:elixir_compiler_file, file)

        result =
          try do
            _ =
              case output do
                {:compile, path} ->
                  :erlang.process_flag(:error_handler, Kernel.ErrorHandler)
                  :erlang.put(:elixir_compiler_dest, path)
                  :elixir_compiler.file_to_path(Path.expand(file), path)

                :compile ->
                  :erlang.process_flag(:error_handler, Kernel.ErrorHandler)
                  :erlang.put(:elixir_compiler_dest, dest)
                  Code.compile_file(file)

                :require ->
                  Code.require_file(file)
              end

            :ok
          catch
            kind, reason ->
              {kind, reason, __STACKTRACE__}
          end

        send(parent, {:file_done, self(), file, result})
        exit(:shutdown)
      end)

    timer_ref = Process.send_after(self(), {:timed_out, pid}, threshold * 1000)
    queued = [{pid, ref, file, timer_ref} | queued]
    spawn_workers(files, waiting, queued, result, warnings, state)
  end

  # No more files, nothing waiting, queue is empty, this cycle is done
  defp spawn_workers([], [], [], result, warnings, state) do
    case state.each_cycle.() do
      [] ->
        modules = for {:module, mod} <- result, do: mod
        warnings = Enum.reverse(warnings)
        {:ok, modules, warnings}

      more ->
        spawn_workers(more, [], [], result, warnings, state)
    end
  end

  # Queued x, waiting for x: POSSIBLE ERROR! Release processes so we get the failures

  # Single entry, just release it.
  defp spawn_workers([], [_] = waiting, [_] = queued, result, warnings, state) do
    [{_, _, ref, _, _}] = waiting
    spawn_workers([{ref, :not_found}], waiting, queued, result, warnings, state)
  end

  # Multiple entries, try to release modules.
  defp spawn_workers([], waiting, queued, result, warnings, state)
       when length(waiting) == length(queued) do
    # The goal of this function is to find leaves in the dependency graph,
    # i.e. to find code that depends on code that we know is not being defined.
    without_definition =
      for {pid, _, _, _} <- queued,
          entry = waiting_on_without_definition(waiting, pid),
          do: entry

    # Note we only release modules because those can be rescued. A missing
    # struct is a guaranteed compile error, so we never release it and treat
    # it exclusively a missing entry/deadlock.
    pending =
      for {:module, _, ref, on, _} <- without_definition,
          do: {on, {ref, :not_found}}

    # Instead of releasing all files at once, we release them in groups
    # based on the module they are waiting on. We pick the module being
    # depended on with less edges, as it is the mostly likely source of
    # error (for example, someone made a typo). This may not always be
    # true though. For example, if there is a macro injecting code into
    # multiple modules and such code becomes faulty, now multiple modules
    # are waiting on the same module required by the faulty code. However,
    # since we need to pick something to be first, the one with fewer edges
    # sounds like a sane choice.
    pending
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.sort_by(&length(elem(&1, 1)))
    |> case do
      [{_on, refs} | _] ->
        spawn_workers(refs, waiting, queued, result, warnings, state)

      [] ->
        # There is a deadlock. Instead of printing a deadlock, let's release
        # structs, as a missing struct error is clearer than a deadlock one.
        structs = for {:struct, _, ref, _, _} <- without_definition, do: {ref, :not_found}

        if structs != [] do
          spawn_workers(structs, waiting, queued, result, warnings, state)
        else
          errors = handle_deadlock(waiting, queued)
          {:error, errors, warnings}
        end
    end
  end

  # No more files, but queue and waiting are not full or do not match
  defp spawn_workers([], waiting, queued, result, warnings, state) do
    wait_for_messages([], waiting, queued, result, warnings, state)
  end

  defp waiting_on_without_definition(waiting, pid) do
    {_, ^pid, _, on, _} = entry = List.keyfind(waiting, pid, 1)

    if Enum.any?(waiting, fn {_, _, _, _, defining} -> on in defining end) do
      nil
    else
      entry
    end
  end

  # Wait for messages from child processes
  defp wait_for_messages(files, waiting, queued, result, warnings, state) do
    %{output: output} = state

    receive do
      {:struct_available, module} ->
        available =
          for {:struct, _, ref, waiting_module, _defining} <- waiting,
              module == waiting_module,
              do: {ref, :found}

        result = [{:struct, module} | result]
        spawn_workers(available ++ files, waiting, queued, result, warnings, state)

      {:module_available, child, ref, file, module, binary} ->
        state.each_module.(file, module, binary)

        # Release the module loader which is waiting for an ack
        send(child, {ref, :ack})

        available =
          for {:module, _, ref, waiting_module, _defining} <- waiting,
              module == waiting_module,
              do: {ref, :found}

        cancel_waiting_timer(queued, child)

        result = [{:module, module} | result]
        spawn_workers(available ++ files, waiting, queued, result, warnings, state)

      # If we are simply requiring files, we do not add to waiting.
      {:waiting, _kind, child, ref, _on, _defining} when output == :require ->
        send(child, {ref, :not_found})
        spawn_workers(files, waiting, queued, result, warnings, state)

      {:waiting, kind, child, ref, on, defining} ->
        # Oops, we already got it, do not put it on waiting.
        # Alternatively, we're waiting on ourselves,
        # send :found so that we can crash with a better error.
        waiting =
          if :lists.any(&match?({^kind, ^on}, &1), result) or on in defining do
            send(child, {ref, :found})
            waiting
          else
            [{kind, child, ref, on, defining} | waiting]
          end

        spawn_workers(files, waiting, queued, result, warnings, state)

      {:timed_out, child} ->
        case List.keyfind(queued, child, 0) do
          {^child, _, file, _} ->
            state.each_long_compilation.(file)

          _ ->
            :ok
        end

        spawn_workers(files, waiting, queued, result, warnings, state)

      {:warning, file, line, message} ->
        file = file && Path.absname(file)
        message = :unicode.characters_to_binary(message)
        warning = {file, line, message}
        wait_for_messages(files, waiting, queued, result, [warning | warnings], state)

      {:file_done, child_pid, file, :ok} ->
        discard_down(child_pid)
        state.each_file.(file)
        cancel_waiting_timer(queued, child_pid)

        # Sometimes we may have spurious entries in the waiting
        # list because someone invoked try/rescue UndefinedFunctionError
        new_files = List.delete(files, child_pid)
        new_queued = List.keydelete(queued, child_pid, 0)
        new_waiting = List.keydelete(waiting, child_pid, 1)
        spawn_workers(new_files, new_waiting, new_queued, result, warnings, state)

      {:file_done, child_pid, file, {kind, reason, stack}} ->
        discard_down(child_pid)
        print_error(file, kind, reason, stack)
        cancel_waiting_timer(queued, child_pid)

        queued
        |> List.keydelete(child_pid, 0)
        |> terminate()

        {:error, [to_error(file, kind, reason, stack)], warnings}

      {:DOWN, ref, :process, _pid, reason} ->
        case handle_down(queued, ref, reason) do
          :ok -> wait_for_messages(files, waiting, queued, result, warnings, state)
          {:error, errors} -> {:error, errors, warnings}
        end
    end
  end

  defp discard_down(pid) do
    receive do
      {:DOWN, _, :process, ^pid, _} -> :ok
    end
  end

  defp handle_down(_queued, _ref, :normal) do
    :ok
  end

  defp handle_down(queued, ref, reason) do
    case List.keyfind(queued, ref, 1) do
      {child_pid, ^ref, file, _timer_ref} ->
        print_error(file, :exit, reason, [])

        queued
        |> List.keydelete(child_pid, 0)
        |> terminate()

        {:error, [to_error(file, :exit, reason, [])]}

      _ ->
        :ok
    end
  end

  defp handle_deadlock(waiting, queued) do
    deadlock =
      for {pid, _, file, _} <- queued do
        {:current_stacktrace, stacktrace} = Process.info(pid, :current_stacktrace)
        Process.exit(pid, :kill)

        {kind, ^pid, _, on, _} = List.keyfind(waiting, pid, 1)
        description = "deadlocked waiting on #{kind} #{inspect(on)}"
        error = CompileError.exception(description: description, file: nil, line: nil)
        print_error(file, :error, error, stacktrace)

        {file, on, description}
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

  defp terminate(queued) do
    for {pid, _, _, _} <- queued, do: Process.exit(pid, :kill)
    for {pid, _, _, _} <- queued, do: discard_down(pid)
    :ok
  end

  defp print_error(file, kind, reason, stack) do
    IO.write([
      "\n== Compilation error in file #{Path.relative_to_cwd(file)} ==\n",
      Kernel.CLI.format_error(kind, reason, stack)
    ])
  end

  defp cancel_waiting_timer(queued, child_pid) do
    case List.keyfind(queued, child_pid, 0) do
      {^child_pid, _ref, _file, timer_ref} ->
        Process.cancel_timer(timer_ref)
        # Let's flush the message in case it arrived before we canceled the
        # timeout.
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

  defp get_line(file, _reason, [{_, _, _, info} | _]) do
    if Keyword.get(info, :file) == to_charlist(Path.relative_to_cwd(file)) do
      Keyword.get(info, :line)
    end
  end

  defp get_line(_, _, _) do
    nil
  end
end
