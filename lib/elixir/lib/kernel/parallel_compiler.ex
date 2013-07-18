defmodule Kernel.ParallelCompiler do
  @moduledoc """
  A module responsible for compiling files in parallel.
  """

  @doc """
  Compiles the given files.

  Those files are compiled in parallel and can automatically
  detect dependencies between them. Once a dependency is found,
  the current file stops being compiled until the dependency is
  resolved.

  If there is an error during compilation or if `warnings_as_errors`
  is set to `true` and there is a warning, this function will fail
  with an exception.

  This function receives a set of callbacks as options:

  * `:each_file` - for each file compiled, invokes the callback passing the file
  * `:each_module` - for each module compiled, invokes the callback
                     passing the file, module and the module bytecode
  * `:each_waiting` - every time a module waits for another module to be compiled,
                      this callback is invoked with the module we are waiting

  The compiler doesn't care about the return values of the callbacks except
  by `each_waiting`, which must return nil or a file as a hint where the source
  could be found.
  """
  def files(files, opts // [])

  def files(files, callback) when is_function(callback) do
    IO.write "[WARNING] Kernel.ParallelCompiler.files(files, callback) is deprecated, " <>
      "please use Kernel.ParallelCompiler.files(files, each_file: callback) instead\n#{Exception.format_stacktrace}"
    spawn_compilers(files, nil, each_file: callback)
  end

  def files(files, callbacks) do
    spawn_compilers(files, nil, callbacks)
  end

  @doc """
  Compiles the given files to the given path.
  Read `files/2` for more information.
  """
  def files_to_path(files, path, opts // [])

  def files_to_path(files, path, callback) when is_function(callback) do
    IO.write "[WARNING] Kernel.ParallelCompiler.files_to_path(files, path, callback) is deprecated, " <>
      "please use Kernel.ParallelCompiler.files_to_path(files, path, each_file: callback) instead\n#{Exception.format_stacktrace}"
    spawn_compilers(files, path, each_file: callback)
  end

  def files_to_path(files, path, callbacks) when is_binary(path) do
    spawn_compilers(files, path, callbacks)
  end

  defp spawn_compilers(files, path, callbacks) do
    Code.ensure_loaded(Kernel.ErrorHandler)
    :elixir_code_server.cast(:reset_warnings)
    schedulers = max(:erlang.system_info(:schedulers_online), 2)

    result = spawn_compilers(files, files, path, callbacks, [], [], schedulers, [])

    # In case --warning-as-errors is enabled and there was a warning,
    # compilation status will be set to error and we fail with Kernel.CompilationError
    case :elixir_code_server.call(:compilation_status) do
      :ok    -> result
      :error -> raise CompileError, [], []
    end
  end

  # We already have 4 currently running, don't spawn new ones
  defp spawn_compilers(files, original, output, callbacks, waiting, queued, schedulers, result) when
      length(queued) - length(waiting) >= schedulers do
    wait_for_messages(files, original, output, callbacks, waiting, queued, schedulers, result)
  end

  # Spawn a compiler for each file in the list until we reach the limit
  defp spawn_compilers([h|t], original, output, callbacks, waiting, queued, schedulers, result) do
    parent = self()

    child  = spawn_link fn ->
      :erlang.put(:elixir_compiler_pid, parent)
      :erlang.put(:elixir_ensure_compiled, true)
      :erlang.process_flag(:error_handler, Kernel.ErrorHandler)

      try do
        if output do
          :elixir_compiler.file_to_path(h, output)
        else
          :elixir_compiler.file(h)
        end
        parent <- { :compiled, self(), h }
      catch
        kind, reason ->
          parent <- { :failure, self(), kind, reason, System.stacktrace }
      end
    end

    spawn_compilers(t, original, output, callbacks, waiting, [{child, h}|queued], schedulers, result)
  end

  # No more files, nothing waiting, queue is empty, we are done
  defp spawn_compilers([], _original, _output, _callbacks, [], [], _schedulers, result), do: result

  # Queued x, waiting for x: POSSIBLE ERROR! Release processes so we get the failures
  defp spawn_compilers([], original, output, callbacks, waiting, queued, schedulers, result) when length(waiting) == length(queued) do
    Enum.each queued, fn { child, _ } -> child <- { :release, self() } end
    wait_for_messages([], original, output, callbacks, waiting, queued, schedulers, result)
  end

  # No more files, but queue and waiting are not full or do not match
  defp spawn_compilers([], original, output, callbacks, waiting, queued, schedulers, result) do
    wait_for_messages([], original, output, callbacks, waiting, queued, schedulers, result)
  end

  # Wait for messages from child processes
  defp wait_for_messages(files, original, output, callbacks, waiting, queued, schedulers, result) do
    receive do
      { :compiled, child, file } ->
        if callback = Keyword.get(callbacks, :each_file) do
          callback.(file)
        end
        new_queued  = List.keydelete(queued, child, 0)

        # Sometimes we may have spurious entries in the waiting
        # list because someone invoked try/rescue UndefinedFunctionError
        new_waiting = List.keydelete(waiting, child, 0)
        spawn_compilers(files, original, output, callbacks, new_waiting, new_queued, schedulers, result)
      { :module_available, child, file, module, binary } ->
        if callback = Keyword.get(callbacks, :each_module) do
          callback.(file, module, binary)
        end

        # Release the module loader which is waiting for an ack
        child <- { self, :ack }

        new_waiting = release_waiting_processes(module, waiting)
        new_result  = [{module, binary}|result]
        wait_for_messages(files, original, output, callbacks, new_waiting, queued, schedulers, new_result)
      { :waiting, child, on } ->
        # If one of the callbacks is each_waiting and we haven't seen
        # the hinted file before, add it to the list to be processed.
        if (callback = Keyword.get(callbacks, :each_waiting)) &&
           (hint = callback.(on)) &&
           not(hint in original) do
          files    = [hint|files]
          original = [hint|original]
        end

        new_waiting = :orddict.store(child, on, waiting)
        spawn_compilers(files, original, output, callbacks, new_waiting, queued, schedulers, result)
      { :failure, child, kind, reason, stacktrace } ->
        if many_missing?(child, files, waiting, queued) do
          IO.puts "== Compilation failed =="
          IO.puts "Compilation failed on the following files:\n"

          Enum.each Enum.reverse(queued), fn { pid, file } ->
            case List.keyfind(waiting, pid, 0) do
              { _, mod } -> IO.puts "* #{file} is missing module #{inspect mod}"
              _ -> :ok
            end
          end

          IO.puts "\nThe first failure is shown below..."
        end

        {^child, file} = List.keyfind(queued, child, 0)
        IO.puts "== Compilation error on file #{file} =="
        :erlang.raise(kind, reason, stacktrace)
    end
  end

  defp many_missing?(child, files, waiting, queued) do
    waiting_length = length(waiting)

    match?({ ^child, _ }, List.keyfind(waiting, child, 0)) and
      waiting_length > 1 and files == [] and
      waiting_length == length(queued)
  end

  # Release waiting processes that are waiting for the given module
  defp release_waiting_processes(module, waiting) do
    Enum.filter waiting, fn { child, waiting_module } ->
      if waiting_module == module do
        child <- { :release, self() }
        false
      else
        true
      end
    end
  end
end
