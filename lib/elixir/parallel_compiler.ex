defmodule Elixir.ParallelCompiler do
  refer Erlang.orddict, as: Orddict

  defexception Error, modules: [] do
    def message(exception) do
      "compilation failed: the following modules were not found " <>
        "or there is a cyclic dependency between them: #{inspect exception.modules}"
    end
  end

  def files_to_path(files, path) do
    Erlang.code.ensure_loaded(:elixir_error_handler)
    files = Enum.map(files, to_char_list(&1))
    path  = to_char_list(path)
    spawn_compilers(files, path, [], [])
  end

  # Spawn a compiler for each file in the list and wait for messages
  defp spawn_compilers([h|t], output, waiting, queued) do
    parent = Process.self()
    child  = spawn_link fn ->
      Process.put(:elixir_parent_compiler, parent)
      Process.flag(:error_handler, :elixir_error_handler)
      result = Erlang.elixir_compiler.file_to_path(h, output)
      parent <- { :compiled, Process.self(), result }
    end
    wait_for_messages(t, output, waiting, [child|queued])
  end

  # No more files, nothing waiting, queue is empty, we are done
  defp spawn_compilers([], _output, [], []), do: :ok

  # No more files, nothing waiting, queue is not empty, wait to fnish
  defp spawn_compilers([], output, [], queued) do
    wait_for_messages([], output, [], queued)
  end

  # No more files, but we are waiting: no file or cyclic dependencies
  defp spawn_compilers([], _output, waiting, _queued) do
    raise Error, Enum.map(waiting, elem(&1, 2))
  end

  # Wait for messages from child processes
  defp wait_for_messages(files, output, waiting, queued) do
    receive do
    match: { :compiled, child, result }
      new_waiting = List.foldl(result, waiting, release_waiting_processes(&1, &2))
      new_queued  = List.delete(queued, child)

      # Nobody was unblocked or we are not waiting on anyone
      if new_waiting == [] or new_waiting == waiting do
        spawn_compilers(files, output, new_waiting, new_queued)
      else:
        wait_for_messages(files, output, new_waiting, new_queued)
      end
    match: { :waiting, child, on }
      spawn_compilers(files, output, Orddict.store(child, on, waiting), queued)
    match: { :EXIT, _child, { reason, where } }
      Erlang.erlang.raise(:error, reason, where)
    end
  end

  # Release waiting processes that are waiting for the given module
  defp release_waiting_processes({ module, _binary }, waiting) do
    Enum.filter waiting, fn({ child, waiting_module }) ->
      if waiting_module == module do
        child <- { :release, Process.self() }
        false
      else:
        true
      end
    end
  end
end