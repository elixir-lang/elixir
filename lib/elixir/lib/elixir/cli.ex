defrecord Elixir.CLI.Config, commands: [], close: [],
  output: ".", compile: [], halt: true, compiler_options: []

defmodule Elixir.CLI do
  @moduledoc false

  import Exception, only: [format_stacktrace: 1]

  # Invoked directly from erlang boot process. It parses all argv
  # options and execute them in the order they are specified.
  def process_argv(options) do
    { config, argv } = process_options(options, Elixir.CLI.Config.new)

    argv = lc arg inlist argv, do: list_to_binary(arg)
    Erlang.gen_server.call(:elixir_code_server, { :argv, argv })

    all_commands = List.reverse(config.commands) ++ List.reverse(config.close)

    try do
      Enum.map all_commands, process_command(&1, config)
      if config.halt do
        at_exit(0)
        halt(0)
      end
    rescue
      exception ->
        at_exit(1)
        stacktrace = System.stacktrace
        IO.puts :standard_error, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
        print_stacktrace(stacktrace)
        halt(1)
    catch
      :exit, reason when is_integer(reason) ->
        at_exit(reason)
        halt(reason)
      :exit, :normal ->
        at_exit(0)
        halt(0)
      kind, reason ->
        at_exit(1)
        stacktrace = System.stacktrace
        IO.puts :standard_error, "** (#{kind}) #{inspect(reason)}"
        print_stacktrace(stacktrace)
        halt(1)
    end
  end

  ## Private

  defp at_exit(status) do
    hooks = Erlang.gen_server.call(:elixir_code_server, :at_exit)
    lc hook inlist hooks do
      try do
        hook.(status)
      rescue
        exception ->
          IO.puts :standard_error, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          print_stacktrace(System.stacktrace)
      catch
        kind, reason ->
          IO.puts :standard_error, "** #{kind} #{inspect(reason)}"
          print_stacktrace(System.stacktrace)
      end
    end
  end

  defp invalid_option(option) do
    IO.puts(:standard_error, "Unknown option #{list_to_binary(option)}")
    halt(1)
  end

  defp shared_option?(list, config, callback) do
    case process_shared(list, config) do
      { [h|t], _ } when h == hd(list) ->
        invalid_option h
      { new_list, new_config } ->
        callback.(new_list, new_config)
    end
  end

  defp print_stacktrace(stacktrace) do
    Enum.each stacktrace, fn s -> IO.puts :standard_error, "    #{format_stacktrace(s)}" end
  end

  # Process shared options

  defp process_shared(['-v'|t], config) do
    IO.puts "Elixir #{System.version}"
    process_shared t, config
  end

  defp process_shared(['-e',h|t], config) do
    process_shared t, config.prepend_commands [{:eval,h}]
  end

  defp process_shared(['-pa',h|t], config) do
    Enum.each File.wildcard(File.expand_path(h)), Code.prepend_path(&1)
    process_shared t, config
  end

  defp process_shared(['-pz',h|t], config) do
    Enum.each File.wildcard(File.expand_path(h)), Code.append_path(&1)
    process_shared t, config
  end

  defp process_shared(['-r',h|t], config) do
    h = list_to_binary(h)
    config = Enum.reduce File.wildcard(h), config, fn path, config ->
      config.prepend_commands [{:require, path}]
    end
    process_shared t, config
  end

  defp process_shared(['-pr',h|t], config) do
    h = list_to_binary(h)
    process_shared t, config.prepend_commands [{:parallel_require, h}]
  end

  defp process_shared(list, config) do
    { list, config }
  end

  # Process init options

  def process_options(['--'|t], config) do
    { config, t }
  end

  def process_options(['--no-halt'|t], config) do
    process_options t, config.halt(false)
  end

  def process_options(['+compile'|t], config) do
    process_compiler t, config
  end

  def process_options([h|t] = list, config) do
    case h do
      '-' ++ _ ->
        shared_option? list, config, process_options(&1, &2)
      _ ->
        h = list_to_binary(h)
        { config.prepend_commands([{:require, h}]), t }
    end
  end

  def process_options([], config) do
    { config, [] }
  end

  # Process compiler options

  defp process_compiler(['--'|t], config) do
    { config, t }
  end

  defp process_compiler(['-o',h|t], config) do
    process_compiler t, config.output(list_to_binary(h))
  end

  defp process_compiler(['--docs'|t], config) do
    process_compiler t, config.merge_compiler_options(docs: true)
  end

  defp process_compiler(['--debug-info'|t], config) do
    process_compiler t, config.merge_compiler_options(debug_info: true)
  end

  # This option is used internally so we can compile
  # Elixir with Elixir without raising module conflicts
  defp process_compiler(['--ignore-module-conflict'|t], config) do
    process_compiler t, config.merge_compiler_options(ignore_module_conflict: true)
  end

  defp process_compiler([h|t] = list, config) do
    case h do
      '-' ++ _ ->
        shared_option? list, config, process_compiler(&1, &2)
      _ ->
        h = list_to_binary(h)
        pattern = if File.dir?(h), do: "#{h}/**/*.ex", else: h
        process_compiler t, config.prepend_compile [pattern]
    end
  end

  defp process_compiler([], config) do
    { config.prepend_commands([{:compile, config.compile}]), [] }
  end

  # Process commands

  defp process_command({:eval, expr}, _config) when is_list(expr) do
    Erlang.elixir.eval(expr, [])
  end

  defp process_command({:require, file}, _config) when is_binary(file) do
    Code.require_file(file)
  end

  defp process_command({:parallel_require, pattern}, _config) when is_binary(pattern) do
    files = File.wildcard(pattern)
    files = List.uniq(files)
    files = Enum.filter files, File.regular?(&1)
    spawn_requires(files, [])
  end

  defp process_command({:compile, patterns}, config) do
    File.mkdir_p(config.output)

    files = Enum.map patterns, File.wildcard(&1)
    files = List.uniq(List.concat(files))
    files = Enum.filter files, File.regular?(&1)

    Code.compiler_options(config.compiler_options)
    Elixir.ParallelCompiler.files_to_path(files, config.output,
      fn file -> IO.puts "Compiled #{file}" end)
  end

  # Responsible for spawning requires in parallel
  # For now, we spawn at maximum four processes at the same time

  defp spawn_requires([], []),      do: :done
  defp spawn_requires([], waiting), do: wait_for_messages([], waiting)

  defp spawn_requires(files, waiting) when length(waiting) >= 4 do
    wait_for_messages(files, waiting)
  end

  defp spawn_requires([h|t], waiting) do
    parent = Process.self

    child  = spawn_link fn ->
      try do
        Code.require_file(h)
        parent <- { :required, Process.self }
      catch
        kind, reason ->
          parent <- { :failure, Process.self, kind, reason, System.stacktrace }
      end
    end

    spawn_requires(t, [child|waiting])
  end

  defp wait_for_messages(files, waiting) do
    receive do
      { :required, child } ->
        spawn_requires(files, List.delete(waiting, child))
      { :failure, _child, kind, reason, stacktrace } ->
        Erlang.erlang.raise(kind, reason, stacktrace)
    end
  end
end
