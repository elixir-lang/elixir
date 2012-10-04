defrecord Kernel.CLI.Config, commands: [], output: ".",
                             compile: [], halt: true, compiler_options: []

defmodule Kernel.CLI do
  @moduledoc false

  # Invoked directly from erlang boot process. It parses all argv
  # options and execute them in the order they are specified.
  def process_argv(options) do
    { config, argv } = process_options(options, Kernel.CLI.Config.new)

    argv = lc arg inlist argv, do: list_to_binary(arg)
    Erlang.gen_server.call(:elixir_code_server, { :argv, argv })

    all_commands = Enum.reverse(config.commands)

    try do
      Enum.map all_commands, process_command(&1, config)
      if config.halt do
        at_exit(0)
        halt(0)
      end
    rescue
      exception ->
        at_exit(1)
        trace = System.stacktrace
        IO.puts :stderr, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
        IO.puts Exception.formatted_stacktrace(trace)
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
        trace = System.stacktrace
        IO.puts :stderr, "** (#{kind}) #{inspect(reason)}"
        IO.puts Exception.formatted_stacktrace(trace)
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
          trace = System.stacktrace
          IO.puts :stderr, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          IO.puts Exception.formatted_stacktrace(trace)
      catch
        kind, reason ->
          trace = System.stacktrace
          IO.puts :stderr, "** #{kind} #{inspect(reason)}"
          IO.puts Exception.formatted_stacktrace(trace)
      end
    end
  end

  defp invalid_option(option) do
    IO.puts(:stderr, "Unknown option #{list_to_binary(option)}")
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

  # Process shared options

  defp process_shared(['-v'|t], config) do
    IO.puts "Elixir #{System.version}"
    process_shared t, config
  end

  defp process_shared(['--no-halt'|t], config) do
    process_shared t, config.halt(false)
  end

  defp process_shared(['-e',h|t], config) do
    process_shared t, config.prepend_commands [eval: h]
  end

  defp process_shared(['-pa',h|t], config) do
    Enum.each File.wildcard(File.expand_path(h)), Code.prepend_path(&1)
    process_shared t, config
  end

  defp process_shared(['--config',file|t], config) do
    File.rm file # because it was temporarily generated anyway
    process_shared t, config
  end

  defp process_shared(['-pz',h|t], config) do
    Enum.each File.wildcard(File.expand_path(h)), Code.append_path(&1)
    process_shared t, config
  end

  defp process_shared(['-r',h|t], config) do
    h = list_to_binary(h)
    config = Enum.reduce File.wildcard(h), config, fn path, config ->
      config.prepend_commands [require: path]
    end
    process_shared t, config
  end

  defp process_shared(['-pr',h|t], config) do
    h = list_to_binary(h)
    process_shared t, config.prepend_commands [parallel_require: h]
  end

  defp process_shared([erl,_|t], config) when erl in ['--erl', '--sname', '--remsh', '--name'] do
    process_shared t, config
  end

  defp process_shared(list, config) do
    { list, config }
  end

  # Process init options

  def process_options(['--'|t], config) do
    { config, t }
  end

  def process_options(['--compile'|t], config) do
    process_compiler t, config
  end

  def process_options(['-S',h|t], config) do
    exec = System.find_executable(h)
    if exec do
      { config.prepend_commands([require: list_to_binary(exec)]), t }
    else
      IO.puts(:stderr, "Could not find executable #{h}")
      halt(1)
    end
  end

  def process_options([h|t] = list, config) do
    case h do
      '-' ++ _ ->
        shared_option? list, config, process_options(&1, &2)
      _ ->
        h = list_to_binary(h)
        { config.prepend_commands([require: h]), t }
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

  defp process_compiler(['--no-docs'|t], config) do
    process_compiler t, config.merge_compiler_options(docs: false)
  end

  defp process_compiler(['--debug-info'|t], config) do
    process_compiler t, config.merge_compiler_options(debug_info: true)
  end

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
    { config.prepend_commands([compile: config.compile]), [] }
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
    Kernel.ParallelRequire.files(files)
  end

  defp process_command({:compile, patterns}, config) do
    File.mkdir_p(config.output)

    files = Enum.map patterns, File.wildcard(&1)
    files = List.uniq(List.concat(files))
    files = Enum.filter files, File.regular?(&1)

    Code.compiler_options(config.compiler_options)
    Kernel.ParallelCompiler.files_to_path(files, config.output,
      fn file -> IO.puts "Compiled #{file}" end)
  end
end
