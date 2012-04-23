defrecord Elixir.CLI.Config, commands: [], close: [],
  output: '.', compile: false, halt: true, compiler_options: []

defmodule Elixir.CLI do
  import Exception, only: [format_stacktrace: 1]

  # Invoked directly from erlang boot process. It parses all argv
  # options and execute them in the order they are specified.
  def process_argv(options) do
    { config, argv } = process_options(options, Elixir.CLI.Config.new)

    argv = lc arg in argv, do: list_to_binary(arg)
    Erlang.gen_server.call(:elixir_code_server, { :argv, argv })

    if config.compile do
      Erlang.file.make_dir(config.output)
    end

    all_commands = List.reverse(config.commands) ++ List.reverse(config.close)

    try do
      Enum.map all_commands, process_command(&1, config)
      if config.halt do
        at_exit(0)
        halt(0)
      end
    rescue: exception
      at_exit(1)
      stacktrace = System.stacktrace
      IO.puts :standard_error, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
      print_stacktrace(stacktrace)
      halt(1)
    catch: :exit, reason when is_integer(reason)
      at_exit(reason)
      halt(reason)
    catch: kind, reason
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
    lc hook in hooks do
      try do
        hook.(status)
      rescue: exception
        IO.puts :standard_error, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
        print_stacktrace(System.stacktrace)
      catch: kind, reason
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
    match: { [h|t], _ } when h == hd(list)
      invalid_option h
    match: { new_list, new_config }
      callback.(new_list, new_config)
    end
  end

  defp print_stacktrace(stacktrace) do
    Enum.each stacktrace, fn(s, do: IO.puts :standard_error, "    #{format_stacktrace(s)}")
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
    Enum.each File.wildcard(h), Code.prepend_path(&1)
    process_shared t, config
  end

  defp process_shared(['-pz',h|t], config) do
    Enum.each File.wildcard(h), Code.append_path(&1)
    process_shared t, config
  end

  defp process_shared(['-r',h|t], config) do
    config = Enum.reduce File.wildcard(h), config, fn(path, config) ->
      config.prepend_commands [{:require, path}]
    end
    process_shared t, config
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
    process_compiler t, config.compile(true)
  end

  def process_options([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, process_options(&1, &2)
    else:
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
    process_compiler t, config.output(h)
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
    match: '-' ++ _
      shared_option? list, config, process_compiler(&1, &2)
    else:
      process_compiler t, config.prepend_commands [{:compile,h}]
    end
  end

  defp process_compiler([], config) do
    { config, [] }
  end

  # Process commands

  defp process_command({:eval, expr}, _config) do
    Erlang.elixir.eval(expr, [])
  end

  defp process_command({:require, file}, _config) do
    Code.require_file(file)
  end

  defp process_command({:compile, pattern}, config) do
    if File.dir?(pattern) do
      compile_patterns ["#{pattern}/**/*"], config.merge_compiler_options(autodiscovery: true)
    else:
      compile_patterns [pattern], config
    end
  end

  defp compile_patterns(lines, config) do
    lines  = Enum.map lines, File.wildcard(&1)
    concat = List.uniq(List.concat(lines))

    Code.compiler_options(config.compiler_options)

    Enum.map concat, fn(file) ->
      IO.puts "Compiling #{list_to_binary(file)}"
      Erlang.elixir_compiler.file_to_path(file, config.output)
    end
  end
end
