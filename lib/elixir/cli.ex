defrecord Elixir.CLI.Config, commands: [], close: [],
  output: '.', compile: false, stop: true, compile_options: []

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
      if config.stop do
        at_exit(0)
        stop(0)
      end
    rescue: exception
      at_exit(1)
      stacktrace = Code.stacktrace
      IO.puts :standard_error, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
      print_stacktrace(stacktrace)
      stop(1)
    catch: :exit, reason when is_integer(reason)
      at_exit(reason)
      stop(reason)
    catch: kind, reason
      at_exit(1)
      stacktrace = Code.stacktrace
      IO.puts :standard_error, "** (#{kind}) #{inspect(reason)}"
      print_stacktrace(stacktrace)
      stop(1)
    end
  end

  ## Private

  defp stop(status) do
    Erlang.init.stop(status)
  end

  defp at_exit(status) do
    hooks = Erlang.gen_server.call(:elixir_code_server, :at_exit)
    lc hook in hooks do
      try do
        hook.(status)
      rescue: exception
        IO.puts :standard_error, "** (#{inspect exception.__record__(:name)}) #{exception.message}"
        print_stacktrace(Code.stacktrace)
      catch: kind, reason
        IO.puts :standard_error, "** #{kind} #{inspect(reason)}"
        print_stacktrace(Code.stacktrace)
      end
    end
  end

  defp invalid_option(option) do
    IO.puts(:standard_error, "Unknown option #{list_to_binary(option)}")
    stop(1)
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
    IO.puts "Elixir #{Code.version}"
    process_shared t, config
  end

  defp process_shared(['-e',h|t], config) do
    process_shared t, config.prepend_commands [{:eval,h}]
  end

  defp process_shared(['-pa',h|t], config) do
    Code.prepend_path(h)
    process_shared t, config
  end

  defp process_shared(['-pz',h|t], config) do
    Code.append_path(h)
    process_shared t, config
  end

  defp process_shared(['-r'|t], config) do
    { rest, files } = process_list(t, [])
    requires = lc file in files, do: {:require,file}
    process_shared rest, config.prepend_commands(requires)
  end

  defp process_shared(list, config) do
    { list, config }
  end

  # Process list

  defp process_list(['-'++_|_] = list, acc) do
    { list, List.reverse(acc) }
  end

  defp process_list([h|t], acc) do
    process_list t, [h|acc]
  end

  defp process_list([], acc) do
    { [], List.reverse(acc) }
  end

  # Process init options

  defp process_options(['--'|t], config) do
    { config, t }
  end

  defp process_options(['--no-stop'|t], config) do
    process_options t, config.stop(false)
  end

  defp process_options(['+compile'|t], config) do
    process_compiler t, config.compile(true)
  end

  defp process_options([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, process_options(&1, &2)
    else:
      { config.prepend_commands([{:require, h}]), t }
    end
  end

  defp process_options([], config) do
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
    process_compiler t, config.merge_compile_options(docs: true)
  end

  defp process_compiler(['--debug-info'|t], config) do
    process_compiler t, config.merge_compile_options(debug_info: true)
  end

  # This option is used internally so we can compile
  # Elixir with Elixir without raising module conflicts
  defp process_compiler(['--ignore-module-conflict'|t], config) do
    process_compiler t, config.merge_compile_options(ignore_module_conflict: true)
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
    Enum.each File.wildcard(file), Code.require_file(&1)
  end

  defp process_command({:compile, pattern}, config) do
    compile_patterns [pattern], config
  end

  defp compile_patterns(lines, config) do
    lines  = Enum.map lines, File.wildcard(&1)
    concat = List.uniq(List.concat(lines))

    Erlang.elixir_compiler.set_opts(config.compile_options)

    Enum.map concat, fn(file) ->
      IO.puts "Compiling #{list_to_binary(file)}"
      Erlang.elixir_compiler.file_to_path(file, config.output)
    end
  end
end
