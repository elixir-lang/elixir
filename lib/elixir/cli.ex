defrecord Elixir::CLI::Config, commands: [], close: [], output: '.', compile: false

defmodule Elixir::CLI do
  import Elixir::Formatter, only: [format_catch: 2, format_stacktrace: 1]

  # Invoked directly from erlang boot process. It parses all argv
  # options and execute them in the order they are specified.
  def process_argv(options) do
    { config, argv } = process_options(options, Elixir::CLI::Config.new)
    Erlang.gen_server.call(:elixir_code_server, { :argv, argv })

    if config.compile do
      Erlang.file.make_dir(config.output)
    end

    all_commands = List.reverse(config.commands) ++ List.reverse(config.close)

    try do
      Enum.map all_commands, process_command(_, config)
    catch: :exit, reason when is_integer(reason)
      at_exit(reason)
      halt(reason)
    catch: kind, reason
      at_exit(1)
      IO.puts :standard_error, "** #{kind} #{format_catch(kind, reason)}"
      print_stacktrace(Code.stacktrace)
      halt(1)
    end

    at_exit(0)
    halt(0)
  end

  ## Private

  defp at_exit(status) do
    hooks = Erlang.gen_server.call(:elixir_code_server, :at_exit)
    lc hook in hooks do
      try do
        hook.(status)
      catch: kind, reason
        IO.puts :standard_error, "** #{kind} #{format_catch(kind, reason)}"
        print_stacktrace(Code.stacktrace)
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
    Enum.each stacktrace, fn(s) { IO.puts :standard_error, "    #{format_stacktrace(s)}" }
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

  defp process_shared(['-r',h|t], config) do
    process_shared t, config.prepend_commands [{:require,h}]
  end

  defp process_shared(list, config) do
    { list, config }
  end

  # Process init options

  defp process_options(['--'|t], config) do
    { config, t }
  end

  defp process_options(['+compile'|t], config) do
    process_compiler t, config.compile(true)
  end

  defp process_options([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, process_options(_, _)
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

  defp process_compiler([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, process_compiler(_, _)
    else:
      process_compiler t, config.prepend_commands[{:compile,h}]
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
    Enum.each File.wildcard(file), Code.require_file(_)
  end

  defp process_command({:compile, pattern}, config) do
    compile_patterns [pattern], config
  end

  defp compile_patterns(lines, config) do
    lines  = Enum.map lines, File.wildcard(_)
    concat = List.uniq(List.append(lines))

    Enum.map concat, fn(file) {
      IO.puts "Compiling #{list_to_binary(file)}"
      Code.compile_file_to_dir(file, config.output)
    }
  end
end