module Elixir::CLI do
  defrecord Config, commands: [], close: [], halt: true, output: '.', compile: false

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
      List.map all_commands, fn(c) { process_command(c, config) }
    catch: { :throw, reason, _ }
      IO.puts :standard_error, "** throw #{Elixir::Formatter.format_catch(:throw, reason)}"
      print_stacktrace(Code.stacktrace)
      halt(1)
    catch: { :error, reason, _ }
      IO.puts :standard_error, "** error #{Elixir::Formatter.format_catch(:error, reason)}"
      print_stacktrace(Code.stacktrace)
      halt(1)
    end

    if config.halt do
      halt(0)
    else:
      Erlang.timer.sleep(:infinity)
    end
  end

  ## Private

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
    List.each stacktrace, fn(s) { IO.puts :standard_error, "    #{Elixir::Formatter.format_stacktrace(s)}" }
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
    Erlang.code.add_patha(h)
    process_shared t, config
  end

  defp process_shared(['-pz',h|t], config) do
    Erlang.code.add_pathz(h)
    process_shared t, config
  end

  defp process_shared(['-f',h|t], config) do
    process_shared t, config.prepend_close [{:eval,h}]
  end

  defp process_shared(list, config) do
    { list, config }
  end

  # Process init options

  defp process_options(['--no-halt'|t], config) do
    process_options t, config.halt(false)
  end

  defp process_options(['--'|t], config) do
    { config, t }
  end

  defp process_options(['+compile'|t], config) do
    process_compiler t, config.compile(true)
  end

  defp process_options([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, fn(nl, ns){ process_options(nl, ns) }
    else:
      { config.prepend_commands([{:load, h}]), t }
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
      shared_option? list, config, fn(nl, ns){ process_compiler(nl, ns) }
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

  defp process_command({:load, file}, _config) do
    Code.require_file file
  end

  defp process_command({:compile, pattern}, config) do
    compile_patterns [pattern], config
  end

  defp compile_patterns(lines, config) do
    lines = List.map lines, fn(line){ Erlang.elixir_glob.wildcard(line, '.') }
    List.map List.uniq(List.append(lines)), fn(file) {
      IO.puts "Compiling #{list_to_binary(file)}"
      Erlang.elixir_compiler.file_to_path(file, config.output)
    }
  end
end