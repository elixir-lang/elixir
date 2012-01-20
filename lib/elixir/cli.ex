defrecord Elixir::CLI::Config, commands: [], close: [], halt: true, output: '.', compile: false

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
      halt(reason)
    catch: kind, reason
      IO.puts :standard_error, "** #{kind} #{format_catch(kind, reason)}"
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
  @visibility :private

  def invalid_option(option) do
    IO.puts(:standard_error, "Unknown option #{list_to_binary(option)}")
    halt(1)
  end

  def shared_option?(list, config, callback) do
    case process_shared(list, config) do
    match: { [h|t], _ } when h == hd(list)
      invalid_option h
    match: { new_list, new_config }
      callback.(new_list, new_config)
    end
  end

  def print_stacktrace(stacktrace) do
    Enum.each stacktrace, fn(s) { IO.puts :standard_error, "    #{format_stacktrace(s)}" }
  end

  # Process shared options

  def process_shared(['-v'|t], config) do
    IO.puts "Elixir #{Code.version}"
    process_shared t, config
  end

  def process_shared(['-e',h|t], config) do
    process_shared t, config.prepend_commands [{:eval,h}]
  end

  def process_shared(['-pa',h|t], config) do
    Erlang.code.add_patha(h)
    process_shared t, config
  end

  def process_shared(['-pz',h|t], config) do
    Erlang.code.add_pathz(h)
    process_shared t, config
  end

  def process_shared(['-f',h|t], config) do
    process_shared t, config.prepend_close [{:eval,h}]
  end

  def process_shared(list, config) do
    { list, config }
  end

  # Process init options

  def process_options(['--no-halt'|t], config) do
    process_options t, config.halt(false)
  end

  def process_options(['--'|t], config) do
    { config, t }
  end

  def process_options(['+compile'|t], config) do
    process_compiler t, config.compile(true)
  end

  def process_options([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, process_options(_, _)
    else:
      { config.prepend_commands([{:load, h}]), t }
    end
  end

  def process_options([], config) do
    { config, [] }
  end

  # Process compiler options

  def process_compiler(['--'|t], config) do
    { config, t }
  end

  def process_compiler(['-o',h|t], config) do
    process_compiler t, config.output(h)
  end

  def process_compiler([h|t] = list, config) do
    case h do
    match: '-' ++ _
      shared_option? list, config, process_compiler(_, _)
    else:
      process_compiler t, config.prepend_commands[{:compile,h}]
    end
  end

  def process_compiler([], config) do
    { config, [] }
  end

  # Process commands

  def process_command({:eval, expr}, _config) do
    Erlang.elixir.eval(expr, [])
  end

  def process_command({:load, file}, _config) do
    Code.require_file file
  end

  def process_command({:compile, pattern}, config) do
    compile_patterns [pattern], config
  end

  def compile_patterns(lines, config) do
    lines  = Enum.map lines, File.wildcard(_)
    concat = List.uniq(List.append(lines))

    Enum.map concat, fn(file) {
      IO.puts "Compiling #{list_to_binary(file)}"
      Code.compile_file_to_dir(file, config.output)
    }
  end
end