module Elixir::CLI

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
  List.map all_commands, fn(c) { process_command(c, config) }

  if config.halt do
    halt(0)
  else:
    Erlang.timer.sleep(:infinity)
  end
end

private

def invalid_option(option) do
  Erlang.io.format(:standard_error, "Unknown option #{list_to_binary(option)}\n")
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

# Process shared options

def process_shared(['-v'|t], config) do
  Erlang.io.format "Elixir #{Code.version}\n"
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
    shared_option? list, config, fn(nl, ns){ process_options(nl, ns) }
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
    shared_option? list, config, fn(nl, ns){ process_compiler(nl, ns) }
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
  lines = List.map lines, fn(line){ Erlang.elixir_glob.wildcard(line, '.') }
  List.map List.uniq(List.append(lines)), fn(file) {
    Erlang.io.format "Compiling #{list_to_binary(file)}\n"
    Erlang.elixir_compiler.file_to_path(file, config.output)
  }
end