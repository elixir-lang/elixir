% Responsible for starting Elixir from the command line.
module Code::Init
  mixin Code::Formatter

  module Config
    attr_accessor ['output]

    def __bound__()
      @('commands: [], 'close: [], 'halt: true, 'output: ".", 'compile: false)
    end

    def compiling?
      @compile
    end

    def compile!
      @('compile, true)
    end

    def halt?
      @halt
    end

    def no_halt!
      @('halt: false)
    end

    def add_command(c)
      @('commands: [c|@commands])
    end

    def add_close(c)
      @('close: [c|@close])
    end

    def final_commands
      @commands.reverse + @close.reverse
    end
  end

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { config, argv } = process_options(options, #Code::Init::Config())
    GenServer.call('elixir_code_server, { 'argv, argv })

    if config.compiling?
      Erlang.file.make_dir(config.output.to_char_list)
    end

    try
      [process_command(c, config) for c in config.final_commands]
    catch kind: error
      if exiting?(kind, error)
        halt!(error)
      else
        io = IO.new('standard_error)
        io.puts "** #{kind} #{format_catch(kind, error)}"
        print_stacktrace(io, __stacktrace__)
        halt!(1)
      end
    end

    if config.halt?
      halt!(0)
    else
      % TODO Is there a better way to suspend the process?
      Erlang.timer.sleep('infinity)
    end
  end

  private

  def halt!(0)
    Erlang.init.stop
  end

  def halt!(status)
    case status.__parent_name__
    match 'String
      Erlang.halt(status.to_char_list)
    match 'Integer
      Erlang.halt(status)
    end
  end

  def exiting?(kind, error)
    kind == 'exit && ['String, 'Integer].include?(error.__parent_name__)
  end

  def print_stacktrace(io, stacktrace)
    stacktrace.each -> (s) io.puts "    #{format_stacktrace(s)}"
  end

  def invalid_option(option)
    IO.new('standard_error).puts "Unknown option #{option.to_bin}"
    halt!(1)
  end

  def shared_option?(list, state, callback)
    case process_shared(list, state)
    match { ~list, _ }
      invalid_option list.head
    match { new_list, new_state }
      callback.(new_list, new_state)
    end
  end

  % Process shared options

  def process_shared([$"-v"|_], _)
    IO.puts "Elixir #{Code.version}"
    halt!(0)
  end

  def process_shared([$"-e",h|t], state)
    process_shared t, state.add_command({'eval, h})
  end

  def process_shared([$"-pa",h|t], state)
    Code.prepend_path(h)
    process_shared t, state
  end

  def process_shared([$"-pz",h|t], state)
    Code.append_path(h)
    process_shared t, state
  end

  def process_shared([$"-f",h|t], state)
    process_shared t, state.add_close({'eval, h})
  end

  def process_shared(list, state)
    { list, state }
  end

  % Process init options

  def process_options([$"--no-halt"|t], state)
    process_options t, state.no_halt!
  end

  def process_options([$"--"|t], state)
    { state, t.map(_.to_bin) }
  end

  def process_options([$"+compile"|t], state)
    process_compiler t, state.compile!
  end

  def process_options([h|t] = list, state)
    if h.to_char_list[0] == $-
      shared_option? list, state, -> (nl, ns) process_options(nl, ns)
    else
      { state.add_command({'load,h}), t.map(_.to_bin) }
    end
  end

  def process_options([], state)
    { state, [] }
  end

  % Process compiler options

  def process_compiler([$"--"|t], state)
    { state, t.map(_.to_bin) }
  end

  def process_compiler([$"-o",h|t], state)
    process_compiler t, state.output(h)
  end

  def process_compiler([$"-s",h|t], state)
    process_compiler t, state.add_command({'compile_spec,h})
  end

  def process_compiler([h|t] = list, state)
    if h.to_char_list[0] == $-
      shared_option? list, state, -> (nl, ns) process_compiler(nl, ns)
    else
      process_compiler t, state.add_command({'compile,h})
    end
  end

  def process_compiler([], state)
    { state, [] }
  end

  % Process commands

  def process_command({'eval, expr}, _state)
    Erlang.elixir.eval(expr, [])
  end

  def process_command({'load, file}, _state)
    Code.load_file(file)
  end

  def process_command({'compile, pattern}, state)
    compile_patterns [pattern], state
  end

  def process_command({'compile_spec, spec}, state)
    contents = File.read(spec)
    compile_patterns contents.split(~r"\n"), state
  end

  def compile_patterns(lines, state)
    lines = [Erlang.filelib.wildcard(line.to_char_list) for line in lines]
    lines.flatten_lists.uniq.each do (file)
      IO.puts "Compiling #{file.to_bin}"
      Code.compile_file_to_path(file, state.output)
    end
  end
end