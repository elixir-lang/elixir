% elixir: cache

% Responsible for starting Elixir from the command line.
module Code::Init
  proto Code::Formatter

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { commands, argv, halt } = process_options(options, [], [], false, true)
    GenServer.call('elixir_code_server, { 'argv, argv })

    try
      commands.each -> (c) process_command(c)
    catch kind: error
      if kind == 'exit && ['String, 'Integer].include?(error.__parent_name__)
        halt!(error)
      else
        io = IO.new('standard_error)
        io.puts "** #{kind} #{format_catch(kind, error)}"
        print_stacktrace(io, __stacktrace__)
        halt!(1)
      end
    end

    if halt
      halt!(0)
    else
      % TODO Is there a better way to suspend the process?
      Erlang.timer.sleep('infinity)
    end
  end

  private

  def halt!(status)
    Erlang.halt(status)
  end

  def process_options([$"-v"|_], _, _, _, _)
    IO.puts "Elixir #{Code.version}"
    halt!(0)
  end

  def process_options([$"-e",h|t], commands, close, _files, halt)
    process_options(t, [{'eval, h}|commands], close, true, halt)
  end

  def process_options([$"-f",h|t], commands, close, _files, halt)
    process_options(t, commands, [{'eval, h}|close], true, halt)
  end

  def process_options([$"--no-halt"|t], commands, close, files, _)
    process_options(t, commands, close, files, false)
  end

  def process_options([h|t], commands, close, files, halt)
    { final, extra } = if h.to_char_list[0] == $-
      if files
        { commands, [h|t] }
      else
        IO.new('standard_error).puts "Unknown option #{String.new h}"
        halt!(1)
      end
    else
      { [{'require,h}|commands], t }
    end

    { final.reverse + close.reverse, extra.map(-> (i) String.new(i)), halt }
  end

  def process_options([], commands, close, _, halt)
    { commands.reverse + close.reverse, [], halt }
  end

  def process_command({'eval, expr})
    Erlang.elixir.eval(expr, [])
  end

  def process_command({'require, file})
    Code.require_file file
  end

  def print_stacktrace(io, stacktrace)
    stacktrace.each -> (s) io.puts "    #{self.format_stacktrace(s)}"
  end
end