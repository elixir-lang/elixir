% elixir: cache

% Responsible for starting Elixir from the command line.
module Code::Init
  proto Code::Formatter

  protected

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { commands, argv, halt } = process_options(options, [], [], false, true)
    GenServer.call('elixir_code_server, { 'argv, argv })

    try
      commands.each -> (c) process_command(c)
    catch kind: error
      io = IO.new('standard_error)
      io.puts "** #{kind} #{self.format_catch(kind, error)}"
      print_stacktrace(io, self.__stacktrace__)
      halt!(1)
    end

    if halt then halt!(0) end
  end

  private

  def halt!(status)
    Erlang.halt(status)
  end

  def process_options([$"-v"|_], _, _, _, _)
    IO.puts "Elixir #{Code.version}"
    halt!(0)
  end

  def process_options([$"-e",h|t], commands, close, files, halt)
    process_options(t, [{'eval, h}|commands], close, files, halt)
  end

  def process_options([$"-f",h|t], commands, close, files, halt)
    process_options(t, commands, [{'eval, h}|close], files, halt)
  end

  def process_options([$"--no-halt"|t], commands, close, files, _)
    process_options(t, commands, close, files, false)
  end

  def process_options([h|t], commands, close, files, halt)
    if h.to_char_list[0] == $-
      if files
        { commands.reverse + close.reverse, [h|t].map(-> (i) String.new(i)), halt }
      else
        IO.new('standard_error).puts "Unknown option #{String.new h}"
        halt!(1)
      end
    else
      process_options(t, [{'require,h}|commands], close, true, halt)
    end
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