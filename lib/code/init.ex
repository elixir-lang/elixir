% elixir: cache

% Responsible for starting Elixir from the command line.
module Code::Init
  proto Code::Formatter

  protected

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { commands, argv } = process_options(options, [], [], false)
    GenServer.call('elixir_code_server, { 'argv, argv })

    try
      commands.each -> (c) process_command(c)
    catch kind: error
      IO.puts 'standard_error, "** #{kind} #{self.format_catch(kind, error)}"
      print_stacktrace(self.__stacktrace__)
    end

    halt!
  end

  private

  def halt!
    Erlang.halt()
  end

  def process_options([$"-v"|_], _, _, _)
    IO.puts "Elixir #{Code.version}"
    halt!
  end

  def process_options([$"-e",h|t], commands, close, files)
    process_options(t, [{'eval, h}|commands], close, files)
  end

  def process_options([$"-f",h|t], commands, close, files)
    process_options(t, commands, [{'eval, h}|close], files)
  end

  def process_options([h|t], commands, close, files)
    if h.to_char_list[0] == $-
      if files
        { commands.reverse + close.reverse, [h|t].map -> (i) String.new(i) }
      else
        IO.puts "Unknown option #{String.new h}"
        halt!
      end
    else
      process_options(t, [{'require,h}|commands], close, true)
    end
  end

  def process_options([], commands, close, _)
    { commands.reverse + close.reverse, [] }
  end

  def process_command({'eval, expr})
    Erlang.elixir.eval(expr, [])
  end

  def process_command({'require, file})
    Code.require_file file
  end

  def print_stacktrace(stacktrace)
    stacktrace.each -> (s) IO.puts 'standard_error, "    #{self.format_stacktrace(s)}"
  end
end