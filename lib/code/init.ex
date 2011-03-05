% elixir: cache

module Code::Init

  protected

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { commands, argv } = process_options(options, [], [], false)
    GenServer.call('elixir_code_server, { 'argv, argv })

    try
      commands.each -> (c) process_command(c)
    catch 'error: {'badsyntax, line, filename, error, token}
      IO.puts 'standard_error, "** #{String.new filename}:#{line} #{String.new error} #{token.to_s}"
      self.__stacktrace__.each -> (s) print_stacktrace(s)
    catch kind: error
      IO.puts 'standard_error, "** #{kind} #{error.inspect}"
      self.__stacktrace__.each -> (s) print_stacktrace(s)
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

  def print_stacktrace({module, method, arity})
    IO.puts 'standard_error, "    #{module}##{method}/#{arity}"
  end
end