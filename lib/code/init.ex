% elixir: cache

% Responsible for starting Elixir from the command line.
module Code::Init
  mixin Code::Formatter

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { commands, argv, halt } = process_options(options, [], [], false, true)
    GenServer.call('elixir_code_server, { 'argv, argv })

    [process_command(c) for c in commands]

    if halt
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
        IO.new('standard_error).puts "Unknown option #{h.to_bin}"
        halt!(1)
      end
    else
      { [{'require,h}|commands], t }
    end

    { final.reverse + close.reverse, extra.map(_.to_bin), halt }
  end

  def process_options([], commands, close, _, halt)
    { commands.reverse + close.reverse, [], halt }
  end

  def process_command({'eval, expr})
    with_catch -> Erlang.elixir.eval(expr, [])
  end

  def process_command({'require, file})
    with_catch -> Code.load_file(file), do (io, kind, _error)
      io.puts "** #{kind} in #{file.to_bin}"
    end
  end

  def with_catch(command, callback := nil)
    command()
  catch kind: error
    if exiting?(kind, error)
      case error.__parent_name__
      match 'String
        halt!(error.to_char_list)
      match 'Integer
        halt!(error)
      end
    else
      io = IO.new('standard_error)
      if callback
        callback(io, kind, error)
      end
      io.puts "** #{kind} #{format_catch(kind, error)}"
      print_stacktrace(io, __stacktrace__)
      halt!(1)
    end
  end

  def exiting?(kind, error)
    kind == 'exit && ['String, 'Integer].include?(error.__parent_name__)
  end

  def print_stacktrace(io, stacktrace)
    stacktrace.each -> (s) io.puts "    #{self.format_stacktrace(s)}"
  end
end