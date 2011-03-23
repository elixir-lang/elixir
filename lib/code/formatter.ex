% elixir: cache
% This module contains some utilities function that formats objects
% and backtraces to be exhibited in output.
% TODO Add tests
module Code::Formatter
  def format_stacktrace({module, method, arity})
    if arity.__parent_name__ == 'List
      "#{module}##{method}(#{format_object(arity)[1,-2]})"
    else
      "#{module}##{method}/#{arity}"
    end
  end

  def format_object(object)
    if object != [] && Erlang.io_lib.printable_list(object)
      String.new object.flatten
    else
      object.inspect
    end
  end

  def format_catch('error, {'badsyntax, {line, filename, error, token}})
    "\n#{String.new filename}:#{line}: #{String.new error} #{format_token token}"
  end

  def format_catch('error, {'badform, {line, filename, module, desc}})
    formatted = Erlang.elixir_errors.format_error(module, desc)
    "\n#{String.new filename}:#{line}: #{String.new formatted}"
  end

  def format_catch(_, reason)
    format_object(reason)
  end

  private

  def format_token([])
    "[]"
  end

  def format_token(obj)
    String.new obj
  end
end