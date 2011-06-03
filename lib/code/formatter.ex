% This module contains some utilities function that formats objects
% and backtraces to be exhibited in output.
module Code::Formatter
  def format_stacktrace({module, method, arity})
    if Erlang.is_list(arity)
      "#{module}##{method}(#{format_object(arity)[1,-2]})"
    else
      "#{module}##{method}/#{arity}"
    end
  end

  def format_object(object)
    if object != [] && Erlang.io_lib.printable_list(object)
      object.flatten.to_bin
    else
      object.inspect
    end
  end

  def format_catch('error, {'badsyntax, {line, filename, error, token}})
    "\n#{filename.to_bin}:#{line}: #{error.to_bin} #{format_token token}"
  end

  def format_catch('error, {'badform, {line, filename, module, desc}})
    formatted = Erlang.elixir_errors.format_error(module, desc)
    "\n#{filename.to_bin}:#{line}: #{formatted.to_bin}"
  end

  def format_catch(_, reason)
    format_object(reason)
  end

  private

  def format_token([])
    "[]"
  end

  def format_token(obj)
    obj.to_bin
  end
end