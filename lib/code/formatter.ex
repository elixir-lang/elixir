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
    object.inspect
  catch 'error: 'undef
    "[Could not inspect object]"
  end

  def format_catch('error, {'badsyntax, {line, filename, error, token}})
    "\n#{filename}:#{line}: #{error} #{token}".rstrip
  end

  def format_catch('error, {'badform, {line, filename, module, desc}})
    formatted = Erlang.elixir_errors.format_error(module, desc)
    "\n#{filename}:#{line}: #{formatted.to_bin}"
  end

  def format_catch(_, reason)
    format_object(reason)
  end
end