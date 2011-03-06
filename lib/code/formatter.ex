% elixir: cache
% This module contains some utilities function that formats objects
% and backtraces to be exhibited in output.
module Code::Formatter
  def format_stacktrace({module, method, arity})
    if arity.__parent__ == 'List
      "#{module}##{method}(#{format_object(arity)})"
    else
      "#{module}##{method}/#{arity}"
    end
  end

  def format_object(object)
    if object.__parent__ == 'List
      try
        String.new Erlang.io_lib.format($"~ts", [object])
      catch 'error: 'badarg
        object.inspect
      end
    else
      object.inspect
    end
  end

  def format_catch('error, {'badsyntax, {line, filename, error, token}})
    "\n#{String.new filename}:#{line}: #{String.new error} #{token.to_s}"
  end

  def format_catch('error, {'badform, {line, filename, module, desc}})
    formatted = Erlang.elixir_errors.format_error(module, desc)
    "\n#{String.new filename}:#{line}: #{String.new formatted}"
  end

  def format_catch(_, reason)
    self.format_object(reason)
  end
end