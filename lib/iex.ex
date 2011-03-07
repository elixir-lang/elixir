% elixir: cache

% A simple REPL (Read-Eval-Print Loop) for Elixir
object IEX
  proto Code::Formatter

  def constructor
    {'binding: [], 'codecache: ""}
  end

  def loop
    prompt = case @codecache
    match ""
      "iex> "
    else
      "...> "
    end

    code = @codecache + IO.gets(prompt)
    try
      {result, new_binding} = Erlang.elixir.eval(code.to_list, @binding)
      IO.puts result.inspect
      % I think we need some syntax sugar of set_ivar :)
      self.set_ivar('binding, new_binding).set_ivar('codecache, "").loop
    catch 'error: {'badsyntax, {_, _, _, '"$end"}}
      self.set_ivar('codecache, code).loop
    catch kind: error
      IO.puts 'standard_error, "** #{kind} #{self.format_catch(kind, error)}"
      self.__stacktrace__.each -> (s) IO.puts 'standard_error, "    #{self.format_stacktrace(s)}"
      self.set_ivar('codecache, code).loop
    end
  end
end

