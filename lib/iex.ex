% elixir: cache

% A simple REPL (Read-Eval-Print Loop) for Elixir
object IEX
  def constructor
    {'binding: [], 'codecache: ""}
  end

  def loop
    prompt = case @codecache
    match ""
      "IEX> "
    else
      "...> "
    end

    code = @codecache + IO.gets(prompt)
    try
      {result, new_binding} = Erlang.elixir.eval(code.to_list, @binding)
      IO.puts result.inspect
      % I think we need some syntax sugar of set_ivar :)
      self.set_ivar('binding, new_binding).set_ivar('codecache, "").loop
    catch 'error: {'badsyntax, _, _, _, '"$end"}
      self.set_ivar('codecache, code).loop
    catch 'error: {'badsyntax, _line, _filename, error, token}
      IO.puts("Syntax Error: #{String.new error} #{token.inspect}")
      self.set_ivar('codecache, "").loop
    catch 'error: e
      IO.puts "Error: #{e.inspect}"
      self.set_ivar('codecache, "").loop
    end
  end
end

