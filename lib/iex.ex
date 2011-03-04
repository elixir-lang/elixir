% A simple REPL (Read-Eval-Print Loop) for Elixir
object IEX
  def constructor
    {'prompt: "IEX> ", 'binding: []}
  end

  def loop
    code = IO.gets @prompt
    try
      {result, new_binding} = Erlang.elixir.eval(code.to_list, @binding)
      IO.puts result.inspect
      self.set_ivar('binding, new_binding).loop
    catch 'error: {'badsyntax, msg}
      IO.puts(String.new msg)
      loop
    catch 'error: e
      IO.puts "Error: #{e.inspect}"
      loop
    end
  end
end

