% elixir: cache

% A simple REPL (Read-Eval-Print Loop) for Elixir
object IEX
  proto Code::Formatter

  module Mixin
    def start
      IO.write "Interactive Elixir (#{Code.version})\nRunning on "
      Erlang.user_drv.start(['"tty_sl -c -e", {'"IEX::Mixin",'spawn,[IEX]}])
    end

    def spawn
      Process.spawn -> self.new.loop
    end
  end

  def initialize
    @('binding: [], 'codecache: "")
  end

  def loop
    prompt = case @codecache
    match ""
      "iex> "
    else
      "...> "
    end

    code = @codecache + IO.gets(prompt)

    { b, c } = try
      {result, new_binding} = Erlang.elixir.eval(code.to_list, @binding)
      IO.puts result.inspect
      { new_binding, "" }
    catch 'error: {'badsyntax, {_, _, _, []}}
      { @binding, code }
    catch kind: error
      io = IO.new 'standard_error
      io.puts "** #{kind} #{self.format_catch(kind, error)}"
      self.__stacktrace__.each -> (s) io.puts "    #{self.format_stacktrace(s)}"
      { @binding, "" }
    end

    @('binding: b, 'codecache: c).loop
  end
end