module Code::Compiler
  def argv
    [file(file) for file in Code.argv]
  end

  def file(wildcard)
    list = Erlang.filelib.wildcard(wildcard)
    [Erlang.elixir.require(file) for file in list]
  end
end