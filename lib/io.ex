module IO
  def puts(message)
    Erlang.io.format(<<message.to_bin|binary, $\n>>)
  end
end