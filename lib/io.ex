module IO
  def puts
    puts("")
  end

  def puts(message)
    Erlang.io.format(<<message.to_bin|binary, $\n>>)
  end
end