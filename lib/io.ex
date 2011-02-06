module IO
  def puts(message)
    Erlang.io.format(message.to_bin)
  end
end