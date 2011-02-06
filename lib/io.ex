module IO
  def puts(message, args)
    Erlang.io.format(message.to_bin, args)
  end
end