module IO
  def puts
    puts("")
  end

  def puts(message)
    Erlang.io.format(<<message.to_s.to_bin|binary, $\n>>)
  end

  def gets
    gets("")
  end

  def gets(prompt)
    String.new Erlang.io.get_line(prompt.to_char_list)
  end
end
