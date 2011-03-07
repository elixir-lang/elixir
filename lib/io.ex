% elixir: cache

module IO
  def write(message)
    Erlang.io.format message.to_bin
  end

  def puts
    puts("")
  end

  def puts(message)
    Erlang.io.format(<<message.to_s.to_bin|binary, $\n>>)
  end

  def puts(output, message)
    Erlang.io.format(output, <<message.to_s.to_bin|binary, $\n>>, [])
  end

  def gets
    gets("")
  end

  def gets(prompt)
    result = Erlang.io.get_line(prompt.to_char_list)
    String.new Erlang.unicode.characters_to_binary(result, 'utf8)
  end
end
