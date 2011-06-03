% TODO: Decouple all this.
module IO
  def new([device])
    #IO(device, [], {})
  end

  def new([device, mode, options])
    #IO(device, mode, options)
  end

  def __bound__(device, mode, options)
    encoding = options['encoding] || 'utf8
    @('device: device, 'mode: mode, 'encoding: encoding)
  end

  @('device: 'standard_io, 'encoding: 'utf8)

  def write(message)
    Erlang.io.format @device, message, []
  end

  def puts
    puts("")
  end

  def puts(message)
    Erlang.io.format @device, <<message.to_s|binary, $\n>>, []
  end

  def gets
    gets("")
  end

  def gets(prompt)
    result = Erlang.io.get_line(@device, prompt.to_char_list)

    if @encoding != 'binary
      Erlang.unicode.characters_to_list(result, @encoding).to_bin
    else
      result
    end
  end
end