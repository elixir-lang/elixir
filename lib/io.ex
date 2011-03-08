% elixir: cache

object IO
  module Methods
    def write(message)
      Erlang.io.format @device, message.to_bin, []
    end

    def puts
      puts("")
    end

    def puts(message)
      Erlang.io.format @device, <<message.to_s.to_bin|binary, $\n>>, []
    end

    def gets
      gets("")
    end

    def gets(prompt)
      result = Erlang.io.get_line(@device, prompt.to_char_list)

      if @encoding == 'utf8
        String.new Erlang.unicode.characters_to_binary(result, 'utf8)
      else
        String.new result
      end
    end
  end

  mixin IO::Methods
  proto IO::Methods

  def constructor(device)
    constructor(device, [], {:})
  end

  def constructor(device, mode, options)
    encoding = options['encoding] || 'utf8
    { 'device: device, 'mode: mode, 'encoding: encoding }
  end

  set_ivar 'device, 'standard_io
  set_ivar 'encoding, 'utf8
end
