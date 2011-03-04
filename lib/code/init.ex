module Code::Init

  protected

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    process_option(options)
  end

  private

  def process_option([$"-v"|_])
    IO.puts "Elixir #{Code.version}"
    process_option([])
  end

  def process_option([$"-e",h|t])
    Erlang.elixir.eval(h, [])
    process_option(t)
  end

  def process_option([h|t])
    Code.require_file h
    process_option(t)
  end

  def process_option([])
    Erlang.halt()
  end
end