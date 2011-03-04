module Code::Init

  protected

  % Invoked directly from erlang boot process. It parses all argv
  % options and execute them in the order they are specified.
  def process_argv(options)
    { real, argv } = split_options(options, [])
    GenServer.call('elixir_code_server, { 'argv, argv })
    process_option(real)
  end

  private

  def split_options([$"--"|t], buffer)
    { buffer.reverse, t.map -> (i) String.new i }
  end

  def split_options([h|t], buffer)
    split_options(t, [h|buffer])
  end

  def split_options([], buffer)
    { buffer.reverse, [] }
  end

  def process_option([$"-v"|_])
    IO.puts "Elixir #{Code.version}"
    process_option([])
  end

  def process_option([$"-e",h|t])
    Erlang.elixir.eval(h, [])
    process_option(t)
  end

  def process_option([$"-f",h|t])
    process_option(t + [$"-e", h])
  end

  def process_option([h|t])
    Code.require_file h
    process_option(t)
  end

  def process_option([])
    Erlang.halt()
  end
end