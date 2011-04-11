% elixir: cache

module Code
  def argv
    server_call 'argv
  end

  def version
    "0.2.0.dev"
  end

  def require(_file)
    IO.puts "[ELIXIR] Code.require is deprecated. Elixir now compile files and " \
      "relies on Erlang's autoload. Check the README for more information."
  end

  def load_file(file)
    Erlang.elixir.file(file.to_char_list)
  end

  private

  def server_call(args)
    GenServer.call('elixir_code_server, args)
  end
end