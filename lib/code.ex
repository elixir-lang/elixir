% elixir: cache

module Code
  def argv
    server_call 'argv
  end

  def version
    "0.2.0.dev"
  end

  private

  def server_call(args)
    GenServer.call('elixir_code_server, args)
  end
end