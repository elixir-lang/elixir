defmodule Code do
  def version, do: "0.4.0.dev"

  def argv do
    Erlang.gen_server.call(:elixir_code_server, :argv)
  end

  def loaded_files do
    Erlang.gen_server.call(:elixir_code_server, :loaded)
  end

  def require_file(file) do
    Erlang.elixir.file file
  end

  def stacktrace do
    Erlang.erlang.get_stacktrace
  end
end