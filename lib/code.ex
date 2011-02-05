module Code
  def require(path)
    Erlang.elixir.require_file(path.to_char_list)
  end
end