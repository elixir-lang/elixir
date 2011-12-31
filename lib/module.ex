module Module do
  def eval(module, filename // 'nofile', line // 1, quoted) do
    Erlang.elixir_module.eval(module, quoted, to_list(filename), line)
  end

  defp to_list(list) when is_list(list),  do: list
  defp to_list(bin)  when is_binary(bin), do: binary_to_list(bin)
end