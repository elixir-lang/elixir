module Module do
  def eval(module, string, filename, line) do
    Erlang.elixir_module.eval(module, to_list(string), to_list(filename), line)
  end

  defp to_list(list) when is_list(list),  do: list
  defp to_list(bin)  when is_binary(bin), do: binary_to_list(bin)
end