defmodule Module do
  def eval_quoted(module, quoted, binding, filename, line) do
    assert_table_exists!(:eval_quoted, module)
    Erlang.elixir_module.eval_quoted(module, quoted, binding, to_list(filename), line)
  end

  defp to_list(list) when is_list(list),  do: list
  defp to_list(bin)  when is_binary(bin), do: binary_to_list(bin)

  defp table_for(module) do
    list_to_atom Erlang.lists.concat([:a, module])
  end

  defp assert_table_exists!(fun, module) do
    table = table_for(module)
    table == Erlang.ets.info(table, :name) orelse
      error { :module_already_compiled,
        "could not call #{fun} on module #{module} because it was already compiled" }
  end
end