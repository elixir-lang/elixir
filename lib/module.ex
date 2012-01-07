defmodule Module do
  # Evalutes the quotes contents in the given module context.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       contents = quote { def sum(a, b), do: a + b }
  #       Module.eval_quoted __MODULE__, contents, [], __FILE__, __LINE__
  #     end
  #
  #     Foo.sum(1, 2) #=> 3
  #
  def eval_quoted(module, quoted, binding, filename, line) do
    assert_already_compiled!(:eval_quoted, module)
    Erlang.elixir_module.eval_quoted(module, quoted, binding, to_list(filename), line)
  end

  # Checks if the module is compiled or not.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       Module.compiled?(__MODULE__) #=> false
  #     end
  #
  #     Module.compiled?(Foo) #=> true
  #
  def compiled?(module) do
    table = table_for(module)
    table == Erlang.ets.info(table, :name)
  end

  ## Helpers

  defp to_list(list) when is_list(list),  do: list
  defp to_list(bin)  when is_binary(bin), do: binary_to_list(bin)

  defp table_for(module) do
    list_to_atom Erlang.lists.concat([:a, module])
  end

  defp assert_already_compiled!(fun, module) do
    compiled?(module) orelse
      error { :module_already_compiled,
        "could not call #{fun} on module #{module} because it was already compiled" }
  end
end