defmodule Module do
  refer Erlang.ets, as: ETS

  # Evalutes the quotes contents in the given module context.
  # Raises an error if the module was already compiled.
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
    table == ETS.info(table, :name)
  end

  # Merge the given `new` data to the module, overriding
  # any previous one.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       Module.merge_data __MODULE__, value: 1
  #     end
  #
  #     Foo.__data__ #=> [value: 1]
  #
  def merge_data(module, new) do
    assert_already_compiled!(:merge_data, module)
    table = table_for(module)
    old   = ETS.lookup_element(table, :data, 2)
    final = Orddict.merge(old, new)
    ETS.insert(table, { :data,  final })
    final
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