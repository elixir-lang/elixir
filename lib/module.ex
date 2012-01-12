# We cannot use to_list because it depends on inspect,
# which depends on protocol, which depends on this module.
require Elixir::Macros, except: [to_list: 1]

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
    { binding, scope } = Erlang.elixir_module.binding_and_scope_for_eval(line, to_list(filename), module, binding)
    Erlang.elixir_def.reset_last(module)
    elem Erlang.elixir.eval_quoted([quoted], binding, scope), 1
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
    table = attribute_table_for(module)
    table == ETS.info(table, :name)
  end

  # Reads the data for the given module. This is used
  # to read data of uncompiled modules. If the module
  # was already compiled, you shoul access the data
  # directly by invoking `__info__(:data)` in that module.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       Module.merge_data __MODULE__, value: 1
  #       Module.read_data __MODULE__ #=> [value: 1]
  #     end
  #
  def read_data(module) do
    assert_already_compiled!(:read_data, module)
    ETS.lookup_element(attribute_table_for(module), :data, 2)
  end

  # Reads the data from `module` at the given key `at`.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       Module.merge_data __MODULE__, value: 1
  #       Module.read_data __MODULE__, :value #=> 1
  #     end
  #
  def read_data(module, at) do
    Orddict.fetch read_data(module), at, nil
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
  #     Foo.__info__(:data) #=> [value: 1]
  #
  def merge_data(module, new) do
    assert_already_compiled!(:merge_data, module)
    table = attribute_table_for(module)
    old   = ETS.lookup_element(table, :data, 2)
    final = Orddict.merge(old, new)
    ETS.insert(table, { :data,  final })
    final
  end

  # Checks if a function was defined, regardless if it is
  # a macro or a private function. Use function_defined?/3
  # to assert for an specific type.
  #
  # ## Examples
  #
  #     defmodule Example do
  #       Module.function_defined? __MODULE__, version: 0 #=> false
  #       def version, do: 1
  #       Module.function_defined? __MODULE__, version: 0 #=> true
  #     end
  #
  def function_defined?(module, tuple) when is_tuple(tuple) do
    assert_already_compiled!(:function_defined?, module)
    table = function_table_for(module)
    ETS.lookup(table, tuple) != []
  end

  # Checks if a function was defined and also for its `kind`.
  # `kind` can be either :def, :defp or :defmacro.
  #
  # ## Examples
  #
  #     defmodule Example do
  #       Module.function_defined? __MODULE__, version: 0, :defp #=> false
  #       def version, do: 1
  #       Module.function_defined? __MODULE__, version: 0, :defp #=> false
  #     end
  #
  def function_defined?(module, tuple, kind) do
    function_defined?(module, tuple) andalso
      (table = function_table_for(module)
       entry = kind_to_entry(kind)
       List.member? ETS.lookup_element(table, entry, 2), tuple)
  end

  # Adds a compilation callback hook that is invoked
  # exactly before the module is compiled.
  #
  # This callback is useful when used with `use` as a mechanism
  # to clean up any internal data in the module before it is compiled.
  #
  # ## Examples
  #
  # Imagine you are creating a module/library that is meant for
  # external usage called `MyLib`. It could be defined as:
  #
  #     defmodule MyLib do
  #       def __using__(target) do
  #         Module.merge_data target, some_data: true
  #         Module.add_compile_callback(target, __MODULE__, :__callback__)
  #       end
  #
  #       defmacro __callback__(target) do
  #         value = Orddict.fetch(Module.read_data(target), :some_data, [])
  #         quote { def my_lib_value, do: unquote(value) }
  #       end
  #     end
  #
  # And a module could use `MyLib` with:
  #
  #     defmodule App do
  #       use ModuleTest::ToBeUsed
  #     end
  #
  # In the example above, `MyLib` defines a data to the target. This data
  # can be updated throughout the module definition and therefore, the final
  # value of the data can only be compiled using a compiation callback,
  # which will read the final value of :some_data and compile to a function.
  def add_compile_callback(module, target, fun // :__compiling__) do
    assert_already_compiled!(:add_compile_callback, module)
    new   = { target, fun }
    table = attribute_table_for(module)
    old   = ETS.lookup_element(table, :callbacks, 2)
    ETS.insert(table, { :callbacks,  [new|old] })
    new
  end

  ## Helpers

  defp kind_to_entry(:def),      do: :public
  defp kind_to_entry(:defp),     do: :private
  defp kind_to_entry(:defmacro), do: :macros

  defp to_list(list) when is_list(list),  do: list
  defp to_list(bin)  when is_binary(bin), do: binary_to_list(bin)

  defp attribute_table_for(module) do
    list_to_atom Erlang.lists.concat([:a, module])
  end

  defp function_table_for(module) do
    list_to_atom Erlang.lists.concat([:f, module])
  end

  defp assert_already_compiled!(fun, module) do
    compiled?(module) orelse
      error { :module_already_compiled,
        "could not call #{fun} on module #{module} because it was already compiled" }
  end
end