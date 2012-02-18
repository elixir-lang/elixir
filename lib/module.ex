# We cannot use to_char_list because it depends on inspect,
# which depends on protocol, which depends on this module.
import Elixir::Builtin, except: [to_char_list: 1]

defmodule Module do
  require Erlang.ets, as: ETS

  # Evalutes the quotes contents in the given module context.
  # Raises an error if the module was already compiled.
  #
  # ## Examples
  #
  #     defmodule Foo do
  #       contents = quote do: (def sum(a, b), do: a + b)
  #       Module.eval_quoted __MODULE__, contents, [], __FILE__, __LINE__
  #     end
  #
  #     Foo.sum(1, 2) #=> 3
  #
  def eval_quoted(module, quoted, binding, filename, line) do
    assert_not_compiled!(:eval_quoted, module)
    { binding, scope } = Erlang.elixir_module.binding_and_scope_for_eval(line, to_char_list(filename), module, binding)
    Erlang.elixir_def.reset_last(module)
    Erlang.elixir.eval_quoted([quoted], binding, line, scope)
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
    table = data_table_for(module)
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
    assert_not_compiled!(:read_data, module)
    ETS.lookup_element(data_table_for(module), :data, 2)
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
    Orddict.get read_data(module), at
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
    assert_not_compiled!(:merge_data, module)
    table = data_table_for(module)
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
  #       Module.function_defined? __MODULE__, { :version, 0 } #=> false
  #       def version, do: 1
  #       Module.function_defined? __MODULE__, { :version, 0 } #=> true
  #     end
  #
  def function_defined?(module, tuple) when is_tuple(tuple) do
    assert_not_compiled!(:function_defined?, module)
    table = function_table_for(module)
    ETS.lookup(table, tuple) != []
  end

  # Checks if a function was defined and also for its `kind`.
  # `kind` can be either :def, :defp or :defmacro.
  #
  # ## Examples
  #
  #     defmodule Example do
  #       Module.function_defined? __MODULE__, { :version, 0 }, :defp #=> false
  #       def version, do: 1
  #       Module.function_defined? __MODULE__, { :version, 0 }, :defp #=> false
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
  #         value = Orddict.get(Module.read_data(target), :some_data, [])
  #         quote do: (def my_lib_value, do: unquote(value))
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
    assert_not_compiled!(:add_compile_callback, module)
    new   = { target, fun }
    table = data_table_for(module)
    old   = ETS.lookup_element(table, :compile_callbacks, 2)
    ETS.insert(table, { :compile_callbacks,  [new|old] })
  end

  # Adds a forwarding to the current module. This is the backend
  # API used by defforward.
  #
  # ## Examples
  #
  #     Module.add_forwarding __MODULE__, [sample: 1], :public, TargetModule
  #
  def add_forwarding(module, pairs, visibility, target) do
    assert_not_compiled!(:add_forwarding, module)
    table = data_table_for(module)
    old   = ETS.lookup_element(table, :forwardings, 2)

    info  = { visibility, target }
    new   = Orddict.from_enum(pairs, fn(x) -> {x, info} end)
    final = Orddict.merge old, new, fn({ name, arity }, { _, old_target }, _current) ->
      raise ArgumentError, message: "forwarding to #{name}/#{arity} already defined by #{inspect(old_target)}"
    end

    ETS.insert(table, { :forwardings,  final })
  end

  # Remove a prevously stablished forwarding.
  #
  # ## Examples
  #
  #     Module.remove_forwarding __MODULE__, [sample: 1]
  #
  def remove_forwarding(module, pair) do
    assert_not_compiled!(:remove_forwarding, module)
    table = data_table_for(module)
    old   = ETS.lookup_element(table, :forwardings, 2)
    final = Enum.reduce pair, old, Orddict.erase(&2, &1)
    ETS.insert(table, { :forwardings,  final })
  end

  # Internal callback that compiles all the forwarding
  # for the given module.
  def compile_forwardings(module, forwardings) do
    contents = Enum.map forwardings, fn({ tuple, other }) ->
      case function_defined?(module, tuple) do
      match: true
        nil
      else:
        contents_for_compile_forwarding(tuple, other)
      end
    end

    eval_quoted module, contents, [], __FILE__, __LINE__
  end

  # Adds an Erlang attribute to the given module with the given
  # key and value. The same attribute can be added more than once.
  #
  # ## Examples
  #
  #     defmodule MyModule do
  #       Module.add_attribute __MODULE__, :custom_threshold_for_lib, 10
  #     end
  #
  def add_attribute(module, key, value) do
    assert_not_compiled!(:add_attribute, module)
    table = attribute_table_for(module)
    ETS.insert(table, { key, value })
  end

  # Deletes all attributes that matches the given key.
  #
  # ## Examples
  #
  #     defmodule MyModule do
  #       Module.add_attribute __MODULE__, :custom_threshold_for_lib, 10
  #       Module.delete_attribute __MODULE__, :custom_threshold_for_lib
  #     end
  #
  def delete_attribute(module, key) do
    assert_not_compiled!(:delete_attribute, module)
    table = attribute_table_for(module)
    ETS.delete(table, key)
  end

  # Registers an attribute. This allows a developer to use the data API
  # but Elixir will register the data as an attribute automatically.
  # By default, `vsn`, `behavior` and other Erlang attributes are
  # automatically registered.
  #
  # ## Examples
  #
  #     defmodule MyModule do
  #       Module.register_attribute __MODULE__, :custom_threshold_for_lib
  #       @custom_threshold_for_lib 10
  #     end
  #
  def register_attribute(module, new) do
    assert_not_compiled!(:register_attribute, module)
    table = data_table_for(module)
    old = ETS.lookup_element(table, :registered_attributes, 2)
    ETS.insert(table, { :registered_attributes,  [new|old] })
  end

  ## Helpers

  defp contents_for_compile_forwarding { name, arity }, { visibility, target } do
    args = lc i in List.seq(1, arity) do
      { binary_to_atom(<<?x, i + 64>>, :utf8), 0, :quoted }
    end

    invoke = quote do
      apply unquote(target), unquote(name), [__MODULE__, unquote_splicing(args)]
    end

    case visibility do
    match: :private
      quote do
        defp unquote(name).(unquote_splicing(args)), do: unquote(invoke)
      end
    match: :public
      quote do
        def unquote(name).(unquote_splicing(args)), do: unquote(invoke)
      end
    end
  end

  defp kind_to_entry(:def),      do: :public
  defp kind_to_entry(:defp),     do: :private
  defp kind_to_entry(:defmacro), do: :macros

  defp to_char_list(list) when is_list(list),  do: list
  defp to_char_list(bin)  when is_binary(bin), do: binary_to_list(bin)

  defp attribute_table_for(module) do
    list_to_atom Erlang.lists.concat([:a, module])
  end

  defp data_table_for(module) do
    list_to_atom Erlang.lists.concat([:d, module])
  end

  defp function_table_for(module) do
    list_to_atom Erlang.lists.concat([:f, module])
  end

  defp assert_not_compiled!(fun, module) do
    compiled?(module) ||
      raise ArgumentError, message:
        "could not call #{fun} on module #{module} because it was already compiled"
  end
end