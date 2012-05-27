# We cannot use to_char_list because it depends on inspect,
# which depends on protocol, which depends on this module.
import Elixir.Builtin, except: [to_char_list: 1]

defmodule Module do
  require Erlang.ets, as: ETS

  @moduledoc """
  This module provides many functions to deal with modules during
  compilation time. It allows a developer to dynamically attach
  documentation, merge data, register attributes and so forth.

  After the module is compiled, using many of the functions in
  this module will raise errors, since it is out of their purpose
  to inspect runtime data. Most of the runtime data can be inspected
  via the `__info__(attr)` function attached to each compiled module.
  """

  @doc """
  Evalutes the quotes contents in the given module context.
  Raises an error if the module was already compiled.

  ## Options

  This function accepts a list of options. The supported
  options are:

  * `:file` - The filename to be used in stacktraces
    and the file reported in the __ENV__ variable.

  * `:line` - The line reported in the __ENV__ variable.

  ## Examples

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents, []
      end

      Foo.sum(1, 2) #=> 3

  This function also accepts a `Macro.Env` as first argument. This
  is useful to evalute the quoted contents inside an existing environment:

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __ENV__, contents, []
      end

      Foo.sum(1, 2) #=> 3

  """
  def eval_quoted(env, quoted, binding // [], opts // [])

  def eval_quoted(Macro.Env[module: module] = env, quoted, binding, opts) do
    eval_quoted(module, quoted, binding, Keyword.merge(env.location, opts))
  end

  def eval_quoted(module, quoted, binding, opts) do
    assert_not_compiled!(:eval_quoted, module)

    { binding, scope } = Erlang.elixir_module.binding_and_scope_for_eval(module, binding, opts)
    Erlang.elixir_def.reset_last(module)

    line = Keyword.get opts, :line, 1
    { value, binding, _scope } = Erlang.elixir.eval_quoted([quoted], binding, line, scope)
    { value, binding }
  end

  @doc """
  Concatenates the list of arguments and returns the module name.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.concat [Foo, Bar]    #=> Foo.Bar
      Module.concat [Foo, "Bar"]  #=> Foo.Bar
      Module.concat [Foo, 'Bar']  #=> Foo.Bar

  """
  def concat(list) when is_list(list) do
    Erlang.elixir_ref.concat(list)
  end

  @doc """
  Concatenates two arguments and returns the module name.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.concat Foo, Bar    #=> Foo.Bar
      Module.concat Foo, "Bar"  #=> Foo.Bar
      Module.concat Foo, 'Bar'  #=> Foo.Bar

  """
  def concat(left, right) do
    Erlang.elixir_ref.concat([left, right])
  end

  @doc """
  Concatenates the list arguments and returns the module
  name only if the module was already referenced.
  If the module was not referenced yet, fails with ArgumentError.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.safe_concat [Unknown, Module]
      #=> ArgumentError

  """
  def safe_concat(list) when is_list(list) do
    Erlang.elixir_ref.safe_concat(list)
  end

  @doc """
  Concatenates two arguments and returns the module
  name only if the module was already referenced.
  If the module was not referenced yet, fails with ArgumentError.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.safe_concat Unknown, Module
      #=> ArgumentError

  """
  def safe_concat(left, right) do
    Erlang.elixir_ref.safe_concat([left, right])
  end

  @doc """
  Checks if the module is compiled or not.

  ## Examples

      defmodule Foo do
        Module.compiled?(__MODULE__) #=> false
      end

      Module.compiled?(Foo) #=> true

  """
  def compiled?(module) do
    table = data_table_for(module)
    table == ETS.info(table, :name)
  end

  @doc """
  Reads the data for the given module. This is used
  to read data of uncompiled modules. If the module
  was already compiled, you shoul access the data
  directly by invoking `__info__(:data)` in that module.

  ## Examples

      defmodule Foo do
        Module.merge_data __MODULE__, value: 1
        Module.read_data __MODULE__ #=> [value: 1]
      end

  """
  def read_data(module) do
    assert_not_compiled!(:read_data, module)
    ETS.lookup_element(data_table_for(module), :data, 2)
  end

  @doc """
  Reads the data from `module` at the given key `at`.

  ## Examples

      defmodule Foo do
        Module.merge_data __MODULE__, value: 1
        Module.read_data __MODULE__, :value #=> 1
      end

  """
  def read_data(module, at) do
    Keyword.get read_data(module), at
  end

  @doc """
  Merge the given data into the module, overriding any
  previous one.

  If any of the given data is a registered attribute, it is
  automatically added to the attribute set, instead of marking
  it as data. See register_attribute/2 and add_attribute/3 for
  more info.

  ## Examples

      defmodule Foo do
        Module.merge_data __MODULE__, value: 1
      end

      Foo.__info__(:data) #=> [value: 1]

  """
  def merge_data(module, data) do
    assert_not_compiled!(:merge_data, module)

    table      = data_table_for(module)
    old        = ETS.lookup_element(table, :data, 2)
    registered = ETS.lookup_element(table, :registered_attributes, 2)
    data       = lc kv in data, do: normalize_data(kv)

    { attrs, new } = :lists.partition fn {k,_} -> List.member?(registered, k) end, data
    lc {k,v} in attrs, do: add_attribute(module, k, v)
    ETS.insert(table, { :data,  Keyword.merge(old, new) })
  end

  @doc """
  Attaches documentation to a given function. It expects
  the module the function belongs to, the line (a non negative
  integer), the kind (def or defmacro), a tuple representing
  the function and its arity and the documentation, which should
  be either a binary or a boolean.

  ## Examples

      defmodule MyModule do
        Module.add_doc(__MODULE__, __ENV__.line + 1, :def, { :version, 0 }, "Manually added docs")
        def version, do: 1
      end

  """
  def add_doc(_module, _line, kind, _tuple, nil) when kind in [:defp, :defmacrop] do
    :ok
  end

  def add_doc(_module, _line, kind, _tuple, _doc) when kind in [:defp, :defmacrop] do
    { :error, :private_doc }
  end

  def add_doc(module, line, kind, tuple, doc) when
      is_binary(doc) or is_boolean(doc) or doc == nil do
    assert_not_compiled!(:add_doc, module)
    table = docs_table_for(module)

    case { ETS.lookup(table, tuple), doc } do
      { [], _ } ->
        ETS.insert(table, { tuple, line, kind, doc })
        :ok
      { _, nil } ->
        :ok
      _ ->
        { :error, :existing_doc }
    end
  end

  @doc """
  Checks if a function was defined, regardless if it is
  a macro or a private function. Use function_defined?/3
  to assert for an specific type.

  ## Examples

      defmodule Example do
        Module.function_defined? __MODULE__, { :version, 0 } #=> false
        def version, do: 1
        Module.function_defined? __MODULE__, { :version, 0 } #=> true
      end

  """
  def function_defined?(module, tuple) when is_tuple(tuple) do
    assert_not_compiled!(:function_defined?, module)
    table = function_table_for(module)
    ETS.lookup(table, tuple) != []
  end

  @doc """
  Checks if a function was defined and also for its `kind`.
  `kind` can be either :def, :defp or :defmacro.

  ## Examples

      defmodule Example do
        Module.function_defined? __MODULE__, { :version, 0 }, :defp #=> false
        def version, do: 1
        Module.function_defined? __MODULE__, { :version, 0 }, :defp #=> false
      end

  """
  def function_defined?(module, tuple, kind) do
    List.member? defined_functions(module, kind), tuple
  end

  @doc """
  Return all functions defined in the given module.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.defined_functions __MODULE__ #=> [{:version,1}]
      end

  """
  def defined_functions(module) do
    assert_not_compiled!(:defined_functions, module)
    table = function_table_for(module)
    lc { tuple, _, _, _, _, _, _, _ } in ETS.tab2list(table), do: tuple
  end

  @doc """
  Returns all functions defined in te given module according
  to its kind.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.defined_functions __MODULE__, :def  #=> [{:version,1}]
        Module.defined_functions __MODULE__, :defp #=> []
      end

  """
  def defined_functions(module, kind) do
    assert_not_compiled!(:defined_functions, module)
    table = function_table_for(module)
    lc { tuple, stored_kind, _, _, _, _, _, _ } in ETS.tab2list(table) when stored_kind == kind, do: tuple
  end

  @doc """
  Makes the given functions in the given module overridable.
  An overridable function is lazily defined, allowing a
  developer to customize it.
  """
  def make_overridable(module, tuples) do
    assert_not_compiled!(:make_overridable, module)
    table = function_table_for(module)
    lc tuple in tuples do
      case ETS.lookup(table, tuple) do
        [clause] ->
          ETS.delete(table, tuple)
          Erlang.elixir_def_overridable.define(module, tuple, clause)
        _ ->
          { name, arity } = tuple
          raise "Cannot make function #{name}/#{arity} overridable because it was not defined"
      end
    end
  end

  @doc """
  Adds a compilation callback hook that is invoked
  exactly before the module is compiled.

  This callback is useful, for example, when used with `use`
  as a mechanism to clean up any internal data in the module
  before it is compiled.

  ## Examples

  Imagine you are creating a module/library that is meant for
  external usage called `MyLib`. It could be defined as:

      defmodule MyLib do
        def __using__(target) do
          Module.merge_data target, some_data: nil
          Module.add_compile_callback(target, __MODULE__, :__callback__)
        end

        defmacro __callback__(target) do
          value = Module.read_data(target, :some_data)
          quote do: (def my_lib_value, do: unquote(value))
        end
      end

  And a module could use `MyLib` with:

      defmodule App do
        use ModuleTest.ToBeUsed
        @some_data :new_value
      end

  In the example above, `MyLib` defines a data on the target.
  This data can be updated throughout the module definition
  and therefore, the final value of the data can only be retrieved
  via the compilation callback.

  In this example, the compilation callback reads the value and
  compile it to a function.
  """
  def add_compile_callback(module, target, fun // :__compiling__) do
    assert_not_compiled!(:add_compile_callback, module)
    new   = { target, fun }
    table = data_table_for(module)
    old   = ETS.lookup_element(table, :compile_callbacks, 2)
    ETS.insert(table, { :compile_callbacks,  [new|old] })
  end

  @doc """
  Adds an Erlang attribute to the given module with the given
  key and value. The same attribute can be added more than once.

  ## Examples

      defmodule MyModule do
        Module.add_attribute __MODULE__, :custom_threshold_for_lib, 10
      end

  """
  def add_attribute(module, key, value) when is_atom(key) do
    assert_not_compiled!(:add_attribute, module)
    table = data_table_for(module)
    attrs = ETS.lookup_element(table, :attributes, 2)
    ETS.insert(table, { :attributes, [{key, value}|attrs] })
  end

  @doc """
  Deletes all attributes that matches the given key.

  ## Examples

      defmodule MyModule do
        Module.add_attribute __MODULE__, :custom_threshold_for_lib, 10
        Module.delete_attribute __MODULE__, :custom_threshold_for_lib
      end

  """
  def delete_attribute(module, key) when is_atom(key) do
    assert_not_compiled!(:delete_attribute, module)
    table = data_table_for(module)
    attrs = ETS.lookup_element(table, :attributes, 2)
    final = lc {k,v} in attrs when k != key, do: {k,v}
    ETS.insert(table, { :attributes, final })
  end

  @doc """
  Registers an attribute. This allows a developer to use the data API
  but Elixir will register the data as an attribute automatically.
  By default, `vsn`, `behavior` and other Erlang attributes are
  automatically registered.

  ## Examples

      defmodule MyModule do
        Module.register_attribute __MODULE__, :custom_threshold_for_lib
        @custom_threshold_for_lib 10
      end

  """
  def register_attribute(module, new) do
    assert_not_compiled!(:register_attribute, module)
    table = data_table_for(module)
    old = ETS.lookup_element(table, :registered_attributes, 2)
    ETS.insert(table, { :registered_attributes,  [new|old] })
  end

  @doc false
  # Used internally to compile documentation. This function
  # is private and must be used only internally.
  def compile_doc(module, line, kind, pair) do
    doc = read_data(module, :doc)
    result = add_doc(module, line, kind, pair, doc)
    merge_data(module, doc: nil)
    result
  end

  ## Helpers

  defp normalize_data({ :on_load, atom }) when is_atom(atom), do: { :on_load, { atom, 0 } }
  defp normalize_data(other), do: other

  defp data_table_for(module) do
    list_to_atom Erlang.lists.concat([:d, module])
  end

  defp function_table_for(module) do
    list_to_atom Erlang.lists.concat([:f, module])
  end

  defp docs_table_for(module) do
    list_to_atom Erlang.lists.concat([:o, module])
  end

  defp assert_not_compiled!(fun, module) do
    compiled?(module) ||
      raise ArgumentError,
        message: "could not call #{fun} on module #{inspect module} because it was already compiled"
  end
end
