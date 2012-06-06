# We cannot use to_char_list because it depends on inspect,
# which depends on protocol, which depends on this module.
import Elixir.Builtin, except: [to_char_list: 1]

defmodule Module do
  require Erlang.ets, as: ETS

  @moduledoc """
  This module provides many functions to deal with modules during
  compilation time. It allows a developer to dynamically attach
  documentation, add, delete and register attributes and so forth.

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

    binding = Erlang.elixir_module.binding_for_eval(module, binding)
    scope   = Erlang.elixir_module.scope_for_eval(module, opts)

    Erlang.elixir_def.reset_last(module)

    line = Keyword.get opts, :line, 1
    { value, binding, _scope } = Erlang.elixir.eval_quoted([quoted], binding, line, scope)
    { value, binding }
  end

  @doc """
  Concatenates the list of aliases and returns a new alias.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.concat [Foo, Bar]    #=> Foo.Bar
      Module.concat [Foo, "Bar"]  #=> Foo.Bar
      Module.concat [Foo, 'Bar']  #=> Foo.Bar

  """
  def concat(list) when is_list(list) do
    Erlang.elixir_aliases.concat(list)
  end

  @doc """
  Concatenates the two given aliases and returns a new alias.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.concat Foo, Bar    #=> Foo.Bar
      Module.concat Foo, "Bar"  #=> Foo.Bar
      Module.concat Foo, 'Bar'  #=> Foo.Bar

  """
  def concat(left, right) do
    Erlang.elixir_aliases.concat([left, right])
  end

  @doc """
  Concatenates the list aliases and returns a new alias only
  if the alias was already referenced. If the alias was not
  referenced yet, fails with ArgumentError.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.safe_concat [Unknown, Module]
      #=> ArgumentError

      Module.safe_concat [List, Chars]
      #=> List.Chars

  """
  def safe_concat(list) when is_list(list) do
    Erlang.elixir_aliases.safe_concat(list)
  end

  @doc """
  Concatenates the two aliases and returns a new alias only
  if the alias was already referenced. If the alias was not
  referenced yet, fails with ArgumentError.
  It handles char lists, binaries and atoms.

  ## Examples

      Module.safe_concat Unknown, Module
      #=> ArgumentError

      Module.safe_concat List, Chars
      #=> List.Chars

  """
  def safe_concat(left, right) do
    Erlang.elixir_aliases.safe_concat([left, right])
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
    lc { tuple, _, _, _, _, _, _, _ } inlist ETS.tab2list(table), do: tuple
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
    lc { tuple, stored_kind, _, _, _, _, _, _ } inlist ETS.tab2list(table), stored_kind == kind, do: tuple
  end

  @doc """
  Makes the given functions in the given module overridable.
  An overridable function is lazily defined, allowing a
  developer to customize it.
  """
  def make_overridable(module, tuples) do
    assert_not_compiled!(:make_overridable, module)
    table = function_table_for(module)
    lc tuple inlist tuples do
      case ETS.lookup(table, tuple) do
        [clause] ->
          ETS.delete(table, tuple)

          old    = Module.read_attribute(module, :__overridable)
          new    = [ { tuple, { 1, [clause] } } ]
          merged = :orddict.merge(fn(_k, { count, v1 }, _v2) -> { count + 1, [clause|v1] } end, old, new)

          Module.add_attribute(module, :__overridable, merged)
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

  This funtion expects the module on compilation followed
  by the module and function name to be invoked as callback.
  The function receives the module being compiled as argument.

  ## Examples

  Imagine you are creating a module/library that is meant for
  external usage called `MyLib`. It could be defined as:

      defmodule MyLib do
        def __using__(args) do
          target = __CALLER__.module
          Module.add_compile_callback(target, __MODULE__, :__callback__)
        end

        defmacro __callback__(target) do
          value = Module.read_attribute(target, :some_data)
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
    old   = ETS.lookup_element(table, :__compile_callbacks, 2)
    ETS.insert(table, { :__compile_callbacks,  [new|old] })
  end

  @doc """
  Adds an Erlang attribute to the given module with the given
  key and value. The semantics of adding the attribute depends
  if the attribute was registered or not via `register_attribute/2`.

  ## Examples

      defmodule MyModule do
        Module.add_attribute __MODULE__, :custom_threshold_for_lib, 10
      end

  """
  def add_attribute(module, key, value) when is_atom(key) do
    assert_not_compiled!(:add_attribute, module)
    table = data_table_for(module)
    value = normalize_attribute(key, value)
    acc   = ETS.lookup_element(table, :__acc_attributes, 2)

    new =
      if List.member?(acc, key) do
        case ETS.lookup(table, key) do
          [{^key,old}] -> [value|old]
          [] -> [value]
        end
      else
        value
      end

    ETS.insert(table, { key, new })
  end

  @doc """
  Reads the given attribute from a module. If the attribute
  was marked as accumulate with `Module.register_attribute`,
  a list is always returned.

  ## Examples

      defmodule Foo do
        Module.add_attribute __MODULE__, :value, 1
        Module.read_attribute __MODULE__, :value #=> 1

        Module.register_attribute __MODULE__, :value, accumulate: true
        Module.add_attribute __MODULE__, :value, 1
        Module.read_attribute __MODULE__, :value #=> [1]
      end

  """
  def read_attribute(module, key) when is_atom(key) do
    assert_not_compiled!(:read_attribute, module)
    table = data_table_for(module)

    case ETS.lookup(table, key) do
      [{^key,old}] -> old
      [] ->
        acc = ETS.lookup_element(table, :__acc_attributes, 2)
        if List.member?(acc, key), do: [], else: nil
    end
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
    ETS.delete(table, key)
  end

  @doc """
  Registers an attribute. By registering an attribute, a developer
  is able to customize how Elixir will store and accumulate the
  attribute values.

  ## Options

  When registering an attribute, two options can be given:

  * `:accumulate` - Several calls to the same attribute will
    accumulate instead of override the previous one;

  * `:persist` - The attribute will be persisted in the Erlang
    Abstract Format. Useful when interfacing with Erlang libraries.

  By default, both options are true. Which means that registering
  an attribute without passing any options will revert the attribute
  behavior to exactly the same expected in Erlang.

  ## Examples

      defmodule MyModule do
        Module.register_attribute __MODULE__,
          :custom_threshold_for_lib,
          accumulate: true, persist: false

        @custom_threshold_for_lib 10
        @custom_threshold_for_lib 20
        @custom_threshold_for_lib #=> [20, 10]
      end

  """
  def register_attribute(module, new, opts // []) do
    assert_not_compiled!(:register_attribute, module)
    table = data_table_for(module)

    if Keyword.get(opts, :persist, true) do
      old = ETS.lookup_element(table, :__persisted_attributes, 2)
      ETS.insert(table, { :__persisted_attributes,  [new|old] })
    end

    if Keyword.get(opts, :accumulate, true) do
      old = ETS.lookup_element(table, :__acc_attributes, 2)
      ETS.insert(table, { :__acc_attributes,  [new|old] })
    end
  end

  @doc false
  # Used internally to compile documentation. This function
  # is private and must be used only internally.
  def compile_doc(module, line, kind, pair) do
    doc = read_attribute(module, :doc)
    result = add_doc(module, line, kind, pair, doc)
    delete_attribute(module, :doc)
    result
  end

  ## Helpers

  defp normalize_attribute(:on_load, atom) when is_atom(atom), do: { atom, 0 }
  defp normalize_attribute(_key, value), do: value

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
