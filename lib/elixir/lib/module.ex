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
  Check if a module is open, i.e. it is currently being defined
  and its attributes and functions can be modified.
  """
  def open?(module) do
    table = data_table_for(module)
    table == ETS.info(table, :name)
  end

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
    Erlang.elixir_module.eval_quoted(module, quoted, binding, opts)
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
  Attaches documentation to a given function. It expects
  the module the function belongs to, the line (a non negative
  integer), the kind (def or defmacro), a tuple representing
  the function and its arity and the documentation, which should
  be either a binary or a boolean.

  ## Examples

      defmodule MyModule do
        Module.add_doc(__MODULE__, __ENV__.line + 1, :def, { :version, 0 }, [], "Manually added docs")
        def version, do: 1
      end

  """
  def add_doc(_module, _line, kind, _tuple, _signature, doc) when kind in [:defp, :defmacrop] do
    if doc, do: { :error, :private_doc }, else: :ok
  end

  def add_doc(module, line, kind, tuple, signature, doc) when
      is_binary(doc) or is_boolean(doc) or doc == nil do
    assert_not_compiled!(:add_doc, module)
    table = docs_table_for(module)

    { signature, _ } = Enum.map_reduce signature, 1, fn(x, acc) ->
      { simplify_signature(x, line, acc), acc + 1 }
    end

    case ETS.lookup(table, tuple) do
      [] ->
        ETS.insert(table, { tuple, line, kind, signature, doc })
        :ok
      [{ tuple, line, kind, old, old_doc }] when old_doc == nil or doc == nil or old_doc == doc ->
        ETS.insert(table, { tuple, line, kind, merge_signatures(old, signature, 1), old_doc || doc })
        :ok
      _ ->
        { :error, :existing_doc }
    end
  end

  # Simplify signatures to be stored in docs

  defp simplify_signature({ ://, defline, [left, right ] }, line, i) do
    { ://, defline, [simplify_signature(left, line, i), right] }
  end

  defp simplify_signature({ var, line, atom }, _, _i) when is_atom(atom) do
    case atom_to_list(var) do
      [?_|_]    -> { var, line, :guess }
      _         -> { var, line, nil }
    end
  end

  defp simplify_signature({ :=, _, [_, right] }, line, i) do
    simplify_signature(right, line, i)
  end

  defp simplify_signature(other, line, i) when is_integer(other), do: { :"int#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_boolean(other), do: { :"bool#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_atom(other),    do: { :"atom#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_list(other),    do: { :"list#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_float(other),   do: { :"float#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_binary(other),  do: { :"binary#{i}", line, :guess }
  defp simplify_signature(_, line, i), do: { :"arg#{i}", line, :guess }

  # Merge signatures

  defp merge_signatures([h1|t1], [h2|t2], i) do
    [merge_signature(h1, h2, i)|merge_signatures(t1, t2, i + 1)]
  end

  defp merge_signatures([], [], _) do
    []
  end

  defp merge_signature({ ://, line, [left, right] }, newer, i) do
    { ://, line, [merge_signature(left, newer, i), right] }
  end

  defp merge_signature(older, { ://, _, [left, _] }, i) do
    merge_signature(older, left, i)
  end

  # The older signature, when given, always have higher precedence
  defp merge_signature({ _, _, nil } = older, _newer, _),        do: older
  defp merge_signature(_older, { _, _, nil } = newer, _),        do: newer

  # Both are a guess, so check if they are the same guess
  defp merge_signature({ var, _, _ } = older, { var, _, _ }, _), do: older

  # Otherwise, returns a generic guess
  defp merge_signature({ _, line, _ }, _newer, i), do: { :"arg#{i}", line, :guess }

  @doc """
  Checks if the module defines the given function or macro.
  Use `defines?/3` to assert for an specific type.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, { :version, 0 } #=> false
        def version, do: 1
        Module.defines? __MODULE__, { :version, 0 } #=> true
      end

  """
  def defines?(module, tuple) when is_tuple(tuple) do
    assert_not_compiled!(:defines?, module)
    table = function_table_for(module)
    ETS.lookup(table, tuple) != []
  end

  @doc """
  Checks if the module defines a function or macro with the
  given `kind`. `kind` can be either `:def`, `:defp`,
  `:defmacro` or `:defmacrop`.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, { :version, 0 }, :defp #=> false
        def version, do: 1
        Module.defines? __MODULE__, { :version, 0 }, :defp #=> false
      end

  """
  def defines?(module, tuple, kind) do
    assert_not_compiled!(:defines?, module)
    table = function_table_for(module)
    case ETS.lookup(table, tuple) do
      [{ _, ^kind, _, _, _, _, _, _ }] -> true
      _ -> false
    end
  end

  @doc """
  Return all functions defined in the given module.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in __MODULE__ #=> [{:version,1}]
      end

  """
  def definitions_in(module) do
    assert_not_compiled!(:definitions_in, module)
    table = function_table_for(module)
    lc { tuple, _, _, _, _, _, _, _ } inlist ETS.tab2list(table), do: tuple
  end

  @doc """
  Returns all functions defined in te given module according
  to its kind.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in __MODULE__, :def  #=> [{:version,1}]
        Module.definitions_in __MODULE__, :defp #=> []
      end

  """
  def definitions_in(module, kind) do
    assert_not_compiled!(:definitions_in, module)
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
  Returns true if the given tuple in module is marked as overridable.
  """
  def overridable?(module, tuple) do
    key = List.keyfind(Module.read_attribute(module, :__overridable), tuple, 1)
    match? { _, { _, [_|_] } }, key
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

  @doc """
  Split the given module name into components.

  ## Examples

      Module.split_name Very.Long.Module.Name.And.Even.Longer
      #=> [Very, Long, Module, Name, And, Even, Longer]

      Module.split_name Very.Long.Module.Name.And.Even.Longer, 1
      #=> {Very, Long.Module.Name.And.Even.Longer}

      Module.split_name Very.Long.Module.Name.And.Even.Longer, 4
      #=> {Very.Long.Module.Name, And.Even.Longer}

      Module.split_name Very.Long.Module.Name.And.Even.Longer, 8
      #=> {Very.Long.Module.Name.And.Even.Longer, :""}

      Module.split_name Very.Long.Module.Name.And.Even.Longer, -1
      #=> {Very.Long.Module.Name.And.Even, Longer}

      Module.split_name Very.Long.Module.Name.And.Even.Longer, -4
      #=> {Very.Long.Module, Name.And.Even.Longer}

      Module.split_name Very.Long.Module.Name.And.Even.Longer, -8
      #=> {:"", Very.Long.Module.Name.And.Even.Longer}
  """
  def split_name(module), do: tl(:string.tokens(atom_to_list(module),'-'))
  def split_name(module, pos) when pos > 0 do
    tokens = split_name(module)
    {split, rest} = Enum.split tokens, pos
    {concat(split), concat(rest)}
    join(split, rest)
  end
  def split_name(module, pos) when pos < 0 do
    tokens = Enum.reverse(split_name(module))
    {rest, split} = Enum.split tokens, -pos
    rest = Enum.reverse(rest)
    split = Enum.reverse(split)
    join(split, rest)
  end
  defp join([], []), do: {:"", :""}
  defp join(split, []), do: {concat(split), :""}
  defp join([], rest), do: {:"", concat(rest)}
  defp join(split, rest), do: {concat(split), concat(rest)}


  @doc false
  # Used internally to compile documentation. This function
  # is private and must be used only internally.
  def compile_doc(module, line, kind, pair, signature) do
    doc = read_attribute(module, :doc)
    result = add_doc(module, line, kind, pair, signature, doc)
    delete_attribute(module, :doc)
    result
  end

  @doc false
  # Used internally to compile types. This function
  # is private and must be used only internally.
  def compile_type(module, key, value) when is_atom(key) do
    assert_not_compiled!(:add_attribute, module)
    table = data_table_for(module)

    new =
      case ETS.lookup(table, key) do
        [{^key,old}] -> [value|old]
        [] -> [value]
      end

    ETS.insert(table, { key, new })
  end

  ## Helpers

  defp normalize_attribute(:on_load, atom) when is_atom(atom) do
    { atom, 0 }
  end

  defp normalize_attribute(kind, atom) when kind in [:behavior, :behaviour] and is_atom(atom) do
    Code.ensure_compiled(atom)
    atom
  end

  defp normalize_attribute(:file, Macro.Env[file: file, line: line]),       do: { binary_to_list(file), line}
  defp normalize_attribute(:file, { binary, line }) when is_binary(binary), do: { binary_to_list(binary), line }
  defp normalize_attribute(:file, other) when not is_tuple(other),          do: normalize_attribute(:file, { other, 1 })

  defp normalize_attribute(key, atom) when key in [:before_compile, :after_compile] and is_atom(atom) do
    { atom, key }
  end

  defp normalize_attribute(key, _value) when key in [:type, :typep, :export_type, :opaque, :callback] do
    raise ArgumentError, message: "Attributes type, typep, export_type, opaque and callback " <>
      "must be set via Kernel.Typespec"
  end

  defp normalize_attribute(_key, value) do
    value
  end

  defp data_table_for(module) do
    module
  end

  defp function_table_for(module) do
    list_to_atom Erlang.lists.concat([:f, module])
  end

  defp docs_table_for(module) do
    list_to_atom Erlang.lists.concat([:o, module])
  end

  defp assert_not_compiled!(fun, module) do
    open?(module) ||
      raise ArgumentError,
        message: "could not call #{fun} on module #{inspect module} because it was already compiled"
  end
end
