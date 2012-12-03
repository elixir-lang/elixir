defmodule Module do
  require :ets, as: ETS

  defmacrop is_env(env) do
    quote do
      is_tuple(unquote(env)) and size(unquote(env)) > 1 and elem(unquote(env), 0) == Macro.Env
    end
  end

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

  A list of environment options can also be given as argument.
  Check `Code.eval` for more information.

  Raises an error if the module was already compiled.

  ## Examples

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents
      end

      Foo.sum(1, 2) #=> 3

  For convenience, you can my pass `__ENV__` as argument and
  all options will be automatically extracted from the environment:

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents, [], __ENV__
      end

      Foo.sum(1, 2) #=> 3

  """
  def eval_quoted(module, quoted, binding // [], opts // [])

  def eval_quoted(env, quoted, binding, opts) when is_env(env) do
    eval_quoted(env.module, quoted, binding, Keyword.merge(env.to_keywords, opts))
  end

  def eval_quoted(module, quoted, binding, env) when is_env(env) do
    eval_quoted(module, quoted, binding, env.to_keywords)
  end

  def eval_quoted(module, quoted, binding, opts) do
    assert_not_compiled!(:eval_quoted, module)
    :elixir_module.eval_quoted(module, quoted, binding, opts)
  end

  @doc """
  Creates a module with the given name and given by
  the given quoted expressions. The line where the module
  is defined and its file can be given as options.

  ## Examples

      contents =
        quote do
          def world, do: true
        end

      Module.create(Hello, contents, __ENV__.location)

      Hello.world #=> true

  ## Differences with `defmodule`

  `Module.create` works similarly to `defmodule` and
  return the same results. While one could also use
  `defmodule` to define modules dynamically, this
  function is preferred when the module body is given
  by a quoted expression.

  Another important distinction is that `Module.create`
  allows you to control the environment variables used
  when defining the module, while `defmodule` automatically
  shares the same environment.
  """
  def create(module, quoted, opts // [])

  def create(module, quoted, env) when is_env(env) do
    create(module, quoted, env.to_keywords)
  end

  def create(module, quoted, opts) when is_atom(module) do
    line = Keyword.get(opts, :line, 1)
    :elixir_module.compile(line, module, quoted, [], :elixir.scope_for_eval(opts))
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
    :elixir_aliases.concat(list)
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
    :elixir_aliases.concat([left, right])
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
    :elixir_aliases.safe_concat(list)
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
    :elixir_aliases.safe_concat([left, right])
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
      kind in [:def, :defmacro] and (is_binary(doc) or is_boolean(doc) or doc == nil) do
    assert_not_compiled!(:add_doc, module)
    table = docs_table_for(module)

    { signature, _ } = Enum.map_reduce signature, 1, fn(x, acc) ->
      { simplify_signature(x, line, acc), acc + 1 }
    end

    case ETS.lookup(table, tuple) do
      [] ->
        ETS.insert(table, { tuple, line, kind, signature, doc })
        :ok
      [{ tuple, line, old_kind, old_sign, old_doc }] when old_doc == nil or doc == nil or old_doc == doc ->
        ETS.insert(table, {
          tuple,
          line,
          kind,
          merge_signatures(old_sign, signature, 1),
          doc || old_doc
        })
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

  # Merge

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

          old    = get_attribute(module, :__overridable)
          new    = [ { tuple, { 1, clause, false } } ]
          merged = :orddict.merge(fn(_k, { count, _, _ }, _v2) -> { count + 1, clause, false } end, old, new)

          put_attribute(module, :__overridable, merged)
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
    !! List.keyfind(get_attribute(module, :__overridable), tuple, 0)
  end

  @doc """
  Puts an Erlang attribute to the given module with the given
  key and value. The semantics of putting the attribute depends
  if the attribute was registered or not via `register_attribute/2`.

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
      end

  """
  def put_attribute(module, key, value) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
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
  Gets the given attribute from a module. If the attribute
  was marked as accumulate with `Module.register_attribute`,
  a list is always returned.

  ## Examples

      defmodule Foo do
        Module.put_attribute __MODULE__, :value, 1
        Module.get_attribute __MODULE__, :value #=> 1

        Module.register_attribute __MODULE__, :value, accumulate: true
        Module.put_attribute __MODULE__, :value, 1
        Module.get_attribute __MODULE__, :value #=> [1]
      end

  """
  def get_attribute(module, key) when is_atom(key) do
    assert_not_compiled!(:get_attribute, module)
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
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
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
  behavior to exactly the same expected in :

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
  Split the given module name into binary parts.

  ## Examples

      Module.split Very.Long.Module.Name.And.Even.Longer
      #=> ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]

  """
  def split(module) do
    tl(String.split(Binary.Chars.to_binary(module), "-"))
  end

  @doc """
  Convert a module name to binary without the Elixir prefix.
  """
  def to_binary(module) do
    "Elixir-" <> rest = Binary.Chars.to_binary(module)
    bc <<r>> inbits rest, do: <<to_dot(r)>>
  end

  defp to_dot(?-), do: ?.
  defp to_dot(l),  do: l

  @doc false
  # Used internally to compile documentation. This function
  # is private and must be used only internally.
  def compile_doc(env, kind, name, args, _guards, _body) do
    module = env.module
    line   = env.line
    arity  = length(args)
    pair   = { name, arity }
    doc    = get_attribute(module, :doc)

    case add_doc(module, line, kind, pair, args, doc) do
      :ok ->
        :ok
      { :error, :private_doc } ->
        IO.puts "#{env.file}:#{line} function #{name}/#{arity} is private, @doc's are always discarded for private functions"
      { :error, :existing_doc } ->
        IO.puts "#{env.file}:#{line} @doc's for function #{name}/#{arity} have been given more than once, the first version is being kept"
    end

    delete_attribute(module, :doc)
  end

  @doc false
  # Used internally to compile types. This function
  # is private and must be used only internally.
  def compile_typespec(module, key, value) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
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

  defp normalize_attribute(:file, env) when is_env(env),      do: { binary_to_list(env.file), env.line}
  defp normalize_attribute(:file, { binary, line }) when is_binary(binary), do: { binary_to_list(binary), line }
  defp normalize_attribute(:file, other) when not is_tuple(other),          do: normalize_attribute(:file, { other, 1 })

  defp normalize_attribute(key, atom) when is_atom(atom) and
      key in [:before_compile, :after_compile, :on_definition] do
    { atom, :"__#{key}__" }
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
    list_to_atom :lists.concat([:f, module])
  end

  defp docs_table_for(module) do
    list_to_atom :lists.concat([:o, module])
  end

  defp assert_not_compiled!(fun, module) do
    open?(module) ||
      raise ArgumentError,
        message: "could not call #{fun} on module #{inspect module} because it was already compiled"
  end
end
