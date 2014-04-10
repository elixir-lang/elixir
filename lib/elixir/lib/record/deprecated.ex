defmodule Record.Deprecated do
  @moduledoc false

  def defrecord(name, fields, opts) do
    block = Keyword.get(opts, :do, nil)
    record_check!(fields)

    quote do
      unquoted_fields = unquote(fields)

      defmodule unquote(name) do
        import Record.DSL

        @record_fields []
        @record_types  []

        Record.Deprecated.deffunctions(unquoted_fields, __ENV__)
        value = unquote(block)
        Record.Deprecated.deftypes(@record_fields, @record_types, __ENV__)
        value
      end
    end
  end

  defp record_check!([{ field, { :::, _, [_, _] }}|_]) when is_atom(field) do
    raise ArgumentError, message: "typespecs are not supported inlined with defrecord, " <>
                                  "please use record_type instead"
  end

  defp record_check!([_|t]), do: record_check!(t)
  defp record_check!(_), do: :ok

  def defrecordp(name, tag, fields) do
    case recordp_split(fields, [], [], false) do
      { :ok, fields, types, def_type } ->
        types = Macro.escape(types)

        # bind_quoted isn't available when bootstrapping record
        quoted = quote [unquote: false] do
          Record.Deprecated.defmacros(name, fields, __ENV__, tag)

          if def_type do
            type = binary_to_atom(atom_to_binary(name) <> "_t")
            @typep unquote(type)() :: { unquote(tag || name), unquote_splicing(types) }
          end
        end

        quote do
          def_type = unquote(def_type)
          fields = unquote(fields)
          types = unquote(types)
          tag = unquote(tag)
          name = unquote(name)
          unquote(quoted)
        end

      :error ->
        quote do
          name = unquote(name)
          Record.Deprecated.defmacros(name, unquote(fields), __ENV__, unquote(tag))
        end
    end
  end

  defp recordp_split([{ field, { :::, _, [default, type] }}|t], defaults, types, _) do
    recordp_split(t, [{ field, default }|defaults], [type|types], true)
  end

  defp recordp_split([other|t], defaults, types, def_type) do
    recordp_split(t, [other|defaults], [quote(do: term)|types], def_type)
  end

  defp recordp_split([], defaults, types, def_type) do
    { :ok, :lists.reverse(defaults), :lists.reverse(types), def_type }
  end

  defp recordp_split(_, _, _, _) do
    :error
  end

  def deffunctions(values, env) do
    values  = for value <- values, do: convert_value(value)
    escaped = Macro.escape(values)

    contents = [
      reflection(escaped),
      initializer(escaped),
      conversions(values),
      record_optimizable(),
      updater(values),
      accessors(values, 1),
      switch_recorder()
    ]

    contents = [quote(do: @record_fields unquote(escaped))|contents]

    # Special case for bootstrapping purposes
    if env == Macro.Env do
      Module.eval_quoted(env, contents, [], [])
    else
      Module.eval_quoted(env.module, contents, [], env.location)
    end
  end

  def deftypes(values, types, env) do
    types  = types || []
    values = for value <- values do
      { name, default } = convert_value(value)
      { name, default, find_spec(types, name) }
    end

    contents = [
      core_specs(values),
      accessor_specs(values, 1, [])
    ]

    # We need to handle bootstraping
    cond do
      :code.ensure_loaded(Kernel.Typespec) != { :module, Kernel.Typespec } ->
        nil
      env == Macro.Env ->
        Module.eval_quoted(env, contents, [], [])
      true ->
        Module.eval_quoted(env.module, contents, [], env.location)
    end
  end

  def defmacros(name, values, env, tag \\ nil)
      when is_atom(name) and is_list(values) and is_atom(tag) do

    escaped = for value <- values do
      { key, value } = convert_value(value)
      { key, Macro.escape(value) }
    end

    tag = tag || name

    contents = quote do
      defmacrop unquote(name)() do
        Record.Deprecated.access(unquote(tag), unquote(escaped), [], __CALLER__)
      end

      defmacrop unquote(name)(args) do
        Record.Deprecated.access(unquote(tag), unquote(escaped), args, __CALLER__)
      end

      defmacrop unquote(name)(record, args) do
        Record.Deprecated.dispatch(unquote(tag), unquote(escaped), record, args, __CALLER__)
      end
    end

    Module.eval_quoted(env.module, contents, [], env.location)
  end

  ## Callbacks

  defmacro __before_compile__(_) do
    quote do
      def __record__(:optimizable), do: @record_optimizable
    end
  end

  def __on_definition__(env, kind, name, args, _guards, _body) do
    tuple     = { name, length(args) }
    module    = env.module
    functions = Module.get_attribute(module, :record_optimizable)

    functions =
      if kind in [:def] and Module.get_attribute(module, :record_optimized) do
        [tuple|functions]
      else
        List.delete(functions, tuple)
      end

    Module.put_attribute(module, :record_optimizable, functions)
  end

  def access(atom, fields, arg, _caller) when is_atom(arg) do
    if index = find_index(fields, arg, 0) do
      index + 1
    else
      raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect arg}"
    end
  end

  def access(atom, fields, keyword, caller) do
    unless is_keyword(keyword) do
      raise ArgumentError, message: "expected contents inside brackets to be a keyword list or an atom, got: #{inspect keyword}"
    end

    in_match = caller.in_match?

    has_underscore_value = Keyword.has_key?(keyword, :_)
    underscore_value     = Keyword.get(keyword, :_, { :_, [], nil })
    keyword              = Keyword.delete keyword, :_

    iterator = fn({field, default}, each_keyword) ->
      new_fields =
        case Keyword.has_key?(each_keyword, field) do
          true  -> Keyword.get(each_keyword, field)
          false ->
            case in_match or has_underscore_value do
              true  -> underscore_value
              false -> Macro.escape(default)
            end
        end

      { new_fields, Keyword.delete(each_keyword, field) }
    end

    { match, remaining } = :lists.mapfoldl(iterator, keyword, fields)

    case remaining do
      [] ->
        { :{}, [], [atom|match] }
      _  ->
        keys = for { key, _ } <- remaining, do: key
        raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect hd(keys)}"
    end
  end

  def dispatch(atom, fields, record, args, caller) do
    cond do
      is_atom(args) ->
        get(atom, fields, record, args)
      is_keyword(args) ->
        update(atom, fields, record, args, caller)
      true ->
        raise ArgumentError, message: "expected arguments to be a compile time atom or keywords"
    end
  end

  defp update(atom, fields, var, keyword, caller) do
    unless is_keyword(keyword) do
      raise ArgumentError, message: "expected arguments to be compile time keywords"
    end

    if caller.in_match? do
      raise ArgumentError, message: "cannot invoke update style macro inside match context"
    end

    Enum.reduce keyword, var, fn({ key, value }, acc) ->
      index = find_index(fields, key, 0)
      if index do
        quote do
          :erlang.setelement(unquote(index + 2), unquote(acc), unquote(value))
        end
      else
        raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect key}"
      end
    end
  end

  defp get(atom, fields, var, key) do
    index = find_index(fields, key, 0)
    if index do
      quote do
        :erlang.element(unquote(index + 2), unquote(var))
      end
    else
      raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect key}"
    end
  end

  ## Function generation

  defp reflection(values) do
    quoted = for { k, _ } <- values do
      index = find_index(values, k, 0)
      quote do
        def __record__(:index, unquote(k)), do: unquote(index + 1)
      end
    end

    quote do
      unquote(quoted)

      @doc false
      def __record__(:index, _), do: nil
      @doc false
      def __record__(:index, arg, _), do: __record__(:index, arg)

      @doc false
      def __record__(kind, _), do: __record__(kind)

      @doc false
      def __record__(:name),   do: __MODULE__
      def __record__(:fields), do: unquote(values)
    end
  end

  defp initializer(values) do
    defaults = for { _, value } <- values, do: value

    # For each value, define a piece of code that will receive
    # an ordered dict of options (opts) and it will try to fetch
    # the given key from the ordered dict, falling back to the
    # default value if one does not exist.
    atom_selective   = for { k, v } <- values, do: initialize_lookup(k, v)
    string_selective = for { k, v } <- values, do: initialize_lookup(atom_to_binary(k), v)

    quote do
      @doc false
      def new(), do: new([])

      @doc false
      def new([]), do: { __MODULE__, unquote_splicing(defaults) }
      def new([{key, _}|_] = opts) when is_atom(key), do: { __MODULE__, unquote_splicing(atom_selective) }
      def new([{key, _}|_] = opts) when is_binary(key), do: { __MODULE__, unquote_splicing(string_selective) }
    end
  end

  defp initialize_lookup(k, v) do
    quote do
      case :lists.keyfind(unquote(k), 1, opts) do
        false -> unquote(v)
        {_, v} -> v
      end
    end
  end

  defp conversions(values) do
    sorted = for { k, _ } <- values do
      index = find_index(values, k, 0)
      { k, quote(do: :erlang.element(unquote(index + 2), record)) }
    end

    quote do
      @doc false
      def to_keywords(record) do
        unquote(sorted)
      end
    end
  end

  defp accessors([{ :__exception__, _ }|t], 1) do
    accessors(t, 2)
  end

  defp accessors([{ key, _default }|t], i) do
    update = binary_to_atom "update_" <> atom_to_binary(key)

    contents = quote do
      @doc false
      def unquote(key)(record) do
        :erlang.element(unquote(i + 1), record)
      end

      @doc false
      def unquote(key)(value, record) do
        :erlang.setelement(unquote(i + 1), record, value)
      end

      @doc false
      def unquote(update)(function, record) do
        :erlang.setelement(unquote(i + 1), record,
          function.(:erlang.element(unquote(i + 1), record)))
      end
    end

    [contents|accessors(t, i + 1)]
  end

  defp accessors([], _i) do
    []
  end

  # Define an updater method that receives a
  # keyword list and updates the record.
  defp updater(values) do
    atom_fields =
      for {key, _default} <- values, do: updater_lookup(key, key, values)

    string_fields =
      for {key, _default} <- values, do: updater_lookup(atom_to_binary(key), key, values)

    atom_contents = quote do: { __MODULE__, unquote_splicing(atom_fields) }
    string_contents = quote do: { __MODULE__, unquote_splicing(string_fields) }

    quote do
      @doc false
      def update([], record) do
        record
      end

      def update([{key, _}|_] = keywords, record) when is_atom(key) do
        unquote(atom_contents)
      end
      def update([{key, _}|_] = keywords, record) when is_binary(key) do
        unquote(string_contents)
      end
    end
  end

  defp updater_lookup(k, key, values) do
    index = find_index(values, key, 0)

    quote do
      case :lists.keyfind(unquote(k), 1, keywords) do
        false -> :erlang.element(unquote(index + 2), record)
        {_, value} -> value
      end
    end
  end

  defp record_optimizable do
    quote do
      @record_optimized true
      @record_optimizable []
      @before_compile { unquote(__MODULE__), :__before_compile__ }
      @on_definition { unquote(__MODULE__), :__on_definition__ }
    end
  end

  defp switch_recorder do
    quote do: @record_optimized false
  end

  ## Types/specs generation

  defp core_specs(values) do
    types   = for { _, _, spec } <- values, do: spec
    options = for { k, _, v } <- values, do: { k, v }

    quote do
      unless Kernel.Typespec.defines_type?(__MODULE__, :t, 0) do
        @type t :: { __MODULE__, unquote_splicing(types) }
      end

      unless Kernel.Typespec.defines_type?(__MODULE__, :options, 0) do
        @type options :: unquote(options) | [{String.t, term}]
      end

      @spec new :: t
      @spec new(options) :: t
      @spec to_keywords(t) :: options
      @spec update(options, t) :: t
      @spec __record__(:name) :: atom
      @spec __record__(:fields) :: [{atom, any}]
      @spec __record__(:index, atom) :: non_neg_integer | nil
    end
  end

  defp accessor_specs([{ :__exception__, _, _ }|t], 1, acc) do
    accessor_specs(t, 2, acc)
  end

  defp accessor_specs([{ key, _default, spec }|t], i, acc) do
    update = binary_to_atom "update_" <> atom_to_binary(key)

    contents = quote do
      @spec unquote(key)(t) :: unquote(spec)
      @spec unquote(key)(unquote(spec), t) :: t
      @spec unquote(update)((unquote(spec) -> unquote(spec)), t) :: t
    end

    accessor_specs(t, i + 1, [contents | acc])
  end

  defp accessor_specs([], _i, acc), do: acc

  ## Helpers

  defp is_keyword(list) when is_list(list), do: :lists.all(&is_keyword_tuple/1, list)
  defp is_keyword(_), do: false

  defp is_keyword_tuple({ x, _ }) when is_atom(x), do: true
  defp is_keyword_tuple(_), do: false

  defp convert_value(atom) when is_atom(atom), do: { atom, nil }

  defp convert_value({ atom, other }) when is_atom(atom), do:
    { atom, check_value(atom, other) }

  defp convert_value({ field, _ }), do:
    raise(ArgumentError, message: "record field name has to be an atom, got #{inspect field}")

  defp check_value(atom, other) when is_list(other) do
    for(i <- other, do: check_value(atom, i))
    other
  end

  defp check_value(atom, other) when is_tuple(other) do
    for(i <- tuple_to_list(other), do: check_value(atom, i))
    other
  end

  defp check_value(atom, other) when is_function(other) do
    unless :erlang.fun_info(other, :env) == { :env, [] } and
           :erlang.fun_info(other, :type) == { :type, :external } do
      raise ArgumentError, message: "record field default value #{inspect atom} can only contain " <>
                                    "functions that point to an existing &Mod.fun/arity"
    end
    other
  end

  defp check_value(atom, other) when is_reference(other) or is_pid(other) or is_port(other) do
    raise(ArgumentError, message: "record field default value #{inspect atom} cannot contain a reference, pid or port")
  end

  defp check_value(_atom, other), do: other

  defp find_index([{ k, _ }|_], k, i), do: i
  defp find_index([{ _, _ }|t], k, i), do: find_index(t, k, i + 1)
  defp find_index([], _k, _i), do: nil

  defp find_spec(types, name) do
    matches = for { k, v } <- types, name == k, do: v
    case matches do
      [h|_] -> h
      _     -> quote do: term
    end
  end
end

defmodule Record.DSL do
  @moduledoc false

  defmacro record_type(opts) when is_list(opts) do
    escaped = for { k, v } <- opts, do: { k, Macro.escape(v) }

    quote do
      @record_types Keyword.merge(@record_types || [], unquote(escaped))
    end
  end
end
