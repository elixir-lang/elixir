defmodule Record do
  @moduledoc """
  Functions to define Elixir records
  """

  @doc """
  Extract record information from an Erlang file and
  return the fields as a list of tuples.

  ## Examples

      defrecord FileInfo, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

  """
  def extract(name, opts) do
    Record.Extractor.retrieve(name, opts)
  end

  @doc """
  Main entry point for records definition. It defines a module
  with the given `name` and the fields specified in `modules`.
  This is invoked directly by `Kernel.defrecord`, so check it
  for more information and documentation.
  """
  def defrecord(name, values, opts) do
    block = Keyword.get(opts, :do, nil)
    opts  = Keyword.delete(opts, :do)

    quote do
      defmodule unquote(name) do
        @moduledoc false
        import Record.DSL

        values = unquote(values)
        opts   = unquote(opts)
        Record.deffunctions(values, opts, __ENV__)
        unquote(block)
        Record.deftypes(values, @record_type, opts, __ENV__)
      end
    end
  end

  @doc """
  Defines record functions skipping the module definition.
  This is called directly by `defrecord`. It expects the
  module environment, the module values and a keyword list
  of options.

  ## Examples

      defmodule CustomRecord do
        Record.deffunctions [:name, :age], __ENV__
        Record.deftypes [:name, :age], __ENV__
      end

  """
  def deffunctions(values, opts // [], env) do
    values     = lc value inlist values, do: convert_value(value)
    escaped    = Macro.escape(values)
    extensions = Keyword.get(opts, :extensions, Record.Extensions)

    contents = [
      reflection(escaped),
      initializer(escaped),
      indexes(escaped),
      readers(values, 1, []),
      conversions(values),
      writers(values, 1, []),
      updater(values),
      extensions(values, 1, [], extensions)
    ]

    # Special case for bootstraping purposes
    if env == Macro.Env do
      :elixir_module.eval_quoted(env, contents, [], [])
    else
      contents = [quote(do: @__record__ unquote(escaped))|contents]
      Module.eval_quoted(env, contents)
    end
  end

  @doc """
  Defines types and specs for the record.
  """
  def deftypes(values, types, opts // [], env) do
    types  = types || []
    values = lc value inlist values do
      { name, default } = convert_value(value)
      { name, default, find_spec(types, name) }
    end

    extensions = Keyword.get(opts, :extensions, Record.Extensions)

    contents = [
      core_specs(values),
      accessor_specs(values, []),
      extensions_specs(values, [], extensions)
    ]

    # Special case for bootstraping purposes
    unless env == Macro.Env do
      Module.eval_quoted(env, contents)
    end
  end

  @doc """
  Defines three macros for reading and writing records values.
  These macros are private to the current module and are
  basically a simple mechanism for manipulating tuples when
  there isn't an interest in exposing the record as a whole.
  In some ways, it is similar to Erlang records, since it is
  only available at compilation time.

  ## Examples

      defmodule CustomModule do
        Record.defmacros :_user, [:name, :age], __ENV__

        def new(name, age) do
          _user(name: name, age: age)
        end

        def name(user, name) do
          _user(user, name: name)
        end

        def age(user) do
          _user(user, :age)
        end

        def to_keywords(user) do
          _user(user)
        end

        def name_and_age(user) do
         _user(user, [:name, :age])
        end

        def age_and_name(user) do
         _user(user, [:age, :name])
        end
      end

  """
  def defmacros(name, values, env) do
    escaped = lc value inlist values, do: Macro.escape(convert_value(value))

    contents = quote do
      defmacrop unquote(name).(record) when is_tuple(record) do
        Record.to_keywords(__CALLER__, __MODULE__, unquote(escaped), record)
      end

      defmacrop unquote(name).(args) do
        Record.access(__CALLER__, __MODULE__, unquote(escaped), args)
      end

      defmacrop unquote(name).(record, key) when is_atom(key) do
        Record.get(__CALLER__, __MODULE__, unquote(escaped), record, key)
      end

      defmacrop unquote(name).(record, args) do
        Record.dispatch(__CALLER__, __MODULE__, unquote(escaped), record, args)
      end
    end

    Module.eval_quoted(env, contents)
  end

  # Implements the access macro used by records.
  # It returns a quoted expression that defines
  # a record or a match in case the record is
  # inside a match.
  @doc false
  def access(caller, atom, fields, keyword) do
    unless is_keyword(keyword) do
      raise "expected contents inside brackets to be a Keyword"
    end

    in_match = caller.in_match?

    has_underscore_value = Keyword.has_key?(keyword, :_)
    underscore_value     = Keyword.get(keyword, :_, { :_, 0, nil })
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
      [] -> { :{}, caller.line, [atom|match] }
      _  ->
        keys = lc { key, _ } inlist remaining, do: key
        raise "record #{inspect atom} does not have the keys: #{inspect keys}"
    end
  end

  # Dispatch the call to either update or to_list depending on the args given.
  @doc false
  def dispatch(caller, atom, fields, record, args) do
    if is_keyword(args) do
      update(caller, atom, fields, record, args)
    else
      to_list(caller, atom, fields, record, args)
    end
  end

  # Implements the update macro defined by defmacros.
  # It returns a quoted expression that represents
  # the access given by the keywords.
  @doc false
  defp update(caller, atom, fields, var, keyword) do
    unless is_keyword(keyword) do
      raise "expected contents inside brackets to be a Keyword"
    end

    if caller.in_match? do
      raise "cannot invoke update style macro inside match context"
    end

    Enum.reduce keyword, var, fn({ key, value }, acc) ->
      index = find_index(fields, key, 0)
      if index do
        quote do
          :erlang.setelement(unquote(index + 2), unquote(acc), unquote(value))
        end
      else
        raise "record #{inspect atom} does not have the key: #{inspect key}"
      end
    end
  end

  # Implements the get macro defined by defmacros.
  # It returns a quoted expression that represents
  # getting the value of a given field.
  @doc false
  def get(_caller, atom, fields, var, key) do
    index = find_index(fields, key, 0)
    if index do
      quote do
        :erlang.element(unquote(index + 2), unquote(var))
      end
    else
      raise "record #{inspect atom} does not have the key: #{inspect key}"
    end
  end

  # Implements to_keywords macro defined by defmacros.
  # It returns a quoted expression that represents
  # converting record to keywords list.
  @doc false
  def to_keywords(_caller, _atom, fields, record) do
    Enum.map Keyword.from_enum(fields),
      fn { key, _default } ->
        index = find_index(fields, key, 0)
        quote do
          {unquote(key), :erlang.element(unquote(index + 2), unquote(record))}
        end
      end
  end

  # Implements to_list macro defined by defmacros.
  # It returns a quoted expression that represents
  # extracting given fields from record.
  @doc false
  defp to_list(_caller, atom, fields, record, keys) do
    Enum.map keys,
      fn(key) ->
        index = find_index(fields, key, 0)
        if index do
          quote do: :erlang.element(unquote(index + 2), unquote(record))
        else
          raise "record #{inspect atom} does not have the key: #{inspect key}"
        end
      end
  end

  ## Function generation

  # Define __record__/1 and __record__/2 as reflection functions
  # that returns the record names and fields.
  #
  # Note that fields are *not* keywords. They are in the same
  # order as given as parameter and reflects the order of the
  # fields in the tuple.
  #
  # ## Examples
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  #     FileInfo.__record__(:name)   #=> FileInfo
  #     FileInfo.__record__(:fields) #=> [atime: nil, mtime: nil]
  #
  defp reflection(values) do
    quote do
      def __record__(kind, _),      do: __record__(kind)
      def __record__(:name),        do: __MODULE__
      def __record__(:fields),      do: unquote(values)
    end
  end

  # Define initializers methods. For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define three methods:
  #
  #     def new() do
  #       new([])
  #     end
  #
  #     def new([]) do
  #       { FileInfo, nil, nil }
  #     end
  #
  #     def new(opts) do
  #       { FileInfo, Keyword.get(opts, :atime), Keyword.get(opts, :mtime) }
  #     end
  #
  defp initializer(values) do
    defaults = lc { _, value } inlist values, do: value

    # For each value, define a piece of code that will receive
    # an ordered dict of options (opts) and it will try to fetch
    # the given key from the ordered dict, falling back to the
    # default value if one does not exist.
    selective = lc { k, v } inlist values do
      quote do: Keyword.get(opts, unquote(k), unquote(v))
    end

    quote do
      def new(), do: new([])
      def new([]), do: { __MODULE__, unquote_splicing(defaults) }
      def new(opts) when is_list(opts), do: { __MODULE__, unquote_splicing(selective) }
      def new(tuple) when is_tuple(tuple), do: :erlang.setelement(1, tuple, __MODULE__)
    end
  end

  # Define method to get index of a given key.
  #
  # Useful if you need to know position of the key for such applications as:
  #  - ets
  #  - mnesia
  #
  # For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define following method:
  #
  #     def __index__(:atime), do: 2
  #     def __index__(:mtime), do: 3
  #     def __index__(_), do: nil
  #
  defp indexes(values) do
    quoted = lc { k, _ } inlist values do
      index = find_index(values, k, 0)
      quote do
        def __index__(unquote(k)), do: unquote(index + 1)
      end
    end
    quote do
      unquote(quoted)
      def __index__(_), do: nil
      def __index__(key, _), do: __index__(key)
    end
  end

  # Define converters method(s). For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define one method, to_keywords, which will return a Keyword
  #
  #    [atime: nil, mtime: nil]
  #
  defp conversions(values) do
    sorted = lc { k, _ } inlist values do
      index = find_index(values, k, 0)
      { k, quote(do: :erlang.element(unquote(index + 2), record)) }
    end

    quote do
      def to_keywords(record) do
        unquote(:orddict.from_list(sorted))
      end
    end
  end

  # Implement readers. For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define four methods:
  #
  #     def :atime.(record) do
  #       elem(record, 1)
  #     end
  #
  #     def :mtime.(record) do
  #       elem(record, 2)
  #     end
  #
  defp readers([{ key, _default }|t], i, acc) do
    contents = quote do
      def unquote(key).(record) do
        :erlang.element(unquote(i + 1), record)
      end
    end

    readers(t, i + 1, [contents | acc])
  end

  defp readers([], _i, acc), do: acc

  # Implement writers. For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define four methods:
  #
  #     def :atime.(value, record) do
  #       setelem(record, 1, value)
  #     end
  #
  #     def :mtime.(record) do
  #       setelem(record, 2, value)
  #     end
  #
  defp writers([{ key, _default }|t], i, acc) do
    contents = quote do
      def unquote(key).(value, record) do
        :erlang.setelement(unquote(i + 1), record, value)
      end
    end

    writers(t, i + 1, [contents | acc])
  end

  defp writers([], _i, acc), do: acc

  # Define an updater method that receives a
  # keyword list and updates the record.
  defp updater(values) do
    fields =
      lc {key, _default} inlist values do
        index = find_index(values, key, 1)
        quote do
          Keyword.get(keywords, unquote(key), elem(record, unquote(index)))
        end
      end

    contents = { :{}, 0, [(quote do: __MODULE__)|fields] }

    quote do
      def update(keywords, record) do
        unquote(contents)
      end
    end
  end

  # Defines extra functions from the definition.
  defp extensions([{ key, default }|t], i, acc, extensions) do
    functions = extensions.functions_for(key, default, i)
    extensions(t, i + 1, [functions | acc], extensions)
  end

  defp extensions([], _i, acc, _), do: acc

  ## Types/specs generation

  defp core_specs(values) do
    types = lc { _, _, spec } inlist values, do: spec

    quote do
      unless Kernel.Typespec.defines_type?(__MODULE__, :t, 0) do
        Kernel.Typespec.deftype t :: { __MODULE__, unquote_splicing(types) }
      end

      @spec new, do: t
      @spec new(Keyword.t | tuple), do: t
      @spec to_keywords(t), do: Keyword.t
      @spec update(Keyword.t, t), do: t
      @spec __index__(atom), do: non_neg_integer | nil
    end
  end

  defp accessor_specs([{ key, _default, spec }|t], acc) do
    contents = quote do
      @spec unquote(key)(t), do: unquote(spec)
      @spec unquote(key)(unquote(spec), t), do: t
    end
    accessor_specs(t, [contents | acc])
  end

  defp accessor_specs([], acc), do: acc

  defp extensions_specs([{ key, default, spec }|t], acc, extensions) do
    specs = extensions.specs_for(key, default, spec)
    extensions_specs(t, [specs | acc], extensions)
  end

  defp extensions_specs([], acc, _), do: acc

  ## Helpers

  defp is_keyword(list) when is_list(list), do: :lists.all(is_keyword_tuple(&1), list)
  defp is_keyword(_), do: false

  defp is_keyword_tuple({ x, _ }) when is_atom(x), do: true
  defp is_keyword_tuple(_), do: false

  defp convert_value(atom) when is_atom(atom), do: { atom, nil }
  defp convert_value(other), do: other

  defp find_index([{ k, _ }|_], k, i), do: i
  defp find_index([{ _, _ }|t], k, i), do: find_index(t, k, i + 1)
  defp find_index([], _k, _i), do: nil

  defp find_spec(types, name) do
    matches = lc { k, v } inlist types, name == k, do: v
    case matches do
      [h|_] -> h
      _     -> quote do: term
    end
  end
end

defmodule Record.DSL do
  @moduledoc false

  @doc """
  Defines the type for each field in the record.
  Expects a keyword list.
  """
  defmacro record_type(opts) when is_list(opts) do
    quote do
      @record_type quote do: unquote(opts)
    end
  end
end

defmodule Record.Extensions do
  @moduledoc false

  # Function definition

  def functions_for(key, default, i) do
    [ default_for(key, default, i),
      extension_for(key, default, i) ]
  end

  defp default_for(:__exception__, _default, _i) do
    nil
  end

  defp default_for(key, _default, i) do
    update  = prefix("update_", key)

    quote do
      def unquote(update).(function, record) do
        current = :erlang.element(unquote(i + 1), record)
        :erlang.setelement(unquote(i + 1), record, function.(current))
      end
    end
  end

  defp extension_for(key, default, i) when is_list(default) do
    prepend = prefix("prepend_", key)
    merge   = prefix("merge_", key)

    quote do
      def unquote(prepend).(value, record) do
        current = :erlang.element(unquote(i + 1), record)
        :erlang.setelement(unquote(i + 1), record, value ++ current)
      end

      def unquote(merge).(value, record) do
        current = :erlang.element(unquote(i + 1), record)
        :erlang.setelement(unquote(i + 1), record, Keyword.merge(current, value))
      end
    end
  end

  defp extension_for(key, default, i) when is_number(default) do
    increment = prefix("increment_", key)

    quote do
      def unquote(increment).(value // 1, record) do
        current = :erlang.element(unquote(i + 1), record)
        :erlang.setelement(unquote(i + 1), record, current + value)
      end
    end
  end

  defp extension_for(key, default, i) when is_boolean(default) do
    toggle = prefix("toggle_", key)

    quote do
      def unquote(toggle).(record) do
        current = :erlang.element(unquote(i + 1), record)
        :erlang.setelement(unquote(i + 1), record, not current)
       end
    end
  end

  defp extension_for(_, _, _), do: nil

  # Specs definition

  def specs_for(key, default, spec) do
    [ default_specs_for(key, default, spec),
      extension_specs_for(key, default, spec) ]
  end

  defp default_specs_for(:__exception__, _default, _spec) do
    nil
  end

  defp default_specs_for(key, _default, spec) do
    update = prefix("update_", key)

    quote do
      @spec unquote(update)(fun(unquote(spec), do: unquote(spec)), t), do: t
    end
  end

  defp extension_specs_for(key, default, _spec) when is_list(default) do
    prepend = prefix("prepend_", key)
    merge   = prefix("merge_", key)

    quote do
      @spec unquote(prepend)(list, t), do: t
      @spec unquote(merge)(Keyword.t, t), do: t
    end
  end

  defp extension_specs_for(key, default, _spec) when is_number(default) do
    increment = prefix("increment_", key)

    quote do
      @spec unquote(increment)(number, t), do: t
    end
  end

  defp extension_specs_for(key, default, _spec) when is_boolean(default) do
    toggle = prefix("toggle_", key)

    quote do
      @spec unquote(toggle)(t), do: t
    end
  end

  defp extension_specs_for(_, _, _), do: nil

  # Helpers

  defp prefix(prefix, key) do
    binary_to_atom prefix <> atom_to_binary(key)
  end
end
