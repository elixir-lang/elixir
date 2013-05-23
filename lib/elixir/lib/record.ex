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
  with the given `name` and the fields specified in `values`.
  This is invoked directly by `Kernel.defrecord`, so check it
  for more information and documentation.
  """
  def defrecord(name, values, opts) do
    block = Keyword.get(opts, :do, nil)

    quote do
      unquoted_values = unquote(values)

      defmodule unquote(name) do
        @moduledoc false
        import Record.DSL

        @record_fields []
        @record_types  []

        # Reassign values to inner scope to
        # avoid conflicts in nested records
        values = unquoted_values

        Record.deffunctions(values, __ENV__)
        value = unquote(block)
        Record.deftypes(values, @record_types, __ENV__)
        value
      end
    end
  end

  @doc """
  Import public record definition as a set of private macros (as defined by defrecordp/2)

  ## Usage

     Record.import Record.Module, as: macro_name

  ## Example

     defmodule Test do
       Record.import File.Stat, as: :file_stat

       def size(file_stat(size: size)), do: size
     end

  """
  defmacro import(module, as: name) do
    quote do
      Record.defmacros(unquote(name), unquote(module).__record__(:fields), __ENV__, unquote(module))
    end
  end

  @doc """
  Main entry point for private records definition. It defines
  a set of macros with the given `name` and the fields specified
  in `values`. This is invoked directly by `Kernel.defrecordp`,
  so check it for more information and documentation.
  """
  def defrecordp(name, fields) do
    quote do
      Record.defmacros(unquote(name), unquote(fields), __ENV__)
    end
  end

  @doc """
  Defines record functions skipping the module definition.
  This is called directly by `defrecord`. It expects the record
  values, a set of options and the module environment.

  ## Examples

      defmodule CustomRecord do
        Record.deffunctions [:name, :age], __ENV__
        Record.deftypes [:name, :age], [name: :binary, age: :integer], __ENV__
      end

  """
  def deffunctions(values, env) do
    values  = lc value inlist values, do: convert_value(value)
    escaped = Macro.escape(values)

    contents = [
      reflection(escaped),
      initializer(escaped),
      indexes(escaped),
      conversions(values),
      record_optimizable(),
      updater(values),
      accessors(values, 1),
      switch_recorder()
    ]

    contents = [quote(do: @record_fields unquote(escaped))|contents]

    # Special case for bootstraping purposes
    if env == Macro.Env do
      Module.eval_quoted(env, contents, [], [])
    else
      Module.eval_quoted(env.module, contents, [], env.location)
    end
  end

  @doc """
  Defines types and specs for the record.
  """
  def deftypes(values, types, env) do
    types  = types || []
    values = lc value inlist values do
      { name, default } = convert_value(value)
      { name, default, find_spec(types, name) }
    end

    contents = [
      core_specs(values),
      accessor_specs(values, 1, [])
    ]

    if env == Macro.Env do
      Module.eval_quoted(env, contents, [], [])
    else
      Module.eval_quoted(env.module, contents, [], env.location)
    end
  end

  @doc """
  Defines macros for manipulating records. This is called
  directly by `defrecordp`. It expects the macro name, the
  record values and the environment.

  ## Examples

      defmodule CustomRecord do
        Record.defmacros :user, [:name, :age], __ENV__
      end

  """
  def defmacros(name, values, env, tag // nil) do
    escaped = lc value inlist values do
      { key, value } = convert_value(value)
      { key, Macro.escape(value) }
    end

    contents = quote do
      defmacrop unquote(name)() do
        Record.access(unquote(tag) || __MODULE__, unquote(escaped), [], __CALLER__)
      end

      defmacrop unquote(name)(record) when is_tuple(record) do
        Record.to_keywords(unquote(tag) || __MODULE__, unquote(escaped), record)
      end

      defmacrop unquote(name)(args) do
        Record.access(unquote(tag) || __MODULE__, unquote(escaped), args, __CALLER__)
      end

      defmacrop unquote(name)(record, key) when is_atom(key) do
        Record.get(unquote(tag) || __MODULE__, unquote(escaped), record, key)
      end

      defmacrop unquote(name)(record, args) do
        Record.dispatch(unquote(tag) || __MODULE__, unquote(escaped), record, args, __CALLER__)
      end
    end

    Module.eval_quoted(env.module, contents, [], env.location)
  end

  ## Callbacks

  # Store all optimizable fields in the record as well
  @doc false
  defmacro __before_compile__(_) do
    quote do
      def __record__(:optimizable), do: @record_optimizable
    end
  end

  # Store fields that can be optimized and that cannot be
  # optimized as they are overriden
  @doc false
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

  # Implements the access macro used by records.
  # It returns a quoted expression that defines
  # a record or a match in case the record is
  # inside a match.
  @doc false
  def access(atom, fields, keyword, caller) do
    unless is_keyword(keyword) do
      raise "expected contents inside brackets to be a Keyword"
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
        quote do: { unquote_splicing([atom|match]) }
      _  ->
        keys = lc { key, _ } inlist remaining, do: key
        raise "record #{inspect atom} does not have the keys: #{inspect keys}"
    end
  end

  # Dispatch the call to either update or to_list depending on the args given.
  @doc false
  def dispatch(atom, fields, record, args, caller) do
    if is_keyword(args) do
      update(atom, fields, record, args, caller)
    else
      to_list(atom, fields, record, args)
    end
  end

  # Implements the update macro defined by defmacros.
  # It returns a quoted expression that represents
  # the access given by the keywords.
  @doc false
  defp update(atom, fields, var, keyword, caller) do
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
  def get(atom, fields, var, key) do
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
  def to_keywords(_atom, fields, record) do
    Enum.map fields,
      fn { key, _default } ->
        index = find_index(fields, key, 0)
        quote do
          { unquote(key), :erlang.element(unquote(index + 2), unquote(record)) }
        end
      end
  end

  # Implements to_list macro defined by defmacros.
  # It returns a quoted expression that represents
  # extracting given fields from record.
  @doc false
  defp to_list(atom, fields, record, keys) do
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
      @doc false
      def __record__(kind, _),      do: __record__(kind)

      @doc false
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
      @doc false
      def new(), do: new([])

      @doc false
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
        @doc false
        def __index__(unquote(k)), do: unquote(index + 1)
      end
    end
    quote do
      unquote(quoted)

      @doc false
      def __index__(_), do: nil

      @doc false
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
      @doc false
      def to_keywords(record) do
        unquote(:orddict.from_list(sorted))
      end
    end
  end

  # Implement accessors. For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define four methods:
  #
  #     def atime(record) do
  #       elem(record, 1)
  #     end
  #
  #     def mtime(record) do
  #       elem(record, 2)
  #     end
  #
  #     def atime(value, record) do
  #       set_elem(record, 1, value)
  #     end
  #
  #     def mtime(record) do
  #       set_elem(record, 2, value)
  #     end
  #
  #     def atime(callback, record) do
  #       set_elem(record, 1, callback.(elem(record, 1)))
  #     end
  #
  #     def mtime(callback, record) do
  #       set_elem(record, 2, callback.(elem(record, 2)))
  #     end
  #
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
    fields =
      lc {key, _default} inlist values do
        index = find_index(values, key, 1)
        quote do
          Keyword.get(keywords, unquote(key), elem(record, unquote(index)))
        end
      end

    contents = quote do: { __MODULE__, unquote_splicing(fields) }

    quote do
      @doc false
      def update([], record) do
        record
      end

      def update(keywords, record) do
        unquote(contents)
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
    types   = lc { _, _, spec } inlist values, do: spec
    options = if values == [], do: [], else: [options_specs(values)]

    quote do
      unless Kernel.Typespec.defines_type?(__MODULE__, :t, 0) do
        @type t :: { __MODULE__, unquote_splicing(types) }
      end

      unless Kernel.Typespec.defines_type?(__MODULE__, :options, 0) do
        @type options :: unquote(options)
      end

      @spec new :: t
      @spec new(options | tuple) :: t
      @spec to_keywords(t) :: options
      @spec update(options, t) :: t
      @spec __record__(:name) :: atom
      @spec __record__(:fields) :: [{atom,any}]
      @spec __index__(atom) :: non_neg_integer | nil
    end
  end

  defp options_specs([{ k, _, v }|t]) do
    :lists.foldl fn { k, _, v }, acc ->
      { :|, [], [{ k, v }, acc] }
    end, { k, v }, t
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

  defp is_keyword(list) when is_list(list), do: :lists.all(is_keyword_tuple(&1), list)
  defp is_keyword(_), do: false

  defp is_keyword_tuple({ x, _ }) when is_atom(x), do: true
  defp is_keyword_tuple(_), do: false

  defp convert_value(atom) when is_atom(atom), do: { atom, nil }

  defp convert_value({ atom, other }) when is_atom(atom) and is_function(other), do:
    raise ArgumentError, message: "record field default value #{inspect atom} cannot be a function"

  defp convert_value({ atom, other }) when is_atom(atom) and (is_reference(other) or is_pid(other) or is_port(other)), do:
    raise ArgumentError, message: "record field default value #{inspect atom} cannot be a reference, pid or port"

  defp convert_value({ atom, _ } = tuple) when is_atom(atom), do: tuple

  defp convert_value({ field, _ }), do:
    raise ArgumentError, message: "record field name has to be an atom, got #{inspect field}"

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
    escaped = lc { k, v } inlist opts, do: { k, Macro.escape(v) }

    quote do
      @record_types Keyword.merge(@record_types || [], unquote(escaped))
    end
  end
end
