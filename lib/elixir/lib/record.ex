defmodule Record do
  @moduledoc %B"""
  Convenience functions for working with Records.

  A record is a tagged tuple which contains one or more elements
  and the first element is an atom. We can manually create a record
  by simply defining such tuple:

      iex> record = { User, "josé", 25 }
      iex> is_record(record, User)
      true

  However, manually constructing tuples can be quite error prone.
  In case we need to add a new field to our User, it would require
  us to carefully change all places where the tuple is used.
  Furthermore, as more and more items are added to the tuple, they
  lose semantic value.

  This module solves those problems by allowing us to name each
  element and encapsulate the generation and manipulation of
  such tuples. Many functionality provided by this module happens
  at compilation time, meaning they don't add any overhead while
  considerably improving the quality of our code.

  For those reasons, Records are frequently used in Elixir and
  they are also very useful when used with Protocols. This module
  provides different mechanisms for working with records and we
  are going to explore them in the following sections.

  ## defrecordp

  One of the simplest ways of working with records is by defining
  calling `defrecordp`:

      defmodule User do
        defrecordp :user, name: "José", age: 25
      end

  In the example above, `defrecordp` is going to generate a set of
  macros named `user` that allows us to create, update and match
  on a record. Our record is going to have two fields, a name with
  default value of "José" and age of 25.

  Let's see some examples:

      # To create records
      user()        #=> { User, "José", 25 }
      user(age: 26) #=> { User, "José", 26 }

  By using the `user` macro, we no longer need to explicitly create
  a tuple with all elements. It also allows us to create and modify
  values by name:

      # Create a new record
      user = user()

      # And now change its age to 26
      user(user, age: 26)

  Since `user` is a macro, all the work happens at compilation time.
  This means all the operations, like changing the age above, works
  as a simple tuple operation at runtime:

      # This update operation...
      user(user, age: 26)

      # Literally translates to this one:
      set_elem(user, 2, 26)

  For this reason, the following operation is not allowed as all
  values need to be explicit:

      new_values = [age: 26]
      user(user, new_values)

  As the name says, `defrecordp` is useful when you don't want to
  expose the record definition. The `user` macro used above, for
  example, is only available inside the `User` module and nowhere else.
  You can find more information in `Kernel.defrecordp/2` docs.

  In general, though, records are used as part of a module's public
  interface. For such use cases, Elixir provides record modules.

  ## defrecord

  By using `defrecord`, a developer can make a Record definition
  available everywhere within Elixir code. Let's see an example:

      defrecord User, name: "José", age: 25

  Notice that, different to `defrecordp`, `defrecord` expects an
  alias as first argument. This is because `defrecord` is going
  to create a module named `User` with all the relevant metadata.
  This module can then be imported and we can manipulate the user
  as with `defrecordp`:

      Record.import User, as: :user
      user()        #=> User[name: "José", age: 25]
      user(age: 26) #=> User[name: "José", age: 26]

  Notice that now, since the record definition is accessible, Elixir
  shows the record nicely formatted, no longer as a simple tuple.

  Since working with external records is frequent, Elixir allows
  developers to skip importing the record altogether in favor
  of a `Module[args]` syntax:

      # Skipping a field uses its default value
      User[]        #=> User[name: "José", age: 25]
      User[age: 26] #=> User[name: "José", age: 26]

  Updates can also be done via the shortcut syntax:

      # The previous syntax
      user(user, age: 26)

      # Now becomes:
      User[user, age: 26]

  The macro name is replaced by the module name and the parenthesis
  are replaced by brackets. When the shortcut syntax is used, there
  is no need to import the record.

  Before we sum up the differences in between `defrecord` and
  `defrecordp`, there is one last functionality introduced by
  `defrecord` that we need to discuss.

  ## Runtime access

  All the functionality discussed above happens at compilation time.
  This means that both `user(user, age: 26)` and `User[user, age: 26]`
  will be transformed into a simple tuple operation at compile time.

  However, there are some situations where we want to set or update
  fields dynamically. `defrecord` (and `defrecord` only) supports
  it out of the box:

      defrecord User, name: "José", age: 25

      opts = [name: "Hello"]
      user = User.new(opts)
      #=> User[name: "Hello", age: 25]
      user.update(age: 26)
      #=> User[name: "Hello", age: 26]

  All the calls above are happening at runtime. It gives Elixir
  records a lot of flexibility at the cost of performance since
  there is more work happening at runtime.

  To sum it up, `defrecordp` should be used when you don't want
  to expose the record information while `defrecord` should be used
  whenever you want to share a record within your code or with other
  libraries or whenever you need to dynamically set or update fields.

  You can learn more about it on `Kernel.defrecord/2` docs. Now
  let's discuss the usefulness of using records with protocols.

  ## Protocols

  Developers can extend existing protocols by creating their own
  records and implementing the desired protocols. For instance,
  imagine that you have created a new representation for storing
  date and time, represented by the year, the week of the year
  and the week day:

      defrecord WeekDate, year: nil, week: nil, week_day: nil

  Now we want this date to be represented as a string and this
  can be done by implementing the `Binary.Chars` protocol for
  our record:

      defimpl Binary.Chars, for: WeekDate do
        def to_binary(WeekDate[year: year, week: week, week_day: day]) do
          "#{year}-W#{week}-#{day}"
        end
      end

  Now we can explicitly convert our WeekDate:

      to_binary WeekDate[year: 2013, week: 26, week_day: 4]
      "2013-W26-4"

  A protocol can be implemented for any record, regardless if
  it was generated by `defrecord` or `defrecordp`. That's because
  records are simply tuples and the protocol works directly on
  the tuple.

  Note: Although a record can be defined using a regular atom
  instead of an alias, as shown below, such records are **not**
  supported in Protocols:

      defrecord :just_an_atom, [:name, :age]

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
  Imports a public record definition as a set of private macros,
  as defined by `Kernel.defrecordp/2`. This is useful when one
  desires to manipulate a record via a set of macros instead
  of the regular access syntax.

  ## Example

     defmodule Test do
       Record.import File.Stat, as: :file_stat

       def size(file_stat(size: size)), do: size
     end

  """
  defmacro import(module, as: name) do
    quote do
      module = unquote(module)

      fields = if module == __MODULE__ do
        @record_fields
      else
        module.__record__(:fields)
      end

      Record.defmacros(unquote(name), fields, __ENV__, module)
    end
  end

  @doc false
  def defrecord(name, values, opts) do
    block = Keyword.get(opts, :do, nil)
    { fields, types } = record_split(values)

    quote do
      unquoted_fields = unquote(fields)

      defmodule unquote(name) do
        import Elixir.Record.DSL

        @record_fields []
        @record_types  unquote(types)

        Elixir.Record.deffunctions(unquoted_fields, __ENV__)
        value = unquote(block)
        Elixir.Record.deftypes(@record_fields, @record_types, __ENV__)
        value
      end
    end
  end

  defp record_split(fields) when is_list(fields) do
    record_split(fields, [], [])
  end

  defp record_split(other) do
    { other, [] }
  end

  defp record_split([{ field, { :::, _, [default, type] }}|t], defaults, types) do
    record_split t, [{ field, default }|defaults], [{ field, Macro.escape(type) }|types]
  end

  defp record_split([other|t], defaults, types) do
    record_split t, [other|defaults], types
  end

  defp record_split([], defaults, types) do
    { :lists.reverse(defaults), types }
  end

  @doc false
  def defrecordp(name, tag, fields) when is_atom(name) and is_atom(tag) and is_list(fields) do
    { fields, types, def_type } = recordp_split(fields, [], [], false)
    type = :"#{name}_t"

    quote do
      Record.defmacros(unquote(name), unquote(fields), __ENV__, unquote(tag))

      if unquote(def_type) do
        @typep unquote(type)() :: { unquote(tag), unquote_splicing(types) }
      end
    end
  end

  defp recordp_split([{ field, { :::, _, [default, type] }}|t], defaults, types, _) do
    recordp_split t, [{ field, default }|defaults], [type|types], true
  end

  defp recordp_split([other|t], defaults, types, def_type) do
    recordp_split t, [other|defaults], [quote(do: term)|types], def_type
  end

  defp recordp_split([], defaults, types, def_type) do
    { :lists.reverse(defaults), :lists.reverse(types), def_type }
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
      raise ArgumentError, message: "expected contents inside brackets to be a Keyword"
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
        raise ArgumentError, message: "record #{inspect atom} does not have the keys: #{inspect keys}"
    end
  end

  # Dispatch the call to either get, to_list or update depending on the args given.
  @doc false
  def dispatch(atom, fields, record, args, caller) do
    cond do
      is_atom(args) ->
        get(atom, fields, record, args)
      is_keyword(args) ->
        update(atom, fields, record, args, caller)
      is_list(args) ->
        to_list(atom, fields, record, args)
      true ->
        raise ArgumentError, message: "expected arguments to be a compile time list or compile time keywords"
    end
  end

  # Implements the update macro defined by defmacros.
  # It returns a quoted expression that represents
  # the access given by the keywords.
  @doc false
  defp update(atom, fields, var, keyword, caller) do
    unless is_keyword(keyword) do
      raise ArgumentError, message: "expected arguments to be a compile time keywords"
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
      raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect key}"
    end
  end

  # Implements to_keywords macro defined by defmacros.
  # It returns a quoted expression that represents
  # converting record to keywords list.
  @doc false
  def to_keywords(_atom, fields, record) do
    { var, extra } = cache_var(record)

    keywords = Enum.map fields,
      fn { key, _default } ->
        index = find_index(fields, key, 0)
        quote do
          { unquote(key), :erlang.element(unquote(index + 2), unquote(var)) }
        end
      end

    quote do
      unquote_splicing(extra)
      unquote(keywords)
    end
  end

  # Implements to_list macro defined by defmacros.
  # It returns a quoted expression that represents
  # extracting given fields from record.
  @doc false
  defp to_list(atom, fields, record, keys) do
    unless is_list(fields) do
      raise ArgumentError, message: "expected arguments to be a compile time list"
    end

    { var, extra } = cache_var(record)

    list = Enum.map keys,
      fn(key) ->
        index = find_index(fields, key, 0)
        if index do
          quote do: :erlang.element(unquote(index + 2), unquote(var))
        else
          raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect key}"
        end
      end

    quote do
      unquote_splicing(extra)
      unquote(list)
    end
  end

  defp cache_var({ var, _, kind } = tuple) when is_atom(var) and is_atom(kind) do
    { tuple, [] }
  end

  defp cache_var(other) do
    quote do
      { x, [x = unquote(other)] }
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
      @spec __record__(:fields) :: [{atom, any}]
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
