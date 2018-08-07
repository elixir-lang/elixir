defmodule Record do
  @moduledoc """
  Module to work with, define, and import records.

  Records are simply tuples where the first element is an atom:

      iex> Record.is_record({User, "john", 27})
      true

  This module provides conveniences for working with records at
  compilation time, where compile-time field names are used to
  manipulate the tuples, providing fast operations on top of
  the tuples' compact structure.

  In Elixir, records are used mostly in two situations:

    1. to work with short, internal data
    2. to interface with Erlang records

  The macros `defrecord/3` and `defrecordp/3` can be used to create records
  while `extract/2` and `extract_all/1` can be used to extract records from
  Erlang files.

  ## Types

  Types can be defined for tuples with the `record/2` macro (only available in
  typespecs). This macro will expand to a tuple as seen in the example below:

      defmodule MyModule do
        require Record
        Record.defrecord(:user, name: "john", age: 25)

        @type user :: record(:user, name: String.t(), age: integer)
        # expands to: "@type user :: {:user, String.t(), integer}"
      end

  """

  @doc """
  Extracts record information from an Erlang file.

  Returns a quoted expression containing the fields as a list
  of tuples.

  `name`, which is the name of the extracted record, is expected to be an atom
  *at compile time*.

  ## Options

  This function accepts the following options, which are exclusive to each other
  (i.e., only one of them can be used in the same call):

    * `:from` - (binary representing a path to a file) path to the Erlang file
      that contains the record definition to extract; with this option, this
      function uses the same path lookup used by the `-include` attribute used in
      Erlang modules.

    * `:from_lib` - (binary representing a path to a file) path to the Erlang
      file that contains the record definition to extract; with this option,
      this function uses the same path lookup used by the `-include_lib`
      attribute used in Erlang modules.

    * `:includes` - (a list of directories as binaries) if the record being
      extracted depends on relative includes, this option allows developers
      to specify the directory where those relative includes exist.

    * `:macros` - (keyword list of macro names and values) if the record
      being extracted depends on the values of macros, this option allows
      the value of those macros to be set.

  These options are expected to be literals (including the binary values) at
  compile time.

  ## Examples

      iex> Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
      [
        size: :undefined,
        type: :undefined,
        access: :undefined,
        atime: :undefined,
        mtime: :undefined,
        ctime: :undefined,
        mode: :undefined,
        links: :undefined,
        major_device: :undefined,
        minor_device: :undefined,
        inode: :undefined,
        uid: :undefined,
        gid: :undefined
      ]

  """
  @spec extract(name :: atom, keyword) :: keyword
  def extract(name, opts) when is_atom(name) and is_list(opts) do
    Record.Extractor.extract(name, opts)
  end

  @doc """
  Extracts all records information from an Erlang file.

  Returns a keyword list of `{record_name, fields}` tuples where `record_name`
  is the name of an extracted record and `fields` is a list of `{field, value}`
  tuples representing the fields for that record.

  ## Options

  This function accepts the following options, which are exclusive to each other
  (i.e., only one of them can be used in the same call):

    * `:from` - (binary representing a path to a file) path to the Erlang file
      that contains the record definitions to extract; with this option, this
      function uses the same path lookup used by the `-include` attribute used in
      Erlang modules.

    * `:from_lib` - (binary representing a path to a file) path to the Erlang
      file that contains the record definitions to extract; with this option,
      this function uses the same path lookup used by the `-include_lib`
      attribute used in Erlang modules.

  These options are expected to be literals (including the binary values) at
  compile time.
  """
  @spec extract_all(keyword) :: [{name :: atom, keyword}]
  def extract_all(opts) when is_list(opts) do
    Record.Extractor.extract_all(opts)
  end

  @doc """
  Checks if the given `data` is a record of kind `kind`.

  This is implemented as a macro so it can be used in guard clauses.

  ## Examples

      iex> record = {User, "john", 27}
      iex> Record.is_record(record, User)
      true

  """
  defguard is_record(data, kind)
           when is_atom(kind) and is_tuple(data) and tuple_size(data) > 0 and
                  elem(data, 0) == kind

  @doc """
  Checks if the given `data` is a record.

  This is implemented as a macro so it can be used in guard clauses.

  ## Examples

      iex> record = {User, "john", 27}
      iex> Record.is_record(record)
      true
      iex> tuple = {}
      iex> Record.is_record(tuple)
      false

  """
  defguard is_record(data)
           when is_tuple(data) and tuple_size(data) > 0 and is_atom(elem(data, 0))

  @doc """
  Defines a set of macros to create, access, and pattern match
  on a record.

  The name of the generated macros will be `name` (which has to be an
  atom). `tag` is also an atom and is used as the "tag" for the record (i.e.,
  the first element of the record tuple); by default (if `nil`), it's the same
  as `name`. `kv` is a keyword list of `name: default_value` fields for the
  new record.

  The following macros are generated:

    * `name/0` to create a new record with default values for all fields
    * `name/1` to create a new record with the given fields and values,
      to get the zero-based index of the given field in a record or to
      convert the given record to a keyword list
    * `name/2` to update an existing record with the given fields and values
      or to access a given field in a given record

  All these macros are public macros (as defined by `defmacro`).

  See the "Examples" section for examples on how to use these macros.

  ## Examples

      defmodule User do
        require Record
        Record.defrecord(:user, name: "meg", age: "25")
      end

  In the example above, a set of macros named `user` but with different
  arities will be defined to manipulate the underlying record.

      # Import the module to make the user macros locally available
      import User

      # To create records
      record = user()        #=> {:user, "meg", 25}
      record = user(age: 26) #=> {:user, "meg", 26}

      # To get a field from the record
      user(record, :name) #=> "meg"

      # To update the record
      user(record, age: 26) #=> {:user, "meg", 26}

      # To get the zero-based index of the field in record tuple
      # (index 0 is occupied by the record "tag")
      user(:name) #=> 1

      # Convert a record to a keyword list
      user(record) #=> [name: "meg", age: 26]

  The generated macros can also be used in order to pattern match on records and
  to bind variables during the match:

      record = user() #=> {:user, "meg", 25}

      user(name: name) = record
      name #=> "meg"

  By default, Elixir uses the record name as the first element of the tuple (the "tag").
  However, a different tag can be specified when defining a record,
  as in the following example, in which we use `Customer` as the second argument of `defrecord/3`:

      defmodule User do
        require Record
        Record.defrecord(:user, Customer, name: nil)
      end

      require User
      User.user() #=> {Customer, nil}

  ## Defining extracted records with anonymous functions in the values

  If a record defines an anonymous function in the default values, an
  `ArgumentError` will be raised. This can happen unintentionally when defining
  a record after extracting it from an Erlang library that uses anonymous
  functions for defaults.

      Record.defrecord(:my_rec, Record.extract(...))
      #=> ** (ArgumentError) invalid value for record field fun_field,
      #=>   cannot escape #Function<12.90072148/2 in :erl_eval.expr/5>.

  To work around this error, redefine the field with your own &M.f/a function,
  like so:

      defmodule MyRec do
        require Record
        Record.defrecord(:my_rec, Record.extract(...) |> Keyword.merge(fun_field: &__MODULE__.foo/2))
        def foo(bar, baz), do: IO.inspect({bar, baz})
      end

  """
  defmacro defrecord(name, tag \\ nil, kv) do
    quote bind_quoted: [name: name, tag: tag, kv: kv] do
      defined_arity =
        Enum.find(0..2, fn arity ->
          Module.defines?(__MODULE__, {name, arity})
        end)

      if defined_arity do
        raise ArgumentError,
              "cannot define record #{inspect(name)} because a definition #{name}/#{defined_arity} already exists"
      end

      tag = tag || name

      fields = Record.__fields__(:defrecord, kv)

      defmacro unquote(name)(args \\ []) do
        Record.__access__(unquote(tag), unquote(fields), args, __CALLER__)
      end

      defmacro unquote(name)(record, args) do
        Record.__access__(unquote(tag), unquote(fields), record, args, __CALLER__)
      end
    end
  end

  @doc """
  Same as `defrecord/3` but generates private macros.
  """
  defmacro defrecordp(name, tag \\ nil, kv) do
    quote bind_quoted: [name: name, tag: tag, kv: kv] do
      defined_arity =
        Enum.find(0..2, fn arity ->
          Module.defines?(__MODULE__, {name, arity})
        end)

      if defined_arity do
        raise ArgumentError,
              "cannot define record #{inspect(name)} because a definition #{name}/#{defined_arity} already exists"
      end

      tag = tag || name

      fields = Record.__fields__(:defrecordp, kv)

      defmacrop unquote(name)(args \\ []) do
        Record.__access__(unquote(tag), unquote(fields), args, __CALLER__)
      end

      defmacrop unquote(name)(record, args) do
        Record.__access__(unquote(tag), unquote(fields), record, args, __CALLER__)
      end
    end
  end

  # Normalizes of record fields to have default values.
  @doc false
  def __fields__(type, fields) do
    normalizer_fun = fn
      {key, value} when is_atom(key) ->
        try do
          Macro.escape(value)
        rescue
          e in [ArgumentError] ->
            raise ArgumentError, "invalid value for record field #{key}, " <> Exception.message(e)
        else
          value -> {key, value}
        end

      key when is_atom(key) ->
        {key, nil}

      other ->
        raise ArgumentError, "#{type} fields must be atoms, got: #{inspect(other)}"
    end

    :lists.map(normalizer_fun, fields)
  end

  # Callback invoked from record/0 and record/1 macros.
  @doc false
  def __access__(tag, fields, args, caller) do
    cond do
      is_atom(args) ->
        index(tag, fields, args)

      Keyword.keyword?(args) ->
        create(tag, fields, args, caller)

      true ->
        fields = Macro.escape(fields)

        case Macro.expand(args, caller) do
          {:{}, _, [^tag | list]} when length(list) == length(fields) ->
            record = List.to_tuple([tag | list])
            Record.__keyword__(tag, fields, record)

          {^tag, arg} when length(fields) == 1 ->
            Record.__keyword__(tag, fields, {tag, arg})

          _ ->
            quote(do: Record.__keyword__(unquote(tag), unquote(fields), unquote(args)))
        end
    end
  end

  # Callback invoked from the record/2 macro.
  @doc false
  def __access__(tag, fields, record, args, caller) do
    cond do
      is_atom(args) ->
        get(tag, fields, record, args)

      Keyword.keyword?(args) ->
        update(tag, fields, record, args, caller)

      true ->
        raise ArgumentError,
              "expected arguments to be a compile time atom or a keyword list, got: " <>
                Macro.to_string(args)
    end
  end

  # Gets the index of field.
  defp index(tag, fields, field) do
    find_index(fields, field, 1) ||
      raise ArgumentError, "record #{inspect(tag)} does not have the key: #{inspect(field)}"
  end

  # Creates a new record with the given default fields and keyword values.
  defp create(tag, fields, keyword, caller) do
    # Using {} here is safe, since it's not valid AST
    default = if Macro.Env.in_match?(caller), do: {:_, [], nil}, else: {}
    {default, keyword} = Keyword.pop(keyword, :_, default)

    {elements, remaining} =
      Enum.map_reduce(fields, keyword, fn {key, field_default}, remaining ->
        case Keyword.pop(remaining, key, default) do
          {{}, remaining} -> {Macro.escape(field_default), remaining}
          {default, remaining} -> {default, remaining}
        end
      end)

    case remaining do
      [] ->
        quote(do: {unquote(tag), unquote_splicing(elements)})

      [{key, _} | _] ->
        raise ArgumentError, "record #{inspect(tag)} does not have the key: #{inspect(key)}"
    end
  end

  # Updates a record given by var with the given keyword.
  defp update(tag, fields, var, keyword, caller) do
    if Macro.Env.in_match?(caller) do
      raise ArgumentError, "cannot invoke update style macro inside match"
    end

    if Keyword.has_key?(keyword, :_) do
      message = "updating a record with a default (:_) is equivalent to creating a new record"
      IO.warn(message, Macro.Env.stacktrace(caller))
      create(tag, fields, keyword, caller)
    else
      case build_updates(keyword, fields, [], [], []) do
        {updates, [], []} ->
          build_update(updates, var)

        {updates, vars, exprs} ->
          quote do
            {unquote_splicing(:lists.reverse(vars))} = {unquote_splicing(:lists.reverse(exprs))}
            unquote(build_update(updates, var))
          end

        {:error, key} ->
          raise ArgumentError, "record #{inspect(tag)} does not have the key: #{inspect(key)}"
      end
    end
  end

  defp build_update(updates, initial) do
    updates
    |> Enum.sort(fn {left, _}, {right, _} -> right <= left end)
    |> Enum.reduce(initial, fn {key, value}, acc ->
      quote(do: :erlang.setelement(unquote(key), unquote(acc), unquote(value)))
    end)
  end

  defp build_updates([{key, value} | rest], fields, updates, vars, exprs) do
    if index = find_index(fields, key, 2) do
      if simple_argument?(value) do
        build_updates(rest, fields, [{index, value} | updates], vars, exprs)
      else
        var = Macro.var(key, __MODULE__)
        build_updates(rest, fields, [{index, var} | updates], [var | vars], [value | exprs])
      end
    else
      {:error, key}
    end
  end

  defp build_updates([], _fields, updates, vars, exprs), do: {updates, vars, exprs}

  defp simple_argument?({name, _, ctx}) when is_atom(name) and is_atom(ctx), do: true
  defp simple_argument?(other), do: Macro.quoted_literal?(other)

  # Gets a record key from the given var.
  defp get(tag, fields, var, key) do
    index =
      find_index(fields, key, 2) ||
        raise ArgumentError, "record #{inspect(tag)} does not have the key: #{inspect(key)}"

    quote do
      :erlang.element(unquote(index), unquote(var))
    end
  end

  defp find_index([{k, _} | _], k, i), do: i
  defp find_index([{_, _} | t], k, i), do: find_index(t, k, i + 1)
  defp find_index([], _k, _i), do: nil

  # Returns a keyword list of the record
  @doc false
  def __keyword__(tag, fields, record) do
    if is_record(record, tag) do
      [_tag | values] = Tuple.to_list(record)

      case join_keyword(fields, values, []) do
        kv when is_list(kv) ->
          kv

        expected_fields ->
          raise ArgumentError,
                "expected argument to be a #{inspect(tag)} record with " <>
                  "#{expected_fields} fields, got: " <> inspect(record)
      end
    else
      raise ArgumentError,
            "expected argument to be a literal atom, literal keyword or " <>
              "a #{inspect(tag)} record, got runtime: " <> inspect(record)
    end
  end

  # Returns a keyword list, or expected number of fields on size mismatch
  defp join_keyword([{field, _default} | fields], [value | values], acc),
    do: join_keyword(fields, values, [{field, value} | acc])

  defp join_keyword([], [], acc), do: :lists.reverse(acc)
  defp join_keyword(rest_fields, _rest_values, acc), do: length(acc) + length(rest_fields)
end
