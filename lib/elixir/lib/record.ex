defmodule Record do
  @moduledoc """
  Module to work with, define and import records.

  Records are simply tuples where the first element is an atom:

      iex> Record.is_record {User, "john", 27}
      true

  This module provides conveniences for working with records at
  compilation time, where compile-time field names are used to
  manipulate the tuples, providing fast operations on top of
  the tuples' compact structure.

  In Elixir, records are used mostly in two situations:

    1. to work with short, internal data
    2. to interface with Erlang records

  The macros `defrecord/3` and `defrecordp/3` can be used to create
  records while `extract/2` can be used to extract records from Erlang
  files.

  ## Types

  Types can be defined for tuples with the `record/2` macro (only available
  in typespecs). Like with the generated record macros it will expand to
  a tuple.

      defmodule MyModule do
        require Record
        Record.defrecord :user, name: "john", age: 25

        @type user :: record(:user, name: String.t, age: integer)
        # expands to: "@type user :: {:user, String.t, integer}"
      end
  """

  @doc """
  Extracts record information from an Erlang file.

  Returns a quoted expression containing the fields as a list
  of tuples. It expects the record name to be an atom and the
  library path to be a string at expansion time.

  ## Examples

      iex> Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
      [size: :undefined, type: :undefined, access: :undefined, atime: :undefined,
       mtime: :undefined, ctime: :undefined, mode: :undefined, links: :undefined,
       major_device: :undefined, minor_device: :undefined, inode: :undefined,
       uid: :undefined, gid: :undefined]

  """
  def extract(name, opts) when is_atom(name) and is_list(opts) do
    Record.Extractor.extract(name, opts)
  end

  @doc """
  Extracts all records information from an Erlang file.

  Returns a keyword list containing extracted record names as keys, and
  lists of tuples describing the fields as values. It expects a named
  argument :from or :from_lib, which correspond to *include* or
  *include_lib* attribute from Erlang modules, respectively.

  """
  def extract_all(opts) when is_list(opts) do
    Record.Extractor.extract_all(opts)
  end

  @doc """
  Checks if the given `data` is a record of `kind`.

  This is implemented as a macro so it can be used in guard clauses.

  ## Examples

      iex> record = {User, "john", 27}
      iex> Record.is_record(record, User)
      true

  """
  defmacro is_record(data, kind) do
    case Macro.Env.in_guard?(__CALLER__) do
      true ->
        quote do
          is_tuple(unquote(data)) and tuple_size(unquote(data)) > 0
            and :erlang.element(1, unquote(data)) == unquote(kind)
        end
      false ->
        quote do
          result = unquote(data)
          is_tuple(result) and tuple_size(result) > 0
            and :erlang.element(1, result) == unquote(kind)
        end
    end
  end

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
  defmacro is_record(data) do
    case Macro.Env.in_guard?(__CALLER__) do
      true ->
        quote do
          is_tuple(unquote(data)) and tuple_size(unquote(data)) > 0
            and is_atom(:erlang.element(1, unquote(data)))
        end
      false ->
        quote do
          result = unquote(data)
          is_tuple(result) and tuple_size(result) > 0
            and is_atom(:erlang.element(1, result))
        end
    end
  end

  @doc """
  Defines a set of macros to create and access a record.

  The macros are going to have `name`, a tag (which defaults)
  to the name if none is given, and a set of fields given by
  `kv`.

  ## Examples

      defmodule User do
        require Record
        Record.defrecord :user, [name: "meg", age: "25"]
      end

  In the example above, a set of macros named `user` but with different
  arities will be defined to manipulate the underlying record:

      # To create records
      record = user()        #=> {:user, "meg", 25}
      record = user(age: 26) #=> {:user, "meg", 26}

      # To get a field from the record
      user(record, :name) #=> "meg"

      # To update the record
      user(record, age: 26) #=> {:user, "meg", 26}

      # Convert a record to a keyword list
      user(record) #=> [name: "meg", age: 26]

  The generated macros can also be used in order to pattern match on records and
  to bind variables during the match:

      record = user() #=> {:user, "meg", 25}

      user(name: name) = record
      name #=> "meg"

  By default, Elixir uses the record name as the first element of
  the tuple (the tag). But it can be changed to something else:

      defmodule User do
        require Record
        Record.defrecord :user, User, name: nil
      end

      require User
      User.user() #=> {User, nil}

  ## Defining extracted records with anonymous functions

  If a record defines an anonymous function, an `ArgumentError`
  will occur if you attempt to create a record with it.
  This can occur unintentionally when defining a record after extracting
  it from an Erlang library that uses anonymous functions for defaults.

      Record.defrecord :my_rec, Record.extract(...)
      #=> ** (ArgumentError) invalid value for record field fun_field,
      cannot escape #Function<12.90072148/2 in :erl_eval.expr/5>.

  To work around this error, redefine the field with your own &M.f/a function,
  like so:

      defmodule MyRec do
        require Record
        Record.defrecord :my_rec, Record.extract(...) |> Keyword.merge(fun_field: &__MODULE__.foo/2)
        def foo(bar, baz), do: IO.inspect({bar, baz})
      end
  """
  defmacro defrecord(name, tag \\ nil, kv) do
    quote bind_quoted: [name: name, tag: tag, kv: kv] do
      tag = tag || name
      fields = Record.__fields__(:defrecord, kv)

      defmacro(unquote(name)(args \\ [])) do
        Record.__access__(unquote(tag), unquote(fields), args, __CALLER__)
      end

      defmacro(unquote(name)(record, args)) do
        Record.__access__(unquote(tag), unquote(fields), record, args, __CALLER__)
      end
    end
  end

  @doc """
  Same as `defrecord/3` but generates private macros.
  """
  defmacro defrecordp(name, tag \\ nil, kv) do
    quote bind_quoted: [name: name, tag: tag, kv: kv] do
      tag = tag || name
      fields = Record.__fields__(:defrecordp, kv)

      defmacrop(unquote(name)(args \\ [])) do
        Record.__access__(unquote(tag), unquote(fields), args, __CALLER__)
      end

      defmacrop(unquote(name)(record, args)) do
        Record.__access__(unquote(tag), unquote(fields), record, args, __CALLER__)
      end
    end
  end

  # Normalizes of record fields to have default values.
  @doc false
  def __fields__(type, fields) do
    :lists.map(fn
      {key, val} when is_atom(key) ->
        try do
          Macro.escape(val)
        rescue
          e in [ArgumentError] ->
            raise ArgumentError, "invalid value for record field #{key}, " <> Exception.message(e)
        else
          val -> {key, val}
        end
      key when is_atom(key) ->
        {key, nil}
      other ->
        raise ArgumentError, "#{type} fields must be atoms, got: #{inspect other}"
    end, fields)
  end

  # Callback invoked from record/0 and record/1 macros.
  @doc false
  def __access__(atom, fields, args, caller) do
    cond do
      is_atom(args) ->
        index(atom, fields, args)
      Keyword.keyword?(args) ->
        create(atom, fields, args, caller)
      true ->
        case Macro.expand(args, caller) do
          {:{}, _, [^atom|list]} when length(list) == length(fields) ->
            record = List.to_tuple([atom|list])
            Macro.escape(Record.__keyword__(atom, fields, record))
          {^atom, arg} when length(fields) == 1 ->
            Macro.escape(Record.__keyword__(atom, fields, {atom, arg}))
          _ ->
            quote do: Record.__keyword__(unquote(atom), unquote(fields), unquote(args))
        end
    end
  end

  # Callback invoked from the record/2 macro.
  @doc false
  def __access__(atom, fields, record, args, caller) do
    cond do
      is_atom(args) ->
        get(atom, fields, record, args)
      Keyword.keyword?(args) ->
        update(atom, fields, record, args, caller)
      true ->
        msg = "expected arguments to be a compile time atom or keywords, got: #{Macro.to_string args}"
        raise ArgumentError, msg
    end
  end

  # Gets the index of field.
  defp index(atom, fields, field) do
    if index = find_index(fields, field, 0) do
      index - 1 # Convert to Elixir index
    else
      raise ArgumentError, "record #{inspect atom} does not have the key: #{inspect field}"
    end
  end

  # Creates a new record with the given default fields and keyword values.
  defp create(atom, fields, keyword, caller) do
    in_match = Macro.Env.in_match?(caller)

    {match, remaining} =
      Enum.map_reduce(fields, keyword, fn({field, default}, each_keyword) ->
        new_fields =
          case Keyword.has_key?(each_keyword, field) do
            true  -> Keyword.get(each_keyword, field)
            false ->
              case in_match do
                true  -> {:_, [], nil}
                false -> Macro.escape(default)
              end
          end

        {new_fields, Keyword.delete(each_keyword, field)}
      end)

    case remaining do
      [] ->
        {:{}, [], [atom|match]}
      _  ->
        keys = for {key, _} <- remaining, do: key
        raise ArgumentError, "record #{inspect atom} does not have the key: #{inspect hd(keys)}"
    end
  end

  # Updates a record given by var with the given keyword.
  defp update(atom, fields, var, keyword, caller) do
    if Macro.Env.in_match?(caller) do
      raise ArgumentError, "cannot invoke update style macro inside match"
    end

    Enum.reduce keyword, var, fn({key, value}, acc) ->
      index = find_index(fields, key, 0)
      if index do
        quote do
          :erlang.setelement(unquote(index), unquote(acc), unquote(value))
        end
      else
        raise ArgumentError, "record #{inspect atom} does not have the key: #{inspect key}"
      end
    end
  end

  # Gets a record key from the given var.
  defp get(atom, fields, var, key) do
    index = find_index(fields, key, 0)
    if index do
      quote do
        :erlang.element(unquote(index), unquote(var))
      end
    else
      raise ArgumentError, "record #{inspect atom} does not have the key: #{inspect key}"
    end
  end

  defp find_index([{k, _}|_], k, i), do: i + 2
  defp find_index([{_, _}|t], k, i), do: find_index(t, k, i + 1)
  defp find_index([], _k, _i), do: nil

  # Returns a keyword list of the record
  @doc false
  def __keyword__(atom, fields, record) do
    if is_record(record, atom) do
      [_tag|values] = Tuple.to_list(record)
      join_keyword(fields, values, [])
    else
      msg = "expected argument to be a literal atom, literal keyword or a #{inspect atom} record, got runtime: #{inspect record}"
      raise ArgumentError, msg
    end
  end

  defp join_keyword([{field, _default}|fields], [value|values], acc),
    do: join_keyword(fields, values, [{field, value}| acc])
  defp join_keyword([], [], acc),
    do: :lists.reverse(acc)
end
