defmodule Record do
  @moduledoc """
  Module to work, define and import records.

  Records are simply tuples where the first element is an atom:

      iex> Record.record? { User, "jose", 27 }
      true

  This module provides conveniences for working with records at
  compilation time, where compile-time field names are used to
  manipulate the tuples, providing fast operations on top of
  the tuples compact structure.

  In Elixir, records are used in two situations:

  1. To work with short, internal data. See `Inspect.Algebra`
     implementation for a good example;

  2. To interface with Erlang records;

  The macros `defrecord/3` and `defrecordp/3` can be used to create
  records while `extract/2` can be used to extract records from Erlang
  files.
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
  defmacro extract(name, opts) when is_atom(name) and is_list(opts) do
    Macro.escape Record.Extractor.extract(name, opts)
  end

  @doc """
  Checks if the given `data` is a record of `kind`.

  This is implemented as a macro so it can be used in guard clauses.

  ## Examples

      iex> record = { User, "jose", 27 }
      iex> Record.record?(record, User)
      true

  """
  defmacro record?(data, kind) do
    case __CALLER__.in_guard? do
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

      iex> record = { User, "jose", 27 }
      iex> Record.record?(record)
      true
      iex> tuple = {}
      iex> Record.record?(tuple)
      false

  """
  defmacro record?(data) do
    case __CALLER__.in_guard? do
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

  @doc false
  def defmacros(name, values, env, tag \\ nil) do
    Record.Deprecated.defmacros(name, values, env, tag)
  end

  @doc false
  def deftypes(values, types, env) do
    Record.Deprecated.deftypes(values, types, env)
  end

  @doc false
  def deffunctions(values, env) do
    Record.Deprecated.deffunctions(values, env)
  end

  @doc """
  Defines a set of macros to create and access a record.

  The macros are going to have `name`, a tag (which defaults)
  to the name if none is given, and a set of fields given by
  `kv`.

  ## Examples

      defmodule User do
        Record.defrecord :user, [name: "José", age: "25"]
      end

  In the example above, a set of macros named `user` but with different
  arities will be defined to manipulate the underlying record:

      # To create records
      user()        #=> { :user, "José", 25 }
      user(age: 26) #=> { :user, "José", 26 }

      # To get a field from the record
      user(record, :name) #=> "José"

      # To update the record
      user(record, age: 26) #=> { :user, "José", 26 }

  By default, Elixir uses the record name as the first element of
  the tuple (the tag). But it can be changed to something else:

      defmodule User do
        Record.defrecord :user, User, name: nil
      end

      user() #=> { User, nil }

  """
  defmacro defrecord(name, tag \\ nil, kv) do
    kv = Macro.escape(kv, unquote: true)

    quote bind_quoted: [name: name, tag: tag, kv: kv] do
      tag = tag || name
      kv  = Macro.expand(kv, __ENV__)

      { fields, _types } = Record.Backend.split_fields_and_types(:defrecord, kv)
      fields = Macro.escape(fields)

      defmacro(unquote(name)(args \\ [])) do
        Record.Backend.access(unquote(tag), unquote(fields), args, __CALLER__)
      end

      defmacro(unquote(name)(record, args)) do
        Record.Backend.access(unquote(tag), unquote(fields), record, args, __CALLER__)
      end
    end
  end

  @doc """
  Same as `defrecord/3` but generates private macros.
  """
  defmacro defrecordp(name, tag \\ nil, kv) do
    kv = Macro.escape(kv, unquote: true)

    quote bind_quoted: [name: name, tag: tag, kv: kv] do
      tag = tag || name
      kv  = Macro.expand(kv, __ENV__)

      { fields, _types } = Record.Backend.split_fields_and_types(:defrecordp, kv)
      fields = Macro.escape(fields)

      defmacrop(unquote(name)(args \\ [])) do
        Record.Backend.access(unquote(tag), unquote(fields), args, __CALLER__)
      end

      defmacrop(unquote(name)(record, args)) do
        Record.Backend.access(unquote(tag), unquote(fields), record, args, __CALLER__)
      end
    end
  end
end
