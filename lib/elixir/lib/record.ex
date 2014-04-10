defmodule Record do
  @doc """
  Extracts record information from an Erlang file.

  Returns the fields as a list of tuples.

  ## Examples

      iex> Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
      [size: :undefined, type: :undefined, access: :undefined, atime: :undefined,
       mtime: :undefined, ctime: :undefined, mode: :undefined, links: :undefined,
       major_device: :undefined, minor_device: :undefined, inode: :undefined,
       uid: :undefined, gid: :undefined]

  """
  def extract(name, opts) do
    Record.Extractor.extract(name, opts)
  end

  @doc """
  Checks if the given `data` is a record of `kind`.

  This is implemented as a macro so it can be used in guard clauses.

  ## Examples

      iex> Record.record?({ User, "jose", 27 }, User)
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

      iex> Record.record?({ User, "jose", 27 })
      true
      iex> Record.record?(13)
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
end
