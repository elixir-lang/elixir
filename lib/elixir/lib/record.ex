defmodule Record do
  @doc """
  Extract record information from an Erlang file.

  Returns the fields as a list of tuples.

  ## Examples

      Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
      #=> [size: :undefined, type: :undefined, access: :undefined, atime: :undefined,
           mtime: :undefined, ctime: :undefined, mode: :undefined, links: :undefined,
           major_device: :undefined, minor_device: :undefined, inode: :undefined,
           uid: :undefined, gid: :undefined]

      defrecord FileInfo, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

  """
  def extract(name, opts) do
    Record.Extractor.extract(name, opts)
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