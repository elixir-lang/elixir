defmodule File do
  require Erlang.filename, as: FN
  require Erlang.filelib,  as: FL

  def expand_path(path) do
    normalize FN.absname(path)
  end

  def expand_path(path, relative_to) do
    normalize FN.absname(FN.absname(path, relative_to))
  end

  def regular?(filename) do
    Erlang.filelib.is_regular(filename)
  end

  @doc """
  Returns the last component of the `filename` or the file
  name itself if it does not contain any directory separators.

  ## Examples

      File.basename("foo")
      #=> "foo"

      File.basename("foo/bar")
      #=> "bar"

      File.basename("/")
      #=> ""

  """
  def basename(path) do
    FN.basename(path)
  end

  @doc """
  Returns the last component of `filename` with the `extension`
  stripped. This function should be used to remove a specific extension
  which might, or might not, be there.

  ## Examples

      File.basename("~/foo/bar.ex", ".ex")
      #=> "bar"
      File.basename("~/foo/bar.exs", ".ex")
      #=> "bar.ecs"
      File.basename("~/foo/bar.old.ex", ".ex")
      #=> "kalle.old"

  """
  def basename(path, extension) do
    FN.basename(path, extension)
  end

  @doc """
  Returns a string with one or more paths components joint by the path separator.
  This function should be used to convert a list of strings in a path.

  ## Examples

      File.join(["~", "foo"])
      #=> "~/foo"
      File.join(["foo"])
      #=> "foo"
      File.join(["/", "foo", "bar"])
      #=> "/foo/bar"
  """
  def join(paths) do
    FN.join(paths)
  end

  @doc """
  Returns a list with the path splitted by the path separator. If an empty string
  is given, then it returns the root path.

  ## Examples

     File.split("")
     #=> ["/"]
     File.split("foo")
     #=> ["foo"]
     File.split("/foo/bar")
     #=> ["/", "foo", "bar"]

  """
  def split(path) do
    FN.split(path)
  end

  # Points to Elixir wildcard version that also handles "**".
  def wildcard(path, relative_to // '.') do
    Erlang.elixir_glob.wildcard(path, relative_to)
  end

  ## Helpers

  # Normalize the given path by removing "..".
  defp normalize(path), do: normalize(FN.split(path), [])

  defp normalize([top|t], [_|acc]) when top == ".." or top == '..' do
    normalize t, acc
  end

  defp normalize([top|t], acc) when top == "." or top == '.' do
    normalize t, acc
  end

  defp normalize([h|t], acc) do
    normalize t, [h|acc]
  end

  defp normalize([], acc) do
    FN.join List.reverse(acc)
  end
end
