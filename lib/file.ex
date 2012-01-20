defmodule File do
  refer Erlang.filename, as: FN
  refer Erlang.filelib,  as: FL

  def expand_path(path) do
    normalize FN.absname(path)
  end

  def expand_path(path, relative_to) do
    normalize FN.absname(FN.absname(path, relative_to))
  end

  def regular?(filename) do
    Erlang.filelib.is_regular(filename)
  end

  # Points to Elixir wildcard version that also handles "**".
  def wildcard(path, relative_to // '.') do
    Erlang.elixir_glob.wildcard(path, relative_to)
  end

  ## Helpers
  @visibility :private

  # Normalize the given path by removing "..".
  def normalize(path), do: normalize(FN.split(path), [])

  def normalize([top|t], [_|acc]) when top == ".." | top == '..' do
    normalize t, acc
  end

  def normalize([h|t], acc) do
    normalize t, [h|acc]
  end

  def normalize([], acc) do
    FN.join List.reverse(acc)
  end
end