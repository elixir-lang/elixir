import Kernel, except: [access: 2]

defprotocol Access do
  @moduledoc """
  The Access protocol is the underlying protocol invoked
  when the brackets syntax is used. For instance, `foo[bar]`
  is translated to `access foo, bar` which, by default,
  invokes the `Access.access` protocol.

  This protocol is limited and is implemented only for the
  following built-in types: keywords, records and functions.
  """

  @doc """
  Receives the element being accessed and the access item.
  """
  def access(container, key)
end

defimpl Access, for: List do
  @doc """
  Access the given key in a tuple list.
  The key is found via the `===` operator.

  ## Examples

      iex> keywords = [a: 1, b: 2]
      ...> keywords[:a]
      1

      iex> star_ratings = [{1.0, "★"}, {1.5, "★☆"}, {2.0, "★★"}]
      ...> star_ratings[1.5]
      "★☆"

  """
  def access(dict, key)
  def access([{ key, value }|_], key), do: value
  def access([{ _, _ }|t], key), do: access(t, key)
  def access([], _key), do: nil
end

defimpl Access, for: Atom do
  @doc """
  The access protocol can only be accessed by atoms
  at compilation time. If we reach this, we should raise
  an exception.
  """
  def access(nil, _) do
    nil
  end

  def access(atom, _) do
    raise "The access protocol can only be invoked for atoms at " <>
      "compilation time, tried to invoke it for #{inspect atom}"
  end
end
