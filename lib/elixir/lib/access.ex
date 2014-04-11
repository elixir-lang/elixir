import Kernel, except: [access: 2]

defprotocol Access do
  @moduledoc """
  The Access protocol is the underlying protocol invoked
  when the brackets syntax is used. For instance, `foo[bar]`
  is translated to `access foo, bar` which, by default,
  invokes the `Access.access` protocol.

  This protocol is implemented by default for Lists, Maps
  and dictionary like types:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:a]
      1

      iex> map = %{ a: 1, b: 2 }
      iex> map[:a]
      1

      iex> star_ratings = %{ 1.0 => "★", 1.5 => "★☆", 2.0 => "★★" }
      iex> star_ratings[1.5]
      "★☆"

  The key access must be implemented using the `===` operator.
  This protocol is limited and is implemented only for the
  following built-in types: keywords, records and functions.
  """

  @doc """
  Receives the element being accessed and the access item.
  """
  def access(container, key)
end

defimpl Access, for: List do
  def access(dict, key)
  def access([{ key, value }|_], key), do: value
  def access([{ _, _ }|t], key), do: access(t, key)
  def access([], _key), do: nil
end

defimpl Access, for: Map do
  def access(map, key) do
    case :maps.find(key, map) do
      { :ok, value } -> value
      :error -> nil
    end
  end
end

defimpl Access, for: Atom do
  def access(nil, _) do
    nil
  end

  def access(atom, _) do
    raise "The access protocol can only be invoked for atoms at " <>
      "compilation time, tried to invoke it for #{inspect atom}"
  end
end
