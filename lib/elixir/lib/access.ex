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

  @only [List, Function, Record, Atom]

  @doc """
  Receives the element being accessed and the access item.
  """
  def access(container, key)
end

defimpl Access, for: List do
  @doc """
  Access the given key in a keyword list.

  ## Examples

      keywords = [a: 1, b: 2]
      keywords[:a] #=> 1

  """

  def access(list, atom) when is_atom(atom) do
    Keyword.get(list, atom)
  end

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

defimpl Access, for: Function do
  @doc """
  The Access protocol for functions simply invokes
  the function passing the item as argument. This
  is useful because it allows a function to be
  passed as argument in places a dict would also fit.
  """
  def access(function, item) do
    function.(item)
  end
end
