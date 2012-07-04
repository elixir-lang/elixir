import Elixir.Builtin, except: [access: 2]

defprotocol Access do
  @moduledoc """
  The Access protocol is the underlying protocol invoked
  when the brackets syntax is used. For instance, `foo[bar]`
  is translated to `access foo, bar` which, by default,
  invokes `Access.access` protocol.

  This protocol is implemented by default for most builtin
  types, like tuples, atoms, functions, etc.
  """

  @only [List, BitString, Record, Tuple, Atom, Function]

  @doc """
  Receives the element being accessed and the access item.
  """
  def access(element, qualifier)
end

defimpl Access, for: Tuple do
  @doc """
  Access the tuple via an integer. Negative indexes
  performs an inverted lookup, for example, -1 can be
  used to retrieve the last item in the tuple. Returns
  nil if an out of bounds access occurs.

  ## Examples

      tuple = { :a, :b, :c }
      tuple[-1] #=> :c

  """
  def access(tuple, integer) when is_integer(integer) and integer > 0 and integer <= tuple_size(tuple) do
    :erlang.element(integer, tuple)
  end

  def access(tuple, integer) when is_integer(integer) and integer < 0 do
    size     = tuple_size(tuple)
    position = integer + size + 1
    if position > size or position < 1,
      do: nil, else: :erlang.element(position, tuple)
  end

  def access(_tuple, integer) when is_integer(integer) do
    nil
  end
end

defimpl Access, for: List do
  @doc """
  Access the list via a predicate.

  If a regular expression, it returns a list with the
  matched contents.

  If an atom, assumes the list is a keywords list and
  access the key in the keywords equals to the given
  atom.

  Notice this protocol does not implement an integer
  lookup. This is intentional since doing an index
  based access on lists is usually undesired.

  ## Examples

      list = 'sample'
      list[%r/a/] #=> 'a'

      keywords = [a: 1, b: 2]
      keywords[:a] #=> 1

  """

  ## Atom

  def access(list, atom) when is_atom(atom) do
    atom_access(list, atom)
  end

  ## Regex

  def access(list, re) when is_regex(re) do
    case Erlang.re.run(list, Regex.re_pattern(re), [{ :capture, :first, :list }]) do
      :nomatch -> nil
      { :match, [result] } -> result
    end
  end

  ## Helpers

  defp atom_access([{k, _}|_], key) when key < k, do: nil
  defp atom_access([{k, _}|d], key) when key > k, do: atom_access(d, key)
  defp atom_access([{_k, value}|_], _key),        do: value
  defp atom_access([], _),                        do: nil
end

defimpl Access, for: BitString do
  @doc """
  Access the binary via a predicate.

  If a regular expression, it returns a binary with the
  matched contents.

  ## Examples

      binary = "abc"
      Binary.access binary, %r(a) #=> "a"

  """

  ## Regex

  def access(binary, re) when is_binary(binary) and is_regex(re) do
    case Erlang.re.run(binary, Regex.re_pattern(re), [{ :capture, :first, :binary }]) do
      :nomatch -> nil
      { :match, [result] } -> result
    end
  end
end

defimpl Access, for: Atom do
  @doc """
  Access the atom via keywords which simply dispatches
  to the function new of the record passing the keywords
  as arguments.

  Notice that the access macro special-cases atoms to
  provide compilation time expansion for faster read
  and write access for records. For more information,
  check `Elxiir.Builtin.access/2`.
  """
  def access(atom, keywords) when is_list(keywords) do
    atom.new(keywords)
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
