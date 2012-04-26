import Elixir.Builtin, except: [access: 2]

defprotocol Access do
  @only [List, BitString, Record, Tuple, Atom, Function]
  def access(element, qualifier)
end

defimpl Access, for: Tuple do
  def access(tuple, integer) when is_integer(integer) and integer > 0 and integer <= size(tuple) do
    :erlang.element(integer, tuple)
  end

  def access(tuple, integer) when is_integer(integer) and integer < 0 do
    size     = size(tuple)
    position = integer + size + 1
    if position > size or position < 1,
      do: nil, else: :erlang.element(position, tuple)
  end

  def access(_tuple, integer) when is_integer(integer) do
    nil
  end
end

defimpl Access, for: List do
  ## Atom

  def access(list, atom) when is_atom(atom) do
    atom_access(list, atom)
  end

  ## Regex

  def access(list, re) when is_regex(re) do
    case Erlang.re.run(list, Regex.re_pattern(re), [{ :capture, :first, :list }]) do
    match: :nomatch
      nil
    match: { :match, [result] }
      result
    end
  end

  ## Helpers

  defp atom_access([{k, _}|_], key) when key < k, do: nil
  defp atom_access([{k, _}|d], key) when key > k, do: atom_access(d, key)
  defp atom_access([{_k, value}|_], _key),        do: value
  defp atom_access([], _),                        do: nil
end

defimpl Access, for: BitString do
  ## Regex

  def access(binary, re) when is_binary(binary) and is_regex(re) do
    case Erlang.re.run(binary, Regex.re_pattern(re), [{ :capture, :first, :binary }]) do
    match: :nomatch
      nil
    match: { :match, [result] }
      result
    end
  end
end

defimpl Access, for: Atom do
  @doc """
  An atom access can only be done via keywords. We assume the
  atom represents a record module that implements new and
  receives keywords as argument.
  """
  def access(atom, keywords) when is_list(keywords) do
    atom.new(keywords)
  end
end

defimpl Access, for: Function do
  @doc """
  A function access simply executes it passing the
  the access item as argument.
  """
  def access(function, item) do
    function.(item)
  end
end
