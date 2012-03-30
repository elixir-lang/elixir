import Elixir.Builtin, except: [access: 2]

defprotocol Access, [access(element, qualifier)],
  only: [List, BitString, Record, Tuple, Atom, PID, Function]

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
  def access(list, integer) when is_integer(integer) and integer > 0 do
    integer_access(list, integer - 1)
  end

  def access(list, integer) when is_integer(integer) and integer < 0 do
    integer_access(Erlang.lists.reverse(list), - integer - 1)
  end

  def access(_list, integer) when is_integer(integer) do
    nil
  end

  ## Helpers

  defp integer_access([h|_], 0) do
    h
  end

  defp integer_access([_|t], counter) do
    integer_access t, counter - 1
  end

  defp integer_access([], _) do
    nil
  end
end

defimpl Access, for: BitString do
  ## Integer

  def access(binary, integer) when is_binary(binary) and
      is_integer(integer) and integer > 0 and integer <= size(binary) do
    :binary.at(binary, integer - 1)
  end

  def access(binary, integer) when is_binary(binary) and
      is_integer(integer) and integer < 0 do
    size     = size(binary)
    position = integer + size
    if position >= size or position < 0,
      do: nil, else: :binary.at(binary, position)
  end

  def access(binary, integer) when is_binary(binary) and is_integer(integer) do
    nil
  end

  ## re_pattern

  def access(binary, re) when is_binary(binary) and is_record(re, :re_pattern) do
    case Erlang.re.run(binary, re, [{ :capture, :first, :binary }]) do
    match: :nomatch
      nil
    match: { :match, [result] }
      result
    end
  end
end