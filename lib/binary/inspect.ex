import Elixir.Builtin, except: [inspect: 1]

defprotocol Binary.Inspect, [inspect(thing)],
  only: [BitString, List, Record, Tuple, Atom, Number, Any]

defimpl Binary.Inspect, for: Atom do
  def inspect(false), do: "false"
  def inspect(true),  do: "true"
  def inspect(nil),   do: "nil"
  def inspect(:""),   do: ":\"\""

  def inspect(atom) do
    list = atom_to_list(atom)

    if valid_identifier?(list) == [] do
      ":" <> atom_to_binary(atom, :utf8)
    elsif: valid_ref_identifier?(list) == []
      '__MAIN__.' ++ rest = list
      list_to_binary rest
    else:
      list_to_binary [?:, CharList.escape(list, ?")]
    end
  end

  # Detect if atom is a module reference (__MAIN__.Foo.Bar.Baz)

  defp valid_ref_identifier?('__MAIN__' ++ rest) do
    valid_ref_piece?(rest)
  end

  defp valid_ref_identifier?(rest) do
    rest
  end

  defp valid_ref_piece?([?.,h|t]) when h >= ?A and h <= ?Z do
    valid_ref_piece? valid_identifier?(t)
  end

  defp valid_ref_piece?(else), do: else

  # Detect if atom is :letter_or_underscore

  defp valid_identifier?([h|t])  \
    when h >= ?a and h <= ?z \
    when h >= ?A and h <= ?Z \
    when h == ?_ do
    valid_identifier? t
  end

  defp valid_identifier?(else), do: else
end

defimpl Binary.Inspect, for: BitString do
  def inspect(thing) when is_binary(thing) do
    list = binary_to_list(thing)
    if Erlang.io_lib.printable_list(list) do
      list_to_binary CharList.escape(list, ?")
    else:
      as_bitstring(thing)
    end
  end

  def inspect(thing) do
    as_bitstring(thing)
  end

  ## Helpers

  defp as_bitstring(thing) do
    erlang = Erlang.io_lib.format('~p', [thing])
    list_to_binary List.reverse(replace(erlang, []))
  end

  defp replace([?:|t], acc),                do: replace(t, [?||acc])
  defp replace([h|t], acc) when is_list(h), do: replace(t, replace(h, acc))
  defp replace([h|t], acc),                 do: replace(t, [h|acc])
  defp replace([], acc),                    do: acc
end

defimpl Binary.Inspect, for: List do
  def inspect([]), do: "[]"

  def inspect(thing) do
    if Erlang.io_lib.printable_list(thing) do
      list_to_binary CharList.escape(thing, ?')
    else:
      container_join(thing, "[", "]")
    end
  end

  ## Helpers

  def container_join([h], acc, last) do
    acc <> Binary.Inspect.inspect(h) <> last
  end

  def container_join([h|t], acc, last) when is_list(t) do
    acc = acc <> Binary.Inspect.inspect(h) <> ","
    container_join(t, acc, last)
  end

  def container_join([h|t], acc, last) do
    acc <> Binary.Inspect.inspect(h) <> "|" <> Binary.Inspect.inspect(t) <> last
  end

  def container_join([], acc, last) do
    acc <> last
  end
end

defimpl Binary.Inspect, for: Tuple do
  def inspect(exception) when is_exception(exception) do
    [name,_|tail] = tuple_to_list(exception)
    Binary.Inspect.Atom.inspect(name) <>
       Binary.Inspect.List.container_join(tail, "{", "}")
  end

  def inspect(thing) do
    Binary.Inspect.List.container_join(tuple_to_list(thing), "{", "}")
  end
end

defimpl Binary.Inspect, for: Number do
  def inspect(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def inspect(thing) do
    list_to_binary float_to_list(thing)
  end
end

defimpl Binary.Inspect, for: Regex do
  def inspect(thing) do
    "%r" <> Binary.Inspect.inspect(Regex.source(thing)) <> Regex.opts(thing)
  end
end

defimpl Binary.Inspect, for: Any do
  def inspect(thing) do
    iolist_to_binary Erlang.io_lib.format('~p', [thing])
  end
end