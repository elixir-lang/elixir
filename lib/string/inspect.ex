import ::Elixir::Builtin, except: [inspect: 1]

defprotocol String::Inspect, [inspect(thing)],
  only: [BitString, List, Record, Tuple, Atom, Number]

defimpl String::Inspect, for: Atom do
  def inspect(false), do: "false"
  def inspect(true),  do: "true"
  def inspect(nil),   do: "nil"
  def inspect(:""),   do: ":\"\""

  def inspect(atom) do
    list = atom_to_list(atom)

    if valid_identifier?(list) == [] do
      ":" <> atom_to_binary(atom, :utf8)
    elsif: valid_const_identifier?(list) == []
      atom_to_binary(atom, :utf8)
    else:
      list_to_binary [?:, String.escape(list, ?")]
    end
  end

  # Detect if atom is a module reference (::Foo::Bar::Baz)

  defp valid_const_identifier?([?:,?:,h|t]) when h >= ?A andalso h <= ?Z do
    valid_const_identifier? valid_identifier?(t)
  end

  defp valid_const_identifier?(else), do: else

  # Detect if atom is :letter_or_underscore

  defp valid_identifier?([h|t])  \
    when h >= ?a andalso h <= ?z \
    when h >= ?A andalso h <= ?Z \
    when h == ?_ do
    valid_identifier? t
  end

  defp valid_identifier?(else), do: else
end

defimpl String::Inspect, for: BitString do
  def inspect(thing) when is_binary(thing) do
    list = binary_to_list(thing)
    if Erlang.io_lib.printable_list(list) do
      list_to_binary String.escape(list, ?")
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

defimpl String::Inspect, for: List do
  def inspect([]), do: "[]"

  def inspect(thing) do
    if Erlang.io_lib.printable_list(thing) do
      list_to_binary String.escape(thing, ?')
    else:
      container_join(thing, "[", "]")
    end
  end

  ## Helpers

  def container_join([h], acc, last) do
    acc <> String::Inspect.inspect(h) <> last
  end

  def container_join([h|t], acc, last) when is_list(t) do
    acc = acc <> String::Inspect.inspect(h) <> ","
    container_join(t, acc, last)
  end

  def container_join([h|t], acc, last) do
    acc <> String::Inspect.inspect(h) <> "|" <> String::Inspect.inspect(t) <> last
  end

  def container_join([], acc, last) do
    acc <> last
  end
end

defimpl String::Inspect, for: Tuple do
  def inspect(exception) when is_exception(exception) do
    [name,_|tail] = tuple_to_list(exception)
    atom_to_binary(name, :utf8) <>
       String::Inspect::List.container_join(tail, "{", "}")
  end

  def inspect(thing) do
    String::Inspect::List.container_join(tuple_to_list(thing), "{", "}")
  end
end

defimpl String::Inspect, for: Number do
  def inspect(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def inspect(thing) do
    list_to_binary float_to_list(thing)
  end
end

defimpl String::Inspect, for: Any do
  def inspect(thing) do
    iolist_to_binary Erlang.io_lib.format('~p', [thing])
  end
end