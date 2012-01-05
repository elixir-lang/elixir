require ::Elixir::Macros, except: [stringify: 1, inspect: 1]

defprotocol String::Inspect, [stringify(thing), inspect(thing)],
  only: [BitString, Tuple, Atom, Number, List]

defimpl String::Inspect, for: Atom do
  def inspect(false), do: "false"
  def inspect(true),  do: "true"
  def inspect(nil),   do: "nil"

  def inspect(atom) do
    list = atom_to_list(atom)

    if valid_identifier?(list) == [] do
      << ?:, atom_to_binary(atom, :utf8) | :binary >>
    elsif: valid_const_identifier?(list) == []
      atom_to_binary(atom, :utf8)
    else:
      list_to_binary [?:, String.escape(list, ?")]
    end
  end

  def stringify(nil) do
    ""
  end

  def stringify(atom) do
    atom_to_binary(atom, :utf8)
  end

  # Detect if atom is a module reference (::Foo::Bar::Baz)

  defp valid_const_identifier?([?:,?:,h|t]) when
      (h >= ?A andalso h <= ?Z) do
    valid_const_identifier? valid_identifier?(t)
  end

  defp valid_const_identifier?(else), do: else

  # Detect if atom is :letter_or_underscore

  defp valid_identifier?([h|t]) when
    (h >= ?a andalso h <= ?z) orelse
    (h >= ?A andalso h <= ?Z) orelse (h == ?_) do
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

  def stringify(thing) when is_binary(thing) do
    thing
  end

  def stringify(thing) do
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
  def inspect(thing) do
    stringify(thing)
  end

  def stringify([]), do: "''"

  def stringify(thing) do
    if Erlang.io_lib.printable_list(thing) do
      list_to_binary String.escape(thing, ?')
    else:
      container_join(thing, "[", "]")
    end
  end

  ## Helpers

  def container_join([h], acc, last) do
    acc = << acc | :binary, String::Inspect.inspect(h) | :binary, last | :binary >>
  end

  def container_join([h|t], acc, last) do
    acc = << acc | :binary, String::Inspect.inspect(h) | :binary, ', ' >>
    container_join(t, acc, last)
  end

  def container_join([], acc, last) do
    << acc | :binary, last | :binary >>
  end
end

defimpl String::Inspect, for: Tuple do
  def inspect(thing), do: stringify(thing)

  def stringify(thing) do
    String::Inspect::List.container_join(tuple_to_list(thing), "{", "}")
  end
end

defimpl String::Inspect, for: Number do
  def inspect(thing), do: stringify(thing)

  def stringify(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def stringify(thing) do
    list_to_binary float_to_list(thing)
  end
end

defimpl String::Inspect, for: Any do
  def inspect(thing), do: stringify(thing)

  def stringify(thing) do
    list_to_binary Erlang.io_lib.format('~p', [thing])
  end
end