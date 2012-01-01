module Inspect do
  defprotocol [inspect(thing), stringify(thing)],
    only: [BitString, Tuple, Atom, Number, List]

  # Handle generating inspect for containers

  def container_join([h], acc, last) do
    acc = bitstr(acc | :binary, Inspect.inspect(h) | :binary, last | :binary)
  end

  def container_join([h|t], acc, last) do
    acc = bitstr(acc | :binary, Inspect.inspect(h) | :binary, ', ')
    container_join(t, acc, last)
  end

  def container_join([], acc, last) do
    bitstr(acc | :binary, last | :binary)
  end

  # Receives a string as a list and escapes all occorrences
  # of char and any string interpolation

  def escape_string(other, char) do
    escape_string(other, char, [char])
  end

  defp escape_string([char|t], char, acc) do
    escape_string(t, char, [char,?\\|acc])
  end

  defp escape_string([?#|t], char, acc) do
    escape_string(t, char, [?#,?\\|acc])
  end

  defp escape_string([h|t], char, acc) do
    escape_string(t, char, [h|acc])
  end

  defp escape_string([], char, acc) do
    List.reverse([char|acc])
  end
end

defimpl Inspect, for: Atom do
  def inspect(atom) do
    list = atom_to_list(atom)

    if valid_lower_identifier?(list) do
      bitstr ?:, atom_to_binary(atom, :utf8) | :binary
    elsif: valid_upper_identifier?(list)
      atom_to_binary(atom, :utf8)
    else:
      list_to_binary [?:, Inspect.escape_string(list, ?")]
    end
  end

  def stringify(atom) do
    atom_to_binary(atom, :utf8)
  end

  # Detect if atom is a module reference (::Foo::Bar::Baz)

  defp valid_upper_identifier?([?:,?:,h|t]) when
      (h >= ?A andalso h <= ?Z) do
    valid_upper_identifier? remove_valid_identifiers(t)
  end

  defp valid_upper_identifier?([]), do: true
  defp valid_upper_identifier?(_),  do: false

  # Detect if atom is :letter_or_underscore

  defp valid_lower_identifier?([h|t]) when
    (h >= ?a andalso h <= ?z) orelse (h == ?_) do
    valid_lower_identifier? remove_valid_identifiers(t)
  end

  defp valid_lower_identifier?([]), do: true
  defp valid_lower_identifier?(_),  do: false

  # Remove all valid identifiers until it reaches an invalid one

  defp remove_valid_identifiers([h|t]) when
      (h >= ?a andalso h <= ?z) orelse
      (h >= ?A andalso h <= ?Z) orelse (h == ?_) do
    remove_valid_identifiers(t)
  end

  defp remove_valid_identifiers(other), do: other
end

defimpl Inspect, for: BitString do
  def inspect(thing) when is_binary(thing) do
    list = binary_to_list(thing)
    if Erlang.io_lib.printable_list(list) do
      list_to_binary Inspect.escape_string(list, ?")
    else:
      list_to_binary Erlang.io_lib.format('~p', [thing])
    end
  end

  def inspect(thing) do
    list_to_binary Erlang.io_lib.format('~p', [thing])
  end

  def stringify(thing) when is_binary(thing) do
    thing
  end

  def stringify(thing) do
    list_to_binary Erlang.io_lib.format('~p', [thing])
  end
end

defimpl Inspect, for: Tuple do
  def inspect(thing) do
    Inspect.container_join(tuple_to_list(thing), "{", "}")
  end

  def stringify(thing) do
    Inspect.container_join(tuple_to_list(thing), "{", "}")
  end
end

defimpl Inspect, for: List do
  def inspect(thing) do
    if Erlang.io_lib.printable_list(thing) do
      list_to_binary Inspect.escape_string(thing, ?')
    else:
      Inspect.container_join(thing, "[", "]")
    end
  end

  def stringify(thing) do
    Inspect.container_join(thing, "[", "]")
  end
end

defimpl Inspect, for: Number do
  def inspect(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def inspect(thing) do
    list_to_binary float_to_list(thing)
  end

  def stringify(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def stringify(thing) do
    list_to_binary float_to_list(thing)
  end
end

defimpl Inspect, for: Any do
  def inspect(thing) do
    list_to_binary Erlang.io_lib.format('~p', [thing])
  end

  def stringify(thing) do
    list_to_binary Erlang.io_lib.format('~p', [thing])
  end
end