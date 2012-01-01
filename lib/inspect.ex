module Inspect do
  defprotocol [inspect(thing), stringify(thing)], only: [Atom]

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
      iolist_to_binary [?:, Inspect.escape_string(list, ?")]
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

defimpl Inspect, for: Any do
  def inspect(thing) do
    iolist_to_binary Erlang.io_lib.format('~p', [thing])
  end

  def stringify(thing) do
    iolist_to_binary Erlang.io_lib.format('~p', [thing])
  end
end