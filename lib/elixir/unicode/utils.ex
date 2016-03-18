defmodule String.Utils do
  @moduledoc false

  @doc """
  Receives a list of integer codepoints and compacts them into ranges.

  Used by String.Unicode.
  """
  def to_range(old) do
    new = to_range(Enum.reverse(old), [])
    # Uncomment this to verify compactation results
    # IO.inspect {length(new), length(old)}
    new
  end

  defp to_range([h|t], acc),
    do: to_range(t, h + 1, byte_size(<<h::utf8>>), h, acc)
  defp to_range([], acc),
    do: Enum.reverse(acc)

  defp to_range([h|t], h, size, first, acc) when byte_size(<<h::utf8>>) == size,
    do: to_range(t, h + 1, size, first, acc)
  defp to_range(t, last, size, first, acc),
    do: to_range(t, [{first, last - 1, size} | acc])

  @doc """
  Receives a string of possibly multiple codepoints and
  converts it into a binary.

  Used by String.Unicode.
  """
  def to_binary(""), do: nil
  def to_binary(codepoints) do
    codepoints = :binary.split(codepoints, " ", [:global])
    Enum.reduce codepoints, "", fn(codepoint, acc) ->
      acc <> <<String.to_integer(codepoint, 16)::utf8>>
    end
  end
end
