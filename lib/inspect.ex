module Inspect do
  defprotocol [inspect(thing), stringify(thing)]

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