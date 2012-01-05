defmodule String do
  # Receives a string as a list and escapes all occorrences
  # of char and any string interpolation
  def escape(other, char) do
    escape(other, char, [char])
  end

  ## Helpers

  defp escape([char|t], char, acc) do
    escape(t, char, [char,?\\|acc])
  end

  defp escape([?#|t], char, acc) do
    escape(t, char, [?#,?\\|acc])
  end

  defp escape([h|t], char, acc) do
    escape(t, char, [h|acc])
  end

  defp escape([], char, acc) do
    List.reverse([char|acc])
  end
end