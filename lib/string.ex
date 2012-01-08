defmodule String do
  # Receives a string as a list and escapes all occorrences
  # of char and any string interpolation
  def escape(other, char) do
    [char|do_escape(other, char)]
  end

  ## Helpers

  defp do_escape([char|t], char) do
    [?\\,char|do_escape(t, char)]
  end

  defp do_escape([?#|t], char) do
    [?\\,?#|do_escape(t, char)]
  end

  defp do_escape([h|t], char) do
    [h|do_escape(t,char)]
  end

  defp do_escape([], char) do
    [char]
  end
end