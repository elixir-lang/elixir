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

  defp do_escape([h|t], char) when
    h == ?#  or h == ?\b or
    h == ?\d or h == ?\e or
    h == ?\f or h == ?\n or
    h == ?\r or h == ?\\ or
    h == ?\t or h == ?\v do
    [?\\,escape_map(h)|do_escape(t, char)]
  end

  defp do_escape([h|t], char) do
    [h|do_escape(t,char)]
  end

  defp do_escape([], char) do
    [char]
  end

  defp escape_map(?#),  do: ?#
  defp escape_map(?\b), do: ?b
  defp escape_map(?\d), do: ?d
  defp escape_map(?\e), do: ?e
  defp escape_map(?\f), do: ?f
  defp escape_map(?\n), do: ?n
  defp escape_map(?\r), do: ?r
  defp escape_map(?\\), do: ?\\
  defp escape_map(?\t), do: ?t
  defp escape_map(?\v), do: ?v
end