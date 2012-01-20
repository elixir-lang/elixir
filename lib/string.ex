defmodule String do
  # Receives a string as a list and escapes all occorrences
  # of char and any string interpolation
  def escape(other, char) do
    [char|do_escape(other, char)]
  end

  @visibility :private

  def do_escape([char|t], char) do
    [?\\,char|do_escape(t, char)]
  end

  def do_escape([h|t], char) when
    h == ?#  | h == ?\b | h == ?\d | h == ?\e |
    h == ?\f | h == ?\n | h == ?\r | h == ?\\ |
    h == ?\t | h == ?\v do
    [?\\,escape_map(h)|do_escape(t, char)]
  end

  def do_escape([h|t], char) do
    [h|do_escape(t,char)]
  end

  def do_escape([], char) do
    [char]
  end

  def escape_map(?#),  do: ?#
  def escape_map(?\b), do: ?b
  def escape_map(?\d), do: ?d
  def escape_map(?\e), do: ?e
  def escape_map(?\f), do: ?f
  def escape_map(?\n), do: ?n
  def escape_map(?\r), do: ?r
  def escape_map(?\\), do: ?\\
  def escape_map(?\t), do: ?t
  def escape_map(?\v), do: ?v
end