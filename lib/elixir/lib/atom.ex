defmodule Atom do
  @moduledoc """
  Atoms are constants whose value are their own name.

  They are often useful to enumerate over distinct values, such as:

      iex> :apple
      :apple
      iex> :orange
      :orange
      iex> :watermelon
      :watermelon

  Atoms are equal if their names are equal.

      iex> :apple == :apple
      true
      iex> :apple == :orange
      false

  Often they are used to express the state of an operation, by using
  values such as `:ok` and `:error`.

  The booleans `true` and `false` are also atoms:

      iex> true == :true
      true
      iex> is_atom(false)
      true
      iex> is_boolean(:false)
      true

  Elixir allows you to skip the leading `:` for the atoms `false`, `true`,
  and `nil`.

  Atoms must be composed of Unicode characters such as letters, numbers,
  underscore, and `@`. If the keyword has a character that does not
  belong to the category above, such as spaces, you can wrap it in
  quotes:

      iex> :"this is an atom with spaces"
      :"this is an atom with spaces"

  """

  @doc """
  Converts an atom to a string.

  Inlined by the compiler.

  ## Examples

      iex> Atom.to_string(:foo)
      "foo"

  """
  @spec to_string(atom) :: String.t()
  def to_string(atom) do
    :erlang.atom_to_binary(atom, :utf8)
  end

  @doc """
  Converts an atom to a charlist.

  Inlined by the compiler.

  ## Examples

      iex> Atom.to_charlist(:"An atom")
      'An atom'

  """
  @spec to_charlist(atom) :: charlist
  def to_charlist(atom) do
    :erlang.atom_to_list(atom)
  end

  @doc false
  @deprecated "Use Atom.to_charlist/1 instead"
  @spec to_char_list(atom) :: charlist
  def to_char_list(atom), do: Atom.to_charlist(atom)
end
