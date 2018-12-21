defmodule Atom do
  @moduledoc """
  Convenience functions for working with atoms.

  See also `Kernel.is_atom/1`.
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
