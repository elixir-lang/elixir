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
  @spec to_string(atom) :: String.t
  def to_string(atom) do
    :erlang.atom_to_binary(atom, :utf8)
  end

  @doc """
  Converts an atom to a char list.

  Inlined by the compiler.

  ## Examples

      iex> Atom.to_char_list(:"An atom")
      'An atom'

  """
  @spec to_char_list(atom) :: char_list
  def to_char_list(atom) do
    :erlang.atom_to_list(atom)
  end
end
