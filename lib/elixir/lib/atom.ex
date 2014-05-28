defmodule Atom do
  @doc """
  Convenience functions for working with atoms.
  """

  @doc """
  Converts an atom to string.

  Inlined by the compiler.
  """
  @spec to_string(atom) :: String.t
  def to_string(atom) do
    :erlang.atom_to_binary(atom, :utf8)
  end

  @doc """
  Converts an atom to a char list.

  Inlined by the compiler.
  """
  @spec to_char_list(atom) :: char_list
  def to_char_list(atom) do
    :erlang.atom_to_list(atom)
  end
end
