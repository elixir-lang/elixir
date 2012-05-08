defmodule Macro do
  @moduledoc """
  This module provides conveniences for working with macros.
  """

  @doc """
  Recursively escapes the given value so it can be inserted
  into a syntax tree. Structures that are valid syntax nodes
  (like atoms, integers, binaries) are represented by themselves.

  ## Examples

      Macro.escape(:foo)
      #=> :foo

      Macro.escape({ :a, :b, :c })
      #=> { :{}, 0, [:a, :b, :c] }
  """
  def escape({ left, right }) do
    { escape(left), escape(right) }
  end

  def escape(tuple) when is_tuple(tuple) do
    { :{}, 0, escape(tuple_to_list(tuple)) }
  end

  def escape(list) when is_list(list) do
    lc item in list, do: escape(item)
  end

  def escape(other), do: other
end