defmodule Bitwise do
  @moduledoc """
  This module provide macros for bitwise operators
  provided by Erlang. These macros can be used in guards.

  The easiest way to use is to simply import them into
  your module:

      import Bitwise

      bnot 1      # -2
      band(1, 1) #=> 1

  """

  @doc """
  Bitwise not.
  """
  defmacro bnot(expr) do
    quote do: __op__ :bnot, unquote(expr)
  end

  @doc """
  Bitwise and.
  """
  defmacro band(left, right) do
    quote do: __op__ :band, unquote(left), unquote(right)
  end

  @doc """
  Bitwise or.
  """
  defmacro bor(left, right) do
    quote do: __op__ :bor, unquote(left), unquote(right)
  end

  @doc """
  Bitwise xor.
  """
  defmacro bxor(left, right) do
    quote do: __op__ :bxor, unquote(left), unquote(right)
  end

  @doc """
  Arithmetic bitshift left.
  """
  defmacro bsl(left, right) do
    quote do: __op__ :bsl, unquote(left), unquote(right)
  end

  @doc """
  Arithmetic bitshift right.
  """
  defmacro bsr(left, right) do
    quote do: __op__ :bsr, unquote(left), unquote(right)
  end
end