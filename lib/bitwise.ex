# This namespace provide macros for bitwise operators
# provided by Erlang. Those can be used in guards.

defmodule Bitwise do
  defmacro bnot(expr) do
    quote do: __OP__ :bnot, unquote(expr)
  end

  defmacro band(left, right) do
    quote do: __OP__ :band, unquote(left), unquote(right)
  end

  defmacro bor(left, right) do
    quote do: __OP__ :bor, unquote(left), unquote(right)
  end

  defmacro bxor(left, right) do
    quote do: __OP__ :bxor, unquote(left), unquote(right)
  end

  defmacro bsl(left, right) do
    quote do: __OP__ :bsl, unquote(left), unquote(right)
  end

  defmacro bsr(left, right) do
    quote do: __OP__ :bsr, unquote(left), unquote(right)
  end
end