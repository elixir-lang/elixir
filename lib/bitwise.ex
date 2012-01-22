# This namespace provide macros for bitwise operators
# provided by Erlang. Those can be used in guards.

defmodule Bitwise do
  defmacro bnot(expr),        do: quote { __OP__ :bnot, unquote(expr) }
  defmacro band(left, right), do: quote { __OP__ :band, unquote(left), unquote(right) }
  defmacro bor(left, right),  do: quote { __OP__ :bor,  unquote(left), unquote(right) }
  defmacro bxor(left, right), do: quote { __OP__ :bxor, unquote(left), unquote(right) }
  defmacro bsl(left, right),  do: quote { __OP__ :bsl,  unquote(left), unquote(right) }
  defmacro bsr(left, right),  do: quote { __OP__ :bsr,  unquote(left), unquote(right) }
end