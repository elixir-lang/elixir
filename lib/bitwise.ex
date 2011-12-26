# This namespace provide macros for bitwise operators
# provided by Erlang. Those can be used in guards.

module Bitwise

defmacro bnot(expr),        do: quote(erlang_op :bnot, unquote(expr))
defmacro band(left, right), do: quote(erlang_op :band, unquote(left), unquote(right))
defmacro bor(left, right),  do: quote(erlang_op :bor,  unquote(left), unquote(right))
defmacro bxor(left, right), do: quote(erlang_op :bxor, unquote(left), unquote(right))
defmacro bsl(left, right),  do: quote(erlang_op :bsl,  unquote(left), unquote(right))
defmacro bsr(left, right),  do: quote(erlang_op :bsr,  unquote(left), unquote(right))