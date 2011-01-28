% Implement methods that are VM operations and are not exposed as erlang methods.
-module(elixir_numeric_methods).
-export([add/2, subtract/2, multiply/2, divide/2, integer_div/2, integer_rem/2]).
-include("elixir.hrl").

add(A, B)         -> A + B.
subtract(A, B)    -> A - B.
multiply(A, B)    -> A * B.
divide(A, B)      -> A / B.
integer_div(A, B) -> A div B.
integer_rem(A, B) -> A rem B.