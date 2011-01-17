% Implement methods that are VM operations and are not exposed as erlang methods.
-module(elixir_numeric_methods).
-export([add/2, subtract/2, multiply/2, divide/2, unary_plus/1, unary_minus/1]).
-include("elixir.hrl").

add(A, B)      -> A + B.
subtract(A, B) -> A - B.
multiply(A, B) -> A * B.
divide(A, B)   -> A / B.
unary_plus(A)  -> + A.
unary_minus(A) -> - A.