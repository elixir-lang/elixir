Definitions.

Add         = (\+|-)

Number      = [0-9]

Rules.

{Add}                    : make_token(add_op,  TokenLine, TokenChars).

{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

endls(Chars) ->
    lists:filter(fun (C) -> C == $\n orelse C == $; end, Chars).

is_reserved("if")      -> true;
is_reserved(_)         -> false.
