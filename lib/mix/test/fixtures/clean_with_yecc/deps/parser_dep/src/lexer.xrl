Definitions.
D = [0-9]
Rules.
{D}+ : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
Erlang code.
