% Lexer syntax for the Elixir language done with leex
% Copyright (C) 2011 Jose Valim

Definitions.

Digit = [0-9]
Whitespace = [\s]

Rules.

%% Numbers
{Digit}+\.{Digit}+ : { token, { float, TokenLine, list_to_float(TokenChars) } }.
{Digit}+           : { token, { integer, TokenLine, list_to_integer(TokenChars) } }.

%% Operators
\+    : { token, { '+', TokenLine } }.
-     : { token, { '-', TokenLine } }.
\*    : { token, { '*', TokenLine } }.
/     : { token, { '/', TokenLine } }.
\(    : { token, { '(', TokenLine } }.
\)    : { token, { ')', TokenLine } }.


%% Skip
{Whitespace}+ : skip_token.

Erlang code.