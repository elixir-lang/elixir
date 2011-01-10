% Lexer syntax for the Elixir language done with leex
% Copyright (C) 2011 Jose Valim

Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]

Rules.

%% Numbers
{Digit}+\.{Digit}+ : { token, { float, TokenLine, list_to_float(TokenChars) } }.
{Digit}+           : { token, { integer, TokenLine, list_to_integer(TokenChars) } }.

%% Variable names
({LowerCase}|_)({UpperCase}|{LowerCase}|{Digit}|_)* : { token, { var, TokenLine, list_to_atom(TokenChars) } }.

%% Operators
\+    : { token, { '+', TokenLine } }.
-     : { token, { '-', TokenLine } }.
\*    : { token, { '*', TokenLine } }.
/     : { token, { '/', TokenLine } }.
\(    : { token, { '(', TokenLine } }.
\)    : { token, { ')', TokenLine } }.
=     : { token, { '=', TokenLine } }.

%% Skip
{Comment} : skip_token.
{Whitespace}+ : skip_token.

%% Newlines (with comment and whitespace checks)
({Comment}|{Whitespace})*(\n({Comment}|{Whitespace})*)+ : { token, { eol, TokenLine } }.

Erlang code.