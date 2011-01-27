% Lexer syntax for the Elixir language done with leex
% Copyright (C) 2011 Jose Valim
%
% Some bits of this lexer were retrieved from from Reia lexer
% Copyright (C)2008-09 Tony Arcieri

Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
IdentifierBase = ({UpperCase}|{LowerCase}|{Digit}|_)
Comment = %.*

Quoted = (\\\^.|\\.|[^\"])*
String = "{Quoted}"
InterpolatedString = "{Quoted}#\{.*\}{Quoted}"
% "

Rules.

%% Numbers
{Digit}+\.{Digit}+ : { token, { float, TokenLine, list_to_float(TokenChars) } }.
{Digit}+           : { token, { integer, TokenLine, list_to_integer(TokenChars) } }.

%% Char
\$. : build_char(TokenChars, TokenLine).

%% Strings
{InterpolatedString} : build_string(interpolated_string, TokenChars, TokenLine, TokenLen).
{String} : build_string(string, TokenChars, TokenLine, TokenLen).

%% Atoms
\'({UpperCase}|{LowerCase}|_){IdentifierBase}* : build_atom(TokenChars, TokenLine, TokenLen). % '
\'{String} : build_quoted_atom(TokenChars, TokenLine, TokenLen). % '

%% Constant and identifier names
{UpperCase}({IdentifierBase}|::)*    : build(constant, TokenLine, TokenChars).
({LowerCase}|_){IdentifierBase}*     : build(identifier, TokenLine, TokenChars).
({LowerCase}|_){IdentifierBase}*[?!] : build(punctuated_identifier, TokenLine, TokenChars).

%% Operators
->    : { token, { '->', TokenLine } }.
\|    : { token, { '|', TokenLine } }.
\+    : { token, { '+', TokenLine } }.
-     : { token, { '-', TokenLine } }.
\*    : { token, { '*', TokenLine } }.
/     : { token, { '/', TokenLine } }.
\(    : { token, { '(', TokenLine } }.
\)    : { token, { ')', TokenLine } }.
\[    : { token, { '[', TokenLine } }.
\]    : { token, { ']', TokenLine } }.
\{    : { token, { '{', TokenLine } }.
\}    : { token, { '}', TokenLine } }.
=     : { token, { '=', TokenLine } }.
;     : { token, { ';', TokenLine } }.
\:    : { token, { ':', TokenLine } }.
,     : { token, { ',', TokenLine } }.
\.    : { token, { '.', TokenLine } }.
\:    : { token, { ':', TokenLine } }.
\@    : { token, { '@', TokenLine } }.
<     : { token, { '<', TokenLine } }.

%% Skip
{Comment} : skip_token.
{Whitespace}+ : skip_token.

%% Newlines (with comment and whitespace checks)
({Comment}|{Whitespace})*(\n({Comment}|{Whitespace})*)+ : { token, { eol, TokenLine } }.

Erlang code.

build(Kind, Line, Chars) ->
  Atom = list_to_atom(Chars),
  case reserved_word(Atom) of
    true ->  { token, {Atom, Line} };
    false -> { token, {Kind, Line, Atom} }
  end.

build_string(Kind, Chars, Line, Len) ->
  String = unescape_string(Kind, lists:sublist(Chars, 2, Len - 2)), 
  {token, {Kind, Line, String}}.

unescape_string(Kind, String) -> unescape_string(Kind, String, []).
unescape_string(Kind, [], Output) -> lists:reverse(Output);

unescape_string(interpolated_string, [$\\, $#|Rest], Output) ->
  unescape_string(interpolated_string, Rest, [$#, $\\|Output]);

unescape_string(Kind, [$\\, Escaped|Rest], Output) ->
  Char = case Escaped of
    $b  -> $\b;
    $d  -> $\d;
    $e  -> $\e;
    $f  -> $\f;
    $n  -> $\n;
    $r  -> $\r;
    $s  -> $\s;
    $t  -> $\t;
    $v  -> $\v;
    _   -> Escaped
  end,
  unescape_string(Kind, Rest, [Char|Output]);

unescape_string(Kind, [Char|Rest], Output) ->
  unescape_string(Kind, Rest, [Char|Output]).

build_atom(Chars, Line, Len) ->
  String = lists:sublist(Chars, 2, Len - 1),
  { token, { atom, Line, list_to_atom(String) } }.

build_quoted_atom(Chars, Line, Len) ->
  String = lists:sublist(Chars, 2, Len - 2),
  build_atom(String, Line, Len - 2).

build_char(Chars, Line) ->
  { token, { integer, Line, lists:nth(2, Chars) } }.

reserved_word('Erlang')    -> true;
reserved_word('end')       -> true;
reserved_word('do')        -> true;
reserved_word('module')    -> true;
reserved_word('object')    -> true;
reserved_word('def')       -> true;
% reserved_word('nil')     -> true;
% reserved_word('true')    -> true;
% reserved_word('false')   -> true;
% reserved_word('class')   -> true;
% reserved_word('begin')   -> true;
% reserved_word('case')    -> true;
% reserved_word('receive') -> true;
% reserved_word('after')   -> true;
% reserved_word('when')    -> true;
% reserved_word('if')      -> true;
% reserved_word('elseif')  -> true;
% reserved_word('else')    -> true;
% reserved_word('unless')  -> true;
% reserved_word('and')     -> true;
% reserved_word('or')      -> true;
% reserved_word('not')     -> true;
% reserved_word('for')     -> true;
% reserved_word('in')      -> true;
% reserved_word('try')     -> true;
% reserved_word('catch')   -> true;
% reserved_word('throw')   -> true;
reserved_word(_)           -> false.