% Lexer syntax for the Elixir language done with leex
% Copyright (C) 2011 Jose Valim
%
% Some bits of this lexer were retrieved from Reia lexer
% Copyright (C)2008-09 Tony Arcieri

Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
IdentifierBase = ({UpperCase}|{LowerCase}|{Digit}|_)
Comment = %.*

Interpol = #{.*}

Parens = (\\\^.|\\.|[^\)])*
BaseParens = \({Parens}\)
InterpolParens = \({Parens}{Interpol}{Parens}\)

Curly = (\\\^.|\\.|[^\}])*
BaseCurly = \{{Curly}\}
InterpolCurly = \{{Curly}{Interpol}{Curly}\}

Brackets = (\\\^.|\\.|[^\]])*
BaseBrackets = \[{Brackets}\]
InterpolBrackets = \[{Brackets}{Interpol}{Brackets}\]

Quoted = (\\\^.|\\.|[^\"])*
BaseQuoted = "{Quoted}"
InterpolQuoted = "{Quoted}{Interpol}{Quoted}"

% "

Rules.

%% Numbers
{Digit}+\.{Digit}+ : { token, { float, TokenLine, list_to_float(TokenChars) } }.
{Digit}+           : { token, { integer, TokenLine, list_to_integer(TokenChars) } }.

%% Char
\$.   : build_char(TokenChars, TokenLine).
\$\\. : build_char(TokenChars, TokenLine).

%% Strings
{InterpolQuoted} : build_interpolated(interpolated_string, TokenChars, TokenLine, TokenLen, 2).
{BaseQuoted} : build_string(string, TokenChars, TokenLine, TokenLen).

%% Atoms
\'({UpperCase}|{LowerCase}|_){IdentifierBase}* : build_atom(TokenChars, TokenLine, TokenLen). % '
\'({InterpolQuoted}|{InterpolCurly}|{InterpolBrackets}|{InterpolParens}) : build_interpolated(interpolated_atom, TokenChars, TokenLine, TokenLen, 3). % '
\'({BaseQuoted}|{BaseCurly}|{BaseBrackets}|{BaseParens}) : build_separator_atom(atom, TokenChars, TokenLine, TokenLen). % '

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

-import(lists, [sublist/3]).

% Generic building block for constants and identifiers.
build(Kind, Line, Chars) ->
  Atom = list_to_atom(Chars),
  case reserved_word(Atom) of
    true ->  { token, {Atom, Line} };
    false -> { token, {Kind, Line, Atom} }
  end.

% Handle chars.
build_char(Chars, Line) ->
  { token, { integer, Line, lists:last(Chars) } }.

% Handle strings without interpolation.
build_string(Kind, Chars, Line, Length) ->
  String = handle_chars(false, Chars, Line, Length, 2),
  { token, { Kind, Line, String } }.

% Handle atoms without separators and without interpolation.
build_atom(Chars, Line, Length) ->
  String = sublist(Chars, 2, Length - 1),
  { token, { atom, Line, list_to_atom(String) } }.

% Handle quoted atoms without interpolation.
build_separator_atom(Kind, Chars, Line, Length) ->
  String = handle_chars(false, Chars, Line, Length, 3),
  { token, { Kind, Line, list_to_atom(String) } }.

% Handle any kind of interpolation piece.
build_interpolated(Kind, Chars, Line, Length, Distance) ->
  String = handle_chars(true, Chars, Line, Length, Distance),
  { token, { Kind, Line, String } }.

% Helpers to unescape and process chars.

handle_chars(Interpol, Chars, Line, Length, Distance) ->
  unescape_chars(Interpol, sublist(Chars, Distance, Length - Distance)).

unescape_chars(Kind, String) -> unescape_chars(Kind, String, []).
unescape_chars(Kind, [], Output) -> lists:reverse(Output);

unescape_chars(true, [$\\, $#|Rest], Output) ->
  unescape_chars(true, Rest, [$#, $\\|Output]);

unescape_chars(Interpol, [$\\, Escaped|Rest], Output) ->
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
  unescape_chars(Interpol, Rest, [Char|Output]);

unescape_chars(Interpol, [Char|Rest], Output) ->
  unescape_chars(Interpol, Rest, [Char|Output]).

reserved_word('Erlang')    -> true;
reserved_word('_')         -> true;
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

% Special operators
reserved_word('div')       -> true;
reserved_word('rem')       -> true;

reserved_word(_)           -> false.