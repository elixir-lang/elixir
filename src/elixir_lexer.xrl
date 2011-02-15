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

InterpolGroup = ({InterpolQuoted}|{InterpolCurly}|{InterpolBrackets}|{InterpolParens})
BaseGroup = ({BaseQuoted}|{BaseCurly}|{BaseBrackets}|{BaseParens})
% "

Rules.

%% Numbers
{Digit}+\.{Digit}+ : { token, { float, TokenLine, list_to_float(TokenChars) } }.
{Digit}+           : { token, { integer, TokenLine, list_to_integer(TokenChars) } }.

%% __FILE__ and __LINE__
__FILE__ : { token, { filename, TokenLine } }.
__LINE__ : { token, { integer, TokenLine, TokenLine } }.

%% Char
\${InterpolGroup} : build_string(interpolated_char_list, TokenChars, TokenLine, TokenLen, 3).
\${BaseGroup} : build_string(char_list, TokenChars, TokenLine, TokenLen, 3).
\$.   : build_char(TokenChars, TokenLine).
\$\\. : build_char(TokenChars, TokenLine).

%% Sigils
~Q{InterpolGroup} : build_string(interpolated_string, TokenChars, TokenLine, TokenLen, 4).
~Q{BaseGroup} : build_string(string, TokenChars, TokenLine, TokenLen, 4).
~q{BaseGroup} : build_string(string, TokenChars, TokenLine, TokenLen, 4).

~L{InterpolGroup} : build_string(interpolated_char_list, TokenChars, TokenLine, TokenLen, 4).
~L{BaseGroup} : build_string(char_list, TokenChars, TokenLine, TokenLen, 4).
~l{BaseGroup} : build_string(char_list, TokenChars, TokenLine, TokenLen, 4).

~R{InterpolGroup}{LowerCase}* : build_regexp(interpolated_regexp, TokenChars, TokenLine, TokenLen).
~R{BaseGroup}{LowerCase}* : build_regexp(regexp, TokenChars, TokenLine, TokenLen).
~r{BaseGroup}{LowerCase}* : build_regexp(regexp, TokenChars, TokenLine, TokenLen).

%% Strings
{InterpolQuoted} : build_string(interpolated_string, TokenChars, TokenLine, TokenLen, 2).
{BaseQuoted} : build_string(string, TokenChars, TokenLine, TokenLen, 2).

%% Atoms
\'({UpperCase}|{LowerCase}|_){IdentifierBase}*[?!]? : build_atom(TokenChars, TokenLine, TokenLen). % '
\'{InterpolGroup} : build_separator_atom(interpolated_atom, TokenChars, TokenLine, TokenLen). % '
\'{BaseGroup} : build_separator_atom(atom, TokenChars, TokenLine, TokenLen). % '

%% Constant and identifier names
{UpperCase}({IdentifierBase}|::)*    : build(constant, TokenLine, TokenChars).
({LowerCase}|_){IdentifierBase}*     : build(identifier, TokenLine, TokenChars).
({LowerCase}|_){IdentifierBase}*[?!] : build(punctuated_identifier, TokenLine, TokenChars).

%% Operators

=:=   : { token, { '=:=', TokenLine } }.
=!=   : { token, { '=!=', TokenLine } }.
!!  	: { token, { '!!', TokenLine } }.
==  	: { token, { '==', TokenLine } }.
!=    : { token, { '!=', TokenLine } }.
<=    : { token, { '<=', TokenLine } }.
>=    : { token, { '>=', TokenLine } }.
->    : { token, { '->', TokenLine } }.
<-    : { token, { '<-', TokenLine } }.
<<    : { token, { '<<', TokenLine } }.
>>    : { token, { '>>', TokenLine } }.
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
@     : { token, { '@', TokenLine } }.
~     : { token, { '~', TokenLine } }.
<     : { token, { '<', TokenLine } }.
>     : { token, { '>', TokenLine } }.
!     : { token, { '!', TokenLine } }.

%% Skip
{Comment} : skip_token.
{Whitespace}+ : skip_token.

%% Newlines (with comment and whitespace checks)
({Comment}|{Whitespace})*(\n({Comment}|{Whitespace})*)+ : { token, { eol, TokenLine } }.

Erlang code.

-import(lists, [sublist/3]).
-export([extract_interpolations/2]).

% Generic building block for constants and identifiers.
build(Kind, Line, Chars) ->
  Atom = list_to_atom(Chars),
  case reserved_word(Atom) of
    true ->  { token, {Atom, Line} };
    false -> { token, {Kind, Line, Atom} }
  end.

% Handle chars.
build_char(Chars, Line) ->
  { token, { integer, Line, lists:last(unescape_chars(false, Chars)) } }.

% Handle strings without interpolation.
build_string(Kind, Chars, Line, Length, Distance) ->
  Interpol = (lists:sublist(atom_to_list(Kind), 12) == "interpolated"),
  { String, Pushback } = handle_chars(Interpol, Chars, Line, Length, Distance),
  { token, { Kind, Line, String }, Pushback }.

% Handle regular expressions.
build_regexp(Kind, Chars, Line, Length) ->
  { Regexp, Options, NewLength } = extract_regexp_options(Chars),
  { String, Pushback } = handle_chars(Kind == interpolated_regexp, Regexp, Line, NewLength, 4),
  { token, { Kind, Line, String, Options }, [] }.

% Handle atoms without separators and without interpolation.
build_atom(Chars, Line, Length) ->
  String = sublist(Chars, 2, Length - 1),
  { token, { atom, Line, list_to_atom(String) } }.

% Handle quoted atoms with or without interpolation.
build_separator_atom(Kind, Chars, Line, Length) ->
  { String, Pushback } = handle_chars(Kind == interpolated_atom, Chars, Line, Length, 3),
  Atom = case Kind of
    atom -> list_to_atom(String);
    _ -> String
  end,
  { token, { Kind, Line, Atom }, Pushback }.

% Helpers to unescape and process chars.

extract_regexp_options(Chars) ->
  Separators = [$", $}, $), $]],
  Max = lists:max(lists:map(fun(X) -> string:rchr(Chars, X) end, Separators)),
  erlang:append_element(lists:split(Max, Chars), Max).

handle_chars(true, Chars, Line, Length, Distance) ->
  Last = lists:last(Chars),
  Unescaped = unescape_chars(true, sublist(Chars, Distance, Length - Distance + 1)),
  extract_interpolations(Unescaped, Last);

handle_chars(false, Chars, Line, Length, Distance) ->
  { unescape_chars(false, sublist(Chars, Distance, Length - Distance)), [] }.

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
reserved_word('true')      -> true;
reserved_word('false')     -> true;
reserved_word('case')      -> true;
reserved_word('match')     -> true;
% reserved_word('begin')   -> true;
% reserved_word('receive') -> true;
% reserved_word('after')   -> true;
% reserved_word('when')    -> true;
reserved_word('if')        -> true;
reserved_word('elsif')     -> true;
reserved_word('else')      -> true;
reserved_word('unless')    -> true;
reserved_word('then')      -> true;
reserved_word('and')       -> true;
reserved_word('andalso')   -> true;
reserved_word('or')        -> true;
reserved_word('orelse')    -> true;
reserved_word('not')       -> true;
% reserved_word('for')     -> true;
% reserved_word('in')      -> true;
% reserved_word('try')     -> true;
% reserved_word('catch')   -> true;
% reserved_word('throw')   -> true;

% Special operators
reserved_word('div')       -> true;
reserved_word('rem')       -> true;

reserved_word(_)           -> false.

% Handle string interpolations

extract_interpolations(String, Last) ->
  extract_interpolations(String, [], [], [], Last).

extract_interpolations([Last], Buffer, [], Output, Last) ->
  { lists:reverse(build_interpol(s, Buffer, Output)), [] };

extract_interpolations([Last], Buffer, Search, Output, Last) ->
  elixir_errors:raise(badarg, "unexpected end of string, expected ~ts", [[hd(Search)]]);

extract_interpolations([$\\, $#, ${|Rest], Buffer, [], Output, Last) ->
  extract_interpolations(Rest, [${,$#|Buffer], [], Output, Last);

extract_interpolations([$#, ${|Rest], Buffer, [], Output, Last) ->
  NewOutput = build_interpol(s, Buffer, Output),
  extract_interpolations(Rest, [], [$}], NewOutput, Last);

extract_interpolations([$}|Rest], Buffer, [$}], Output, Last) ->
  NewOutput = build_interpol(i, Buffer, Output),
  extract_interpolations(Rest, [], [], NewOutput, Last);

extract_interpolations([$\\,Char|Rest], Buffer, [], Output, Last) ->
  extract_interpolations(Rest, [Char,$\\|Buffer], [], Output, Last);

extract_interpolations([Last|Remaining], Buffer, [], Output, Last) ->
  { lists:reverse(build_interpol(s, Buffer, Output)), Remaining };

extract_interpolations([Char|Rest], Buffer, [], Output, Last) ->
  extract_interpolations(Rest, [Char|Buffer], [], Output, Last);

% Check for available separators "", {}, [] and () inside interpolation

extract_interpolations([$"|Rest], Buffer, [$"|Search], Output, Last) ->
  extract_interpolations(Rest, [$"|Buffer], Search, Output, Last);

extract_interpolations([$"|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Rest, [$"|Buffer], [$"|Search], Output, Last);

extract_interpolations([${|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Rest, [${|Buffer], [$}|Search], Output, Last);

extract_interpolations([$}|Rest], Buffer, [$}|Search], Output, Last) ->
  extract_interpolations(Rest, [$}|Buffer], Search, Output, Last);

extract_interpolations([$[|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Rest, [$[|Buffer], [$]|Search], Output, Last);

extract_interpolations([$]|Rest], Buffer, [$]|Search], Output, Last) ->
  extract_interpolations(Rest, [$]|Buffer], Search, Output, Last);

extract_interpolations([$(|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Rest, [$(|Buffer], [$)|Search], Output, Last);

extract_interpolations([$)|Rest], Buffer, [$)|Search], Output, Last) ->
  extract_interpolations(Rest, [$)|Buffer], Search, Output, Last);

% Else

extract_interpolations([Char|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Rest, [Char|Buffer], Search, Output, Last).

build_interpol(Piece, [], Output) ->
  Output;

build_interpol(Piece, Buffer, Output) ->
  [{Piece, lists:reverse(Buffer)}|Output].