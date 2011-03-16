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

Parens = (\\.|[^\)])*
BaseParens = \({Parens}\)
InterpolParens = \({Parens}{Interpol}{Parens}\)

Curly = (\\.|[^\}])*
BaseCurly = \{{Curly}\}
InterpolCurly = \{{Curly}{Interpol}{Curly}\}

Brackets = (\\.|[^\]])*
BaseBrackets = \[{Brackets}\]
InterpolBrackets = \[{Brackets}{Interpol}{Brackets}\]

Quoted = (\\.|[^\"])*
BaseQuoted = "{Quoted}"
InterpolQuoted = "{Quoted}{Interpol}{Quoted}"

InterpolGroup = ({InterpolQuoted}|{InterpolCurly}|{InterpolBrackets}|{InterpolParens})
BaseGroup = ({BaseQuoted}|{BaseCurly}|{BaseBrackets}|{BaseParens})

% "

Rules.

%% Numbers
{Digit}+(_{Digit}+)*\.{Digit}+(_{Digit}+)*     : { token, { float, TokenLine, list_to_float(string:join(string:tokens(TokenChars, "_"), "")) } }.
{Digit}+\.{Digit}+                             : { token, { float, TokenLine, list_to_float(TokenChars) } }.
{Digit}+(_{Digit}+)*                           : { token, { integer, TokenLine, list_to_integer(string:join(string:tokens(TokenChars, "_"), "")) } }.
{Digit}+                                       : { token, { integer, TokenLine, list_to_integer(TokenChars) } }.

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
~~.*([^~]|~[^~]|[^\n]~~|~~[^\n]|[^\n]~~[^\n])*~~ : build_heredoc(TokenChars, TokenLine, TokenLen).
{InterpolQuoted} : build_string(interpolated_string, TokenChars, TokenLine, TokenLen, 2).
{BaseQuoted} : build_string(string, TokenChars, TokenLine, TokenLen, 2).

%% Atoms
\'(\+|\-|\*|\/|\<\-|\[\]|${Digit}+) : build_atom(TokenChars, TokenLine, TokenLen). % '
\'@?({UpperCase}|{LowerCase}|_){IdentifierBase}*[?!]? : build_atom(TokenChars, TokenLine, TokenLen). % '
\'{InterpolGroup} : build_separator_atom(interpolated_atom, TokenChars, TokenLine, TokenLen). % '
\'{BaseGroup} : build_separator_atom(atom, TokenChars, TokenLine, TokenLen). % '

%% Constant and identifier names
{UpperCase}({IdentifierBase}|::)*       : build(constant, TokenLine, TokenChars).
({LowerCase}|_){IdentifierBase}*[?!]?\[ : build_bracket_identifier(TokenLine, TokenChars, TokenLen).
({LowerCase}|_){IdentifierBase}*[?!]    : build(punctuated_identifier, TokenLine, TokenChars).
({LowerCase}|_){IdentifierBase}*        : build(identifier, TokenLine, TokenChars).

%% Operators

=:=   : { token, { '=:=', TokenLine } }.
=!=   : { token, { '=!=', TokenLine } }.
!!  	: { token, { '!!', TokenLine } }.
:=  	: { token, { ':=', TokenLine } }.
==  	: { token, { '==', TokenLine } }.
!=    : { token, { '!=', TokenLine } }.
<=    : { token, { '<=', TokenLine } }.
>=    : { token, { '>=', TokenLine } }.
->    : { token, { '->', TokenLine } }.
<-    : { token, { '<-', TokenLine } }.
<<    : { token, { '<<', TokenLine } }.
>>    : { token, { '>>', TokenLine } }.
&&    : { token, { '&&', TokenLine } }.
\|\|  : { token, { '||', TokenLine } }.
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
<     : { token, { '<', TokenLine } }.
>     : { token, { '>', TokenLine } }.
!     : { token, { '!', TokenLine } }.

%% Skip
{Comment} : skip_token.
{Whitespace}+ : skip_token.
\\\n : skip_token.

%% Newlines (with comment and whitespace checks)
({Comment}|{Whitespace})*(\n({Comment}|{Whitespace})*)+ : { token, { eol, TokenLine } }.

Erlang code.

-import(lists, [sublist/2, sublist/3]).
-export([extract_interpolations/3]).

% The following directive is needed for (significantly) faster compilation
% of the generated .erl file by the HiPE compiler. Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

% Generic building block for constants and identifiers.
build(Kind, Line, Chars) ->
  Atom = list_to_atom(Chars),
  case reserved_word(Atom) of
    true ->  { token, {Atom, Line} };
    false -> { token, {Kind, Line, Atom} }
  end.

% Handle heredoc and multiline heredocs
build_heredoc(Chars, Line, Length) ->
  Start = string:str(Chars, "\n"),
  Stop = string:rstr(Chars, "\n"),
  Pushback = extract_heredoc_pushback(Chars, Start),
  Substring = sublist(Chars, Start + 1, Stop - Start),
  case extract_interpolations(true, Substring, []) of
    { error, Message } -> { error, Message };
    { String, [] } -> 
      { token, { interpolated_string, Line, String }, Pushback }
  end.

% Build a bracket identifier.
build_bracket_identifier(Line, Chars, Length) ->
  Atom = list_to_atom(sublist(Chars, Length - 1)),
  Token = case reserved_bracket_word(Atom) of
    true ->  {Atom, Line};
    false -> {bracket_identifier, Line, Atom}
  end,
  { token, Token, "[" }.

% Handle chars.
build_char(Chars, Line) ->
  { token, { integer, Line, lists:last(unescape_chars(true, Chars)) } }.

% Handle strings without interpolation.
build_string(Kind, Chars, Line, Length, Distance) ->
  Interpol = (sublist(atom_to_list(Kind), 12) == "interpolated"),
  case handle_chars(true, Interpol, Chars, Line, Length, Distance) of
    { error, Message } -> { error, Message };
    { String, Pushback } -> 
      { token, { Kind, Line, String }, Pushback }
  end.

% Handle regular expressions.
build_regexp(Kind, Chars, Line, Length) ->
  { Regexp, Options, NewLength } = extract_regexp_options(Chars),
  case handle_chars(false, Kind == interpolated_regexp, Regexp, Line, NewLength, 4) of
    { error, Message } -> { error, Message };
    { String, Pushback } -> 
      { token, { Kind, Line, String, Options }, [] }
  end.

% Handle atoms without separators and without interpolation.
build_atom(Chars, Line, Length) ->
  String = sublist(Chars, 2, Length - 1),
  { token, { atom, Line, list_to_atom(String) } }.

% Handle quoted atoms with or without interpolation.
build_separator_atom(Kind, Chars, Line, Length) ->
  case handle_chars(true, Kind == interpolated_atom, Chars, Line, Length, 3) of
    { error, Message } -> { error, Message };
    { String, Pushback } -> 
      Atom = case Kind of
        atom -> list_to_atom(String);
        _ -> String
      end,
      { token, { Kind, Line, Atom }, Pushback }
  end.

% Helpers to unescape and process chars.

extract_heredoc_pushback(Chars, Start) ->
  FirstLine = sublist(Chars, Start - 1),
  {match, [{0,Limit}|_]} = re:run(FirstLine, "~~[a-zA-Z0-9_]*"),
  sublist(FirstLine, Limit + 1, length(FirstLine)).

extract_regexp_options(Chars) ->
  Separators = [$", $}, $), $]],
  Max = lists:max(lists:map(fun(X) -> string:rchr(Chars, X) end, Separators)),
  erlang:append_element(lists:split(Max, Chars), Max).

handle_chars(Escaping, true, Chars, Line, Length, Distance) ->
  Last = lists:last(Chars),
  Substring = sublist(Chars, Distance, Length - Distance + 1),
  extract_interpolations(Escaping, Substring, Last);

handle_chars(Escaping, false, Chars, Line, Length, Distance) ->
  { unescape_chars(Escaping, sublist(Chars, Distance, Length - Distance)), [] }.

unescape_chars(Escaping, String) -> unescape_chars(Escaping, String, []).
unescape_chars(Escaping, [], Output) -> lists:reverse(Output);

% Do not escape everything, just a few. Used by regular expressions.
unescape_chars(false, [$\\, Escaped|Rest], Output) ->
  Char = case Escaped of
    $f  -> $\f;
    $n  -> $\n;
    $r  -> $\r;
    $t  -> $\t;
    $v  -> $\v;
    _   -> []
  end,

  case Char of
    [] -> unescape_chars(false, Rest, [Escaped, $\\|Output]);
    _  -> unescape_chars(false, Rest, [Char|Output])
  end;

unescape_chars(true, [$\\, Escaped|Rest], Output) ->
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
  unescape_chars(true, Rest, [Char|Output]);

unescape_chars(Escaping, [Char|Rest], Output) ->
  unescape_chars(Escaping, Rest, [Char|Output]).

reserved_word('Erlang')  -> true;
reserved_word('_')       -> true;
reserved_word('end')     -> true;
reserved_word('do')      -> true;
reserved_word('module')  -> true;
reserved_word('object')  -> true;
reserved_word('def')     -> true;
reserved_word('case')    -> true;
reserved_word('match')   -> true;
reserved_word('try')     -> true;
reserved_word('catch')   -> true;
reserved_word('receive') -> true;
reserved_word('after')   -> true;
reserved_word('when')    -> true;
reserved_word('if')      -> true;
reserved_word('elsif')   -> true;
reserved_word('else')    -> true;
reserved_word('unless')  -> true;
reserved_word('then')    -> true;
reserved_word('and')     -> true;
reserved_word('andalso') -> true;
reserved_word('or')      -> true;
reserved_word('orelse')  -> true;
reserved_word('not')     -> true;
reserved_word('for')     -> true;
reserved_word('in')      -> true;
reserved_word('inlist')  -> true;
reserved_word('inbin')   -> true;
reserved_word('div')     -> true;
reserved_word('rem')     -> true;
reserved_word(Else)      -> reserved_bracket_word(Else).

% Keywords that are also valid when followed by brackets.

reserved_bracket_word(true)  -> true;
reserved_bracket_word(false) -> true;
reserved_bracket_word(nil)   -> true;
reserved_bracket_word(_)     -> false.

% Handle string interpolations

extract_interpolations(Escaping, String, Last) ->
  extract_interpolations(Escaping, String, [], [], [], Last).

extract_interpolations(Escaping, [], Buffer, [], Output, []) ->
  { lists:reverse(build_interpol(s, Escaping, Buffer, Output)), [] };

extract_interpolations(Escaping, [Last], Buffer, [], Output, Last) ->
  { lists:reverse(build_interpol(s, Escaping, Buffer, Output)), [] };

extract_interpolations(Escaping, [Last], Buffer, Search, Output, Last) ->
  { error, io_lib:format("unexpected end of string, expected ~ts", [[hd(Search)]]) };

extract_interpolations(Escaping, [$\\, $#, ${|Rest], Buffer, [], Output, Last) ->
  extract_interpolations(Escaping, Rest, [${,$#|Buffer], [], Output, Last);

extract_interpolations(Escaping, [$\\,Char|Rest], Buffer, [], Output, Last) ->
  extract_interpolations(Escaping, Rest, [Char,$\\|Buffer], [], Output, Last);

extract_interpolations(Escaping, [$#, ${|Rest], Buffer, [], Output, Last) ->
  NewOutput = build_interpol(s, Escaping, Buffer, Output),
  extract_interpolations(Escaping, Rest, [], [$}], NewOutput, Last);

extract_interpolations(Escaping, [$}|Rest], Buffer, [$}], Output, Last) ->
  NewOutput = build_interpol(i, Escaping, Buffer, Output),
  extract_interpolations(Escaping, Rest, [], [], NewOutput, Last);

extract_interpolations(Escaping, [Last|Remaining], Buffer, [], Output, Last) ->
  { lists:reverse(build_interpol(s, Escaping, Buffer, Output)), Remaining };

extract_interpolations(Escaping, [Char|Rest], Buffer, [], Output, Last) ->
  extract_interpolations(Escaping, Rest, [Char|Buffer], [], Output, Last);

% Check for available separators "", {}, [] and () inside interpolation

extract_interpolations(Escaping, [$"|Rest], Buffer, [$"|Search], Output, Last) ->
  extract_interpolations(Escaping, Rest, [$"|Buffer], Search, Output, Last);

extract_interpolations(Escaping, [$"|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Escaping, Rest, [$"|Buffer], [$"|Search], Output, Last);

extract_interpolations(Escaping, [${|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Escaping, Rest, [${|Buffer], [$}|Search], Output, Last);

extract_interpolations(Escaping, [$}|Rest], Buffer, [$}|Search], Output, Last) ->
  extract_interpolations(Escaping, Rest, [$}|Buffer], Search, Output, Last);

extract_interpolations(Escaping, [$[|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Escaping, Rest, [$[|Buffer], [$]|Search], Output, Last);

extract_interpolations(Escaping, [$]|Rest], Buffer, [$]|Search], Output, Last) ->
  extract_interpolations(Escaping, Rest, [$]|Buffer], Search, Output, Last);

extract_interpolations(Escaping, [$(|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Escaping, Rest, [$(|Buffer], [$)|Search], Output, Last);

extract_interpolations(Escaping, [$)|Rest], Buffer, [$)|Search], Output, Last) ->
  extract_interpolations(Escaping, Rest, [$)|Buffer], Search, Output, Last);

% Else

extract_interpolations(Escaping, [Char|Rest], Buffer, Search, Output, Last) ->
  extract_interpolations(Escaping, Rest, [Char|Buffer], Search, Output, Last).

build_interpol(Piece, Escaping, [], Output) ->
  Output;

build_interpol(s, Escaping, Buffer, Output) ->
  [{s, unescape_chars(Escaping, lists:reverse(Buffer))}|Output];

build_interpol(i, Escaping, Buffer, Output) ->
  [{i, lists:reverse(Buffer)}|Output].