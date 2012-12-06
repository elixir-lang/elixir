-module(elixir_tokenizer).
-include("elixir.hrl").
-export([tokenize/3]).
-import(elixir_interpolation, [unescape_chars/1, unescape_tokens/1]).

-record(scope, {
  file,
  terminators=[],
  check_terminators=true,
  existing_atoms_only=false
}).

-define(is_digit(S), S >= $0 andalso S =< $9).
-define(is_hex(S), ?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f)).
-define(is_bin(S), S >= $0 andalso S =< $1).
-define(is_octal(S), S >= $0 andalso S =< $7).
-define(is_upcase(S), S >= $A andalso S =< $Z).
-define(is_downcase(S), S >= $a andalso S =< $z).
-define(is_word(S), ?is_digit(S) orelse ?is_upcase(S) orelse ?is_downcase(S)).
-define(is_quote(S), S == $" orelse S == $').
-define(is_space(S), S == $\s; S == $\r; S == $\t; S == $\n).

-define(container2(T1, T2),
  T1 == ${, T2 == $};
  T1 == $[, T2 == $]
).

-define(comp3(T1, T2, T3),
  T1 == $=, T2 == $=, T3 == $=;
  T1 == $!, T2 == $=, T3 == $=
).

-define(op3(T1, T2, T3),
  T1 == $<, T2 == $<, T3 == $<;
  T1 == $>, T2 == $>, T3 == $>;
  T1 == $^, T2 == $^, T3 == $^;
  T1 == $~, T2 == $~, T3 == $~;
  T1 == $&, T2 == $&, T3 == $&;
  T1 == $|, T2 == $|, T3 == $|
).

-define(comp2(T1, T2),
  T1 == $=, T2 == $=;
  T1 == $!, T2 == $=;
  T1 == $<, T2 == $=;
  T1 == $>, T2 == $=
).

-define(op2(T1, T2),
  T1 == $&, T2 == $&;
  T1 == $|, T2 == $|;
  T1 == $<, T2 == $>;
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-;
  T1 == $*, T2 == $*;
  T1 == $/, T2 == $/;
  T1 == $:, T2 == $:;
  T1 == $<, T2 == $-;
  T1 == $-, T2 == $>;
  T1 == $., T2 == $.;
  T1 == $/, T2 == $>;
  T1 == $=, T2 == $~
).

-define(comp1(T),
  T == $<;
  T == $>
).

-define(op1(T),
  T == $+;
  T == $-;
  T == $*;
  T == $/;
  T == $=;
  T == $|;
  T == $!;
  T == $^;
  T == $@
).

-define(unary_op(T),
  T == '+';
  T == '-';
  T == '@';
  T == '!';
  T == '^'
).

tokenize(String, Line, Opts) ->
  File = case lists:keyfind(file, 1, Opts) of
    { file, V1 } -> V1;
    false -> <<"nofile">>
  end,

  Existing = case lists:keyfind(existing_atoms_only, 1, Opts) of
    { existing_atoms_only, true } -> true;
    false -> false
  end,

  Check = case lists:keyfind(check_terminators, 1, Opts) of
    { check_terminators, false } -> false;
    false -> true
  end,

  Scope = #scope{
    file=File,
    existing_atoms_only=Existing,
    check_terminators=Check
  },

  tokenize(String, Line, Scope, []).

tokenize([], _Line, #scope{terminators=[]}, Tokens) ->
  { ok, lists:reverse(Tokens) };

tokenize([], EndLine, #scope{terminators=[{ Start, StartLine }|_]}, _Tokens) ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~s (for \"~s\" starting at line ~B)", [End, Start, StartLine]),
  { error, { EndLine, Message, [] } };

% Base integers

tokenize([$0,X,H|T], Line, Scope, Tokens) when (X == $x orelse X == $X), ?is_hex(H) ->
  { Rest, Number } = tokenize_hex([H|T], []),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

tokenize([$0,O,H|T], Line, Scope, Tokens) when (O == $o orelse O == $O), ?is_octal(H) ->
  { Rest, Number } = tokenize_octal([H|T], []),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

tokenize([$0,B,H|T], Line, Scope, Tokens) when (B == $b orelse B == $B), ?is_bin(H) ->
  { Rest, Number } = tokenize_bin([H|T], []),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

% Comments

tokenize([$#|String], Line, Scope, Tokens) ->
  Rest = tokenize_comment(String),
  tokenize(Rest, Line, Scope, Tokens);

% Sigils

tokenize([$%,S,H,H,H|T], Line, #scope{file=File} = Scope, Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, File, ?is_downcase(S), T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(Final, Line, Scope, [{ sigil, Line, S, Parts, Modifiers }|Tokens])
  end;

tokenize([$%,S,H|T], Line, #scope{file=File} = Scope, Tokens) when not(?is_word(H)), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, File, ?is_downcase(S), T, sigil_terminator(H)) of
    { NewLine, Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(Final, NewLine, Scope, [{ sigil, Line, S, Parts, Modifiers }|Tokens]);
    Error ->
      Sigil = [$%,S,H],
      interpolation_error(Error, " (for sigil ~s starting at line ~B)", [Sigil, Line])
  end;

% Char tokens

tokenize([$?,$\\,H|T], Line, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,Char|T], Line, Scope, Tokens) ->
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

% Dot identifier/operators

tokenize("..." ++ Rest, Line, Scope, Tokens) ->
  Token = tokenize_call_identifier(identifier, Line, '...', Rest),
  tokenize(Rest, Line, Scope, [Token|Tokens]);

% ## Containers
tokenize(".<<>>" ++ Rest, Line, Scope, Tokens) ->
  handle_call_identifier(Rest, Line, '<<>>', Scope, Tokens);

tokenize([$.,T1,T2|Rest], Line, Scope, Tokens) when ?container2(T1, T2) ->
  handle_call_identifier(Rest, Line, list_to_atom([T1, T2]), Scope, Tokens);

% ## Three Token Operators
tokenize([$.,T1,T2,T3|Rest], Line, Scope, Tokens) when ?comp3(T1, T2, T3); ?op3(T1, T2, T3) ->
  handle_call_identifier(Rest, Line, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Two Token Operators
tokenize([$.,T1,T2|Rest], Line, Scope, Tokens) when ?comp2(T1, T2); ?op2(T1, T2) ->
  handle_call_identifier(Rest, Line, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators
tokenize([$.,T|Rest], Line, Scope, Tokens) when ?comp1(T); ?op1(T); T == $& ->
  handle_call_identifier(Rest, Line, list_to_atom([T]), Scope, Tokens);

% Dot call

% ## Exception for .( as it needs to be treated specially in the parser
tokenize([$.,$(|Rest], Line, Scope, Tokens) ->
  tokenize([$(|Rest], Line, Scope, add_token_with_nl({ dot_call_op, Line, '.' }, Tokens));

tokenize([$.,H|T], Line, #scope{file=File} = Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, [Part], Rest } when is_binary(Part) ->
      Atom  = unsafe_to_atom(Part, Scope),
      Token = tokenize_call_identifier(identifier, Line, Atom, Rest),
      tokenize(Rest, NewLine, Scope, [Token|add_token_with_nl({ '.', Line }, Tokens)]);
    Error ->
      interpolation_error(Error, " (for function name starting at line ~B)", [Line])
  end;

% Heredocs

tokenize("\"\"\"" ++ T, Line, Scope, Tokens) ->
  handle_heredocs(T, Line, $", Scope, Tokens);

tokenize("'''" ++ T, Line, Scope, Tokens) ->
  handle_heredocs(T, Line, $', Scope, Tokens);

% Strings

tokenize([$"|T], Line, Scope, Tokens) ->
  handle_strings(T, Line, $", Scope, Tokens);
tokenize([$'|T], Line, Scope, Tokens) ->
  handle_strings(T, Line, $', Scope, Tokens);

% Atoms

tokenize([$:,T|String], Line, Scope, Tokens) when ?is_upcase(T); ?is_downcase(T); T == $_ ->
  { Rest, Atom } = tokenize_atom([T|String], [], Scope),
  tokenize(Rest, Line, Scope, [{ atom, Line, Atom }|Tokens]);

tokenize([$:,H|T], Line, #scope{file=File} = Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, Parts, Rest } ->
      Token = case unescape_tokens(Parts) of
        [Part]    -> { atom, Line, unsafe_to_atom(Part, Scope) };
        Unescaped -> { atom, Line, Unescaped }
      end,
      tokenize(Rest, NewLine, Scope, [Token|Tokens]);
    Error ->
      interpolation_error(Error, " (for atom starting at line ~B)", [Line])
  end;

% Atom identifiers/operators

tokenize(":..." ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, '...' }|Tokens]);

% ## Containers
tokenize(":<<>>" ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, '<<>>' }|Tokens]);

tokenize([$:,T1,T2|Rest], Line, Scope, Tokens) when ?container2(T1, T2) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T1,T2]) }|Tokens]);

% ## Three Token Operators
tokenize([$:,T1,T2,T3|Rest], Line, Scope, Tokens) when ?comp3(T1, T2, T3); ?op3(T1, T2, T3)  ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T1,T2,T3]) }|Tokens]);

% ## Two Token Operators
tokenize([$:,T1,T2|Rest], Line, Scope, Tokens) when ?comp2(T1, T2); ?op2(T1, T2) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T1,T2]) }|Tokens]);

% ## Single Token Operators
tokenize([$:,T|Rest], Line, Scope, Tokens) when ?comp1(T); ?op1(T); T == $&; T == $. ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T]) }|Tokens]);

% End of line

tokenize(";" ++ Rest, Line, Scope, []) ->
  tokenize(Rest, Line, Scope, eol(Line, ';', []));

tokenize(";" ++ Rest, Line, Scope, [Top|Tokens]) when element(1, Top) /= eol ->
  tokenize(Rest, Line, Scope, eol(Line, ';', [Top|Tokens]));

tokenize("\\\n" ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line + 1, Scope, Tokens);

tokenize("\\\r\n" ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line + 1, Scope, Tokens);

tokenize("\n" ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line + 1, Scope, eol(Line, newline, Tokens));

tokenize("\r\n" ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line + 1, Scope, eol(Line, newline, Tokens));

% Stand-alone tokens

% ## &
tokenize([$&,H|Rest], Line, Scope, Tokens) when ?is_digit(H) ->
  tokenize(Rest, Line, Scope, [{ '&', Line, [list_to_integer([H])] }|Tokens]);

% ## Comparison three token operators
tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?comp3(T1, T2, T3) ->
  handle_comp_op(Rest, Line, list_to_atom([T1,T2,T3]), Scope, Tokens);

% ## Three token operators
tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?op3(T1, T2, T3) ->
  handle_op(Rest, Line, list_to_atom([T1,T2,T3]), Scope, Tokens);

% ## Containers + punctuation tokens
tokenize([T,T|Rest], Line, Scope, Tokens) when T == $<; T == $> ->
  Token = { list_to_atom([T,T]), Line },
  case handle_terminator(Token, Scope) of
    { error, _ } = Error -> Error;
    New -> tokenize(Rest, Line, New, [Token|Tokens])
  end;

tokenize([T|Rest], Line, Scope, Tokens) when T == $(;
    T == ${; T == $}; T == $[; T == $]; T == $); T == $, ->
  Token = { list_to_atom([T]), Line },
  case handle_terminator(Token, Scope) of
    { error, _ } = Error -> Error;
    New -> tokenize(Rest, Line, New, [Token|Tokens])
  end;

% ## Comparison two token operators
tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?comp2(T1, T2) ->
  handle_comp_op(Rest, Line, list_to_atom([T1, T2]), Scope, Tokens);

% ## Two Token Operators
tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?op2(T1, T2) ->
  handle_op(Rest, Line, list_to_atom([T1, T2]), Scope, Tokens);

% ## Comparison single token operators
tokenize([T|Rest], Line, Scope, Tokens) when ?comp1(T) ->
  handle_comp_op(Rest, Line, list_to_atom([T]), Scope, Tokens);

% ## Single Token Operators
tokenize([T|Rest], Line, Scope, Tokens) when ?op1(T) ->
  handle_op(Rest, Line, list_to_atom([T]), Scope, Tokens);

tokenize([$.|Rest], Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, add_token_with_nl({ '.', Line }, Tokens));

% Integers and floats

tokenize([H|_] = String, Line, Scope, Tokens) when ?is_digit(H) ->
  { Rest, Number } = tokenize_number(String, [], false),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

% Aliases

tokenize([H|_] = String, Line, Scope, Tokens) when ?is_upcase(H) ->
  { Rest, Alias } = tokenize_identifier(String, [], false),
  Atom = unsafe_to_atom(Alias, Scope),
  case Rest of
    [$:|T] when hd(T) /= $: ->
      tokenize(T, Line, Scope, [{ kw_identifier, Line, Atom }|Tokens]);
    _ ->
      tokenize(Rest, Line, Scope, [{ '__aliases__', Line, [Atom] }|Tokens])
  end;

% Identifier

tokenize([H|_] = String, Line, Scope, Tokens) when ?is_downcase(H); H == $_ ->
  { Rest, { Kind, _, Identifier } } = tokenize_any_identifier(String, Line, [], Scope),
  case handle_keyword(Line, Kind, Identifier, Tokens) of
    false ->
      tokenize(Rest, Line, Scope, [{ Kind, Line, Identifier }|Tokens]);
    [Check|T] ->
      case handle_terminator(Check, Scope) of
        { error, _ } = Error -> Error;
        New -> tokenize(Rest, Line, New, [Check|T])
      end
  end;

% Ambiguous unary/binary operators tokens

tokenize([Space, Sign, NotMarker|T], Line, Scope, [{ Identifier, _, _ } = H|Tokens]) when
  Sign == $+ orelse Sign == $-,
  Space == $\s orelse Space == $\t,
  NotMarker /= $\s, NotMarker /= $\t, NotMarker /= $\r,
  NotMarker /= $\n, NotMarker /= $:, NotMarker /= $(,
  NotMarker /= $+, NotMarker /= $-, NotMarker /= $>,
  Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Rest, Line, Scope, [{ list_to_atom([Sign]), Line }, setelement(1, H, op_identifier)|Tokens]);

% Spaces

tokenize([T|Rest], Line, Scope, Tokens) when T == $\s; T == $\r; T == $\t ->
  tokenize(Rest, Line, Scope, Tokens);

tokenize([{line,Line}|Rest], _Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, Tokens);

tokenize(T, Line, _Scope, _Tokens) ->
  { error, { Line, "invalid token: ", until_eol(T) } }.

until_eol("\r\n" ++ _) -> [];
until_eol("\n" ++ _)   -> [];
until_eol([])          -> [];
until_eol([H|T])       -> [H|until_eol(T)].

%% Handlers

handle_heredocs(T, Line, H, #scope{file=File} = Scope, Tokens) ->
  case extract_heredoc_with_interpolation(Line, File, true, T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      Token = { string_type(H), Line, unescape_tokens(Parts) },
      tokenize(Rest, Line, Scope, [Token|Tokens])
  end.

handle_strings(T, Line, H, #scope{file=File} = Scope, Tokens) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, Parts, [$:|Rest] } when hd(Rest) /= $: ->
      case Parts of
        [Bin] when is_binary(Bin) ->
          Atom = unsafe_to_atom(unescape_chars(Bin), Scope),
          tokenize(Rest, NewLine, Scope, [{ kw_identifier, Line, Atom }|Tokens]);
        _ ->
          { error, { Line, "invalid interpolation in key", [$"|T] } }
      end;
    { NewLine, Parts, Rest } ->
      Token = { string_type(H),Line,unescape_tokens(Parts) },
      tokenize(Rest, NewLine, Scope, [Token|Tokens]);
    Error ->
      interpolation_error(Error, " (for string starting at line ~B)", [Line])
  end.

handle_comp_op([$:|Rest], Line, Op, Scope, Tokens) when hd(Rest) /= $: ->
  verify_kw_and_space(Line, Op, Rest, Scope),
  tokenize(Rest, Line, Scope, [{ kw_identifier, Line, Op }|Tokens]);

handle_comp_op(Rest, Line, Op, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, add_token_with_nl({ comp_op, Line, Op }, Tokens)).

handle_op([$:|Rest], Line, Op, Scope, Tokens) when hd(Rest) /= $: ->
  verify_kw_and_space(Line, Op, Rest, Scope),
  tokenize(Rest, Line, Scope, [{ kw_identifier, Line, Op }|Tokens]);

handle_op(Rest, Line, Op, Scope, Tokens) when ?unary_op(Op) ->
  tokenize(Rest, Line, Scope, [{ Op, Line }|Tokens]);

handle_op(Rest, Line, Op, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, add_token_with_nl({ Op, Line }, Tokens)).

handle_call_identifier(Rest, Line, Op, Scope, Tokens) ->
  Token = tokenize_call_identifier(identifier, Line, Op, Rest),
  tokenize(Rest, Line, Scope, [Token|add_token_with_nl({ '.', Line }, Tokens)]).

%% Helpers

eol(_Line, _Mod, [{',',_}|_] = Tokens)   -> Tokens;
eol(_Line, _Mod, [{eol,_,_}|_] = Tokens) -> Tokens;
eol(Line, Mod, Tokens) -> [{eol,Line,Mod}|Tokens].

unsafe_to_atom(Binary, #scope{existing_atoms_only=true}) when is_binary(Binary) ->
  binary_to_existing_atom(Binary, utf8);
unsafe_to_atom(Binary, #scope{}) when is_binary(Binary) ->
  binary_to_atom(Binary, utf8);
unsafe_to_atom(List, #scope{existing_atoms_only=true}) when is_list(List) ->
  list_to_existing_atom(List);
unsafe_to_atom(List, #scope{}) when is_list(List) ->
  list_to_atom(List).

collect_modifiers([H|T], Buffer) when ?is_downcase(H) ->
  collect_modifiers(T, [H|Buffer]);

collect_modifiers(Rest, Buffer) ->
  { Rest, lists:reverse(Buffer) }.

% Extract heredocs

extract_heredoc_with_interpolation(Line, File, Interpol, T, H) ->
  case extract_heredoc(Line, T, H) of
    { error, _ } = Error ->
      Error;
    { Body, Rest } ->
      case elixir_interpolation:extract(Line, File, Interpol, Body, 0) of
        { _, Parts, [] } -> { Parts, Rest };
        Error -> interpolation_error(Error, " (for heredoc starting at line ~B)", [Line])
      end
  end.

extract_heredoc(Line0, Rest0, Marker) ->
  case extract_heredoc_line(Rest0, []) of
    { ok, Extra, Rest1 } ->
      %% We prepend a new line so we can transparently remove
      %% spaces later. This new line is removed by calling `tl`
      %% in the final heredoc body three lines below.
      case extract_heredoc_body(Line0, Marker, [$\n|Rest1], []) of
        { Line1, Body, Rest2, Spaces } ->
          { tl(remove_heredoc_spaces(Body, Spaces)), merge_heredoc_extra(Line1, Extra, Rest2) };
        { error, Line1 } ->
          heredoc_error(Line1, Line0, Marker)
      end;
    { error, eof } ->
      heredoc_error(Line0, Line0, Marker)
  end.

heredoc_error(ErrorLine, StartLine, Marker) ->
  Terminator = [Marker, Marker, Marker],
  Message    = io_lib:format("missing terminator: ~s (for heredoc starting at line ~B)", [Terminator, StartLine]),
  { error, { ErrorLine, Message, [] } }.

%% Remove spaces from heredoc based on the position of the final quotes.

remove_heredoc_spaces(Body, 0) ->
  lists:reverse([0|Body]);

remove_heredoc_spaces(Body, Spaces) ->
  remove_heredoc_spaces([0|Body], [], Spaces, Spaces).

remove_heredoc_spaces([H,$\n|T], [Backtrack|Buffer], Spaces, Original) when Spaces > 0, H == $\s orelse H == $\t ->
  remove_heredoc_spaces([Backtrack,$\n|T], Buffer, Spaces - 1, Original);

remove_heredoc_spaces([$\n=H|T], Buffer, _Spaces, Original) ->
  remove_heredoc_spaces(T, [H|Buffer], Original, Original);

remove_heredoc_spaces([H|T], Buffer, Spaces, Original) ->
  remove_heredoc_spaces(T, [H|Buffer], Spaces, Original);

remove_heredoc_spaces([], Buffer, _Spaces, _Original) ->
  Buffer.

%% Extract heredoc body. It returns the heredoc body (in reverse order),
%% the remaining of the document and the number of spaces the heredoc
%% is aligned.

extract_heredoc_body(Line, Marker, Rest, Buffer) ->
  case extract_heredoc_line(Marker, Rest, Buffer, 0) of
    { ok, NewBuffer, NewRest } ->
      extract_heredoc_body(Line + 1, Marker, NewRest, NewBuffer);
    { ok, NewBuffer, NewRest, Spaces } ->
      { Line + 1, NewBuffer, NewRest, Spaces };
    { error, eof } ->
      { error, Line }
  end.

%% Extract a line from the heredoc prepending its contents to a buffer.

extract_heredoc_line("\r\n" ++ Rest, Buffer) ->
  { ok, [$\n|Buffer], Rest };

extract_heredoc_line("\n" ++ Rest, Buffer) ->
  { ok, [$\n|Buffer], Rest };

extract_heredoc_line([H|T], Buffer) ->
  extract_heredoc_line(T, [H|Buffer]);

extract_heredoc_line(_, _) ->
  { error, eof }.

%% Extract each heredoc line trying to find a match according to the marker.

extract_heredoc_line(Marker, [H|T], Buffer, Counter) when H == $\t; H == $\s ->
  extract_heredoc_line(Marker, T, [H|Buffer], Counter + 1);

extract_heredoc_line(Marker, [Marker,Marker,Marker|T] = Rest, Buffer, Counter) ->
  case next_is_break(T) of
    false -> extract_heredoc_line(Rest, Buffer);
    Final -> { ok, Buffer, Final, Counter }
  end;

extract_heredoc_line(_Marker, Rest, Buffer, _Counter) ->
  extract_heredoc_line(Rest, Buffer).

next_is_break([H|T]) when H == $\t; H == $\s -> next_is_break(T);
next_is_break([$\r,$\n|T]) -> T;
next_is_break([$\n|T]) -> T;
next_is_break([]) -> [];
next_is_break(_) -> false.

%% Merge heredoc extra by replying it on the buffer. It also adds
%% a special { line, Line } token to force a line change along the way.

merge_heredoc_extra(Line, Extra, Buffer) ->
  merge_heredoc_extra(Extra, [{line,Line}|Buffer]).

merge_heredoc_extra([H|T], Buffer) ->
  merge_heredoc_extra(T, [H|Buffer]);

merge_heredoc_extra([], Buffer) ->
  Buffer.

% Integers and floats
% At this point, we are at least sure the first digit is a number.

% Check if we have a point followed by a number;
tokenize_number([$.,H|T], Acc, false) when ?is_digit(H) ->
  tokenize_number(T, [H,$.|Acc], true);

% Check if we have an underscore followed by a number;
tokenize_number([$_,H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

% Check if we have e- followed by numbers. Valid only for floats.
tokenize_number([$e,S,H|T], Acc, true) when ?is_digit(H), S == $+ orelse S == $- ->
  tokenize_number(T, [H,S,$e|Acc], true);

% Check if we have e followed by numbers. Valid only for floats.
tokenize_number([$e,H|T], Acc, true) when ?is_digit(H) ->
  tokenize_number(T, [H,$e|Acc], true);

% Just numbers;
tokenize_number([H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

% Cast to float...
tokenize_number(Rest, Acc, true) ->
  { Rest, list_to_float(lists:reverse(Acc)) };

% Or integer.
tokenize_number(Rest, Acc, false) ->
  { Rest, list_to_integer(lists:reverse(Acc)) }.

% Hex
tokenize_hex([H|T], Acc) when ?is_hex(H) -> tokenize_hex(T, [H|Acc]);
tokenize_hex(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 16) }.

% Octal
tokenize_octal([H|T], Acc) when ?is_octal(H) -> tokenize_octal(T, [H|Acc]);
tokenize_octal(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 8) }.

% Bin
tokenize_bin([H|T], Acc) when ?is_bin(H) -> tokenize_bin(T, [H|Acc]);
tokenize_bin(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 2) }.

% Comments

tokenize_comment("\r\n" ++ _ = Rest) -> Rest;
tokenize_comment("\n" ++ _ = Rest)   -> Rest;
tokenize_comment([_|Rest])           -> tokenize_comment(Rest);
tokenize_comment([])                 -> [].

% Identifiers

% Tokenize identifier. At this point, the validity of
% the first character was already verified.

tokenize_identifier([H|T], Acc, Marker) when Marker == atom, H == $@;
    ?is_digit(H); ?is_upcase(H); ?is_downcase(H); H == $_ ->
  tokenize_identifier(T, [H|Acc], Marker);

tokenize_identifier([], Acc, _Marker) ->
  { [], lists:reverse(Acc) };

tokenize_identifier(Rest, Acc, _Marker) ->
  { Rest, lists:reverse(Acc) }.

% Tokenize atom identifier, which also accepts punctuated identifiers
tokenize_atom(String, Acc, Scope) ->
  { Rest, Identifier } = tokenize_identifier(String, Acc, atom),
  case Rest of
    [H|T] when H == $?; H == $! ->
      { T, unsafe_to_atom(Identifier ++ [H], Scope) };
    _ ->
      { Rest, unsafe_to_atom(Identifier, Scope) }
  end.

% Tokenize any identifier, handling kv, punctuated, paren, bracket and do identifiers.
tokenize_any_identifier(String, Line, Acc, Scope) ->
  { Rest, Identifier } = tokenize_identifier(String, Acc, false),

  case Rest of
    [H,$:|T] when H == $? orelse H == $!, hd(T) /= $: ->
      Atom = unsafe_to_atom(Identifier ++ [H], Scope),
      verify_kw_and_space(Line, Atom, T, Scope),
      { T, { kw_identifier, Line, Atom } };
    [H|T] when H == $?; H == $! ->
      Atom = unsafe_to_atom(Identifier ++ [H], Scope),
      { T, tokenize_call_identifier(punctuated_identifier, Line, Atom, T) };
    [$:|T] when hd(T) /= $: ->
      Atom = unsafe_to_atom(Identifier, Scope),
      verify_kw_and_space(Line, Atom, T, Scope),
      { T, { kw_identifier, Line, Atom } };
    _ ->
      { Rest, tokenize_call_identifier(identifier, Line, unsafe_to_atom(Identifier, Scope), Rest) }
  end.

tokenize_call_identifier(Kind, Line, Atom, Rest) ->
  case Rest of
    [$(|_] -> { paren_identifier, Line, Atom };
    [$[|_] -> { bracket_identifier, Line, Atom };
    _ ->
      case next_is_block(Rest) of
        false           -> { Kind, Line, Atom };
        BlockIdentifier -> { BlockIdentifier, Line, Atom }
      end
  end.

verify_kw_and_space(_Line, _Atom, [H|_], #scope{}) when ?is_space(H) -> ok;
verify_kw_and_space(Line, Atom, _, #scope{file=File}) ->
  io:format("~ts:~w: keyword argument ~s: must be followed by space~n", [File, Line, Atom]).

next_is_block([Space|Tokens]) when Space == $\t; Space == $\s ->
  next_is_block(Tokens);

next_is_block([$d,$o,H|_]) when
  ?is_digit(H); ?is_upcase(H); ?is_downcase(H); H == $_; H == $: ->
  false;

next_is_block([$d,$o|_]) ->
  do_identifier;

next_is_block(_) ->
  false.

add_token_with_nl(Left, [{eol,_,newline}|T]) -> [Left|T];
add_token_with_nl(Left, T) -> [Left|T].

% Error handling

interpolation_error({ error, { Line, Message, Token } }, Extension, Args) ->
  { error, { Line, io_lib:format("~s" ++ Extension, [Message|Args]), Token } }.

% Terminators

string_type($") -> bin_string;
string_type($') -> list_string.

sigil_terminator($() -> $);
sigil_terminator($[) -> $];
sigil_terminator(${) -> $};
sigil_terminator($<) -> $>;
sigil_terminator(O) -> O.

handle_terminator(_, #scope{check_terminators=false} = Scope) ->
  Scope;
handle_terminator(Token, #scope{terminators=Terminators} = Scope) ->
  case check_terminator(Token, Terminators) of
    { error, _ } = Error -> Error;
    New -> Scope#scope{terminators=New}
  end.

check_terminator({ S, Line }, Terminators) when S == 'fn' ->
  [{ fn, Line }|Terminators];

check_terminator({ S, _ } = New, Terminators) when
    S == 'do';
    S == '(';
    S == '[';
    S == '{';
    S == '<<' ->
  [New|Terminators];

check_terminator({ E, _ }, [{ S, _ }|Terminators]) when
    S == 'do', E == 'end';
    S == 'fn', E == 'end';
    S == '(',  E == ')';
    S == '[',  E == ']';
    S == '{',  E == '}';
    S == '<<', E == '>>' ->
  Terminators;

check_terminator({ E, Line }, [{ Start, StartLine }|_]) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~s (for \"~s\" starting at line ~B)", [End, Start, StartLine]),
  { error, { Line, Message, [] } };

check_terminator({ E, Line }, []) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  { error, { Line, "unexpected token: ", atom_to_list(E) } };

check_terminator(_, Terminators) ->
  Terminators.

terminator('fn') -> 'end';
terminator('do') -> 'end';
terminator('(')  -> ')';
terminator('[')  -> ']';
terminator('{')  -> '}';
terminator('<<') -> '>>'.

% Keywords check
handle_keyword(Line, Identifier, Atom, [{ '.', _ }|_] = Tokens) ->
  [{ Identifier, Line, Atom }|Tokens];

handle_keyword(Line, Identifier, Atom, Tokens) when
    Identifier ==  identifier; Identifier == do_identifier;
    Identifier ==  bracket_identifier; Identifier == paren_identifier ->
  case keyword(Atom) of
    true  -> [{ Atom, Line }|Tokens];
    op    -> add_token_with_nl({ Atom, Line }, Tokens);
    block -> [{ block_identifier, Line, Atom }|Tokens];
    false -> false
  end;

handle_keyword(_, _, _, _) -> false.

% Keywords
keyword('fn')    -> true;
keyword('do')    -> true;
keyword('end')   -> true;
keyword('true')  -> true;
keyword('false') -> true;
keyword('nil')   -> true;
keyword('not')   -> true;

% Bin operator keywords
keyword('and')    -> op;
keyword('or')     -> op;
keyword('xor')    -> op;
keyword('when')   -> op;
keyword('in')     -> op;
keyword('inlist') -> op;
keyword('inbits') -> op;

% Block keywords
keyword('after')  -> block;
keyword('else')   -> block;
keyword('rescue') -> block;
keyword('catch')  -> block;

keyword(_) -> false.