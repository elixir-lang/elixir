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

-define(container(T1, T2),
  T1 == ${, T2 == $};
  T1 == $[, T2 == $]
).

-define(at_op(T),
  T == $@).

-define(unary_op(T),
  T == $&;
  T == $!;
  T == $^).

-define(unary_op3(T1, T2, T3),
  T1 == $~, T2 == $~, T3 == $~).

-define(exp_op3(T1, T2, T3),
  T1 == $^, T2 == $^, T3 == $^).

-define(two_op(T1, T2),
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-;
  T1 == $*, T2 == $*).

-define(than_op(T1, T2),
  T1 == $<, T2 == $>).

-define(mult_op(T),
  T == $* orelse T == $/).

-define(dual_op(T),
  T == $+ orelse T == $-).

-define(range_op(T1, T2),
  T1 == $., T2 == $.).

-define(arrow_op3(T1, T2, T3),
  T1 == $<, T2 == $<, T3 == $<;
  T1 == $>, T2 == $>, T3 == $>).

-define(arrow_op(T1, T2),
  T1 == $<, T2 == $-;
  T1 == $|, T2 == $>).

-define(comp_op(T),
  T == $<;
  T == $>).

-define(comp_op2(T1, T2),
  T1 == $=, T2 == $=;
  T1 == $=, T2 == $~;
  T1 == $!, T2 == $=;
  T1 == $<, T2 == $=;
  T1 == $>, T2 == $=).

-define(comp_op3(T1, T2, T3),
  T1 == $=, T2 == $=, T3 == $=;
  T1 == $!, T2 == $=, T3 == $=).

-define(and_op(T1, T2),
  T1 == $&, T2 == $&).

-define(or_op(T1, T2),
  T1 == $|, T2 == $|).

-define(and_op3(T1, T2, T3),
  T1 == $&, T2 == $&, T3 == $&).

-define(or_op3(T1, T2, T3),
  T1 == $|, T2 == $|, T3 == $|).

-define(match_op(T),
  T == $=).

-define(tail_op(T),
  T == $|).

-define(default_op(T1, T2),
  T1 == $/, T2 == $/).

-define(stab_op(T1, T2),
  T1 == $-, T2 == $>).

-define(type_op(T1, T2),
  T1 == $:, T2 == $:).

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
  Message = io_lib:format("missing terminator: ~ts (for \"~ts\" starting at line ~B)", [End, Start, StartLine]),
  { error, { EndLine, Message, [] } };

% Base integers

tokenize([$0,X,H|T], Line, Scope, Tokens) when (X == $x orelse X == $X), ?is_hex(H) ->
  { Rest, Number } = tokenize_hex([H|T], []),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

tokenize([$0,B,H|T], Line, Scope, Tokens) when (B == $b orelse B == $B), ?is_bin(H) ->
  { Rest, Number } = tokenize_bin([H|T], []),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

tokenize([$0,H|T], Line, Scope, Tokens) when ?is_octal(H) ->
  { Rest, Number } = tokenize_octal([H|T], []),
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

tokenize([$%,S,H|T], Line, #scope{file=File} = Scope, Tokens) when not(?is_identifier(H)), not(?is_terminator(H)), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, File, ?is_downcase(S), T, sigil_terminator(H)) of
    { NewLine, Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(Final, NewLine, Scope, [{ sigil, Line, S, Parts, Modifiers }|Tokens]);
    Error ->
      Sigil = [$%,S,H],
      interpolation_error(Error, " (for sigil ~ts starting at line ~B)", [Sigil, Line])
  end;

% Char tokens

tokenize([$?,$\\,P,${,A,B,C,D,E,F,$}|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,${,A,B,C,D,E,F,$}]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,${,A,B,C,D,E,$}|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,${,A,B,C,D,E,$}]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,${,A,B,C,D,$}|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,${,A,B,C,D,$}]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,${,A,B,C,$}|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,${,A,B,C,$}]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,${,A,B,$}|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,${,A,B,$}]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,${,A,$}|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,${,A,$}]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,A,B|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,A,B]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,P,A|T], Line, Scope, Tokens) when (P == $x orelse P == $X), ?is_hex(A) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,P,A]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,A,B,C|T], Line, Scope, Tokens) when ?is_octal(A), A =< $3,?is_octal(B), ?is_octal(C) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,A,B,C]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,A,B|T], Line, Scope, Tokens) when ?is_octal(A), ?is_octal(B) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,A,B]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,A|T], Line, Scope, Tokens) when ?is_octal(A) ->
  [Char] = unicode:characters_to_list(elixir_interpolation:unescape_chars(list_to_binary([$\\,A]))),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,$\\,H|T], Line, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

tokenize([$?,Char|T], Line, Scope, Tokens) ->
  tokenize(T, Line, Scope, [{ number, Line, Char }|Tokens]);

% Dot identifier/operators
tokenize("..." ++ Rest, Line, Scope, Tokens) ->
  Token = check_call_identifier(identifier, Line, '...', Rest),
  tokenize(Rest, Line, Scope, [Token|Tokens]);

tokenize([$.,T|Tail], Line, Scope, Tokens) when ?is_space(T) ->
  case [T|Tail] of
    [$\r,$\n|Rest] -> tokenize([$.|Rest], Line + 1, Scope, Tokens);
    [$\n|Rest]     -> tokenize([$.|Rest], Line + 1, Scope, Tokens);
    [_|Rest]       -> tokenize([$.|Rest], Line, Scope, Tokens)
  end;

% ## Containers
tokenize(".<<>>" ++ Rest, Line, Scope, Tokens) ->
  handle_call_identifier(Rest, Line, '<<>>', Scope, Tokens);

tokenize([$.,T1,T2|Rest], Line, Scope, Tokens) when ?container(T1, T2) ->
  handle_call_identifier(Rest, Line, list_to_atom([T1, T2]), Scope, Tokens);

% ## Three Token Operators
tokenize([$.,T1,T2,T3|Rest], Line, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?exp_op3(T1, T2, T3) ->
  handle_call_identifier(Rest, Line, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Two Token Operators
tokenize([$.,T1,T2|Rest], Line, Scope, Tokens) when
    ?comp_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2); ?arrow_op(T1, T2);
    ?range_op(T1, T2); ?than_op(T1, T2); ?default_op(T1, T2); ?two_op(T1, T2);
    ?stab_op(T1, T2); ?type_op(T1, T2) ->
  handle_call_identifier(Rest, Line, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators
tokenize([$.,T|Rest], Line, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?dual_op(T); ?mult_op(T); ?comp_op(T);
    ?match_op(T); ?tail_op(T) ->
  handle_call_identifier(Rest, Line, list_to_atom([T]), Scope, Tokens);

% Dot call

% ## Exception for .( as it needs to be treated specially in the parser
tokenize([$.,$(|Rest], Line, Scope, Tokens) ->
  tokenize([$(|Rest], Line, Scope, add_token_with_nl({ dot_call_op, Line, '.' }, Tokens));

tokenize([$.,H|T], Line, #scope{file=File} = Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, [Part], Rest } when is_binary(Part) ->
      Atom  = unsafe_to_atom(Part, Scope),
      Token = check_call_identifier(identifier, Line, Atom, Rest),
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

tokenize([$:,H|T], Line, #scope{file=File} = Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, Parts, Rest } ->
      Token = case unescape_tokens(Parts) of
        [Part] when is_list(Part) or is_binary(Part) -> { atom, Line, unsafe_to_atom(Part, Scope) };
        Unescaped -> { atom, Line, Unescaped }
      end,
      tokenize(Rest, NewLine, Scope, [Token|Tokens]);
    Error ->
      interpolation_error(Error, " (for atom starting at line ~B)", [Line])
  end;

tokenize([$:,T|String], Line, Scope, Tokens) when ?is_atom_start(T) ->
  { Rest, Atom } = tokenize_atom([T|String], [], Scope),
  tokenize(Rest, Line, Scope, [{ atom, Line, Atom }|Tokens]);

% Atom identifiers/operators

tokenize(":..." ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, '...' }|Tokens]);

% ## Containers
tokenize(":<<>>" ++ Rest, Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, '<<>>' }|Tokens]);

tokenize([$:,T1,T2|Rest], Line, Scope, Tokens) when ?container(T1, T2) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T1,T2]) }|Tokens]);

% ## Three Token Operators
tokenize([$:,T1,T2,T3|Rest], Line, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?exp_op3(T1, T2, T3) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T1,T2,T3]) }|Tokens]);

% ## Two Token Operators
tokenize([$:,T1,T2|Rest], Line, Scope, Tokens) when
    ?comp_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2); ?arrow_op(T1, T2);
    ?range_op(T1, T2); ?than_op(T1, T2); ?default_op(T1, T2); ?two_op(T1, T2);
    ?stab_op(T1, T2); ?type_op(T1, T2) ->
  tokenize(Rest, Line, Scope, [{ atom, Line, list_to_atom([T1,T2]) }|Tokens]);

% ## Single Token Operators
tokenize([$:,T|Rest], Line, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?dual_op(T); ?mult_op(T); ?comp_op(T);
    ?match_op(T); ?tail_op(T); T == $. ->
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

% ## Three token operators
tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?unary_op3(T1, T2, T3) ->
  handle_nonl_op(Rest, Line, unary_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?comp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, comp_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?and_op3(T1, T2, T3) ->
  handle_op(Rest, Line, and_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?or_op3(T1, T2, T3) ->
  handle_op(Rest, Line, or_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?arrow_op3(T1, T2, T3) ->
  handle_op(Rest, Line, arrow_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Scope, Tokens) when ?exp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, exp_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

% ## Containers + punctuation tokens
tokenize([T,T|Rest], Line, Scope, Tokens) when T == $<; T == $> ->
  Token = { list_to_atom([T,T]), Line },
  handle_terminator(Rest, Line, Scope, Token, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when T == $(;
    T == ${; T == $}; T == $[; T == $]; T == $); T == $, ->
  Token = { list_to_atom([T]), Line },
  handle_terminator(Rest, Line, Scope, Token, Tokens);

% ## Two Token Operators
tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?two_op(T1, T2) ->
  handle_op(Rest, Line, two_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?than_op(T1, T2) ->
  handle_op(Rest, Line, than_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?range_op(T1, T2) ->
  handle_op(Rest, Line, range_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?arrow_op(T1, T2) ->
  handle_op(Rest, Line, arrow_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?comp_op2(T1, T2) ->
  handle_op(Rest, Line, comp_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?and_op(T1, T2) ->
  handle_op(Rest, Line, and_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?or_op(T1, T2) ->
  handle_op(Rest, Line, or_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?default_op(T1, T2) ->
  handle_op(Rest, Line, default_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?stab_op(T1, T2) ->
  handle_op(Rest, Line, stab_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Scope, Tokens) when ?type_op(T1, T2) ->
  handle_op(Rest, Line, type_op, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators

%% Handle &1 and friends with special precedence.
tokenize([$&,D|Rest], Line, Scope, Tokens) when ?is_digit(D) ->
  tokenize([D|Rest], Line, Scope, [{ '&', Line }|Tokens]);

tokenize([T|Rest], Line, Scope, Tokens) when ?at_op(T) ->
  handle_nonl_op(Rest, Line, at_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when ?unary_op(T) ->
  handle_nonl_op(Rest, Line, unary_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when ?comp_op(T) ->
  handle_op(Rest, Line, comp_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when ?dual_op(T) ->
  handle_nonl_op(Rest, Line, dual_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when ?mult_op(T) ->
  handle_op(Rest, Line, mult_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when ?match_op(T) ->
  handle_op(Rest, Line, match_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Scope, Tokens) when ?tail_op(T) ->
  handle_op(Rest, Line, tail_op, list_to_atom([T]), Scope, Tokens);

tokenize([$.|Rest], Line, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, add_token_with_nl({ '.', Line }, Tokens));

% Integers and floats

tokenize([H|_] = String, Line, Scope, Tokens) when ?is_digit(H) ->
  { Rest, Number } = tokenize_number(String, [], false),
  tokenize(Rest, Line, Scope, [{ number, Line, Number }|Tokens]);

% Aliases

tokenize([H|_] = String, Line, Scope, Tokens) when ?is_upcase(H) ->
  { Rest, Alias } = tokenize_identifier(String, []),
  Atom = unsafe_to_atom(Alias, Scope),
  case Rest of
    [$:|T] when ?is_space(hd(T)) ->
      tokenize(T, Line, Scope, [{ kw_identifier, Line, Atom }|Tokens]);
    _ ->
      tokenize(Rest, Line, Scope, [{ aliases, Line, [Atom] }|Tokens])
  end;

% Identifier

tokenize([H|_] = String, Line, Scope, Tokens) when ?is_downcase(H); H == $_ ->
  case tokenize_any_identifier(String, Line, Scope, Tokens) of
    { keyword, Rest, Check, T } ->
      handle_terminator(Rest, Line, Scope, Check, T);
    { identifier, Rest, Token } ->
      tokenize(Rest, Line, Scope, [Token|Tokens]);
    { error, _ } = Error ->
      Error
  end;

% Ambiguous unary/binary operators tokens

tokenize([Space, Sign, NotMarker|T], Line, Scope, [{ Identifier, _, _ } = H|Tokens]) when
    ?dual_op(Sign),
    ?is_horizontal_space(Space),
    not(?is_space(NotMarker)),
    NotMarker /= $(, NotMarker /= $+, NotMarker /= $-, NotMarker /= $>,
    Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Rest, Line, Scope, [{ dual_op, Line, list_to_atom([Sign]) }, setelement(1, H, op_identifier)|Tokens]);

% Spaces

tokenize([T|Rest], Line, Scope, Tokens) when ?is_horizontal_space(T) ->
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
    { NewLine, Parts, [$:|Rest] } when ?is_space(hd(Rest)) ->
      case Parts of
        [Bin] when is_binary(Bin) ->
          Atom = unsafe_to_atom(unescape_chars(Bin), Scope),
          tokenize(Rest, NewLine, Scope, [{ kw_identifier, Line, Atom }|Tokens]);
        _ ->
          { error, { Line, "invalid interpolation in key ", [$"|T] } }
      end;
    { NewLine, Parts, Rest } ->
      Token = { string_type(H), Line, unescape_tokens(Parts) },
      tokenize(Rest, NewLine, Scope, [Token|Tokens]);
    Error ->
      interpolation_error(Error, " (for string starting at line ~B)", [Line])
  end.

handle_nonl_op([$:|Rest], Line, _Kind, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Scope, [{ kw_identifier, Line, Op }|Tokens]);

handle_nonl_op(Rest, Line, Kind, Op, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, [{ Kind, Line, Op }|Tokens]).

handle_op([$:|Rest], Line, _Kind, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Scope, [{ kw_identifier, Line, Op }|Tokens]);

handle_op(Rest, Line, Kind, Op, Scope, Tokens) ->
  tokenize(Rest, Line, Scope, add_token_with_nl({ Kind, Line, Op }, Tokens)).

handle_call_identifier(Rest, Line, Op, Scope, Tokens) ->
  Token = check_call_identifier(identifier, Line, Op, Rest),
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

%% Heredocs

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
  Message    = io_lib:format("missing terminator: ~ts (for heredoc starting at line ~B)", [Terminator, StartLine]),
  { error, { ErrorLine, Message, [] } }.

%% Remove spaces from heredoc based on the position of the final quotes.

remove_heredoc_spaces(Body, 0) ->
  lists:reverse([0|Body]);

remove_heredoc_spaces(Body, Spaces) ->
  remove_heredoc_spaces([0|Body], [], Spaces, Spaces).

remove_heredoc_spaces([H,$\n|T], [Backtrack|Buffer], Spaces, Original) when Spaces > 0, ?is_horizontal_space(H) ->
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

extract_heredoc_line(Marker, [H|T], Buffer, Counter) when ?is_horizontal_space(H) ->
  extract_heredoc_line(Marker, T, [H|Buffer], Counter + 1);

extract_heredoc_line(Marker, [Marker,Marker,Marker|T] = Rest, Buffer, Counter) ->
  case next_is_break(T) of
    false -> extract_heredoc_line(Rest, Buffer);
    Final -> { ok, Buffer, Final, Counter }
  end;

extract_heredoc_line(_Marker, Rest, Buffer, _Counter) ->
  extract_heredoc_line(Rest, Buffer).

next_is_break([H|T]) when ?is_horizontal_space(H) -> next_is_break(T);
next_is_break("\r\n" ++ T) -> T;
next_is_break("\n" ++ T) -> T;
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

%% Integers and floats
%% At this point, we are at least sure the first digit is a number.

%% Check if we have a point followed by a number;
tokenize_number([$.,H|T], Acc, false) when ?is_digit(H) ->
  tokenize_number(T, [H,$.|Acc], true);

%% Check if we have an underscore followed by a number;
tokenize_number([$_,H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

%% Check if we have e- followed by numbers (valid only for floats);
tokenize_number([$e,S,H|T], Acc, true) when ?is_digit(H), S == $+ orelse S == $- ->
  tokenize_number(T, [H,S,$e|Acc], true);

%% Check if we have e followed by numbers (valid only for floats);
tokenize_number([$e,H|T], Acc, true) when ?is_digit(H) ->
  tokenize_number(T, [H,$e|Acc], true);

%% Finally just numbers.
tokenize_number([H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

%% Cast to float...
tokenize_number(Rest, Acc, true) ->
  { Rest, list_to_float(lists:reverse(Acc)) };

%% Or integer.
tokenize_number(Rest, Acc, false) ->
  { Rest, list_to_integer(lists:reverse(Acc)) }.

tokenize_hex([H|T], Acc) when ?is_hex(H) -> tokenize_hex(T, [H|Acc]);
tokenize_hex(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 16) }.

tokenize_octal([H|T], Acc) when ?is_octal(H) -> tokenize_octal(T, [H|Acc]);
tokenize_octal(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 8) }.

tokenize_bin([H|T], Acc) when ?is_bin(H) -> tokenize_bin(T, [H|Acc]);
tokenize_bin(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 2) }.

%% Comments

tokenize_comment("\r\n" ++ _ = Rest) -> Rest;
tokenize_comment("\n" ++ _ = Rest)   -> Rest;
tokenize_comment([_|Rest])           -> tokenize_comment(Rest);
tokenize_comment([])                 -> [].

%% Atoms
%% Handle atoms specially since they support @

tokenize_atom([H|T], Acc, Scope) when ?is_atom(H) ->
  tokenize_atom(T, [H|Acc], Scope);

tokenize_atom([H|T], Acc, Scope) when H == $?; H == $! ->
  { T, unsafe_to_atom(lists:reverse([H|Acc]), Scope) };

tokenize_atom(Rest, Acc, Scope) ->
  { Rest, unsafe_to_atom(lists:reverse(Acc), Scope) }.

%% Identifiers
%% At this point, the validity of the first character was already verified.

tokenize_identifier([H|T], Acc) when ?is_identifier(H) ->
  tokenize_identifier(T, [H|Acc]);

tokenize_identifier(Rest, Acc) ->
  { Rest, lists:reverse(Acc) }.

%% Tokenize any identifier, handling kv, punctuated, paren, bracket and do identifiers.

tokenize_any_identifier(String, Line, Scope, Tokens) ->
  { Rest, Identifier } = tokenize_identifier(String, []),

  case Rest of
    [H|T] when H == $?; H == $! ->
      Atom = unsafe_to_atom(Identifier ++ [H], Scope),
      tokenize_kw_or_other(T, punctuated_identifier, Line, Atom, Tokens);
    _ ->
      Atom = unsafe_to_atom(Identifier, Scope),
      tokenize_kw_or_other(Rest, identifier, Line, Atom, Tokens)
  end.

tokenize_kw_or_other([$:,H|T], _Kind, Line, Atom, _Tokens) when ?is_space(H) ->
  { identifier, [H|T], { kw_identifier, Line, Atom } };

tokenize_kw_or_other([$:,H|_], _Kind, Line, Atom, _Tokens) when ?is_atom_start(H) ->
  { error, { Line, "keyword argument must be followed by space after: ", atom_to_list(Atom) ++ [$:] } };

tokenize_kw_or_other(Rest, Kind, Line, Atom, Tokens) ->
  case check_keyword(Line, Atom, Tokens) of
    nomatch ->
      { identifier, Rest, check_call_identifier(Kind, Line, Atom, Rest) };
    { ok, [Check|T] } ->
      { keyword, Rest, Check, T };
    { error, Token } ->
      { error, { Line, "syntax error before: ", Token } }
  end.

%% Check if it is a call identifier (paren | bracket | do)

check_call_identifier(_Kind, Line, Atom, [$(|_]) -> { paren_identifier, Line, Atom };
check_call_identifier(_Kind, Line, Atom, [$[|_]) -> { bracket_identifier, Line, Atom };
check_call_identifier(Kind, Line, Atom, _Rest)   -> { Kind, Line, Atom }.

add_token_with_nl(Left, [{eol,_,newline}|T]) -> [Left|T];
add_token_with_nl(Left, T) -> [Left|T].

%% Error handling

interpolation_error({ error, { Line, Message, Token } }, Extension, Args) ->
  { error, { Line, io_lib:format("~ts" ++ Extension, [Message|Args]), Token } }.

%% Terminators

handle_terminator(Rest, Line, Scope, Token, Tokens) ->
  case handle_terminator(Token, Scope) of
    { error, _ } = Error -> Error;
    New -> tokenize(Rest, Line, New, [Token|Tokens])
  end.

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
  End = terminator(Start),
  Message = io_lib:format("\"~ts\" starting at line ~B is missing terminator \"~ts\". "
                          "Unexpected token: ", [Start, StartLine, End]),
  { error, { Line, Message, atom_to_list(E) } };

check_terminator({ E, Line }, []) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  { error, { Line, "unexpected token: ", atom_to_list(E) } };

check_terminator(_, Terminators) ->
  Terminators.

string_type($") -> bin_string;
string_type($') -> list_string.

sigil_terminator($() -> $);
sigil_terminator($[) -> $];
sigil_terminator(${) -> $};
sigil_terminator($<) -> $>;
sigil_terminator(O) -> O.

terminator('fn') -> 'end';
terminator('do') -> 'end';
terminator('(')  -> ')';
terminator('[')  -> ']';
terminator('{')  -> '}';
terminator('<<') -> '>>'.

%% Keywords checking

check_keyword(_Line, _Atom, [{ '.', _ }|_]) ->
  nomatch;

check_keyword(Line, do, [{ identifier, Line, Atom }|T]) ->
  { ok, [{ do, Line }, { do_identifier, Line, Atom }|T] };

check_keyword(Line, do, Tokens) ->
  case do_keyword_valid(Tokens) of
    true  -> { ok, [{ do, Line }|Tokens] };
    false -> { error, "do" }
  end;

check_keyword(Line, Atom, Tokens) ->
  case keyword(Atom) of
    false    -> nomatch;
    token    -> { ok, [{ Atom, Line }|Tokens] };
    block    -> { ok, [{ block_identifier, Line, Atom }|Tokens] };
    unary_op -> { ok, [{ unary_op, Line, Atom }|Tokens] };
    Kind     -> { ok, add_token_with_nl({ Kind, Line, Atom }, Tokens) }
  end.

%% do is only valid after the end, true, false and nil keywords
do_keyword_valid([{ Atom, _ }|_]) ->
  case Atom of
    'end' -> true;
    nil   -> true;
    true  -> true;
    false -> true;
    _     -> keyword(Atom) == false
  end;

do_keyword_valid(_) ->
  true.

% Regular keywords
keyword('fn')    -> token;
keyword('do')    -> token;
keyword('end')   -> token;
keyword('true')  -> token;
keyword('false') -> token;
keyword('nil')   -> token;

% Operators keywords
keyword('not')    -> unary_op;
keyword('and')    -> and_op;
keyword('or')     -> or_op;
keyword('xor')    -> or_op;
keyword('when')   -> when_op;
keyword('in')     -> in_op;
keyword('inlist') -> inc_op;
keyword('inbits') -> inc_op;

% Block keywords
keyword('after')  -> block;
keyword('else')   -> block;
keyword('rescue') -> block;
keyword('catch')  -> block;

keyword(_) -> false.
