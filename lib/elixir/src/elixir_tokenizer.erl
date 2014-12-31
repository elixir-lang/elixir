-module(elixir_tokenizer).
-include("elixir.hrl").
-export([tokenize/3, tokenize/4]).
-import(elixir_interpolation, [unescape_tokens/1]).

-define(at_op(T),
  T == $@).

-define(capture_op(T),
  T == $&).

-define(unary_op(T),
  T == $!;
  T == $^).

-define(unary_op3(T1, T2, T3),
  T1 == $~, T2 == $~, T3 == $~).

-define(hat_op3(T1, T2, T3),
  T1 == $^, T2 == $^, T3 == $^).

-define(two_op(T1, T2),
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-;
  T1 == $<, T2 == $>;
  T1 == $., T2 == $.).

-define(mult_op(T),
  T == $* orelse T == $/).

-define(dual_op(T),
  T == $+ orelse T == $-).

-define(arrow_op3(T1, T2, T3),
  T1 == $<, T2 == $<, T3 == $<;
  T1 == $>, T2 == $>, T3 == $>;
  T1 == $~, T2 == $>, T3 == $>;
  T1 == $<, T2 == $<, T3 == $~;
  T1 == $<, T2 == $~, T3 == $>;
  T1 == $<, T2 == $|, T3 == $>).

-define(arrow_op(T1, T2),
  T1 == $|, T2 == $>;
  T1 == $~, T2 == $>;
  T1 == $<, T2 == $~).

-define(rel_op(T),
  T == $<;
  T == $>).

-define(rel_op2(T1, T2),
  T1 == $<, T2 == $=;
  T1 == $>, T2 == $=).

-define(comp_op2(T1, T2),
  T1 == $=, T2 == $=;
  T1 == $=, T2 == $~;
  T1 == $!, T2 == $=).

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

-define(in_match_op(T1, T2),
  T1 == $<, T2 == $-;
  T1 == $\\, T2 == $\\).

-define(stab_op(T1, T2),
  T1 == $-, T2 == $>).

-define(type_op(T1, T2),
  T1 == $:, T2 == $:).

-define(pipe_op(T),
  T == $|).

tokenize(String, Line, Column, #elixir_tokenizer{} = Scope) ->
  tokenize(String, Line, Column, Scope, []);

tokenize(String, Line, Column, Opts) ->
  File = case lists:keyfind(file, 1, Opts) of
    {file, V1} -> V1;
    false -> <<"nofile">>
  end,

  Existing = case lists:keyfind(existing_atoms_only, 1, Opts) of
    {existing_atoms_only, true} -> true;
    false -> false
  end,

  Check = case lists:keyfind(check_terminators, 1, Opts) of
    {check_terminators, false} -> false;
    false -> true
  end,

  tokenize(String, Line, Column, #elixir_tokenizer{
    file=File,
    existing_atoms_only=Existing,
    check_terminators=Check
 }).

tokenize(String, Line, Opts) ->
  tokenize(String, Line, 0, Opts).

tokenize([], Line, Column, #elixir_tokenizer{terminators=[]}, Tokens) ->
  {ok, Line, Column, lists:reverse(Tokens)};

tokenize([], EndLine, _Column, #elixir_tokenizer{terminators=[{Start, [StartLine, _, _]}|_]}, Tokens) ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~ts (for \"~ts\" starting at line ~B)", [End, Start, StartLine]),
  {error, {EndLine, Message, []}, [], Tokens};

% Base integers

tokenize([$0,$x,H|T], Line, Column, Scope, Tokens) when ?is_hex(H) ->
  {Rest, Number, Length} = tokenize_hex([H|T], []),
  tokenize(Rest, Line, Column + 2 + Length, Scope, [{number, [Line, Column, Column + 2 + Length], Number}|Tokens]);

tokenize([$0,$b,H|T], Line, Column, Scope, Tokens) when ?is_bin(H) ->
  {Rest, Number, Length} = tokenize_bin([H|T], []),
  tokenize(Rest, Line, Column + 2 + Length, Scope, [{number, [Line, Column, Column + 2 + Length], Number}|Tokens]);

tokenize([$0,$o,H|T], Line, Column, Scope, Tokens) when ?is_octal(H) ->
  {Rest, Number, Length} = tokenize_octal([H|T], []),
  tokenize(Rest, Line, Column + 2 + Length, Scope, [{number, [Line, Column, Column + 2 + Length], Number}|Tokens]);

% Comments

tokenize([$#|String], Line, Column, Scope, Tokens) ->
  Rest = tokenize_comment(String),
  tokenize(Rest, Line, Column, Scope, Tokens);

% Sigils

tokenize([$~,S,H,H,H|T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, ?is_downcase(S), T, H) of
    {ok, NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      tokenize(Final, NewLine, NewColumn, Scope, [{sigil, [Line, Column, NewColumn], S, Parts, Modifiers}|Tokens]);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

tokenize([$~,S,H|T] = Original, Line, Column, Scope, Tokens) when ?is_sigil(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, Column, Scope, ?is_downcase(S), T, sigil_terminator(H)) of
    {NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      tokenize(Final, NewLine, NewColumn, Scope, [{sigil, [Line, Column, NewColumn], S, Parts, Modifiers}|Tokens]);
    {error, Reason} ->
      Sigil = [$~,S,H],
      interpolation_error(Reason, Original, Tokens, " (for sigil ~ts starting at line ~B)", [Sigil, Line])
  end;

% Char tokens

tokenize([$?,$\\,$x,${,A,B,C,D,E,F,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  Char = escape_char([$\\,$x,${,A,B,C,D,E,F,$}]),
  tokenize(T, Line, Column + 12, Scope, [{number, [Line, Column, Column + 12], Char}|Tokens]);

tokenize([$?,$\\,$x,${,A,B,C,D,E,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  Char = escape_char([$\\,$x,${,A,B,C,D,E,$}]),
  tokenize(T, Line, Column + 11, Scope, [{number, [Line, Column, Column + 11], Char}|Tokens]);

tokenize([$?,$\\,$x,${,A,B,C,D,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  Char = escape_char([$\\,$x,${,A,B,C,D,$}]),
  tokenize(T, Line, Column + 10, Scope, [{number, [Line, Column, Column + 10], Char}|Tokens]);

tokenize([$?,$\\,$x,${,A,B,C,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  Char = escape_char([$\\,$x,${,A,B,C,$}]),
  tokenize(T, Line, Column + 9, Scope, [{number, [Line, Column, Column + 9], Char}|Tokens]);

tokenize([$?,$\\,$x,${,A,B,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B) ->
  Char = escape_char([$\\,$x,${,A,B,$}]),
  tokenize(T, Line, Column + 8, Scope, [{number, [Line, Column, Column + 8], Char}|Tokens]);

tokenize([$?,$\\,$x,${,A,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A) ->
  Char = escape_char([$\\,$x,${,A,$}]),
  tokenize(T, Line, Column + 7, Scope, [{number, [Line, Column, Column + 7], Char}|Tokens]);

tokenize([$?,$\\,$x,A,B|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B) ->
  Char = escape_char([$\\,$x,A,B]),
  tokenize(T, Line, Column + 6, Scope, [{number, [Line, Column, Column + 6], Char}|Tokens]);

tokenize([$?,$\\,$x,A|T], Line, Column, Scope, Tokens)
    when ?is_hex(A) ->
  Char = escape_char([$\\,$x,A]),
  tokenize(T, Line, Column + 5, Scope, [{number, [Line, Column, Column + 5], Char}|Tokens]);

tokenize([$?, $\\, $\\|T], Line, Column, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map($\\),
  tokenize(T, Line, Column + 5, Scope, [{number, [Line, Column, Column + 5], Char}|Tokens]);

tokenize([$?,$\\,H|T], Line, Column, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H), % @ elixir_interpolation:unescape_map
  tokenize(T, Line, Column + 4, Scope, [{number, [Line, Column, Column + 4], Char}|Tokens]);

tokenize([$?,Char|T], Line, Column, Scope, Tokens) ->
  case handle_char(Char) of
    {Escape, Name} ->
      Msg = io_lib:format("found ? followed by codepoint 0x~.16B (~ts), please use ~ts instead",
                          [Char, Name, Escape]),
      elixir_errors:warn(Line, Scope#elixir_tokenizer.file, Msg);
    false ->
      ok
  end,
  tokenize(T, Line, Column + 2, Scope, [{number, [Line, Column, Column + 2], Char}|Tokens]);

% Heredocs

tokenize("\"\"\"" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $", Scope, Tokens);

tokenize("'''" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $', Scope, Tokens);

% Strings

tokenize([$"|T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column, $", Scope, Tokens);
tokenize([$'|T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column, $', Scope, Tokens);

% Atoms

tokenize([$:,H|T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column + 1, Scope, true, T, H) of % @ elixir_interpolation:extract
    {NewLine, NewColumn, Parts, Rest} ->
      Unescaped = unescape_tokens(Parts),
      Key = case Scope#elixir_tokenizer.existing_atoms_only of
        true  -> atom_safe;
        false -> atom_unsafe
      end,
      tokenize(Rest, NewLine, NewColumn, Scope, [{Key, [Line, Column, NewColumn], Unescaped}|Tokens]);
    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for atom starting at line ~B)", [Line])
  end;

tokenize([$:,T|String] = Original, Line, Column, Scope, Tokens) when ?is_atom_start(T) ->
  {Rest, Part, Length} = tokenize_atom([T|String], []),
  case unsafe_to_atom(Part, Line, Scope) of
    {ok, Atom} ->
      tokenize(Rest, Line, Column + 1 + Length, Scope, [{atom, [Line, Column, Column + 1 + Length], Atom}|Tokens]);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

% %% Special atom identifiers / operators

tokenize(":..." ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, [Line, Column, Column + 4], '...'}|Tokens]);
tokenize(":<<>>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 5, Scope, [{atom, [Line, Column, Column + 5], '<<>>'}|Tokens]);
tokenize(":%{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, [Line, Column, Column + 4], '%{}'}|Tokens]);
tokenize(":%" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, [Line, Column, Column + 2], '%'}|Tokens]);
tokenize(":{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, [Line, Column, Column + 3], '{}'}|Tokens]);

tokenize("...:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, [Line, Column, Column + 4], '...'}|Tokens]);
tokenize("<<>>:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 5, Scope, [{kw_identifier, [Line, Column, Column + 5], '<<>>'}|Tokens]);
tokenize("%{}:" ++ Rest, Column, Line, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, [Line, Column, Column + 4], '%{}'}|Tokens]);
tokenize("%:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, [Line, Column, Column + 2], '%'}|Tokens]);
tokenize("{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 3, Scope, [{kw_identifier, [Line, Column, Column + 3], '{}'}|Tokens]);

% ## Three Token Operators
tokenize([$:,T1,T2,T3|Rest], Line, Column, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?hat_op3(T1, T2, T3) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, [Line, Column, Column + 4], list_to_atom([T1,T2,T3])}|Tokens]);

% ## Two Token Operators
tokenize([$:,T1,T2|Rest], Line, Column, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?stab_op(T1, T2);
    ?type_op(T1, T2) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, [Line, Column, Column + 3], list_to_atom([T1,T2])}|Tokens]);

% ## Single Token Operators
tokenize([$:,T|Rest], Line, Column, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T); T == $. ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, [Line, Column, Column + 2], list_to_atom([T])}|Tokens]);

% End of line

tokenize(";" ++ Rest, Line, Column, Scope, []) ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', [Line, Column, Column + 1]}]);

tokenize(";" ++ Rest, Line, Column, Scope, [Top|_] = Tokens) when element(1, Top) /= ';' ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', [Line, Column, Column + 1]}|Tokens]);

tokenize("\\" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\r\n" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 0, Scope, Tokens);

tokenize("\\\r\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 0, Scope, Tokens);

tokenize("\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 0, Scope, eol(Line, Column, Tokens));

tokenize("\r\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 0, Scope, eol(Line, Column, Tokens));

% Stand-alone tokens

tokenize("..." ++ Rest, Line, Column, Scope, Tokens) ->
  Token = check_call_identifier(identifier, Line, Column, '...', Rest),
  tokenize(Rest, Line, Column + 3, Scope, [Token|Tokens]);

tokenize("=>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, add_token_with_nl({assoc_op, [Line, Column, Column + 2], '=>'}, Tokens));

% ## Three token operators
tokenize([T1,T2,T3|Rest], Line, Column, Scope, Tokens) when ?unary_op3(T1, T2, T3) ->
  handle_unary_op(Rest, Line, Column + 3, unary_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Column, Scope, Tokens) when ?comp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, comp_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Column, Scope, Tokens) when ?and_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, and_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Column, Scope, Tokens) when ?or_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, or_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Column, Scope, Tokens) when ?arrow_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, arrow_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

tokenize([T1,T2,T3|Rest], Line, Column, Scope, Tokens) when ?hat_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, hat_op, list_to_atom([T1,T2,T3]), Scope, Tokens);

% ## Containers + punctuation tokens
tokenize([T,T|Rest], Line, Column, Scope, Tokens) when T == $<; T == $> ->
  Token = {list_to_atom([T,T]), [Line, Column, Column + 2]},
  handle_terminator(Rest, Line, Column + 2, Scope, Token, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when T == $(;
    T == ${; T == $}; T == $[; T == $]; T == $); T == $, ->
  Token = {list_to_atom([T]), [Line, Column, Column + 1]},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

% ## Two Token Operators
tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?two_op(T1, T2) ->
  handle_op(Rest, Line, Column, two_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?arrow_op(T1, T2) ->
  handle_op(Rest, Line, Column, arrow_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?comp_op2(T1, T2) ->
  handle_op(Rest, Line, Column, comp_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?rel_op2(T1, T2) ->
  handle_op(Rest, Line, Column, rel_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?and_op(T1, T2) ->
  handle_op(Rest, Line, Column, and_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?or_op(T1, T2) ->
  handle_op(Rest, Line, Column, or_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?in_match_op(T1, T2) ->
  handle_op(Rest, Line, Column, in_match_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?type_op(T1, T2) ->
  handle_op(Rest, Line, Column, type_op, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1,T2|Rest], Line, Column, Scope, Tokens) when ?stab_op(T1, T2) ->
  handle_op(Rest, Line, Column, stab_op, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?at_op(T) ->
  handle_unary_op(Rest, Line, Column, at_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?capture_op(T) ->
  handle_unary_op(Rest, Line, Column, capture_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?unary_op(T) ->
  handle_unary_op(Rest, Line, Column, unary_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?rel_op(T) ->
  handle_op(Rest, Line, Column, rel_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?dual_op(T) ->
  handle_unary_op(Rest, Line, Column, dual_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?mult_op(T) ->
  handle_op(Rest, Line, Column, mult_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?match_op(T) ->
  handle_op(Rest, Line, Column, match_op, list_to_atom([T]), Scope, Tokens);

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?pipe_op(T) ->
  handle_op(Rest, Line, Column, pipe_op, list_to_atom([T]), Scope, Tokens);

% Others

tokenize([$%|T], Line, Column, Scope, Tokens) ->
  case strip_space(T, 0, Column + 1) of
    {[${|_] = Rest, Counter, Offset} -> tokenize(Rest, Line + Counter, Offset, Scope, [{'%{}', [Line, Column, Column + 1]}|Tokens]);
    {Rest, Counter, Offset}          -> tokenize(Rest, Line + Counter, Offset, Scope, [{'%', [Line, Column, Column + 1]}|Tokens])
  end;

tokenize([$.|T], Line, Column, Scope, Tokens) ->
  {Rest, Counter, Offset} = strip_space(T, 0, Column + 1),
  handle_dot([$.|Rest], Line + Counter, Offset - 1, Column, Scope, Tokens);

% Integers and floats

tokenize([H|_] = String, Line, Column, Scope, Tokens) when ?is_digit(H) ->
  {Rest, Number, Length} = tokenize_number(String, [], false),
  tokenize(Rest, Line, Column + Length, Scope, [{number, [Line, Column, Column + Length], Number}|Tokens]);

% Aliases

tokenize([H|_] = Original, Line, Column, Scope, Tokens) when ?is_upcase(H) ->
  {Rest, Alias, Length} = tokenize_identifier(Original, []),
  case unsafe_to_atom(Alias, Line, Scope) of
    {ok, Atom} ->
      case Rest of
        [$:|T] when ?is_space(hd(T)) ->
          tokenize(T, Line, Column + Length + 1, Scope, [{kw_identifier, [Line, Column, Column + Length + 1], Atom}|Tokens]);
        _ ->
          tokenize(Rest, Line, Column + Length, Scope, [{aliases, [Line, Column, Column + Length], [Atom]}|Tokens])
      end;
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

% Identifier

tokenize([H|_] = String, Line, Column, Scope, Tokens) when ?is_downcase(H); H == $_ ->
  case tokenize_any_identifier(String, Line, Column, Scope, Tokens) of
    {keyword, Rest, {_, [_, _, EndColumn]} = Check, T} ->
      handle_terminator(Rest, Line, EndColumn, Scope, Check, T);
    {keyword, Rest, {_, [_, _, EndColumn], _} = Check, T} ->
      handle_terminator(Rest, Line, EndColumn, Scope, Check, T);
    {identifier, Rest, {_, [_, _, EndColumn], _} = Token} ->
      tokenize(Rest, Line, EndColumn, Scope, [Token|Tokens]);
    {error, _, _, _} = Error ->
      Error
  end;

% Spaces

tokenize([T|Rest], Line, Column, Scope, Tokens) when ?is_horizontal_space(T) ->
  case strip_horizontal_space(Rest) of
    {Remaining, Stripped} ->
      handle_space_sensitive_tokens(Remaining, Line, Column + 1 + Stripped, Scope, Tokens)
  end;

tokenize([T|Rest] = Original, Line, _Column, _Scope, Tokens) when ?is_invalid_space(T) ->
  Message = io_lib:format("invalid space character U+~.16B before: ", [T]),
  {error, {Line, lists:flatten(Message), until_eol(Rest)}, Original, Tokens};

tokenize(T, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid token: ", until_eol(T)}, T, Tokens}.

strip_horizontal_space(T) ->
  strip_horizontal_space(T, 0).

strip_horizontal_space([H|T], Counter) when ?is_horizontal_space(H) ->
  strip_horizontal_space(T, Counter + 1);
strip_horizontal_space(T, Counter) ->
  {T, Counter}.

strip_space(T, Counter, Column) ->
  case strip_horizontal_space(T) of
    {"\r\n" ++ Rest, _} -> strip_space(Rest, Counter + 1, 0);
    {"\n" ++ Rest, _}   -> strip_space(Rest, Counter + 1, 0);
    {Rest, Length}      -> {Rest, Counter, Column + Length}
  end.

until_eol("\r\n" ++ _) -> [];
until_eol("\n" ++ _)   -> [];
until_eol([])          -> [];
until_eol([H|T])       -> [H|until_eol(T)].

handle_char(7)   -> {"\\a", "alert"};
handle_char($\b) -> {"\\b", "backspace"};
handle_char($\d) -> {"\\d", "delete"};
handle_char($\e) -> {"\\e", "escape"};
handle_char($\f) -> {"\\f", "form feed"};
handle_char($\n) -> {"\\n", "newline"};
handle_char($\r) -> {"\\r", "carriage return"};
handle_char($\s) -> {"\\s", "space"};
handle_char($\t) -> {"\\t", "tab"};
handle_char($\v) -> {"\\v", "vertical tab"};
handle_char(_)  -> false.

escape_char(List) ->
  <<Char/utf8>> = elixir_interpolation:unescape_chars(list_to_binary(List)), % @ elixir_interpolation:unescape_chars
  Char.

%% Handlers

handle_heredocs(T, Line, Column, H, Scope, Tokens) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, true, T, H) of
    {ok, NewLine, NewColumn, Parts, Rest} ->
      Token = {string_type(H), [Line, Column, NewColumn], unescape_tokens(Parts)},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token|Tokens]);
    {error, Reason} ->
      {error, Reason, [H, H, H] ++ T, Tokens}
  end.

handle_strings(T, Line, Column, H, Scope, Tokens) ->
  case elixir_interpolation:extract(Line, Column, Scope, true, T, H) of
    {error, Reason} ->
      interpolation_error(Reason, [H|T], Tokens, " (for string starting at line ~B)", [Line]);
    {NewLine, NewColumn, Parts, [$:|Rest]} when ?is_space(hd(Rest)) ->
      Unescaped = unescape_tokens(Parts),
      Key = case Scope#elixir_tokenizer.existing_atoms_only of
        true  -> kw_identifier_safe;
        false -> kw_identifier_unsafe
      end,
      tokenize(Rest, NewLine, NewColumn, Scope, [{Key, [Line, Column, NewColumn], Unescaped}|Tokens]);
    {NewLine, NewColumn, Parts, Rest} ->
      Token = {string_type(H), [Line, Column, NewColumn], unescape_tokens(Parts)},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token|Tokens])
  end.

handle_unary_op([$:|Rest], Line, Column, _Kind, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + atom_length(Op) + 1, Scope, [{kw_identifier, [Line, Column, Column + atom_length(Op)], Op}|Tokens]);

handle_unary_op(Rest, Line, Column, Kind, Op, Scope, Tokens) ->
  case strip_horizontal_space(Rest) of
    {[$/|_] = Remaining, Length} -> tokenize(Remaining, Line, Column + atom_length(Op) + Length, Scope, [{identifier, [Line, Column, Column + atom_length(Op)], Op}|Tokens]);
    {Remaining, Length} -> tokenize(Remaining, Line, Column + atom_length(Op) + Length, Scope, [{Kind, [Line, Column, Column + atom_length(Op)], Op}|Tokens])
  end.

handle_op([$:|Rest], Line, Column, _Kind, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + atom_length(Op) + 1, Scope, [{kw_identifier, [Line, Column, Column + atom_length(Op)], Op}|Tokens]);

handle_op(Rest, Line, Column, Kind, Op, Scope, Tokens) ->
  case strip_horizontal_space(Rest) of
    {[$/|_] = Remaining, Length} -> tokenize(Remaining, Line, Column + atom_length(Op) + Length, Scope, [{identifier, [Line, Column, Column + atom_length(Op)], Op}|Tokens]);
    {Remaining, Length} -> tokenize(Remaining, Line, Column + atom_length(Op) + Length, Scope, add_token_with_nl({Kind, [Line, Column, Column + atom_length(Op)], Op}, Tokens))
  end.

% ## Three Token Operators
handle_dot([$.,T1,T2,T3|Rest], Line, Column, DotColumn, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?hat_op3(T1, T2, T3) ->
  handle_call_identifier(Rest, Line, Column + 1, DotColumn, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Two Token Operators
handle_dot([$.,T1,T2|Rest], Line, Column, DotColumn, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?stab_op(T1, T2);
    ?type_op(T1, T2) ->
  handle_call_identifier(Rest, Line, Column + 1, DotColumn, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators
handle_dot([$.,T|Rest], Line, Column, DotColumn, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T); T == $% ->
  handle_call_identifier(Rest, Line, Column + 1, DotColumn, list_to_atom([T]), Scope, Tokens);

% ## Exception for .( as it needs to be treated specially in the parser
handle_dot([$.,$(|Rest], Line, Column, DotColumn, Scope, Tokens) ->
  tokenize([$(|Rest], Line, Column + 2, Scope, add_token_with_nl({dot_call_op, [Line, DotColumn, DotColumn + 1], '.'}, Tokens));

handle_dot([$.,H|T] = Original, Line, Column, DotColumn, Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column, Scope, true, T, H) of
    {NewLine, NewColumn, [Part], Rest} when is_binary(Part) ->
      case unsafe_to_atom(Part, Line, Scope) of
        {ok, Atom} ->
          Token = check_call_identifier(identifier, Line, Column, Atom, Rest),
          tokenize(Rest, NewLine, NewColumn, Scope, [Token|add_token_with_nl({'.', [Line, DotColumn, DotColumn + 1]}, Tokens)]);
        {error, Reason} ->
          {error, Reason, Original, Tokens}
      end;
    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for function name starting at line ~B)", [Line])
  end;

handle_dot([$.|Rest], Line, Column, DotColumn, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 1, Scope, add_token_with_nl({'.', [Line, DotColumn, DotColumn + 1]}, Tokens)).

handle_call_identifier(Rest, Line, Column, DotColumn, Op, Scope, Tokens) ->
  {_, [_, _, NewColumn], _} = Token = check_call_identifier(identifier, Line, Column, Op, Rest),
  tokenize(Rest, Line, NewColumn, Scope, [Token|add_token_with_nl({'.', [Line, DotColumn, DotColumn + 1]}, Tokens)]).

% ## Ambiguous unary/binary operators tokens
handle_space_sensitive_tokens([Sign, NotMarker|T], Line, Column, Scope, [{Identifier, _, _} = H|Tokens]) when
    ?dual_op(Sign),
    not(?is_space(NotMarker)),
    NotMarker /= $(, NotMarker /= $[, NotMarker /= $<, NotMarker /= ${,                  %% containers
    NotMarker /= $%, NotMarker /= $+, NotMarker /= $-, NotMarker /= $/, NotMarker /= $>, %% operators
    Identifier == identifier ->
  Rest = [NotMarker|T],
  tokenize(Rest, Line, Column + 1, Scope, [{dual_op, [Line, Column, Column + 1], list_to_atom([Sign])}, setelement(1, H, op_identifier)|Tokens]);

handle_space_sensitive_tokens(String, Line, Column, Scope, Tokens) ->
  tokenize(String, Line, Column, Scope, Tokens).

%% Helpers

eol(_Line, _Column, [{';',_}|_] = Tokens) -> Tokens;
eol(_Line, _Column, [{',',_}|_] = Tokens) -> Tokens;
eol(_Line, _Column, [{eol,_}|_] = Tokens) -> Tokens;
eol(Line, Column, Tokens) -> [{eol,[Line, Column, Column + 1]}|Tokens].

unsafe_to_atom(Part, Line, #elixir_tokenizer{}) when
    is_binary(Part) andalso size(Part) > 255;
    is_list(Part) andalso length(Part) > 255 ->
  {error, {Line, "atom length must be less than system limit", ":"}};
unsafe_to_atom(Binary, _Line, #elixir_tokenizer{existing_atoms_only=true}) when is_binary(Binary) ->
  {ok, binary_to_existing_atom(Binary, utf8)};
unsafe_to_atom(Binary, _Line, #elixir_tokenizer{}) when is_binary(Binary) ->
  {ok, binary_to_atom(Binary, utf8)};
unsafe_to_atom(List, _Line, #elixir_tokenizer{existing_atoms_only=true}) when is_list(List) ->
  {ok, list_to_existing_atom(List)};
unsafe_to_atom(List, _Line, #elixir_tokenizer{}) when is_list(List) ->
  {ok, list_to_atom(List)}.

collect_modifiers([H|T], Buffer) when ?is_downcase(H) or ?is_upcase(H) ->
  collect_modifiers(T, [H|Buffer]);

collect_modifiers(Rest, Buffer) ->
  {Rest, lists:reverse(Buffer)}.

%% Heredocs

extract_heredoc_with_interpolation(Line, Column, Scope, Interpol, T, H) ->
  case extract_heredoc(Line, Column, T, H) of
    {ok, NewLine, NewColumn, Body, Rest} ->
      case elixir_interpolation:extract(Line + 1, 0, Scope, Interpol, Body, 0) of
        {error, Reason} ->
          {error, interpolation_format(Reason, " (for heredoc starting at line ~B)", [Line])};
        {_, _, Parts, []} ->
          {ok, NewLine, NewColumn, Parts, Rest}
      end;
    {error, _} = Error ->
      Error
  end.

extract_heredoc(Line0, Column0, Rest0, Marker) ->
  case extract_heredoc_header(Rest0) of
    {ok, Rest1} ->
      %% We prepend a new line so we can transparently remove
      %% spaces later. This new line is removed by calling `tl`
      %% in the final heredoc body three lines below.
      case extract_heredoc_body(Line0, Column0, Marker, [$\n|Rest1], []) of
        {ok, Line1, Column1, Body, Rest2, Spaces} ->
          {ok, Line1, Column1, tl(remove_heredoc_spaces(Body, Spaces)), Rest2};
        {error, ErrorLine} ->
          Terminator = [Marker, Marker, Marker],
          Message = "missing terminator: ~ts (for heredoc starting at line ~B)",
          {error, {ErrorLine, io_lib:format(Message, [Terminator, Line0]), []}}
      end;
    error ->
      Message = "heredoc start must be followed by a new line after ",
      {error, {Line0, io_lib:format(Message, []), [Marker, Marker, Marker]}}
  end.

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

%% Extract the heredoc header.

extract_heredoc_header("\r\n" ++ Rest) ->
  {ok, Rest};
extract_heredoc_header("\n" ++ Rest) ->
  {ok, Rest};
extract_heredoc_header([H|T]) when ?is_horizontal_space(H) ->
  extract_heredoc_header(T);
extract_heredoc_header(_) ->
  error.

%% Extract heredoc body. It returns the heredoc body (in reverse order),
%% the remaining of the document and the number of spaces the heredoc
%% is aligned.

extract_heredoc_body(Line, _Column, Marker, Rest, Buffer) ->
  case extract_heredoc_line(Marker, Rest, Buffer, 0) of
    {ok, NewBuffer, NewRest} ->
      extract_heredoc_body(Line + 1, 0, Marker, NewRest, NewBuffer);
    {ok, NewBuffer, NewRest, Spaces} ->
      {ok, Line, length(NewBuffer), NewBuffer, NewRest, Spaces};
    {error, eof} ->
      {error, Line}
  end.

%% Extract a line from the heredoc prepending its contents to a buffer.

extract_heredoc_line("\r\n" ++ Rest, Buffer) ->
  {ok, [$\n|Buffer], Rest};
extract_heredoc_line("\n" ++ Rest, Buffer) ->
  {ok, [$\n|Buffer], Rest};
extract_heredoc_line([H|T], Buffer) ->
  extract_heredoc_line(T, [H|Buffer]);
extract_heredoc_line(_, _) ->
  {error, eof}.

%% Extract each heredoc line trying to find a match according to the marker.

extract_heredoc_line(Marker, [H|T], Buffer, Counter) when ?is_horizontal_space(H) ->
  extract_heredoc_line(Marker, T, [H|Buffer], Counter + 1);
extract_heredoc_line(Marker, [Marker,Marker,Marker|T], Buffer, Counter) ->
  {ok, Buffer, T, Counter};
extract_heredoc_line(_Marker, Rest, Buffer, _Counter) ->
  extract_heredoc_line(Rest, Buffer).

%% Integers and floats
%% At this point, we are at least sure the first digit is a number.

%% Check if we have a point followed by a number;
tokenize_number([$.,H|T], Acc, false) when ?is_digit(H) ->
  tokenize_number(T, [H,$.|Acc], true);

%% Check if we have an underscore followed by a number;
tokenize_number([$_,H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

%% Check if we have e- followed by numbers (valid only for floats);
tokenize_number([E,S,H|T], Acc, true)
    when (E == $E) or (E == $e), ?is_digit(H), S == $+ orelse S == $- ->
  tokenize_number(T, [H,S,$e|Acc], true);

%% Check if we have e followed by numbers (valid only for floats);
tokenize_number([E,H|T], Acc, true)
    when (E == $E) or (E == $e), ?is_digit(H) ->
  tokenize_number(T, [H,$e|Acc], true);

%% Finally just numbers.
tokenize_number([H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

%% Cast to float...
tokenize_number(Rest, Acc, true) ->
  {Rest, list_to_float(lists:reverse(Acc)), length(Acc)};

%% Or integer.
tokenize_number(Rest, Acc, false) ->
  {Rest, list_to_integer(lists:reverse(Acc)), length(Acc)}.

tokenize_hex([H|T], Acc) when ?is_hex(H) -> tokenize_hex(T, [H|Acc]);
tokenize_hex(Rest, Acc) -> {Rest, list_to_integer(lists:reverse(Acc), 16), length(Acc)}.

tokenize_octal([H|T], Acc) when ?is_octal(H) -> tokenize_octal(T, [H|Acc]);
tokenize_octal(Rest, Acc) -> {Rest, list_to_integer(lists:reverse(Acc), 8), length(Acc)}.

tokenize_bin([H|T], Acc) when ?is_bin(H) -> tokenize_bin(T, [H|Acc]);
tokenize_bin(Rest, Acc) -> {Rest, list_to_integer(lists:reverse(Acc), 2), length(Acc)}.

%% Comments

tokenize_comment("\r\n" ++ _ = Rest) -> Rest;
tokenize_comment("\n" ++ _ = Rest)   -> Rest;
tokenize_comment([_|Rest])           -> tokenize_comment(Rest);
tokenize_comment([])                 -> [].

%% Atoms
%% Handle atoms specially since they support @

tokenize_atom(T, Acc) ->
  tokenize_atom(T, Acc, 0).

tokenize_atom([H|T], Acc, Length) when ?is_atom(H) ->
  tokenize_atom(T, [H|Acc], Length + 1);

tokenize_atom([H|T], Acc, Length) when H == $?; H == $! ->
  {T, lists:reverse([H|Acc]), Length + 1};

tokenize_atom(Rest, Acc, Length) ->
  {Rest, lists:reverse(Acc), Length}.

%% Identifiers
%% At this point, the validity of the first character was already verified.

tokenize_identifier([H|T], Acc) when ?is_identifier(H) ->
  tokenize_identifier(T, [H|Acc]);

tokenize_identifier(Rest, Acc) ->
  {Rest, lists:reverse(Acc), length(Acc)}.

%% Tokenize any identifier, handling kv, punctuated, paren, bracket and do identifiers.

tokenize_any_identifier(Original, Line, Column, Scope, Tokens) ->
  {Rest, Identifier, Length} = tokenize_identifier(Original, []),

  {AllIdentifier, AllRest, _AllLength} =
    case Rest of
      [H|T] when H == $?; H == $! -> {Identifier ++ [H], T, Length + 1};
      _ -> {Identifier, Rest, Length}
    end,

  case unsafe_to_atom(AllIdentifier, Line, Scope) of
    {ok, Atom} ->
      tokenize_kw_or_other(AllRest, identifier, Line, Column, Atom, Tokens);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end.

tokenize_kw_or_other([$:,H|T], _Kind, Line, Column, Atom, _Tokens) when ?is_space(H) ->
  {identifier, [H|T], {kw_identifier, [Line, Column, Column + atom_length(Atom)], Atom}};

tokenize_kw_or_other([$:,H|T], _Kind, Line, _Column, Atom, Tokens) when ?is_atom_start(H); ?is_digit(H) ->
  Original = atom_to_list(Atom) ++ [$:],
  Reason   = {Line, "keyword argument must be followed by space after: ", Original},
  {error, Reason, Original ++ [H|T], Tokens};

tokenize_kw_or_other(Rest, Kind, Line, Column, Atom, Tokens) ->
  case check_keyword(Line, Column, Atom, Tokens) of
    nomatch ->
      {identifier, Rest, check_call_identifier(Kind, Line, Column, Atom, Rest)};
    {ok, [Check|T]} ->
      {keyword, Rest, Check, T};
    {error, Token} ->
      {error, {Line, "syntax error before: ", Token}, atom_to_list(Atom) ++ Rest, Tokens}
  end.

%% Check if it is a call identifier (paren | bracket | do)

check_call_identifier(_Kind, Line, Column, Atom, [$(|_]) ->
  {paren_identifier, [Line, Column, Column + atom_length(Atom)], Atom};
check_call_identifier(_Kind, Line, Column, Atom, [$[|_]) ->
  {bracket_identifier, [Line, Column, Column + atom_length(Atom)], Atom};
check_call_identifier(Kind, Line, Column, Atom, _Rest) ->
  {Kind, [Line, Column, Column + atom_length(Atom)], Atom}.

add_token_with_nl(Left, [{eol,_}|T]) -> [Left|T];
add_token_with_nl(Left, T) -> [Left|T].

%% Error handling

interpolation_error(Reason, Rest, Tokens, Extension, Args) ->
  {error, interpolation_format(Reason, Extension, Args), Rest, Tokens}.

interpolation_format({string, Line, Message, Token}, Extension, Args) ->
  {Line, io_lib:format("~ts" ++ Extension, [Message|Args]), Token};
interpolation_format({_, _, _} = Reason, _Extension, _Args) ->
  Reason.

%% Terminators

handle_terminator(Rest, Line, Column, Scope, Token, Tokens) ->
  case handle_terminator(Token, Scope) of
    {error, Reason} ->
      {error, Reason, atom_to_list(element(1, Token)) ++ Rest, Tokens};
    New ->
      tokenize(Rest, Line, Column, New, [Token|Tokens])
  end.

handle_terminator(_, #elixir_tokenizer{check_terminators=false} = Scope) ->
  Scope;
handle_terminator(Token, #elixir_tokenizer{terminators=Terminators} = Scope) ->
  case check_terminator(Token, Terminators) of
    {error, _} = Error -> Error;
    New -> Scope#elixir_tokenizer{terminators=New}
  end.

check_terminator({S, Line}, Terminators) when S == 'fn' ->
  [{fn, Line}|Terminators];

check_terminator({S, _} = New, Terminators) when
    S == 'do';
    S == '(';
    S == '[';
    S == '{';
    S == '<<' ->
  [New|Terminators];

check_terminator({E, _}, [{S, _}|Terminators]) when
    S == 'do', E == 'end';
    S == 'fn', E == 'end';
    S == '(',  E == ')';
    S == '[',  E == ']';
    S == '{',  E == '}';
    S == '<<', E == '>>' ->
  Terminators;

check_terminator({E, [Line, _, _]}, [{Start, [StartLine, _, _]}|_]) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  End = terminator(Start),
  Message = io_lib:format("\"~ts\" starting at line ~B is missing terminator \"~ts\". "
                          "Unexpected token: ", [Start, StartLine, End]),
  {error, {Line, Message, atom_to_list(E)}};

check_terminator({E, Line}, []) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  {error, {Line, "unexpected token: ", atom_to_list(E)}};

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

check_keyword(_Line, _Column, _Atom, [{'.', _}|_]) ->
  nomatch;
check_keyword(DoLine, DoColumn, do, [{Identifier, [Line, Column, EndColumn], Atom}|T]) when Identifier == identifier ->
  {ok, add_token_with_nl({do, [DoLine, DoColumn, DoColumn + 2]}, [{do_identifier, [Line, Column, EndColumn], Atom}|T])};
check_keyword(Line, Column, do, Tokens) ->
  case do_keyword_valid(Tokens) of
    true  -> {ok, add_token_with_nl({do, [Line, Column, Column + 2]}, Tokens)};
    false -> {error, "do"}
  end;
check_keyword(Line, Column, Atom, Tokens) ->
  case keyword(Atom) of
    false    -> nomatch;
    token    -> {ok, [{Atom, [Line, Column, Column + atom_length(Atom)]}|Tokens]};
    block    -> {ok, [{block_identifier, [Line, Column, Column + atom_length(Atom)], Atom}|Tokens]};
    unary_op -> {ok, [{unary_op, [Line, Column, Column + atom_length(Atom)], Atom}|Tokens]};
    Kind     -> {ok, add_token_with_nl({Kind, [Line, Column, Column + atom_length(Atom)], Atom}, Tokens)}
  end.

%% do is only valid after the end, true, false and nil keywords
do_keyword_valid([{Atom, _}|_]) ->
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
keyword('end')   -> token;
keyword('true')  -> token;
keyword('false') -> token;
keyword('nil')   -> token;

% Operators keywords
keyword('not')    -> unary_op;
keyword('and')    -> and_op;
keyword('or')     -> or_op;
keyword('when')   -> when_op;
keyword('in')     -> in_op;

% Block keywords
keyword('after')  -> block;
keyword('else')   -> block;
keyword('rescue') -> block;
keyword('catch')  -> block;

keyword(_) -> false.

atom_length(Atom) ->
  length(atom_to_list(Atom)).
