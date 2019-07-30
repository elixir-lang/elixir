-module(elixir_tokenizer).
-include("elixir.hrl").
-export([tokenize/1, tokenize/3, tokenize/4, invalid_do_error/1]).

%% Numbers
-define(is_hex(S), (?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f))).
-define(is_bin(S), (S >= $0 andalso S =< $1)).
-define(is_octal(S), (S >= $0 andalso S =< $7)).

%% Digits and letters
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).

%% Others
-define(is_quote(S), (S == $" orelse S == $')).
-define(is_sigil(S), ((S == $/) orelse (S == $<) orelse (S == $") orelse (S == $') orelse
                      (S == $[) orelse (S == $() orelse (S == ${) orelse (S == $|))).

%% Spaces
-define(is_horizontal_space(S), ((S == $\s) orelse (S == $\t))).
-define(is_vertical_space(S), ((S == $\r) orelse (S == $\n))).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).

%% Operators
-define(at_op(T),
  T == $@).

-define(capture_op(T),
  T == $&).

-define(unary_op(T),
  T == $!;
  T == $^).

-define(unary_op3(T1, T2, T3),
  T1 == $~, T2 == $~, T3 == $~).

-define(list_op(T1, T2),
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-).

-define(two_op(T1, T2),
  T1 == $<, T2 == $>;
  T1 == $., T2 == $.).

-define(three_op(T1, T2, T3),
  T1 == $^, T2 == $^, T3 == $^).

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
  IdentifierTokenizer =
    elixir_config:get(identifier_tokenizer, 'Elixir.String.Tokenizer'),

  Scope =
    lists:foldl(fun
      ({file, File}, Acc) when is_binary(File) ->
        Acc#elixir_tokenizer{file=File};
      ({existing_atoms_only, ExistingAtomsOnly}, Acc) when is_boolean(ExistingAtomsOnly) ->
        Acc#elixir_tokenizer{existing_atoms_only=ExistingAtomsOnly};
      ({static_atoms_encoder, StaticAtomsEncoder}, Acc) when is_function(StaticAtomsEncoder) ->
        Acc#elixir_tokenizer{static_atoms_encoder=StaticAtomsEncoder};
      ({check_terminators, CheckTerminators}, Acc) when is_boolean(CheckTerminators) ->
        Acc#elixir_tokenizer{check_terminators=CheckTerminators};
      ({preserve_comments, PreserveComments}, Acc) when is_function(PreserveComments) ->
        Acc#elixir_tokenizer{preserve_comments=PreserveComments};
      ({unescape, Unescape}, Acc) when is_boolean(Unescape) ->
        Acc#elixir_tokenizer{unescape=Unescape};
      ({warn_on_unnecessary_quotes, Unnecessary}, Acc) when is_boolean(Unnecessary) ->
        Acc#elixir_tokenizer{warn_on_unnecessary_quotes=Unnecessary};
      (_, Acc) ->
        Acc
    end, #elixir_tokenizer{identifier_tokenizer=IdentifierTokenizer}, Opts),

  tokenize(String, Line, Column, Scope, []).

tokenize(String, Line, Opts) ->
  tokenize(String, Line, 1, Opts).

tokenize([], _Line, _Column, #elixir_tokenizer{terminators=[]}, Tokens) ->
  {ok, lists:reverse(Tokens)};

tokenize([], EndLine, Column, Scope, Tokens) ->
  #elixir_tokenizer{terminators=[{Start, StartLine, _} | _]} = Scope,
  End = terminator(Start),
  Hint = missing_terminator_hint(Start, End, Scope),

  Message =
    io_lib:format("missing terminator: ~ts (for \"~ts\" starting at line ~B)", [End, Start, StartLine]),

  {error, {EndLine, Column, [Message, Hint], []}, [], Tokens};

% VC merge conflict

tokenize(("<<<<<<<" ++ _) = Original, Line, 1, _Scope, Tokens) ->
  FirstLine = lists:takewhile(fun(C) -> C =/= $\n andalso C =/= $\r end, Original),
  {error, {Line, 1, "found an unexpected version control marker, please resolve the conflicts: ", FirstLine}, Original, Tokens};

% Base integers

tokenize([$0, $x, H | T], Line, Column, Scope, Tokens) when ?is_hex(H) ->
  {Rest, Number, OriginalRepresentation, Length} = tokenize_hex(T, [H], 1),
  Token = {int, {Line, Column, Number}, OriginalRepresentation},
  tokenize(Rest, Line, Column + 2 + Length, Scope, [Token | Tokens]);

tokenize([$0, $b, H | T], Line, Column, Scope, Tokens) when ?is_bin(H) ->
  {Rest, Number, OriginalRepresentation, Length} = tokenize_bin(T, [H], 1),
  Token = {int, {Line, Column, Number}, OriginalRepresentation},
  tokenize(Rest, Line, Column + 2 + Length, Scope, [Token | Tokens]);

tokenize([$0, $o, H | T], Line, Column, Scope, Tokens) when ?is_octal(H) ->
  {Rest, Number, OriginalRepresentation, Length} = tokenize_octal(T, [H], 1),
  Token = {int, {Line, Column, Number}, OriginalRepresentation},
  tokenize(Rest, Line, Column + 2 + Length, Scope, [Token | Tokens]);

% Comments

tokenize([$# | String], Line, Column, Scope, Tokens) ->
  {Rest, Comment} = tokenize_comment(String, [$#]),
  preserve_comments(Line, Column, Tokens, Comment, Rest, Scope),
  tokenize(Rest, Line, Column, Scope, reset_eol(Tokens));

% Sigils

tokenize([$~, S, H, H, H | T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, ?is_downcase(S), T, H) of
    {ok, NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      Token = {sigil, {Line, Column, nil}, S, Parts, Modifiers, <<H, H, H>>},
      NewColumnWithModifiers = NewColumn + length(Modifiers),
      tokenize(Final, NewLine, NewColumnWithModifiers, Scope, [Token | Tokens]);

    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

tokenize([$~, S, H | T] = Original, Line, Column, Scope, Tokens) when ?is_sigil(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, Column + 3, Scope, ?is_downcase(S), T, sigil_terminator(H)) of
    {NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      Token = {sigil, {Line, Column, nil}, S, tokens_to_binary(Parts), Modifiers, <<H>>},
      NewColumnWithModifiers = NewColumn + length(Modifiers),
      tokenize(Final, NewLine, NewColumnWithModifiers, Scope, [Token | Tokens]);

    {error, Reason} ->
      Sigil = [$~, S, H],
      interpolation_error(Reason, Original, Tokens, " (for sigil ~ts starting at line ~B)", [Sigil, Line])
  end;

tokenize([$~, S, H | _] = Original, Line, Column, _Scope, Tokens) when ?is_upcase(S) orelse ?is_downcase(S) ->
  MessageString =
    "\"~ts\" (column ~p, code point U+~4.16.0B). The available delimiters are: "
    "//, ||, \"\", '', (), [], {}, <>",
  Message = io_lib:format(MessageString, [[H], Column + 2, H]),
  {error, {Line, Column, "invalid sigil delimiter: ", Message}, Original, Tokens};

% Char tokens

% We tokenize char literals (?a) as {char, _, CharInt} instead of {number, _,
% CharInt}. This is exactly what Erlang does with Erlang char literals
% ($a). This means we'll have to adjust the error message for char literals in
% elixir_errors.erl as by default {char, _, _} tokens are "hijacked" by Erlang
% and printed with Erlang syntax ($a) in the parser's error messages.

tokenize([$?, $\\, H | T], Line, Column, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  Token = {char, {Line, Column, [$?, $\\, H]}, Char},
  tokenize(T, Line, Column + 3, Scope, [Token | Tokens]);

tokenize([$?, Char | T], Line, Column, Scope, Tokens) ->
  case handle_char(Char) of
    {Escape, Name} ->
      Msg = io_lib:format("found ? followed by code point 0x~.16B (~ts), please use ?~ts instead",
                          [Char, Name, Escape]),
      elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, Msg);
    false ->
      ok
  end,
  Token = {char, {Line, Column, [$?, Char]}, Char},
  tokenize(T, Line, Column + 2, Scope, [Token | Tokens]);

% Heredocs

tokenize("\"\"\"" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $", Scope, Tokens);

tokenize("'''" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $', Scope, Tokens);

% Strings

tokenize([$" | T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column + 1, $", Scope, Tokens);
tokenize([$' | T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column + 1, $', Scope, Tokens);

% Operator atoms

tokenize("...:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, Column, nil}, '...'} | Tokens]);
tokenize("<<>>:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 5, Scope, [{kw_identifier, {Line, Column, nil}, '<<>>'} | Tokens]);
tokenize("%{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, Column, nil}, '%{}'} | Tokens]);
tokenize("%:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, {Line, Column, nil}, '%'} | Tokens]);
tokenize("{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 3, Scope, [{kw_identifier, {Line, Column, nil}, '{}'} | Tokens]);

tokenize(":..." ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, Column, nil}, '...'} | Tokens]);
tokenize(":<<>>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 5, Scope, [{atom, {Line, Column, nil}, '<<>>'} | Tokens]);
tokenize(":%{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, Column, nil}, '%{}'} | Tokens]);
tokenize(":%" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, {Line, Column, nil}, '%'} | Tokens]);
tokenize(":{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, {Line, Column, nil}, '{}'} | Tokens]);

% ## Three Token Operators
tokenize([$:, T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?three_op(T1, T2, T3) ->
  Token = {atom, {Line, Column, nil}, list_to_atom([T1, T2, T3])},
  tokenize(Rest, Line, Column + 4, Scope, [Token | Tokens]);

% ## Two Token Operators
tokenize([$:, T1, T2 | Rest], Line, Column, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?list_op(T1, T2);
    ?stab_op(T1, T2); ?type_op(T1, T2) ->
  Token = {atom, {Line, Column, nil}, list_to_atom([T1, T2])},
  tokenize(Rest, Line, Column + 3, Scope, [Token | Tokens]);

% ## Single Token Operators
tokenize([$:, T | Rest], Line, Column, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T); T == $. ->
  Token = {atom, {Line, Column, nil}, list_to_atom([T])},
  tokenize(Rest, Line, Column + 2, Scope, [Token | Tokens]);

% Stand-alone tokens

tokenize("..." ++ Rest, Line, Column, Scope, Tokens) ->
  maybe_warn_too_many_of_same_char("...", Rest, Line, Scope),
  Token = check_call_identifier(Line, Column, '...', Rest),
  tokenize(Rest, Line, Column + 3, Scope, [Token | Tokens]);

tokenize("=>" ++ Rest, Line, Column, Scope, Tokens) ->
  Token = {assoc_op, {Line, Column, previous_was_eol(Tokens)}, '=>'},
  tokenize(Rest, Line, Column + 2, Scope, add_token_with_eol(Token, Tokens));

% ## Three token operators
tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?unary_op3(T1, T2, T3) ->
  handle_unary_op(Rest, Line, Column, unary_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?comp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, comp_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?and_op3(T1, T2, T3) ->
  maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Scope),
  handle_op(Rest, Line, Column, and_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?or_op3(T1, T2, T3) ->
  maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Scope),
  handle_op(Rest, Line, Column, or_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?three_op(T1, T2, T3) ->
  maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Scope),
  handle_op(Rest, Line, Column, three_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?arrow_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, arrow_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Containers + punctuation tokens
tokenize([$, | Rest], Line, Column, Scope, Tokens) ->
  Token = {',', {Line, Column, 0}},
  tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

tokenize([$<, $< | Rest], Line, Column, Scope, Tokens) ->
  Token = {'<<', {Line, Column, nil}},
  handle_terminator(Rest, Line, Column + 2, Scope, Token, Tokens);

tokenize([$>, $> | Rest], Line, Column, Scope, Tokens) ->
  Token = {'>>', {Line, Column, previous_was_eol(Tokens)}},
  handle_terminator(Rest, Line, Column + 2, Scope, Token, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when T == $(; T == ${; T == $[ ->
  Token = {list_to_atom([T]), {Line, Column, nil}},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when T == $); T == $}; T == $] ->
  Token = {list_to_atom([T]), {Line, Column, previous_was_eol(Tokens)}},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

% ## Two Token Operators
tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?two_op(T1, T2) ->
  handle_op(Rest, Line, Column, two_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?list_op(T1, T2) ->
  maybe_warn_too_many_of_same_char([T1, T2], Rest, Line, Scope),
  handle_op(Rest, Line, Column, two_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?arrow_op(T1, T2) ->
  handle_op(Rest, Line, Column, arrow_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?comp_op2(T1, T2) ->
  handle_op(Rest, Line, Column, comp_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?rel_op2(T1, T2) ->
  handle_op(Rest, Line, Column, rel_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?and_op(T1, T2) ->
  handle_op(Rest, Line, Column, and_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?or_op(T1, T2) ->
  handle_op(Rest, Line, Column, or_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?in_match_op(T1, T2) ->
  handle_op(Rest, Line, Column, in_match_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?type_op(T1, T2) ->
  handle_op(Rest, Line, Column, type_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?stab_op(T1, T2) ->
  handle_op(Rest, Line, Column, stab_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?at_op(T) ->
  handle_unary_op(Rest, Line, Column, at_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?capture_op(T) ->
  handle_unary_op(Rest, Line, Column, capture_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?unary_op(T) ->
  handle_unary_op(Rest, Line, Column, unary_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?rel_op(T) ->
  handle_op(Rest, Line, Column, rel_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?dual_op(T) ->
  handle_unary_op(Rest, Line, Column, dual_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?mult_op(T) ->
  handle_op(Rest, Line, Column, mult_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?match_op(T) ->
  handle_op(Rest, Line, Column, match_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?pipe_op(T) ->
  handle_op(Rest, Line, Column, pipe_op, 1, list_to_atom([T]), Scope, Tokens);

% Non-operator Atoms

tokenize([$:, H | T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column + 2, Scope, true, T, H) of
    {NewLine, NewColumn, Parts, Rest} ->
      case is_unnecessary_quote(Parts, Scope) of
        true ->
          elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, io_lib:format(
            "found quoted atom \"~ts\" but the quotes are not required. "
            "Atoms made exclusively of Unicode letters, numbers, underscore, "
            "and @ do not require quotes",
            [hd(Parts)]
          ));

        false ->
          ok
      end,

      case unescape_tokens(Parts, Scope) of
        {ok, [Part]} when is_binary(Part) ->
          case unsafe_to_atom(Part, Line, Column, Scope) of
            {ok, Atom} ->
              Token = {atom, {Line, Column, nil}, Atom},
              tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);

            {error, Reason} ->
              {error, Reason, Rest, Tokens}
          end;

        {ok, Unescaped} ->
          Key = case Scope#elixir_tokenizer.existing_atoms_only of
            true  -> atom_safe;
            false -> atom_unsafe
          end,
          Token = {Key, {Line, Column, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);

        {error, Msg} ->
          {error, {Line, Column, Msg, [$:, H]}, Rest, Tokens}
      end;

    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for atom starting at line ~B)", [Line])
  end;

tokenize([$: | String] = Original, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Column, Scope) of
    {_Kind, Atom, Rest, Length, _Ascii, _Special} ->
      maybe_warn_for_ambiguous_bang_before_equals(atom, Atom, Rest, Scope, Line),
      Token = {atom, {Line, Column, nil}, Atom},
      tokenize(Rest, Line, Column + 1 + Length, Scope, [Token | Tokens]);
    empty ->
      unexpected_token(Original, Line, Column, Tokens);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

% Integers and floats
% We use int and flt otherwise elixir_parser won't format them
% properly in case of errors.

tokenize([H | T], Line, Column, Scope, Tokens) when ?is_digit(H) ->
  case tokenize_number(T, [H], 1, false) of
    {error, Reason, Number} ->
      {error, {Line, Column, Reason, Number}, T, Tokens};
    {Rest, Number, Original, Length} when is_integer(Number) ->
      Token = {int, {Line, Column, Number}, Original},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {Rest, Number, Original, Length} ->
      Token = {flt, {Line, Column, Number}, Original},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens])
  end;

% Spaces

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?is_horizontal_space(T) ->
  {Remaining, Stripped} = strip_horizontal_space(Rest, 0),
  handle_space_sensitive_tokens(Remaining, Line, Column + 1 + Stripped, Scope, Tokens);

% End of line

tokenize(";" ++ Rest, Line, Column, Scope, []) ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', {Line, Column, 0}}]);

tokenize(";" ++ Rest, Line, Column, Scope, [Top | _] = Tokens) when element(1, Top) /= ';' ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', {Line, Column, 0}} | Tokens]);

tokenize("\\" = Original, Line, Column, _Scope, Tokens) ->
  {error, {Line, Column, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" = Original, Line, Column, _Scope, Tokens) ->
  {error, {Line, Column, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\r\n" = Original, Line, Column, _Scope, Tokens) ->
  {error, {Line, Column, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, Tokens);

tokenize("\\\r\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, Tokens);

tokenize("\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, eol(Line, Column, Tokens));

tokenize("\r\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, eol(Line, Column, Tokens));

% Others

tokenize([$%, $[ | Rest], Line, Column, _Scope, Tokens) ->
  Reason = {Line, Column, "expected %{ to define a map, got: ", [$%, $[]},
  {error, Reason, Rest, Tokens};

tokenize([$%, ${ | T], Line, Column, Scope, Tokens) ->
  tokenize([${ | T], Line, Column + 1, Scope, [{'%{}', {Line, Column, nil}} | Tokens]);

tokenize([$% | T], Line, Column, Scope, Tokens) ->
  tokenize(T, Line, Column + 1, Scope, [{'%', {Line, Column, nil}} | Tokens]);

tokenize([$. | T], Line, Column, Scope, Tokens) ->
  DotInfo = {Line, Column, nil},
  {Rest, EndLine, EndColumn} = strip_dot_space(T, Line, Column + 1, [{'.', DotInfo}| Tokens], Scope),
  handle_dot([$. | Rest], EndLine, EndColumn, DotInfo, Scope, Tokens);

% Identifiers

tokenize(String, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Column, Scope) of
    {Kind, Atom, Rest, Length, Ascii, Special} ->
      HasAt = lists:member($@, Special),

      case Rest of
        [$: | T] when ?is_space(hd(T)) ->
          Token = {kw_identifier, {Line, Column, nil}, Atom},
          tokenize(T, Line, Column + Length + 1, Scope, [Token | Tokens]);

        [$: | T] when hd(T) /= $: ->
          AtomName = atom_to_list(Atom) ++ [$:],
          Reason = {Line, Column, "keyword argument must be followed by space after: ", AtomName},
          {error, Reason, String, Tokens};

        _ when HasAt ->
          Reason = {Line, Column, invalid_character_error(Kind, $@), atom_to_list(Atom)},
          {error, Reason, String, Tokens};

        _ when Kind == alias ->
          tokenize_alias(Rest, Line, Column, Atom, Length, Ascii, Special, Scope, Tokens);

        _ when Kind == identifier ->
          maybe_warn_for_ambiguous_bang_before_equals(identifier, Atom, Rest, Scope, Line),
          tokenize_other(Rest, Line, Column, Atom, Length, Scope, Tokens);

        _ ->
          unexpected_token(String, Line, Column, Tokens)
      end;

    empty ->
      unexpected_token(String, Line, Column, Tokens);

    {error, Reason} ->
      {error, Reason, String, Tokens}
  end.

unexpected_token([T | Rest], Line, Column, Tokens) ->
  Message = io_lib:format("\"~ts\" (column ~p, code point U+~4.16.0B)", [[T], Column, T]),
  {error, {Line, Column, "unexpected token: ", Message}, Rest, Tokens}.

tokenize_eol(Rest, Line, Scope, Tokens) ->
  {StrippedRest, Indentation} = strip_horizontal_space(Rest, 0),
  IndentedScope = Scope#elixir_tokenizer{indentation=Indentation},
  tokenize(StrippedRest, Line + 1, Indentation + 1, IndentedScope, Tokens).

strip_horizontal_space([H | T], Counter) when ?is_horizontal_space(H) ->
  strip_horizontal_space(T, Counter + 1);
strip_horizontal_space(T, Counter) ->
  {T, Counter}.

strip_dot_space(T, Line, Column, Tokens, Scope) ->
  case strip_horizontal_space(T, 0) of
    {"#" ++ R, _} ->
      {Rest, Comment} = tokenize_comment(R, [$#]),
      preserve_comments(Line, Column, Tokens, Comment, Rest, Scope),
      strip_dot_space(Rest, Line, 1, reset_eol(Tokens), Scope);
    {"\r\n" ++ Rest, _} ->
      strip_dot_space(Rest, Line + 1, 1, eol(Line, Column, Tokens), Scope);
    {"\n" ++ Rest, _} ->
      strip_dot_space(Rest, Line + 1, 1, eol(Line, Column, Tokens), Scope);
    {Rest, Length} ->
      {Rest, Line, Column + Length}
  end.

handle_char(0)   -> {"\\0", "null byte"};
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

%% Handlers

handle_heredocs(T, Line, Column, H, Scope, Tokens) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, true, T, H) of
    {ok, NewLine, NewColumn, Parts, Rest} ->
      case unescape_tokens(Parts, Scope) of
        {ok, Unescaped} ->
          Token = {heredoc_type(H), {Line, Column, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);

        {error, Msg} ->
          {error, {Line, Column, Msg, [H, H, H]}, Rest, Tokens}
      end;

    {error, Reason} ->
      {error, Reason, [H, H, H] ++ T, Tokens}
  end.

handle_strings(T, Line, Column, H, Scope, Tokens) ->
  case elixir_interpolation:extract(Line, Column, Scope, true, T, H) of
    {error, Reason} ->
      interpolation_error(Reason, [H | T], Tokens, " (for string starting at line ~B)", [Line]);

    {NewLine, NewColumn, Parts, [$: | Rest]} when ?is_space(hd(Rest)) ->
      case is_unnecessary_quote(Parts, Scope) of
        true ->
          elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, io_lib:format(
            "found quoted keyword \"~ts\" but the quotes are not required. "
            "Note that keywords are always atoms, even when quoted. "
            "Similar to atoms, keywords made exclusively of Unicode "
            "letters, numbers, underscore, and @ do not require quotes",
            [hd(Parts)]
          ));

        false ->
          ok
      end,

      case unescape_tokens(Parts, Scope) of
        {ok, Unescaped} ->
          Key = case Scope#elixir_tokenizer.existing_atoms_only of
            true  -> kw_identifier_safe;
            false -> kw_identifier_unsafe
          end,
          Token = {Key, {Line, Column - 1, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn + 1, Scope, [Token | Tokens]);

        {error, Msg} ->
          {error, {Line, Column, Msg, [H]}, Rest, Tokens}
      end;

    {NewLine, NewColumn, Parts, Rest} ->
      case unescape_tokens(Parts, Scope) of
        {ok, Unescaped} ->
          Token = {string_type(H), {Line, Column - 1, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);

        {error, Msg} ->
          {error, {Line, Column, Msg, [H]}, Rest, Tokens}
      end
  end.



handle_unary_op([$: | Rest], Line, Column, _Kind, Length, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  Token = {kw_identifier, {Line, Column, nil}, Op},
  tokenize(Rest, Line, Column + Length + 1, Scope, [Token | Tokens]);

handle_unary_op(Rest, Line, Column, Kind, Length, Op, Scope, Tokens) ->
  case strip_horizontal_space(Rest, 0) of
    {[$/ | _] = Remaining, Extra} ->
      Token = {identifier, {Line, Column, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, [Token | Tokens]);
    {Remaining, Extra} ->
      Token = {Kind, {Line, Column, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, [Token | Tokens])
  end.

handle_op([$: | Rest], Line, Column, _Kind, Length, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  Token = {kw_identifier, {Line, Column, nil}, Op},
  tokenize(Rest, Line, Column + Length + 1, Scope, [Token | Tokens]);

handle_op(Rest, Line, Column, Kind, Length, Op, Scope, Tokens) ->
  case strip_horizontal_space(Rest, 0) of
    {[$/ | _] = Remaining, Extra} ->
      Token = {identifier, {Line, Column, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, [Token | Tokens]);
    {Remaining, Extra} ->
      Token = {Kind, {Line, Column, previous_was_eol(Tokens)}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, add_token_with_eol(Token, Tokens))
  end.

% ## Three Token Operators
handle_dot([$., T1, T2, T3 | Rest], Line, Column, DotInfo, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?three_op(T1, T2, T3) ->
  handle_call_identifier(Rest, Line, Column, DotInfo, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Two Token Operators
handle_dot([$., T1, T2 | Rest], Line, Column, DotInfo, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?list_op(T1, T2); ?type_op(T1, T2) ->
  handle_call_identifier(Rest, Line, Column, DotInfo, 2, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators
handle_dot([$., T | Rest], Line, Column, DotInfo, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T) ->
  handle_call_identifier(Rest, Line, Column, DotInfo, 1, list_to_atom([T]), Scope, Tokens);

% ## Exception for .( as it needs to be treated specially in the parser
handle_dot([$., $( | Rest], Line, Column, DotInfo, Scope, Tokens) ->
  TokensSoFar = add_token_with_eol({dot_call_op, DotInfo, '.'}, Tokens),
  tokenize([$( | Rest], Line, Column, Scope, TokensSoFar);

handle_dot([$., H | T] = Original, Line, Column, DotInfo, Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column + 1, Scope, true, T, H) of
    {NewLine, NewColumn, [Part], Rest} when is_list(Part) ->
      case is_unnecessary_quote([Part], Scope) of
        true ->
          elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, io_lib:format(
            "found quoted call \"~ts\" but the quotes are not required. "
            "Calls made exclusively of Unicode letters, numbers, and underscore "
            "do not require quotes",
            [Part]
          ));

        false ->
          ok
      end,

      case unsafe_to_atom(Part, Line, Column, Scope) of
        {ok, Atom} ->
          Token = check_call_identifier(Line, Column, Atom, Rest),
          TokensSoFar = add_token_with_eol({'.', DotInfo}, Tokens),
          tokenize(Rest, NewLine, NewColumn, Scope, [Token | TokensSoFar]);

        {error, Reason} ->
          {error, Reason, Original, Tokens}
      end;
    {_NewLine, _NewColumn, _Parts, Rest} ->
        {error, {Line, Column, "interpolation is not allowed when calling function/macro. Found interpolation in a call starting with: ", [H]}, Rest, Tokens};
    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for function name starting at line ~B)", [Line])
  end;

handle_dot([$. | Rest], Line, Column, DotInfo, Scope, Tokens) ->
  TokensSoFar = add_token_with_eol({'.', DotInfo}, Tokens),
  tokenize(Rest, Line, Column, Scope, TokensSoFar).

handle_call_identifier(Rest, Line, Column, DotInfo, Length, Op, Scope, Tokens) ->
  Token = check_call_identifier(Line, Column, Op, Rest),
  TokensSoFar = add_token_with_eol({'.', DotInfo}, Tokens),
  tokenize(Rest, Line, Column + Length, Scope, [Token | TokensSoFar]).

% ## Ambiguous unary/binary operators tokens
handle_space_sensitive_tokens([Sign, NotMarker | T], Line, Column, Scope, [{Identifier, _, _} = H | Tokens]) when
    ?dual_op(Sign),
    not(?is_space(NotMarker)),
    NotMarker /= $(, NotMarker /= $[, NotMarker /= $<, NotMarker /= ${,                  %% containers
    NotMarker /= $%, NotMarker /= $+, NotMarker /= $-, NotMarker /= $/, NotMarker /= $>, %% operators
    Identifier == identifier ->
  Rest = [NotMarker | T],
  DualOpToken = {dual_op, {Line, Column, nil}, list_to_atom([Sign])},
  tokenize(Rest, Line, Column + 1, Scope, [DualOpToken, setelement(1, H, op_identifier) | Tokens]);

handle_space_sensitive_tokens(String, Line, Column, Scope, Tokens) ->
  tokenize(String, Line, Column, Scope, Tokens).

%% Helpers

eol(_Line, _Column, [{',', {Line, Column, Count}} | Tokens]) ->
  [{',', {Line, Column, Count + 1}} | Tokens];
eol(_Line, _Column, [{';', {Line, Column, Count}} | Tokens]) ->
  [{';', {Line, Column, Count + 1}} | Tokens];
eol(_Line, _Column, [{eol, {Line, Column, Count}} | Tokens]) ->
  [{eol, {Line, Column, Count + 1}} | Tokens];
eol(Line, Column, Tokens) ->
  [{eol, {Line, Column, 1}} | Tokens].

is_unnecessary_quote([Part], #elixir_tokenizer{warn_on_unnecessary_quotes=true} = Scope) when is_list(Part) ->
  case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(Part) of
    {identifier, _, [], _, _, _} -> true;
    _ -> false
  end;

is_unnecessary_quote(_Parts, _Scope) ->
  false.

unsafe_to_atom(Part, Line, Column, #elixir_tokenizer{}) when
    is_binary(Part) andalso byte_size(Part) > 255;
    is_list(Part) andalso length(Part) > 255 ->
  {error, {Line, Column, "atom length must be less than system limit: ", elixir_utils:characters_to_list(Part)}};
unsafe_to_atom(Part, Line, Column, #elixir_tokenizer{static_atoms_encoder=StaticAtomsEncoder} = Scope) when
    is_function(StaticAtomsEncoder) ->
  Metadata = [{line, Line}, {column, Column}, {file, Scope#elixir_tokenizer.file}],
  Value = elixir_utils:characters_to_binary(Part),
  case StaticAtomsEncoder(Value, Metadata) of
    {ok, Term} ->
      {ok, Term};
    {error, Reason} when is_binary(Reason) ->
      {error, {Line, Column, elixir_utils:characters_to_list(Reason) ++ ": ", elixir_utils:characters_to_list(Part)}}
  end;
unsafe_to_atom(Binary, Line, Column, #elixir_tokenizer{existing_atoms_only=true}) when is_binary(Binary) ->
  try
    {ok, binary_to_existing_atom(Binary, utf8)}
  catch
    error:badarg -> {error, {Line, Column, "unsafe atom does not exist: ", elixir_utils:characters_to_list(Binary)}}
  end;
unsafe_to_atom(Binary, _Line, _Column, #elixir_tokenizer{}) when is_binary(Binary) ->
  {ok, binary_to_atom(Binary, utf8)};
unsafe_to_atom(List, Line, Column, #elixir_tokenizer{existing_atoms_only=true}) when is_list(List) ->
  try
    {ok, list_to_existing_atom(List)}
  catch
    error:badarg -> {error, {Line, Column, "unsafe atom does not exist: ", List}}
  end;
unsafe_to_atom(List, _Line, _Column, #elixir_tokenizer{}) when is_list(List) ->
  {ok, list_to_atom(List)}.

collect_modifiers([H | T], Buffer) when ?is_downcase(H) or ?is_upcase(H) ->
  collect_modifiers(T, [H | Buffer]);

collect_modifiers(Rest, Buffer) ->
  {Rest, lists:reverse(Buffer)}.

%% Heredocs

extract_heredoc_with_interpolation(Line, Column, Scope, Interpol, T, H) ->
  case extract_heredoc(Line, Column, T, H, Scope) of
    {ok, NewLine, NewColumn, Body, Rest} ->
      case elixir_interpolation:extract(Line + 1, 1, Scope, Interpol, Body, none) of
        {error, Reason} ->
          {error, interpolation_format(Reason, " (for heredoc starting at line ~B)", [Line])};

        {_, _, Parts, []} ->
          {ok, NewLine, NewColumn, tokens_to_binary(Parts), Rest}
      end;

    {error, _} = Error ->
      Error
  end.

extract_heredoc(Line0, Column0, Rest0, Marker, Scope) ->
  case extract_heredoc_header(Rest0) of
    {ok, Rest1} ->
      %% We prepend a new line so we can transparently remove
      %% spaces later. This new line is removed by calling "tl"
      %% in the final heredoc body three lines below.
      case extract_heredoc_body(Line0, Column0, Marker, [$\n | Rest1], []) of
        {ok, Line1, Body, Rest2, Spaces} ->
          {ok, Line1, 4 + Spaces, tl(remove_heredoc_spaces(Body, Spaces, Marker, Scope)), Rest2};
        {error, Reason, ErrorLine, ErrorColumn} ->
          Terminator = [Marker, Marker, Marker],
          {Message, Token} = heredoc_error_message(Reason, Line0, Terminator),
          {error, {ErrorLine, ErrorColumn, Message, Token}}
      end;
    error ->
      Message = "heredoc allows only zero or more whitespace characters followed by a new line after ",
      {error, {Line0, Column0, io_lib:format(Message, []), [Marker, Marker, Marker]}}
  end.

heredoc_error_message(eof, Line, Terminator) ->
  {io_lib:format("missing terminator: ~ts (for heredoc starting at line ~B)",
                 [Terminator, Line]),
   []};
heredoc_error_message(badterminator, _Line, Terminator) ->
  {"invalid location for heredoc terminator, please escape token or move it to its own line: ",
   Terminator}.

%% Remove spaces from heredoc based on the position of the final quotes.

remove_heredoc_spaces(Body, Spaces, Marker, Scope) ->
  case trim_spaces(Body, [], Spaces, false) of
    {Acc, false} ->
      Acc;

    {Acc, Line} ->
      Msg = io_lib:format("outdented heredoc line. The contents inside the heredoc should be indented "
                          "at the same level as the closing ~ts. The following is forbidden:~n~n"
                          "    def text do~n"
                          "      \"\"\"~n"
                          "    contents~n"
                          "      \"\"\"~n"
                          "    end~n~n"
                          "Instead make sure the contents are indented as much as the heredoc closing:~n~n"
                          "    def text do~n"
                          "      \"\"\"~n"
                          "      contents~n"
                          "      \"\"\"~n"
                          "    end~n~n"
                          "The current heredoc line is indented too little", [[Marker, Marker, Marker]]),
      elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, Msg),
      Acc
  end.

trim_spaces([{Line, Entry} | Rest], Acc, Spaces, Warned) ->
  case trim_space(lists:reverse(Entry), Spaces) of
    {Trimmed, true} when Warned == false ->
      trim_spaces(Rest, Trimmed ++ Acc, Spaces, Line);
    {Trimmed, _} ->
      trim_spaces(Rest, Trimmed ++ Acc, Spaces, Warned)
  end;
trim_spaces([], Acc, _Spaces, Warned) ->
  {Acc, Warned}.

trim_space(Rest, 0) -> {Rest, false};
trim_space([$\n], _) -> {[$\n], false};
trim_space([H | T], Spaces) when ?is_horizontal_space(H) -> trim_space(T, Spaces - 1);
trim_space(Rest, _Spaces) -> {Rest, true}.

%% Extract the heredoc header.

extract_heredoc_header("\r\n" ++ Rest) ->
  {ok, Rest};
extract_heredoc_header("\n" ++ Rest) ->
  {ok, Rest};
extract_heredoc_header([H | T]) when ?is_horizontal_space(H) ->
  extract_heredoc_header(T);
extract_heredoc_header(_) ->
  error.

%% Extract heredoc body. It returns the heredoc body (in reverse order),
%% the remaining of the document and the number of spaces the heredoc
%% is aligned.

extract_heredoc_body(Line, Column, Marker, Rest, Buffer) ->
  case extract_heredoc_line(Marker, Rest, [], 0) of
    {ok, Entry, NewRest} ->
      extract_heredoc_body(Line + 1, 1, Marker, NewRest, [{Line, Entry} | Buffer]);
    {done, Entry, NewRest, Spaces} ->
      {ok, Line, [{Line, Entry} | Buffer], NewRest, Spaces};
    {error, Reason} ->
      {error, Reason, Line, Column}
  end.

%% Extract a line from the heredoc prepending its contents to a buffer.
%% Allow lazy escaping (e.g. \""")

extract_heredoc_line(Marker, [$\\, $\\ | T], Buffer) ->
  extract_heredoc_line(Marker, T, [$\\, $\\ | Buffer]);
extract_heredoc_line(Marker, [$\\, Marker | T], Buffer) ->
  extract_heredoc_line(Marker, T, [Marker, $\\ | Buffer]);
extract_heredoc_line(Marker, [Marker, Marker, Marker | _], _) ->
  {error, badterminator};
extract_heredoc_line(_, "\r\n" ++ Rest, Buffer) ->
  {ok, [$\n | Buffer], Rest};
extract_heredoc_line(_, "\n" ++ Rest, Buffer) ->
  {ok, [$\n | Buffer], Rest};
extract_heredoc_line(Marker, [H | T], Buffer) ->
  extract_heredoc_line(Marker, T, [H | Buffer]);
extract_heredoc_line(_, _, _) ->
  {error, eof}.

%% Extract each heredoc line trying to find a match according to the marker.

extract_heredoc_line(Marker, [H | T], Buffer, Counter) when ?is_horizontal_space(H) ->
  extract_heredoc_line(Marker, T, [H | Buffer], Counter + 1);
extract_heredoc_line(Marker, [Marker, Marker, Marker | T], Buffer, Counter) ->
  {done, Buffer, T, Counter};
extract_heredoc_line(Marker, Rest, Buffer, _Counter) ->
  extract_heredoc_line(Marker, Rest, Buffer).

unescape_tokens(Tokens, #elixir_tokenizer{unescape=true}) ->
  elixir_interpolation:unescape_tokens(Tokens);
unescape_tokens(Tokens, #elixir_tokenizer{unescape=false}) ->
  {ok, tokens_to_binary(Tokens)}.

tokens_to_binary(Tokens) ->
  [if is_list(Token) -> elixir_utils:characters_to_binary(Token); true -> Token end
   || Token <- Tokens].

%% Integers and floats
%% At this point, we are at least sure the first digit is a number.

%% Check if we have a point followed by a number;
tokenize_number([$., H | T], Acc, Length, false) when ?is_digit(H) ->
  tokenize_number(T, [H, $. | Acc], Length + 2, true);

%% Check if we have an underscore followed by a number;
tokenize_number([$_, H | T], Acc, Length, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H, $_ | Acc], Length + 2, Bool);

%% Check if we have e- followed by numbers (valid only for floats);
tokenize_number([E, S, H | T], Acc, Length, true)
    when (E == $E) or (E == $e), ?is_digit(H), S == $+ orelse S == $- ->
  tokenize_number(T, [H, S, E | Acc], Length + 3, true);

%% Check if we have e followed by numbers (valid only for floats);
tokenize_number([E, H | T], Acc, Length, true)
    when (E == $E) or (E == $e), ?is_digit(H) ->
  tokenize_number(T, [H, E | Acc], Length + 2, true);

%% Finally just numbers.
tokenize_number([H | T], Acc, Length, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H | Acc], Length + 1, Bool);

%% Cast to float...
tokenize_number(Rest, Acc, Length, true) ->
  try
    {Number, Original} = reverse_number(Acc, [], []),
    {Rest, list_to_float(Number), Original, Length}
  catch
    error:badarg -> {error, "invalid float number ", lists:reverse(Acc)}
  end;

%% Or integer.
tokenize_number(Rest, Acc, Length, false) ->
  {Number, Original} = reverse_number(Acc, [], []),
  {Rest, list_to_integer(Number), Original, Length}.

tokenize_hex([H | T], Acc, Length) when ?is_hex(H) ->
  tokenize_hex(T, [H | Acc], Length + 1);
tokenize_hex([$_, H | T], Acc, Length) when ?is_hex(H) ->
  tokenize_hex(T, [H, $_ | Acc], Length + 2);
tokenize_hex(Rest, Acc, Length) ->
  {Number, Original} = reverse_number(Acc, [], []),
  {Rest, list_to_integer(Number, 16), [$0, $x | Original], Length}.

tokenize_octal([H | T], Acc, Length) when ?is_octal(H) ->
  tokenize_octal(T, [H | Acc], Length + 1);
tokenize_octal([$_, H | T], Acc, Length) when ?is_octal(H) ->
  tokenize_octal(T, [H, $_ | Acc], Length + 2);
tokenize_octal(Rest, Acc, Length) ->
  {Number, Original} = reverse_number(Acc, [], []),
  {Rest, list_to_integer(Number, 8), [$0, $o | Original], Length}.

tokenize_bin([H | T], Acc, Length) when ?is_bin(H) ->
  tokenize_bin(T, [H | Acc], Length + 1);
tokenize_bin([$_, H | T], Acc, Length) when ?is_bin(H) ->
  tokenize_bin(T, [H, $_ | Acc], Length + 2);
tokenize_bin(Rest, Acc, Length) ->
  {Number, Original} = reverse_number(Acc, [], []),
  {Rest, list_to_integer(Number, 2), [$0, $b | Original], Length}.

reverse_number([$_ | T], Number, Original) ->
  reverse_number(T, Number, [$_ | Original]);
reverse_number([H | T], Number, Original) ->
  reverse_number(T, [H | Number], [H | Original]);
reverse_number([], Number, Original) ->
  {Number, Original}.

%% Comments

reset_eol([{eol, {Line, Column, _}} | Rest]) -> [{eol, {Line, Column, 0}} | Rest];
reset_eol(Rest) -> Rest.

tokenize_comment("\r\n" ++ _ = Rest, Acc) ->
  {Rest, lists:reverse(Acc)};
tokenize_comment("\n" ++ _ = Rest, Acc) ->
  {Rest, lists:reverse(Acc)};
tokenize_comment([H | Rest], Acc) ->
  tokenize_comment(Rest, [H | Acc]);
tokenize_comment([], Acc) ->
  {[], lists:reverse(Acc)}.

preserve_comments(Line, Column, Tokens, Comment, Rest, Scope) ->
  case Scope#elixir_tokenizer.preserve_comments of
    Fun when is_function(Fun) ->
      Fun(Line, Column, Tokens, Comment, Rest);
    nil ->
      ok
  end.

%% Identifiers

tokenize([H | T]) when ?is_upcase(H) ->
  {Acc, Rest, Length, Special} = tokenize_continue(T, [H], 1, []),
  {alias, lists:reverse(Acc), Rest, Length, true, Special};
tokenize([H | T]) when ?is_downcase(H); H == $_ ->
  {Acc, Rest, Length, Special} = tokenize_continue(T, [H], 1, []),
  {identifier, lists:reverse(Acc), Rest, Length, true, Special};
tokenize(_List) ->
  {error, empty}.

tokenize_continue([$@ | T], Acc, Length, Special) ->
  tokenize_continue(T, [$@ | Acc], Length + 1, [$@ | lists:delete($@, Special)]);
tokenize_continue([$! | T], Acc, Length, Special) ->
  {[$! | Acc], T, Length + 1, [$! | Special]};
tokenize_continue([$? | T], Acc, Length, Special) ->
  {[$? | Acc], T, Length + 1, [$? | Special]};
tokenize_continue([H | T], Acc, Length, Special) when ?is_upcase(H); ?is_downcase(H); ?is_digit(H); H == $_ ->
  tokenize_continue(T, [H | Acc], Length + 1, Special);
tokenize_continue(Rest, Acc, Length, Special) ->
  {Acc, Rest, Length, Special}.

tokenize_identifier(String, Line, Column, Scope) ->
  case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(String) of
    {Kind, Acc, Rest, Length, Ascii, Special} ->
      case unsafe_to_atom(Acc, Line, Column, Scope) of
        {ok, Atom} ->
          {Kind, Atom, Rest, Length, Ascii, Special};
        {error, _Reason} = Error ->
          Error
      end;
    {error, {not_nfc, Wrong}} ->
      Right = unicode:characters_to_nfc_list(Wrong),
      RightCodepoints = list_to_codepoint_hex(Right),
      WrongCodepoints = list_to_codepoint_hex(Wrong),
      Message = io_lib:format("Elixir expects unquoted Unicode atoms, variables, and calls to be in NFC form.\n\n"
                              "Got:\n\n    \"~ts\" (code points~ts)\n\n"
                              "Expected:\n\n    \"~ts\" (code points~ts)\n\n"
                              "Syntax error before: ",
                              [Wrong, WrongCodepoints, Right, RightCodepoints]),
      {error, {Line, Column, Message, Wrong}};
    {error, empty} ->
      empty
  end.

list_to_codepoint_hex(List) ->
  [io_lib:format(" 0x~4.16.0B", [Codepoint]) || Codepoint <- List].

tokenize_alias(Rest, Line, Column, Atom, Length, Ascii, Special, Scope, Tokens) ->
  if
    not Ascii ->
      AtomName = atom_to_list(Atom),
      Invalid = hd([C || C <- AtomName, C > 127]),
      Reason = {Line, Column, invalid_character_error("alias (only ASCII characters are allowed)", Invalid), AtomName},
      {error, Reason, AtomName ++ Rest, Tokens};
    Special /= [] ->
      AtomName = atom_to_list(Atom),
      Reason = {Line, Column, invalid_character_error("alias", hd(Special)), AtomName},
      {error, Reason, AtomName ++ Rest, Tokens};
    true ->
      AliasesToken = {alias, {Line, Column, nil}, Atom},
      tokenize(Rest, Line, Column + Length, Scope, [AliasesToken | Tokens])
  end.

tokenize_other(Rest, Line, Column, Atom, Length, Scope, Tokens) ->
  case tokenize_keyword_or_identifier(Rest, Line, Column, Atom, Tokens) of
    {keyword, NewRest, NewCheck, NewTokens} ->
      handle_terminator(NewRest, Line, Column + Length, Scope, NewCheck, NewTokens);
    {identifier, NewRest, NewTokens} ->
      tokenize(NewRest, Line, Column + Length, Scope, NewTokens);
    {error, _, _, _} = Error ->
      Error
  end.

tokenize_keyword_or_identifier(Rest, Line, Column, Atom, Tokens) ->
  case check_keyword(Line, Column, Atom, Tokens, Rest) of
    nomatch ->
      {identifier, Rest, [check_call_identifier(Line, Column, Atom, Rest) | Tokens]};
    {ok, [{in_op, _, in} | [{unary_op, NotInfo, 'not'} | T]]} ->
      {keyword, Rest, {in_op, NotInfo, 'not in'}, T};
    {ok, [Check | T]} ->
      {keyword, Rest, Check, T};
    {error, Message, Token} ->
      {error, {Line, Column, Message, Token}, atom_to_list(Atom) ++ Rest, Tokens}
  end.

%% Check if it is a call identifier (paren | bracket | do)

check_call_identifier(Line, Column, Atom, [$( | _]) ->
  {paren_identifier, {Line, Column, nil}, Atom};
check_call_identifier(Line, Column, Atom, [$[ | _]) ->
  {bracket_identifier, {Line, Column, nil}, Atom};
check_call_identifier(Line, Column, Atom, _Rest) ->
  {identifier, {Line, Column, nil}, Atom}.

add_token_with_eol({unary_op, _, _} = Left, T) -> [Left | T];
add_token_with_eol(Left, [{eol, _} | T]) -> [Left | T];
add_token_with_eol(Left, T) -> [Left | T].

previous_was_eol([{',', {_, _, Count}} | _]) when Count > 0 -> Count;
previous_was_eol([{';', {_, _, Count}} | _]) when Count > 0 -> Count;
previous_was_eol([{eol, {_, _, Count}} | _]) when Count > 0 -> Count;
previous_was_eol(_) -> nil.

%% Error handling

interpolation_error(Reason, Rest, Tokens, Extension, Args) ->
  {error, interpolation_format(Reason, Extension, Args), Rest, Tokens}.

interpolation_format({string, Line, Column, Message, Token}, Extension, Args) ->
  {Line, Column, [Message, io_lib:format(Extension, Args)], Token};
interpolation_format({_, _, _, _} = Reason, _Extension, _Args) ->
  Reason.

%% Terminators

handle_terminator(Rest, Line, Column, Scope, Token, Tokens) ->
  case handle_terminator(Token, Scope) of
    {error, Reason} ->
      {error, Reason, atom_to_list(element(1, Token)) ++ Rest, Tokens};
    New ->
      tokenize(Rest, Line, Column, New, [Token | Tokens])
  end.

handle_terminator(_, #elixir_tokenizer{check_terminators=false} = Scope) ->
  Scope;
handle_terminator(Token, #elixir_tokenizer{terminators=Terminators} = Scope) ->
  case check_terminator(Token, Terminators, Scope) of
    {error, _} = Error -> Error;
    NewScope -> NewScope
  end.

check_terminator({Start, {Line, _, _}}, Terminators, Scope)
    when Start == '('; Start == '['; Start == '{'; Start == '<<' ->
  Indentation = Scope#elixir_tokenizer.indentation,
  Scope#elixir_tokenizer{terminators=[{Start, Line, Indentation} | Terminators]};

check_terminator({Start, {Line, _, _}}, Terminators, Scope) when Start == 'fn'; Start == 'do' ->
  Indentation = Scope#elixir_tokenizer.indentation,

  NewScope =
    case Terminators of
      %% If the do is indented equally or less than the previous do, it may be a missing end error!
      [{Start, _, PreviousIndentation} = Previous | _] when Indentation =< PreviousIndentation ->
        Scope#elixir_tokenizer{mismatch_hints=[Previous | Scope#elixir_tokenizer.mismatch_hints]};

      _ ->
        Scope
    end,

  NewScope#elixir_tokenizer{terminators=[{Start, Line, Indentation} | Terminators]};

check_terminator({'end', {EndLine, _, _}}, [{'do', _, Indentation} | Terminators], Scope) ->
  NewScope =
    %% If the end is more indented than the do, it may be a missing do error!
    case Scope#elixir_tokenizer.indentation > Indentation of
      true ->
        Hint = {'end', EndLine, Scope#elixir_tokenizer.indentation},
        Scope#elixir_tokenizer{mismatch_hints=[Hint | Scope#elixir_tokenizer.mismatch_hints]};

      false ->
        Scope
    end,

  NewScope#elixir_tokenizer{terminators=Terminators};

check_terminator({End, _}, [{Start, _, _} | Terminators], Scope)
    when Start == 'fn', End == 'end';
         Start == '(',  End == ')';
         Start == '[',  End == ']';
         Start == '{',  End == '}';
         Start == '<<', End == '>>' ->
  Scope#elixir_tokenizer{terminators=Terminators};

check_terminator({End, {EndLine, EndColumn, _}}, [{Start, StartLine, _} | _], Scope)
    when End == 'end'; End == ')'; End == ']'; End == '}'; End == '>>' ->
  ExpectedEnd = terminator(Start),

  Suffix =
    [io_lib:format(". The \"~ts\" at line ~B is missing terminator \"~ts\"", [Start, StartLine, ExpectedEnd]),
     missing_terminator_hint(Start, ExpectedEnd, Scope)],

  {error, {EndLine, EndColumn, {"unexpected token: ", Suffix}, [atom_to_list(End)]}};

check_terminator({'end', {Line, Column, _}}, [], #elixir_tokenizer{mismatch_hints=Hints}) ->
  Suffix =
    case lists:keyfind('end', 1, Hints) of
      {'end', HintLine, _Identation} ->
        io_lib:format("\n\n    HINT: it looks like the \"end\" on line ~B "
                      "does not have a matching \"do\" defined before it\n", [HintLine]);
      false ->
        ""
    end,

  {error, {Line, Column, {"unexpected token: ", Suffix}, "end"}};

check_terminator({End, {Line, Column, _}}, [], _Scope)
    when End == ')'; End == ']'; End == '}'; End == '>>' ->
  {error, {Line, Column, "unexpected token: ", atom_to_list(End)}};

check_terminator(_, _, Scope) ->
  Scope.

missing_terminator_hint(Start, End, #elixir_tokenizer{mismatch_hints=Hints}) ->
  case lists:keyfind(Start, 1, Hints) of
    {Start, HintLine, _} ->
      io_lib:format("\n\n    HINT: it looks like the \"~ts\" on line ~B does not have a matching \"~ts\"\n",
                    [Start, HintLine, End]);
    false ->
      ""
  end.

string_type($") -> bin_string;
string_type($') -> list_string.

heredoc_type($") -> bin_heredoc;
heredoc_type($') -> list_heredoc.

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

check_keyword(_Line, _Column, _Atom, [{'.', _} | _], _Rest) ->
  nomatch;
check_keyword(DoLine, DoColumn, do, [{identifier, {Line, Column, Meta}, Atom} | T], _Rest) ->
  {ok, add_token_with_eol({do, {DoLine, DoColumn, nil}},
                          [{do_identifier, {Line, Column, Meta}, Atom} | T])};
check_keyword(_Line, _Column, do, [{'fn', _} | _], _Rest) ->
  {error, invalid_do_with_fn_error("unexpected token: "), "do"};
check_keyword(Line, Column, do, Tokens, _Rest) ->
  case do_keyword_valid(Tokens) of
    true  -> {ok, add_token_with_eol({do, {Line, Column, nil}}, Tokens)};
    false -> {error, invalid_do_error("unexpected token: "), "do"}
  end;
check_keyword(_Line, _Column, Atom, _Tokens, _Rest) when Atom == '__aliases__'; Atom == '__block__' ->
  {error, "reserved token: ", atom_to_list(Atom)};
check_keyword(Line, Column, Atom, Tokens, Rest) ->
  case keyword(Atom) of
    false ->
      nomatch;
    token ->
      {ok, [{Atom, {Line, Column, nil}} | Tokens]};
    block ->
      {ok, [{block_identifier, {Line, Column, nil}, Atom} | Tokens]};
    Kind ->
      case strip_horizontal_space(Rest, 0) of
        {[$/ | _], _} ->
          {ok, [{identifier, {Line, Column, nil}, Atom} | Tokens]};
        _ ->
          {ok, add_token_with_eol({Kind, {Line, Column, previous_was_eol(Tokens)}, Atom}, Tokens)}
      end
  end.

%% Fail early on invalid do syntax. For example, after
%% most keywords, after comma and so on.
do_keyword_valid([{Atom, _} | _]) ->
  case Atom of
    ','   -> false;
    ';'   -> false;
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

invalid_character_error(What, Char) ->
  io_lib:format("invalid character \"~ts\" (code point U+~4.16.0B) in ~ts: ", [[Char], Char, What]).

invalid_do_error(Prefix) ->
  {Prefix, ". In case you wanted to write a \"do\" expression, "
  "you must either use do-blocks or separate the keyword argument with comma. "
  "For example, you should either write:\n\n"
  "    if some_condition? do\n"
  "      :this\n"
  "    else\n"
  "      :that\n"
  "    end\n\n"
  "or the equivalent construct:\n\n"
  "    if(some_condition?, do: :this, else: :that)\n\n"
  "where \"some_condition?\" is the first argument and the second argument is a keyword list"}.

invalid_do_with_fn_error(Prefix) ->
  {Prefix, ". Anonymous functions are written as:\n\n"
  "    fn pattern -> expression end"}.

% TODO: Turn into an error on v2.0
maybe_warn_too_many_of_same_char([T | _] = Token, [T | _] = _Rest, Line, Scope) ->
  Warning =
    case T of
      $. -> "please use parens around \"...\" instead";
      _ -> io_lib:format("please use a space between \"~ts\" and the next \"~ts\"", [Token, [T]])
    end,
  Message = io_lib:format("found \"~ts\" followed by \"~ts\", ~ts", [Token, [T], Warning]),
  elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, Message);
maybe_warn_too_many_of_same_char(_Token, _Rest, _Line, _Scope) ->
  ok.

%% TODO: Turn into an error on v2.0
maybe_warn_for_ambiguous_bang_before_equals(Kind, Atom, [$= | _], Scope, Line) ->
  {What, Identifier} =
    case Kind of
      atom -> {"atom", [$: | atom_to_list(Atom)]};
      identifier -> {"identifier", atom_to_list(Atom)}
    end,

  case lists:last(Identifier) of
    Last when Last == $!; Last == $? ->
      Msg = io_lib:format("found ~ts \"~ts\", ending with \"~ts\", followed by =. "
                          "It is unclear if you mean \"~ts ~ts=\" or \"~ts =\". Please add "
                          "a space before or after ~ts to remove the ambiguity",
                          [What, Identifier, [Last], lists:droplast(Identifier), [Last], Identifier, [Last]]),
      elixir_errors:erl_warn(Line, Scope#elixir_tokenizer.file, Msg);
    _ ->
      ok
  end;
maybe_warn_for_ambiguous_bang_before_equals(_Kind, _Atom, _Rest, _Scope, _Line) ->
  ok.
