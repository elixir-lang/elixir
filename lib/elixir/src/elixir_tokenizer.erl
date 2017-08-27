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

-define(two_op(T1, T2),
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-;
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
    elixir_config:safe_get(identifier_tokenizer, 'Elixir.String.Tokenizer'),

  Scope =
    lists:foldl(fun
      ({file, File}, Acc) when is_binary(File) ->
        Acc#elixir_tokenizer{file=File};
      ({existing_atoms_only, ExistingAtomsOnly}, Acc) when is_boolean(ExistingAtomsOnly) ->
        Acc#elixir_tokenizer{existing_atoms_only=ExistingAtomsOnly};
      ({check_terminators, CheckTerminators}, Acc) when is_boolean(CheckTerminators) ->
        Acc#elixir_tokenizer{check_terminators=CheckTerminators};
      ({preserve_comments, PreserveComments}, Acc) when is_boolean(PreserveComments) ->
        Acc#elixir_tokenizer{preserve_comments=PreserveComments};
      ({unescape, Unescape}, Acc) when is_boolean(Unescape) ->
        Acc#elixir_tokenizer{unescape=Unescape};
      (_, Acc) ->
        Acc
    end, #elixir_tokenizer{identifier_tokenizer=IdentifierTokenizer}, Opts),

  tokenize(String, Line, Column, Scope, []).

tokenize(String, Line, Opts) ->
  tokenize(String, Line, 1, Opts).

tokenize([], Line, Column, #elixir_tokenizer{terminators=[]}, Tokens) ->
  {ok, Line, Column, lists:reverse(Tokens)};

tokenize([], EndLine, _Column, #elixir_tokenizer{terminators=[{Start, {StartLine, {_, _}, _}} | _]}, Tokens) ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~ts (for \"~ts\" starting at line ~B)", [End, Start, StartLine]),
  {error, {EndLine, Message, []}, [], Tokens};

% VC merge conflict

tokenize(("<<<<<<<" ++ _) = Original, Line, 1, _Scope, Tokens) ->
  FirstLine = lists:takewhile(fun(C) -> C =/= $\n andalso C =/= $\r end, Original),
  {error, {Line, "found an unexpected version control marker, please resolve the conflicts: ", FirstLine}, Original, Tokens};

% Base integers

tokenize([$0, $x, H | T], Line, Column, Scope, Tokens) when ?is_hex(H) ->
  {Rest, Number, OriginalRepresentation, Length} = tokenize_hex(T, [H], 1),
  Token = {int, {Line, {Column, Column + 2 + Length}, Number}, OriginalRepresentation},
  tokenize(Rest, Line, Column + 2 + Length, Scope, [Token | Tokens]);

tokenize([$0, $b, H | T], Line, Column, Scope, Tokens) when ?is_bin(H) ->
  {Rest, Number, OriginalRepresentation, Length} = tokenize_bin(T, [H], 1),
  Token = {int, {Line, {Column, Column + 2 + Length}, Number}, OriginalRepresentation},
  tokenize(Rest, Line, Column + 2 + Length, Scope, [Token | Tokens]);

tokenize([$0, $o, H | T], Line, Column, Scope, Tokens) when ?is_octal(H) ->
  {Rest, Number, OriginalRepresentation, Length} = tokenize_octal(T, [H], 1),
  Token = {int, {Line, {Column, Column + 2 + Length}, Number}, OriginalRepresentation},
  tokenize(Rest, Line, Column + 2 + Length, Scope, [Token | Tokens]);

% Comments

tokenize([$# | String], Line, Column, Scope, Tokens) ->
  {Rest, Comment, Length} = tokenize_comment(String, [$#], 1),
  case Scope#elixir_tokenizer.preserve_comments of
    true  ->
      CommentToken = {comment, {Line, {Column, Column + Length}, nil}, Comment},
      tokenize(Rest, Line, Column + Length, Scope, [CommentToken | Tokens]);
    false ->
      tokenize(Rest, Line, Column, Scope, Tokens)
  end;

% Sigils

tokenize([$~, S, H, H, H | T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, ?is_downcase(S), T, H) of
    {ok, NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      Token = {sigil, {Line, {Column, NewColumn}, nil}, S, Parts, Modifiers, <<H, H, H>>},
      tokenize(Final, NewLine, NewColumn, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

tokenize([$~, S, H | T] = Original, Line, Column, Scope, Tokens) when ?is_sigil(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, Column + 3, Scope, ?is_downcase(S), T, sigil_terminator(H)) of
    {NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      Token = {sigil, {Line, {Column, NewColumn}, nil}, S, Parts, Modifiers, <<H>>},
      tokenize(Final, NewLine, NewColumn, Scope, [Token | Tokens]);
    {error, Reason} ->
      Sigil = [$~, S, H],
      interpolation_error(Reason, Original, Tokens, " (for sigil ~ts starting at line ~B)", [Sigil, Line])
  end;

tokenize([$~, S, H | _] = Original, Line, Column, _Scope, Tokens) when ?is_upcase(S) orelse ?is_downcase(S) ->
  MessageString =
    "\"~ts\" (column ~p, codepoint U+~4.16.0B). The available delimiters are: "
    "//, ||, \"\", '', (), [], {}, <>",
  Message = io_lib:format(MessageString, [[H], Column + 2, H]),
  {error, {Line, "invalid sigil delimiter: ", Message}, Original, Tokens};

% Char tokens

% We tokenize char literals (?a) as {char, _, CharInt} instead of {number, _,
% CharInt}. This is exactly what Erlang does with Erlang char literals
% ($a). This means we'll have to adjust the error message for char literals in
% elixir_errors.erl as by default {char, _, _} tokens are "hijacked" by Erlang
% and printed with Erlang syntax ($a) in the parser's error messages.

tokenize([$?, $\\, H | T], Line, Column, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  Token = {int, {Line, {Column, Column + 3}, Char}, [$?, $\\, H]},
  tokenize(T, Line, Column + 3, Scope, [Token | Tokens]);

tokenize([$?, Char | T], Line, Column, Scope, Tokens) ->
  case handle_char(Char) of
    {Escape, Name} ->
      Msg = io_lib:format("found ? followed by codepoint 0x~.16B (~ts), please use ~ts instead",
                          [Char, Name, Escape]),
      elixir_errors:warn(Line, Scope#elixir_tokenizer.file, Msg);
    false ->
      ok
  end,
  Token = {int, {Line, {Column, Column + 2}, Char}, [$?, Char]},
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
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, {Column, Column + 4}, nil}, '...'} | Tokens]);
tokenize("<<>>:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 5, Scope, [{kw_identifier, {Line, {Column, Column + 5}, nil}, '<<>>'} | Tokens]);
tokenize("%{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, {Column, Column + 4}, nil}, '%{}'} | Tokens]);
tokenize("%:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, {Line, {Column, Column + 2}, nil}, '%'} | Tokens]);
tokenize("{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 3, Scope, [{kw_identifier, {Line, {Column, Column + 3}, nil}, '{}'} | Tokens]);

tokenize(":..." ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, {Column, Column + 4}, nil}, '...'} | Tokens]);
tokenize(":<<>>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 5, Scope, [{atom, {Line, {Column, Column + 5}, nil}, '<<>>'} | Tokens]);
tokenize(":%{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, {Column, Column + 4}, nil}, '%{}'} | Tokens]);
tokenize(":%" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, {Line, {Column, Column + 2}, nil}, '%'} | Tokens]);
tokenize(":{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, {Line, {Column, Column + 3}, nil}, '{}'} | Tokens]);

% ## Three Token Operators
tokenize([$:, T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?three_op(T1, T2, T3) ->
  Token = {atom, {Line, {Column, Column + 4}, nil}, list_to_atom([T1, T2, T3])},
  tokenize(Rest, Line, Column + 4, Scope, [Token | Tokens]);

% ## Two Token Operators
tokenize([$:, T1, T2 | Rest], Line, Column, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?stab_op(T1, T2);
    ?type_op(T1, T2) ->
  Token = {atom, {Line, {Column, Column + 3}, nil}, list_to_atom([T1, T2])},
  tokenize(Rest, Line, Column + 3, Scope, [Token | Tokens]);

% ## Single Token Operators
tokenize([$:, T | Rest], Line, Column, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T); T == $. ->
  Token = {atom, {Line, {Column, Column + 2}, nil}, list_to_atom([T])},
  tokenize(Rest, Line, Column + 2, Scope, [Token | Tokens]);

% Stand-alone tokens

tokenize("..." ++ Rest, Line, Column, Scope, Tokens) ->
  Token = check_call_identifier(Line, Column, 3, '...', Rest),
  tokenize(Rest, Line, Column + 3, Scope, [Token | Tokens]);

tokenize("=>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, add_token_with_nl({assoc_op, {Line, {Column, Column + 2}, nil}, '=>'}, Tokens));

% ## Three token operators
tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?unary_op3(T1, T2, T3) ->
  handle_unary_op(Rest, Line, Column, unary_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?comp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, comp_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?and_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, and_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?or_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, or_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?three_op(T1, T2, T3) ->
  handle_op(Rest, Line, Column, three_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?arrow_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, arrow_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Containers + punctuation tokens
tokenize([T, T | Rest], Line, Column, Scope, Tokens) when T == $<; T == $> ->
  Token = {list_to_atom([T, T]), {Line, {Column, Column + 2}, nil}},
  handle_terminator(Rest, Line, Column + 2, Scope, Token, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when T == $(;
    T == ${; T == $}; T == $[; T == $]; T == $); T == $, ->
  Token = {list_to_atom([T]), {Line, {Column, Column + 1}, nil}},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

% ## Two Token Operators
tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?two_op(T1, T2) ->
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
      Unescaped = unescape_tokens(Parts, Scope),
      Key = case Scope#elixir_tokenizer.existing_atoms_only of
        true  -> atom_safe;
        false -> atom_unsafe
      end,
      Token = {Key, {Line, {Column, NewColumn}, nil}, Unescaped},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);
    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for atom starting at line ~B)", [Line])
  end;

tokenize([$: | String] = Original, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Scope) of
    {_Kind, Atom, Rest, Length, _Ascii, _Special} ->
      Token = {atom, {Line, {Column, Column + 1 + Length}, nil}, Atom},
      tokenize(Rest, Line, Column + 1 + Length, Scope, [Token | Tokens]);
    empty ->
      unexpected_token(Original, Line, Column, Tokens);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

% Integers and floats

tokenize([H | T], Line, Column, Scope, Tokens) when ?is_digit(H) ->
  case tokenize_number(T, [H], 1, false) of
    {error, Reason, Number} ->
      {error, {Line, Reason, Number}, T, Tokens};
    {Rest, Number, Original, Length} when is_integer(Number) ->
      Token = {int, {Line, {Column, Column + Length}, Number}, Original},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {Rest, Number, Original, Length} ->
      Token = {float, {Line, {Column, Column + Length}, Number}, Original},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens])
  end;

% Spaces

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?is_horizontal_space(T) ->
  {Remaining, Stripped} = strip_horizontal_space(Rest),
  handle_space_sensitive_tokens(Remaining, Line, Column + 1 + Stripped, Scope, Tokens);

% End of line

tokenize(";" ++ Rest, Line, Column, Scope, []) ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', {Line, {Column, Column + 1}, nil}}]);

tokenize(";" ++ Rest, Line, Column, Scope, [Top | _] = Tokens) when element(1, Top) /= ';' ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', {Line, {Column, Column + 1}, nil}} | Tokens]);

tokenize("\\" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\r\n" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, Tokens);

tokenize("\\\r\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, Tokens);

tokenize("\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, eol(Line, Column, Tokens));

tokenize("\r\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, eol(Line, Column, Tokens));

% Others

tokenize([$%, ${ | T], Line, Column, Scope, Tokens) ->
  tokenize([${ | T], Line, Column + 1, Scope, [{'%{}', {Line, {Column, Column + 1}, nil}} | Tokens]);

tokenize([$% | T], Line, Column, Scope, Tokens) ->
  tokenize(T, Line, Column + 1, Scope, [{'%', {Line, {Column, Column + 1}, nil}} | Tokens]);

tokenize([$. | T], Line, Column, Scope, Tokens) ->
  {Rest, Counter, Offset, CommentTokens} = strip_dot_space(T, 0, Column + 1, Line, []),
  handle_dot([$. | Rest], Line, Offset - 1, Column, Scope, Tokens, CommentTokens, Counter);

% Identifiers

tokenize(String, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Scope) of
    {Kind, Atom, Rest, Length, Ascii, Special} ->
      HasAt = lists:member($@, Special),

      case Rest of
        [$: | T] when ?is_space(hd(T)) ->
          Token = {kw_identifier, {Line, {Column, Column + Length + 1}, nil}, Atom},
          tokenize(T, Line, Column + Length + 1, Scope, [Token | Tokens]);
        [$: | T] when hd(T) /= $: ->
          AtomName = atom_to_list(Atom) ++ [$:],
          Reason = {Line, "keyword argument must be followed by space after: ", AtomName},
          {error, Reason, String, Tokens};
        _ when HasAt ->
          Reason = {Line, invalid_character_error(Kind, $@), atom_to_list(Atom)},
          {error, Reason, String, Tokens};
        _ when Kind == alias ->
          tokenize_alias(Rest, Line, Column, Atom, Length, Ascii, Special, Scope, Tokens);
        _ when Kind == identifier ->
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
  Message = io_lib:format("\"~ts\" (column ~p, codepoint U+~4.16.0B)", [[T], Column, T]),
  {error, {Line, "unexpected token: ", Message}, Rest, Tokens}.

strip_horizontal_space(T) ->
  strip_horizontal_space(T, 0).

strip_horizontal_space([H | T], Counter) when ?is_horizontal_space(H) ->
  strip_horizontal_space(T, Counter + 1);
strip_horizontal_space(T, Counter) ->
  {T, Counter}.

strip_dot_space(T, Counter, Column, StartLine, Tokens) ->
  case strip_horizontal_space(T) of
    {"#" ++ R, _} ->
      {Rest, Comment, Length} = tokenize_comment(R, [$#], 1),
      CommentToken = {comment, {StartLine + Counter, {Column, Column + Length}, nil}, Comment},
      strip_dot_space(Rest, Counter, 1, StartLine, [CommentToken | Tokens]);
    {"\r\n" ++ Rest, _} ->
      strip_dot_space(Rest, Counter + 1, 1, StartLine, Tokens);
    {"\n" ++ Rest, _} ->
      strip_dot_space(Rest, Counter + 1, 1, StartLine, Tokens);
    {Rest, Length} ->
      {Rest, Counter, Column + Length, Tokens}
  end.

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
      Token = {heredoc_type(H), {Line, {Column, NewColumn}, nil}, unescape_tokens(Parts, Scope)},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, [H, H, H] ++ T, Tokens}
  end.

handle_strings(T, Line, Column, H, Scope, Tokens) ->
  case elixir_interpolation:extract(Line, Column, Scope, true, T, H) of
    {error, Reason} ->
      interpolation_error(Reason, [H | T], Tokens, " (for string starting at line ~B)", [Line]);
    {NewLine, NewColumn, Parts, [$: | Rest]} when ?is_space(hd(Rest)) ->
      Unescaped = unescape_tokens(Parts, Scope),
      Key = case Scope#elixir_tokenizer.existing_atoms_only of
        true  -> kw_identifier_safe;
        false -> kw_identifier_unsafe
      end,
      Token = {Key, {Line, {Column - 1, NewColumn}, nil}, Unescaped},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);
    {NewLine, NewColumn, Parts, Rest} ->
      Token = {string_type(H), {Line, {Column - 1, NewColumn}, nil}, unescape_tokens(Parts, Scope)},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens])
  end.

handle_unary_op([$: | Rest], Line, Column, _Kind, Length, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  Token = {kw_identifier, {Line, {Column, Column + Length}, nil}, Op},
  tokenize(Rest, Line, Column + Length + 1, Scope, [Token | Tokens]);

handle_unary_op(Rest, Line, Column, Kind, Length, Op, Scope, Tokens) ->
  case strip_horizontal_space(Rest) of
    {[$/ | _] = Remaining, Extra} ->
      Token = {identifier, {Line, {Column, Column + Length}, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, [Token | Tokens]);
    {Remaining, Extra} ->
      Token = {Kind, {Line, {Column, Column + Length}, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, [Token | Tokens])
  end.

handle_op([$: | Rest], Line, Column, _Kind, Length, Op, Scope, Tokens) when ?is_space(hd(Rest)) ->
  Token = {kw_identifier, {Line, {Column, Column + Length}, nil}, Op},
  tokenize(Rest, Line, Column + Length + 1, Scope, [Token | Tokens]);

handle_op(Rest, Line, Column, Kind, Length, Op, Scope, Tokens) ->
  case strip_horizontal_space(Rest) of
    {[$/ | _] = Remaining, Extra} ->
      Token = {identifier, {Line, {Column, Column + Length}, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, [Token | Tokens]);
    {Remaining, Extra} ->
      Token = {Kind, {Line, {Column, Column + Length}, nil}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, Scope, add_token_with_nl(Token, Tokens))
  end.

handle_comments(CommentTokens, Tokens, Scope) ->
  case Scope#elixir_tokenizer.preserve_comments of
    true  -> lists:append(CommentTokens, Tokens);
    false -> Tokens
  end.

% ## Three Token Operators
handle_dot([$., T1, T2, T3 | Rest], Line, Column, DotColumn, Scope, Tokens, CommentTokens, Counter) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?three_op(T1, T2, T3) ->
  handle_call_identifier(Rest, Line, Column + 1, DotColumn, 3, list_to_atom([T1, T2, T3]), Scope, Tokens, CommentTokens, Counter);

% ## Two Token Operators
handle_dot([$., T1, T2 | Rest], Line, Column, DotColumn, Scope, Tokens, CommentTokens, Counter) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?stab_op(T1, T2);
    ?type_op(T1, T2) ->
  handle_call_identifier(Rest, Line, Column + 1, DotColumn, 2, list_to_atom([T1, T2]), Scope, Tokens, CommentTokens, Counter);

% ## Single Token Operators
handle_dot([$., T | Rest], Line, Column, DotColumn, Scope, Tokens, CommentTokens, Counter) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T) ->
  handle_call_identifier(Rest, Line, Column + 1, DotColumn, 1, list_to_atom([T]), Scope, Tokens, CommentTokens, Counter);

% ## Exception for .( as it needs to be treated specially in the parser
handle_dot([$., $( | Rest], Line, Column, DotColumn, Scope, Tokens, CommentTokens, Counter) ->
  TokensSoFar = add_token_with_nl({dot_call_op, {Line, {DotColumn, DotColumn + 1}, nil}, '.'}, Tokens),
  tokenize([$( | Rest], Line + Counter, Column + 2, Scope, handle_comments(CommentTokens, TokensSoFar, Scope));

handle_dot([$., H | T] = Original, Line, Column, DotColumn, Scope, Tokens, CommentTokens, Counter) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column + 2, Scope, true, T, H) of
    {NewLine, NewColumn, [Part], Rest} when is_binary(Part) ->
      case unsafe_to_atom(Part, Line, Scope) of
        {ok, Atom} ->
          Token = check_call_identifier(Line + Counter, Column, max(NewColumn - Column, 0), Atom, Rest),
          TokensSoFar = add_token_with_nl({'.', {Line, {DotColumn, DotColumn + 1}, nil}}, Tokens),
          tokenize(Rest, NewLine, NewColumn, Scope, [Token | handle_comments(CommentTokens, TokensSoFar, Scope)]);
        {error, Reason} ->
          {error, Reason, Original, Tokens}
      end;
    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for function name starting at line ~B)", [Line])
  end;

handle_dot([$. | Rest], Line, Column, DotColumn, Scope, Tokens, CommentTokens, Counter) ->
  TokensSoFar = add_token_with_nl({'.', {Line, {DotColumn, DotColumn + 1}, nil}}, Tokens),
  tokenize(Rest, Line + Counter, Column + 1, Scope, handle_comments(CommentTokens, TokensSoFar, Scope)).

handle_call_identifier(Rest, Line, Column, DotColumn, Length, Op, Scope, Tokens, CommentTokens, Counter) ->
  {_, {NewLine, {_, NewColumn}, _}, _} = Token = check_call_identifier(Line + Counter, Column, Length, Op, Rest),
  TokensSoFar = add_token_with_nl({'.', {Line, {DotColumn, DotColumn + 1}, nil}}, Tokens),
  tokenize(Rest, NewLine, NewColumn, Scope, [Token | handle_comments(CommentTokens, TokensSoFar, Scope)]).

% ## Ambiguous unary/binary operators tokens
handle_space_sensitive_tokens([Sign, NotMarker | T], Line, Column, Scope, [{Identifier, _, _} = H | Tokens]) when
    ?dual_op(Sign),
    not(?is_space(NotMarker)),
    NotMarker /= $(, NotMarker /= $[, NotMarker /= $<, NotMarker /= ${,                  %% containers
    NotMarker /= $%, NotMarker /= $+, NotMarker /= $-, NotMarker /= $/, NotMarker /= $>, %% operators
    Identifier == identifier ->
  Rest = [NotMarker | T],
  DualOpToken = {dual_op, {Line, {Column, Column + 1}, nil}, list_to_atom([Sign])},
  tokenize(Rest, Line, Column + 1, Scope, [DualOpToken, setelement(1, H, op_identifier) | Tokens]);

handle_space_sensitive_tokens(String, Line, Column, Scope, Tokens) ->
  tokenize(String, Line, Column, Scope, Tokens).

%% Helpers

eol(_Line, _Column, [{';', _} | _] = Tokens) -> Tokens;
eol(_Line, _Column, [{',', _} | _] = Tokens) -> Tokens;
eol(_Line, _Column, [{eol, _} | _] = Tokens) -> Tokens;
eol(Line, Column, Tokens) -> [{eol, {Line, {Column, Column + 1}, nil}} | Tokens].

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

collect_modifiers([H | T], Buffer) when ?is_downcase(H) or ?is_upcase(H) ->
  collect_modifiers(T, [H | Buffer]);

collect_modifiers(Rest, Buffer) ->
  {Rest, lists:reverse(Buffer)}.

%% Heredocs

extract_heredoc_with_interpolation(Line, Column, Scope, Interpol, T, H) ->
  case extract_heredoc(Line, Column, T, H) of
    {ok, NewLine, NewColumn, Body, Rest} ->
      case elixir_interpolation:extract(Line + 1, 1, Scope, Interpol, Body, 0) of
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
      %% spaces later. This new line is removed by calling "tl"
      %% in the final heredoc body three lines below.
      case extract_heredoc_body(Line0, Column0, Marker, [$\n | Rest1], []) of
        {ok, Line1, Body, Rest2, Spaces} ->
          {ok, Line1, 1, tl(remove_heredoc_spaces(Body, Spaces)), Rest2};
        {error, Reason, ErrorLine} ->
          Terminator = [Marker, Marker, Marker],
          {Message, Token} = heredoc_error_message(Reason, Line0, Terminator),
          {error, {ErrorLine, Message, Token}}
      end;
    error ->
      Message = "heredoc start must be followed by a new line after ",
      {error, {Line0, io_lib:format(Message, []), [Marker, Marker, Marker]}}
  end.

heredoc_error_message(eof, Line, Terminator) ->
  {io_lib:format("missing terminator: ~ts (for heredoc starting at line ~B)",
                 [Terminator, Line]),
   []};
heredoc_error_message(misplacedterminator, _Line, Terminator) ->
  {"invalid location for heredoc terminator, please escape token or move it to its own line: ",
   Terminator}.
%% Remove spaces from heredoc based on the position of the final quotes.

remove_heredoc_spaces(Body, 0) ->
  lists:reverse([0 | Body]);
remove_heredoc_spaces(Body, Spaces) ->
  remove_heredoc_spaces([0 | Body], [], Spaces, Spaces).
remove_heredoc_spaces([H, $\n | T], [Backtrack | Buffer], Spaces, Original) when Spaces > 0, ?is_horizontal_space(H) ->
  remove_heredoc_spaces([Backtrack, $\n | T], Buffer, Spaces - 1, Original);
remove_heredoc_spaces([$\n=H | T], Buffer, _Spaces, Original) ->
  remove_heredoc_spaces(T, [H | Buffer], Original, Original);
remove_heredoc_spaces([H | T], Buffer, Spaces, Original) ->
  remove_heredoc_spaces(T, [H | Buffer], Spaces, Original);
remove_heredoc_spaces([], Buffer, _Spaces, _Original) ->
  Buffer.

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

extract_heredoc_body(Line, _Column, Marker, Rest, Buffer) ->
  case extract_heredoc_line(Marker, Rest, Buffer, 0) of
    {ok, NewBuffer, NewRest} ->
      extract_heredoc_body(Line + 1, 1, Marker, NewRest, NewBuffer);
    {ok, NewBuffer, NewRest, Spaces} ->
      {ok, Line, NewBuffer, NewRest, Spaces};
    {error, Reason} ->
      {error, Reason, Line}
  end.

%% Extract a line from the heredoc prepending its contents to a buffer.
%% Allow lazy escaping (e.g. \""")

extract_heredoc_line(Marker, [$\\, $\\ | T], Buffer) ->
  extract_heredoc_line(Marker, T, [$\\, $\\ | Buffer]);
extract_heredoc_line(Marker, [$\\, Marker | T], Buffer) ->
  extract_heredoc_line(Marker, T, [Marker, $\\ | Buffer]);
extract_heredoc_line(Marker, [Marker, Marker, Marker | _], _) ->
  {error, misplacedterminator};
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
  {ok, Buffer, T, Counter};
extract_heredoc_line(Marker, Rest, Buffer, _Counter) ->
  extract_heredoc_line(Marker, Rest, Buffer).

unescape_tokens(Tokens, #elixir_tokenizer{unescape=true}) ->
  elixir_interpolation:unescape_tokens(Tokens);
unescape_tokens(Tokens, #elixir_tokenizer{unescape=false}) ->
  Tokens.

%% Integers and floats
%% At this point, we are at least sure the first digit is a number.

%% Check if we have a point followed by a number;
tokenize_number([$., H | T], Acc, Length, false) when ?is_digit(H) ->
  tokenize_number(T, [H, $. | Acc], Length + 2, true);

%% Check if we have an underscore followed by a number;
tokenize_number([$_, H | T], Acc, Length, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H | Acc], Length + 2, Bool);

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
    Digits = lists:reverse(Acc),
    {Rest, list_to_float(Digits), Digits, Length}
  catch
    error:badarg -> {error, "invalid float number ", lists:reverse(Acc)}
  end;

%% Or integer.
tokenize_number(Rest, Acc, Length, false) ->
  Digits = lists:reverse(Acc),
  {Rest, list_to_integer(Digits), Digits, Length}.

tokenize_hex([H | T], Acc, Length) when ?is_hex(H) ->
  tokenize_hex(T, [H | Acc], Length + 1);
tokenize_hex([$_, H | T], Acc, Length) when ?is_hex(H) ->
  tokenize_hex(T, [H | Acc], Length + 2);
tokenize_hex(Rest, Acc, Length) ->
  Digits = lists:reverse(Acc),
  {Rest, list_to_integer(Digits, 16), [$0, $x | Digits], Length}.

tokenize_octal([H | T], Acc, Length) when ?is_octal(H) ->
  tokenize_octal(T, [H | Acc], Length + 1);
tokenize_octal([$_, H | T], Acc, Length) when ?is_octal(H) ->
  tokenize_octal(T, [H | Acc], Length + 2);
tokenize_octal(Rest, Acc, Length) ->
  Digits = lists:reverse(Acc),
  {Rest, list_to_integer(Digits, 8), [$0, $o | Digits], Length}.

tokenize_bin([H | T], Acc, Length) when ?is_bin(H) ->
  tokenize_bin(T, [H | Acc], Length + 1);
tokenize_bin([$_, H | T], Acc, Length) when ?is_bin(H) ->
  tokenize_bin(T, [H | Acc], Length + 2);
tokenize_bin(Rest, Acc, Length) ->
  Digits = lists:reverse(Acc),
  {Rest, list_to_integer(Digits, 2), [$0, $b | Digits], Length}.

%% Comments

tokenize_comment("\r\n" ++ _ = Rest, Acc, Length) ->
  {Rest, lists:reverse(Acc), Length};
tokenize_comment("\n" ++ _ = Rest, Acc, Length) ->
  {Rest, lists:reverse(Acc), Length};
tokenize_comment([H | Rest], Acc, Length) ->
  tokenize_comment(Rest, [H | Acc], Length + 1);
tokenize_comment([], Acc, Length) ->
  {[], lists:reverse(Acc), Length}.

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

tokenize_identifier(String, Line, Scope) ->
  case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(String) of
    {Kind, Acc, Rest, Length, Ascii, Special} ->
      case unsafe_to_atom(Acc, Line, Scope) of
        {ok, Atom} ->
          {Kind, Atom, Rest, Length, Ascii, Special};
        {error, _Reason} = Error ->
          Error
      end;
    {error, {not_nfc, Wrong}} ->
      Right = unicode:characters_to_nfc_list(Wrong),
      RightCodepoints = list_to_codepoint_hex(Right),
      WrongCodepoints = list_to_codepoint_hex(Wrong),
      Message = io_lib:format("Elixir expects unquoted Unicode atoms and variables to be in NFC form.\n\n"
                              "Got:\n\n    \"~ts\" (codepoints~ts)\n\n"
                              "Expected:\n\n    \"~ts\" (codepoints~ts)\n\n"
                              "Syntax error before: ",
                              [Wrong, WrongCodepoints, Right, RightCodepoints]),
      {error, {Line, Message, Wrong}};
    {error, empty} ->
      empty
  end.

list_to_codepoint_hex(List) ->
  [io_lib:format(" ~4.16.0B", [Codepoint]) || Codepoint <- List].

tokenize_alias(Rest, Line, Column, Atom, Length, Ascii, Special, Scope, Tokens) ->
  if
    not Ascii ->
      AtomName = atom_to_list(Atom),
      Invalid = hd([C || C <- AtomName, C > 127]),
      Reason = {Line, invalid_character_error("alias (only ascii characters are allowed)", Invalid), AtomName},
      {error, Reason, AtomName ++ Rest, Tokens};
    Special /= [] ->
      AtomName = atom_to_list(Atom),
      Reason = {Line, invalid_character_error("alias", hd(Special)), AtomName},
      {error, Reason, AtomName ++ Rest, Tokens};
    true ->
      AliasesToken = {aliases, {Line, {Column, Column + Length}, nil}, [Atom]},
      tokenize(Rest, Line, Column + Length, Scope, [AliasesToken | Tokens])
  end.

tokenize_other(Rest, Line, Column, Atom, Length, Scope, Tokens) ->
  case tokenize_keyword_or_identifier(Rest, Line, Column, Length, Atom, Tokens) of
    {keyword, Rest, {_, {_, {_, EndColumn}, _}} = Check, T} ->
      handle_terminator(Rest, Line, EndColumn, Scope, Check, T);
    {keyword, Rest, {_, {_, {_, EndColumn}, _}, _} = Check, T} ->
      handle_terminator(Rest, Line, EndColumn, Scope, Check, T);
    {identifier, Rest, {_, {_, {_, EndColumn}, _}, _} = Token} ->
      tokenize(Rest, Line, EndColumn, Scope, [Token | Tokens]);
    {error, _, _, _} = Error ->
      Error
  end.

tokenize_keyword_or_identifier(Rest, Line, Column, Length, Atom, Tokens) ->
  case check_keyword(Line, Column, Length, Atom, Tokens, Rest) of
    nomatch ->
      {identifier, Rest, check_call_identifier(Line, Column, Length, Atom, Rest)};
    {ok, [{in_op, {_, {_, InEndColumn}, _}, in} | [{unary_op, {NotLine, {NotColumn, _}, _}, 'not'} | T]]} ->
      {keyword, Rest, {in_op, {NotLine, {NotColumn, InEndColumn}, nil}, 'not in'}, T};
    {ok, [Check | T]} ->
      {keyword, Rest, Check, T};
    {error, Message, Token} ->
      {error, {Line, Message, Token}, atom_to_list(Atom) ++ Rest, Tokens}
  end.

%% Check if it is a call identifier (paren | bracket | do)

check_call_identifier(Line, Column, Length, Atom, [$( | _]) ->
  {paren_identifier, {Line, {Column, Column + Length}, nil}, Atom};
check_call_identifier(Line, Column, Length, Atom, [$[ | _]) ->
  {bracket_identifier, {Line, {Column, Column + Length}, nil}, Atom};
check_call_identifier(Line, Column, Length, Atom, _Rest) ->
  {identifier, {Line, {Column, Column + Length}, nil}, Atom}.

add_token_with_nl({unary_op, _, _} = Left, T) -> [Left | T];
add_token_with_nl(Left, [{eol, _} | T]) -> [Left | T];
add_token_with_nl(Left, T) -> [Left | T].

%% Error handling

interpolation_error(Reason, Rest, Tokens, Extension, Args) ->
  {error, interpolation_format(Reason, Extension, Args), Rest, Tokens}.

interpolation_format({string, Line, Message, Token}, Extension, Args) ->
  {Line, io_lib:format("~ts" ++ Extension, [Message | Args]), Token};
interpolation_format({_, _, _} = Reason, _Extension, _Args) ->
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
  case check_terminator(Token, Terminators) of
    {error, _} = Error -> Error;
    New -> Scope#elixir_tokenizer{terminators=New}
  end.

check_terminator({S, _} = New, Terminators) when
    S == 'fn';
    S == 'do';
    S == '(';
    S == '[';
    S == '{';
    S == '<<' ->
  [New | Terminators];

check_terminator({E, _}, [{S, _} | Terminators]) when
    S == 'do', E == 'end';
    S == 'fn', E == 'end';
    S == '(',  E == ')';
    S == '[',  E == ']';
    S == '{',  E == '}';
    S == '<<', E == '>>' ->
  Terminators;

check_terminator({E, {Line, _, _}}, [{Start, {StartLine, {_, _}, _}} | _]) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  End = terminator(Start),
  MessagePrefix = io_lib:format("\"~ts\" is missing terminator \"~ts\". unexpected token: \"",
                                [Start, End]),
  MessageSuffix = io_lib:format("\" at line ~B", [Line]),
  {error, {StartLine, {MessagePrefix, MessageSuffix}, [atom_to_list(E)]}};

check_terminator({E, Line}, []) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  {error, {Line, "unexpected token: ", atom_to_list(E)}};

check_terminator(_, Terminators) ->
  Terminators.

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

check_keyword(_Line, _Column, _Length, _Atom, [{'.', _} | _], _Rest) ->
  nomatch;
check_keyword(DoLine, DoColumn, _Length, do,
              [{Identifier, {Line, {Column, EndColumn}, Meta}, Atom} | T], _Rest) when Identifier == identifier ->
  {ok, add_token_with_nl({do, {DoLine, {DoColumn, DoColumn + 2}, nil}},
       [{do_identifier, {Line, {Column, EndColumn}, Meta}, Atom} | T])};
check_keyword(_Line, _Column, _Length, do, [{'fn', _} | _], _Rest) ->
  {error, do_with_fn_error("unexpected token \"do\""), "do"};
check_keyword(Line, Column, _Length, do, Tokens, _Rest) ->
  case do_keyword_valid(Tokens) of
    true  -> {ok, add_token_with_nl({do, {Line, {Column, Column + 2}, nil}}, Tokens)};
    false -> {error, invalid_do_error("unexpected token \"do\""), "do"}
  end;
check_keyword(Line, Column, Length, Atom, Tokens, Rest) ->
  case keyword(Atom) of
    false ->
      nomatch;
    token ->
      {ok, [{Atom, {Line, {Column, Column + Length}, nil}} | Tokens]};
    block ->
      {ok, [{block_identifier, {Line, {Column, Column + Length}, nil}, Atom} | Tokens]};
    Kind ->
      case strip_horizontal_space(Rest) of
        {[$/ | _], _} ->
          {ok, [{identifier, {Line, {Column, Column + Length}, nil}, Atom} | Tokens]};
        _ ->
          {ok, add_token_with_nl({Kind, {Line, {Column, Column + Length}, nil}, Atom}, Tokens)}
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
  io_lib:format("invalid character \"~ts\" (codepoint U+~4.16.0B) in ~ts: ", [[Char], Char, What]).

invalid_do_error(Prefix) ->
  Prefix ++ ". In case you wanted to write a \"do\" expression, "
  "you must either separate the keyword argument with comma or use do-blocks. "
  "For example, the following construct:\n\n"
  "    if some_condition? do\n"
  "      :this\n"
  "    else\n"
  "      :that\n"
  "    end\n\n"
  "is syntactic sugar for the Elixir construct:\n\n"
  "    if(some_condition?, do: :this, else: :that)\n\n"
  "where \"some_condition?\" is the first argument and the second argument is a keyword list.\n\n"
  "Syntax error before: ".

do_with_fn_error(Prefix) ->
  Prefix ++ ". Anonymous functions are written as:\n\n"
  "    fn pattern -> expression end\n\n"
  "Syntax error before: ".
