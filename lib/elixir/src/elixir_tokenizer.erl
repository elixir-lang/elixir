-module(elixir_tokenizer).
-include("elixir.hrl").
-include("elixir_tokenizer.hrl").
-export([tokenize/1, tokenize/3, tokenize/4, invalid_do_error/1, terminator/1]).

-define(at_op(T),
  T =:= $@).

-define(capture_op(T),
  T =:= $&).

-define(unary_op(T),
  T =:= $!;
  T =:= $^).

-define(range_op(T1, T2),
  T1 =:= $., T2 =:= $.).

-define(concat_op(T1, T2),
  T1 =:= $+, T2 =:= $+;
  T1 =:= $-, T2 =:= $-;
  T1 =:= $<, T2 =:= $>).

-define(concat_op3(T1, T2, T3),
  T1 =:= $+, T2 =:= $+, T3 =:= $+;
  T1 =:= $-, T2 =:= $-, T3 =:= $-).

-define(power_op(T1, T2),
  T1 =:= $*, T2 =:= $*).

-define(mult_op(T),
  T =:= $* orelse T =:= $/).

-define(dual_op(T),
  T =:= $+ orelse T =:= $-).

-define(arrow_op3(T1, T2, T3),
  T1 =:= $<, T2 =:= $<, T3 =:= $<;
  T1 =:= $>, T2 =:= $>, T3 =:= $>;
  T1 =:= $~, T2 =:= $>, T3 =:= $>;
  T1 =:= $<, T2 =:= $<, T3 =:= $~;
  T1 =:= $<, T2 =:= $~, T3 =:= $>;
  T1 =:= $<, T2 =:= $|, T3 =:= $>).

-define(arrow_op(T1, T2),
  T1 =:= $|, T2 =:= $>;
  T1 =:= $~, T2 =:= $>;
  T1 =:= $<, T2 =:= $~).

-define(rel_op(T),
  T =:= $<;
  T =:= $>).

-define(rel_op2(T1, T2),
  T1 =:= $<, T2 =:= $=;
  T1 =:= $>, T2 =:= $=).

-define(comp_op2(T1, T2),
  T1 =:= $=, T2 =:= $=;
  T1 =:= $=, T2 =:= $~;
  T1 =:= $!, T2 =:= $=).

-define(comp_op3(T1, T2, T3),
  T1 =:= $=, T2 =:= $=, T3 =:= $=;
  T1 =:= $!, T2 =:= $=, T3 =:= $=).

-define(ternary_op(T1, T2),
  T1 =:= $/, T2 =:= $/).

-define(and_op(T1, T2),
  T1 =:= $&, T2 =:= $&).

-define(or_op(T1, T2),
  T1 =:= $|, T2 =:= $|).

-define(and_op3(T1, T2, T3),
  T1 =:= $&, T2 =:= $&, T3 =:= $&).

-define(or_op3(T1, T2, T3),
  T1 =:= $|, T2 =:= $|, T3 =:= $|).

-define(match_op(T),
  T =:= $=).

-define(in_match_op(T1, T2),
  T1 =:= $<, T2 =:= $-;
  T1 =:= $\\, T2 =:= $\\).

-define(stab_op(T1, T2),
  T1 =:= $-, T2 =:= $>).

-define(type_op(T1, T2),
  T1 =:= $:, T2 =:= $:).

-define(pipe_op(T),
  T =:= $|).

-define(ellipsis_op3(T1, T2, T3),
  T1 =:= $., T2 =:= $., T3 =:= $.).

%% Deprecated operators

-define(unary_op3(T1, T2, T3),
  T1 =:= $~, T2 =:= $~, T3 =:= $~).

-define(xor_op3(T1, T2, T3),
  T1 =:= $^, T2 =:= $^, T3 =:= $^).

tokenize(String, Line, Column, #elixir_tokenizer{} = Scope) ->
  tokenize(String, Line, Column, Scope, []);

tokenize(String, Line, Column, Opts) ->
  IdentifierTokenizer = elixir_config:identifier_tokenizer(),

  Scope =
    lists:foldl(fun
      ({check_terminators, false}, Acc) ->
        Acc#elixir_tokenizer{terminators=none};
      ({cursor_completion, true}, Acc) ->
        Acc#elixir_tokenizer{cursor_completion=prune_and_cursor};
      ({existing_atoms_only, ExistingAtomsOnly}, Acc) when is_boolean(ExistingAtomsOnly) ->
        Acc#elixir_tokenizer{existing_atoms_only=ExistingAtomsOnly};
      ({static_atoms_encoder, StaticAtomsEncoder}, Acc) when is_function(StaticAtomsEncoder) ->
        Acc#elixir_tokenizer{static_atoms_encoder=StaticAtomsEncoder};
      ({preserve_comments, PreserveComments}, Acc) when is_function(PreserveComments) ->
        Acc#elixir_tokenizer{preserve_comments=PreserveComments};
      ({unescape, Unescape}, Acc) when is_boolean(Unescape) ->
        Acc#elixir_tokenizer{unescape=Unescape};
      (_, Acc) ->
        Acc
    end, #elixir_tokenizer{identifier_tokenizer=IdentifierTokenizer, column=Column}, Opts),

  tokenize(String, Line, Column, Scope, []).

tokenize(String, Line, Opts) ->
  tokenize(String, Line, 1, Opts).

tokenize([], Line, Column, #elixir_tokenizer{cursor_completion=Cursor} = Scope, Tokens) when Cursor /= false ->
  #elixir_tokenizer{ascii_identifiers_only=Ascii, terminators=Terminators, warnings=Warnings} = Scope,

  {CursorColumn, CursorTerminators, AccTokens} =
    add_cursor(Line, Column, Cursor, Terminators, Tokens),

  AllWarnings = maybe_unicode_lint_warnings(Ascii, Tokens, Warnings),
  {AccTerminators, _AccColumn} = cursor_complete(Line, CursorColumn, CursorTerminators),
  {ok, Line, CursorColumn, AllWarnings, AccTokens, AccTerminators};

tokenize([], EndLine, EndColumn, #elixir_tokenizer{terminators=[{Start, {StartLine, StartColumn, _}, _} | _]} = Scope, Tokens) ->
  End = terminator(Start),
  Hint = missing_terminator_hint(Start, End, Scope),
  Message = "missing terminator: ~ts",
  Formatted = io_lib:format(Message, [End]),
  Meta = [
    {opening_delimiter, Start},
    {expected_delimiter, End},
    {line, StartLine},
    {column, StartColumn},
    {end_line, EndLine},
    {end_column, EndColumn}
  ],
  error({Meta, [Formatted, Hint], []}, [], Scope, Tokens);

tokenize([], Line, Column, #elixir_tokenizer{} = Scope, Tokens) ->
  #elixir_tokenizer{ascii_identifiers_only=Ascii, warnings=Warnings} = Scope,
  AllWarnings = maybe_unicode_lint_warnings(Ascii, Tokens, Warnings),
  {ok, Line, Column, AllWarnings, Tokens, []};

% VC merge conflict

tokenize(("<<<<<<<" ++ _) = Original, Line, 1, Scope, Tokens) ->
  FirstLine = lists:takewhile(fun(C) -> C =/= $\n andalso C =/= $\r end, Original),
  Reason = {?LOC(Line, 1), "found an unexpected version control marker, please resolve the conflicts: ", FirstLine},
  error(Reason, Original, Scope, Tokens);

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
  case tokenize_comment(String, [$#]) of
    {error, Char} ->
      error_comment(Char, [$# | String], Line, Column, Scope, Tokens);
    {Rest, Comment} ->
      preserve_comments(Line, Column, Tokens, Comment, Rest, Scope),
      tokenize(Rest, Line, Column, Scope, reset_eol(Tokens))
  end;

% Sigils

tokenize([$~, H | _T] = Original, Line, Column, Scope, Tokens) when ?is_upcase(H) orelse ?is_downcase(H) ->
  tokenize_sigil(Original, Line, Column, Scope, Tokens);

% Char tokens

% We tokenize char literals (?a) as {char, _, CharInt} instead of {number, _,
% CharInt}. This is exactly what Erlang does with Erlang char literals
% ($a). This means we'll have to adjust the error message for char literals in
% elixir_errors.erl as by default {char, _, _} tokens are "hijacked" by Erlang
% and printed with Erlang syntax ($a) in the parser's error messages.

tokenize([$?, $\\, H | T], Line, Column, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),

  NewScope = if
    H =:= Char, H =/= $\\ ->
      case handle_char(Char) of
        {Escape, Name} ->
          Msg = io_lib:format("found ?\\ followed by code point 0x~.16B (~ts), please use ?~ts instead",
                              [Char, Name, Escape]),
          prepend_warning(Line, Column, Msg, Scope);

        false when ?is_downcase(H); ?is_upcase(H) ->
          Msg = io_lib:format("unknown escape sequence ?\\~tc, use ?~tc instead", [H, H]),
          prepend_warning(Line, Column, Msg, Scope);

        false ->
          Scope
      end;
    true ->
      Scope
  end,

  Token = {char, {Line, Column, [$?, $\\, H]}, Char},
  tokenize(T, Line, Column + 3, NewScope, [Token | Tokens]);

tokenize([$?, Char | T], Line, Column, Scope, Tokens) ->
  NewScope = case handle_char(Char) of
    {Escape, Name} ->
      Msg = io_lib:format("found ? followed by code point 0x~.16B (~ts), please use ?~ts instead",
                          [Char, Name, Escape]),
      prepend_warning(Line, Column, Msg, Scope);
    false ->
      Scope
  end,
  Token = {char, {Line, Column, [$?, Char]}, Char},
  tokenize(T, Line, Column + 2, NewScope, [Token | Tokens]);

% Heredocs

tokenize("\"\"\"" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $", Scope, Tokens);

%% TODO: Remove me in Elixir v2.0
tokenize("'''" ++ T, Line, Column, Scope, Tokens) ->
  NewScope = prepend_warning(Line, Column, "single-quoted string represent charlists. Use ~c''' if you indeed want a charlist or use \"\"\" instead", Scope),
  handle_heredocs(T, Line, Column, $', NewScope, Tokens);

% Strings

tokenize([$" | T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column + 1, $", Scope, Tokens);

%% TODO: Remove me in Elixir v2.0
tokenize([$' | T], Line, Column, Scope, Tokens) ->
  Message = "single-quoted strings represent charlists. "
    "Use ~c\"\" if you indeed want a charlist or use \"\" instead.\n"
    "You may run \"mix format --migrate\" to fix this warning automatically.",
  NewScope = prepend_warning(Line, Column, Message, Scope),
  handle_strings(T, Line, Column + 1, $', NewScope, Tokens);

% Operator atoms

tokenize(".:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, {Line, Column, nil}, '.'} | Tokens]);

tokenize("<<>>:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 5, Scope, [{kw_identifier, {Line, Column, nil}, '<<>>'} | Tokens]);
tokenize("%{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, Column, nil}, '%{}'} | Tokens]);
tokenize("%:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, {Line, Column, nil}, '%'} | Tokens]);
tokenize("&:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, {Line, Column, nil}, '&'} | Tokens]);
tokenize("{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 3, Scope, [{kw_identifier, {Line, Column, nil}, '{}'} | Tokens]);
tokenize("..//:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 5, Scope, [{kw_identifier, {Line, Column, nil}, '..//'} | Tokens]);

tokenize(":<<>>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 5, Scope, [{atom, {Line, Column, nil}, '<<>>'} | Tokens]);
tokenize(":%{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, Column, nil}, '%{}'} | Tokens]);
tokenize(":%" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, {Line, Column, nil}, '%'} | Tokens]);
tokenize(":{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, {Line, Column, nil}, '{}'} | Tokens]);
tokenize(":..//" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 5, Scope, [{atom, {Line, Column, nil}, '..//'} | Tokens]);

% ## Three Token Operators
tokenize([$:, T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?xor_op3(T1, T2, T3); ?concat_op3(T1, T2, T3); ?ellipsis_op3(T1, T2, T3) ->
  Token = {atom, {Line, Column, nil}, list_to_atom([T1, T2, T3])},
  tokenize(Rest, Line, Column + 4, Scope, [Token | Tokens]);

% ## Two Token Operators

tokenize([$:, $:, $: | Rest], Line, Column, Scope, Tokens) ->
  Message = "atom ::: must be written between quotes, as in :\"::\", to avoid ambiguity",
  NewScope = prepend_warning(Line, Column, Message, Scope),
  Token = {atom, {Line, Column, nil}, '::'},
  tokenize(Rest, Line, Column + 3, NewScope, [Token | Tokens]);

tokenize([$:, T1, T2 | Rest], Line, Column, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?concat_op(T1, T2); ?power_op(T1, T2);
    ?stab_op(T1, T2); ?range_op(T1, T2) ->
  Token = {atom, {Line, Column, nil}, list_to_atom([T1, T2])},
  tokenize(Rest, Line, Column + 3, Scope, [Token | Tokens]);

% ## Single Token Operators
tokenize([$:, T | Rest], Line, Column, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T); T =:= $. ->
  Token = {atom, {Line, Column, nil}, list_to_atom([T])},
  tokenize(Rest, Line, Column + 2, Scope, [Token | Tokens]);

% ## Stand-alone tokens

tokenize("=>" ++ Rest, Line, Column, Scope, Tokens) ->
  Token = {assoc_op, {Line, Column, previous_was_eol(Tokens)}, '=>'},
  tokenize(Rest, Line, Column + 2, Scope, add_token_with_eol(Token, Tokens));

tokenize("..//" ++ Rest = String, Line, Column, Scope, Tokens) ->
  case strip_horizontal_space(Rest, 0) of
    {[$/ | _] = Remaining, Extra} ->
      Token = {identifier, {Line, Column, nil}, '..//'},
      tokenize(Remaining, Line, Column + 4 + Extra, Scope, [Token | Tokens]);
    {_, _} ->
      unexpected_token(String, Line, Column, Scope, Tokens)
  end;

% ## Ternary operator

% ## Three token operators
tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?unary_op3(T1, T2, T3) ->
  handle_unary_op(Rest, Line, Column, unary_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?ellipsis_op3(T1, T2, T3) ->
  handle_unary_op(Rest, Line, Column, ellipsis_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?comp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, comp_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?and_op3(T1, T2, T3) ->
  NewScope = maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Column, Scope),
  handle_op(Rest, Line, Column, and_op, 3, list_to_atom([T1, T2, T3]), NewScope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?or_op3(T1, T2, T3) ->
  NewScope = maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Column, Scope),
  handle_op(Rest, Line, Column, or_op, 3, list_to_atom([T1, T2, T3]), NewScope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?xor_op3(T1, T2, T3) ->
  NewScope = maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Column, Scope),
  handle_op(Rest, Line, Column, xor_op, 3, list_to_atom([T1, T2, T3]), NewScope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?concat_op3(T1, T2, T3) ->
  NewScope = maybe_warn_too_many_of_same_char([T1, T2, T3], Rest, Line, Column, Scope),
  handle_op(Rest, Line, Column, concat_op, 3, list_to_atom([T1, T2, T3]), NewScope, Tokens);

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

tokenize([${ | Rest], Line, Column, Scope, [{'%', _} | _] = Tokens) ->
  Message =
    "unexpected space between % and {\n\n"
    "If you want to define a map, write %{...}, with no spaces.\n"
    "If you want to define a struct, write %StructName{...}.\n\n"
    "Syntax error before: ",
  error({?LOC(Line, Column), Message, [${]}, Rest, Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when T =:= $(; T =:= ${; T =:= $[ ->
  Token = {list_to_atom([T]), {Line, Column, nil}},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when T =:= $); T =:= $}; T =:= $] ->
  Token = {list_to_atom([T]), {Line, Column, previous_was_eol(Tokens)}},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

% ## Two Token Operators
tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?ternary_op(T1, T2) ->
  Op = list_to_atom([T1, T2]),
  Token = {ternary_op, {Line, Column, previous_was_eol(Tokens)}, Op},
  tokenize(Rest, Line, Column + 2, Scope, add_token_with_eol(Token, Tokens));

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?power_op(T1, T2) ->
  handle_op(Rest, Line, Column, power_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?range_op(T1, T2) ->
  handle_op(Rest, Line, Column, range_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?concat_op(T1, T2) ->
  handle_op(Rest, Line, Column, concat_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

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

tokenize([$& | Rest], Line, Column, Scope, Tokens) ->
  Kind =
    case strip_horizontal_space(Rest, 0) of
      {[Int | _], 0} when ?is_digit(Int) ->
        capture_int;

      {[$/ | NewRest], _} ->
        case strip_horizontal_space(NewRest, 0) of
          {[$/ | _], _} -> capture_op;
          {_, _} -> identifier
        end;

      {_, _} ->
        capture_op
    end,

  Token = {Kind, {Line, Column, nil}, '&'},
  tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?at_op(T) ->
  handle_unary_op(Rest, Line, Column, at_op, 1, list_to_atom([T]), Scope, Tokens);

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
    {NewLine, NewColumn, Parts, Rest, InterScope} ->
      NewScope = case is_unnecessary_quote(Parts, InterScope) of
        true ->
          WarnMsg = io_lib:format(
            "found quoted atom \"~ts\" but the quotes are not required. "
            "Atoms made exclusively of ASCII letters, numbers, underscores, "
            "beginning with a letter or underscore, and optionally ending with ! or ? "
            "do not require quotes",
            [hd(Parts)]
          ),
          prepend_warning(Line, Column, WarnMsg, InterScope);

        false ->
          InterScope
      end,

      case unescape_tokens(Parts, Line, Column, NewScope) of
        {ok, [Part]} when is_binary(Part) ->
          case unsafe_to_atom(Part, Line, Column, Scope) of
            {ok, Atom} ->
              Token = {atom_quoted, {Line, Column, nil}, Atom},
              tokenize(Rest, NewLine, NewColumn, NewScope, [Token | Tokens]);

            {error, Reason} ->
              error(Reason, Rest, NewScope, Tokens)
          end;

        {ok, Unescaped} ->
          Key = case Scope#elixir_tokenizer.existing_atoms_only of
            true  -> atom_safe;
            false -> atom_unsafe
          end,
          Token = {Key, {Line, Column, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn, NewScope, [Token | Tokens]);

        {error, Reason} ->
          error(Reason, Rest, NewScope, Tokens)
      end;

    {error, Reason} ->
      Message = " (for atom starting at line ~B)",
      interpolation_error(Reason, Original, Scope, Tokens, Message, [Line], Line, Column + 1, [H], [H])
  end;

tokenize([$: | String] = Original, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Column, Scope, false) of
    {_Kind, Unencoded, Atom, Rest, Length, Ascii, _Special} ->
      NewScope = maybe_warn_for_ambiguous_bang_before_equals(atom, Unencoded, Rest, Line, Column, Scope),
      TrackedScope = track_ascii(Ascii, NewScope),
      Token = {atom, {Line, Column, Unencoded}, Atom},
      tokenize(Rest, Line, Column + 1 + Length, TrackedScope, [Token | Tokens]);
    empty when Scope#elixir_tokenizer.cursor_completion == false ->
      unexpected_token(Original, Line, Column, Scope, Tokens);
    empty ->
      tokenize([], Line, Column, Scope, Tokens);
    {unexpected_token, Length} ->
      unexpected_token(lists:nthtail(Length - 1, String), Line, Column + Length - 1, Scope, Tokens);
    {error, Reason} ->
      error(Reason, Original, Scope, Tokens)
  end;

% Integers and floats
% We use int and flt otherwise elixir_parser won't format them
% properly in case of errors.

tokenize([H | T], Line, Column, Scope, Tokens) when ?is_digit(H) ->
  case tokenize_number(T, [H], 1, false) of
    {error, Reason, Original} ->
      error({?LOC(Line, Column), Reason, Original}, T, Scope, Tokens);
    {[I | Rest], Number, Original, _Length} when ?is_upcase(I); ?is_downcase(I); I == $_ ->
      if
        Number == 0, (I =:= $x) orelse (I =:= $o) orelse (I =:= $b), Rest == [],
        Scope#elixir_tokenizer.cursor_completion /= false ->
          tokenize([], Line, Column, Scope, Tokens);

        true ->
          Msg =
            io_lib:format(
              "invalid character \"~ts\" after number ~ts. If you intended to write a number, "
              "make sure to separate the number from the character (using comma, space, etc). "
              "If you meant to write a function name or a variable, note that identifiers in "
              "Elixir cannot start with numbers. Unexpected token: ",
              [[I], Original]
            ),

          error({?LOC(Line, Column), Msg, [I]}, T, Scope, Tokens)
      end;
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

tokenize("\\" = Original, Line, Column, Scope, Tokens) ->
  error({?LOC(Line, Column), "invalid escape \\ at end of file", []}, Original, Scope, Tokens);

tokenize("\\\n" = Original, Line, Column, Scope, Tokens) ->
  error({?LOC(Line, Column), "invalid escape \\ at end of file", []}, Original, Scope, Tokens);

tokenize("\\\r\n" = Original, Line, Column, Scope, Tokens) ->
  error({?LOC(Line, Column), "invalid escape \\ at end of file", []}, Original, Scope, Tokens);

tokenize("\\\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, Tokens);

tokenize("\\\r\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, Tokens);

tokenize("\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, eol(Line, Column, Tokens));

tokenize("\r\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize_eol(Rest, Line, Scope, eol(Line, Column, Tokens));

% Others

tokenize([$%, $( | Rest], Line, Column, Scope, Tokens) ->
  Reason = {?LOC(Line, Column), "expected %{ to define a map, got: ", [$%, $(]},
  error(Reason, Rest, Scope, Tokens);

tokenize([$%, $[ | Rest], Line, Column, Scope, Tokens) ->
  Reason = {?LOC(Line, Column), "expected %{ to define a map, got: ", [$%, $[]},
  error(Reason, Rest, Scope, Tokens);

tokenize([$%, ${ | T], Line, Column, Scope, Tokens) ->
  Token = {'{', {Line, Column, nil}},
  handle_terminator(T, Line, Column + 2, Scope, Token, [{'%{}', {Line, Column, nil}} | Tokens]);

tokenize([$% | T], Line, Column, Scope, Tokens) ->
  tokenize(T, Line, Column + 1, Scope, [{'%', {Line, Column, nil}} | Tokens]);

tokenize([$. | T], Line, Column, Scope, Tokens) ->
  tokenize_dot(T, Line, Column + 1, {Line, Column, nil}, Scope, Tokens);

% Identifiers

tokenize(String, Line, Column, OriginalScope, Tokens) ->
  case tokenize_identifier(String, Line, Column, OriginalScope, not previous_was_dot(Tokens)) of
    {Kind, Unencoded, Atom, Rest, Length, Ascii, Special} ->
      HasAt = lists:member(at, Special),
      Scope = track_ascii(Ascii, OriginalScope),

      case Rest of
        [$: | T] when ?is_space(hd(T)) ->
          Token = {kw_identifier, {Line, Column, Unencoded}, Atom},
          tokenize(T, Line, Column + Length + 1, Scope, [Token | Tokens]);

        [$: | T] when hd(T) =/= $: ->
          AtomName = atom_to_list(Atom) ++ [$:],
          Reason = {?LOC(Line, Column), "keyword argument must be followed by space after: ", AtomName},
          error(Reason, String, Scope, Tokens);

        _ when HasAt ->
          Reason = {?LOC(Line, Column), invalid_character_error(Kind, $@), atom_to_list(Atom)},
          error(Reason, String, Scope, Tokens);

        _ when Atom == '__aliases__'; Atom == '__block__' ->
          error({?LOC(Line, Column), "reserved token: ", atom_to_list(Atom)}, Rest, Scope, Tokens);

        _ when Kind == alias ->
          tokenize_alias(Rest, Line, Column, Unencoded, Atom, Length, Ascii, Special, Scope, Tokens);

        _ when Kind == identifier ->
          NewScope = maybe_warn_for_ambiguous_bang_before_equals(identifier, Unencoded, Rest, Line, Column, Scope),
          Token = check_call_identifier(Line, Column, Unencoded, Atom, Rest),
          tokenize(Rest, Line, Column + Length, NewScope, [Token | Tokens]);

        _ ->
          unexpected_token(String, Line, Column, Scope, Tokens)
      end;

    {keyword, Atom, Type, Rest, Length} ->
      tokenize_keyword(Type, Rest, Line, Column, Atom, Length, OriginalScope, Tokens);

    empty when OriginalScope#elixir_tokenizer.cursor_completion == false ->
      unexpected_token(String, Line, Column, OriginalScope, Tokens);

    empty  ->
      case String of
        [$~, L] when ?is_upcase(L); ?is_downcase(L) -> tokenize([], Line, Column, OriginalScope, Tokens);
        [$~] -> tokenize([], Line, Column, OriginalScope, Tokens);
        _ -> unexpected_token(String, Line, Column, OriginalScope, Tokens)
      end;

    {unexpected_token, Length} ->
      unexpected_token(lists:nthtail(Length - 1, String), Line, Column + Length - 1, OriginalScope, Tokens);

    {error, Reason} ->
      error(Reason, String, OriginalScope, Tokens)
  end.

previous_was_dot([{'.', _} | _]) -> true;
previous_was_dot(_) -> false.

unexpected_token([T | Rest], Line, Column, Scope, Tokens) ->
  Message =
    case handle_char(T) of
      {_Escaped, Explanation} ->
        io_lib:format("~ts (column ~p, code point U+~4.16.0B)", [Explanation, Column, T]);
      false ->
        io_lib:format("\"~ts\" (column ~p, code point U+~4.16.0B)", [[T], Column, T])
    end,
  error({?LOC(Line, Column), "unexpected token: ", Message}, Rest, Scope, Tokens).

tokenize_eol(Rest, Line, Scope, Tokens) ->
  {StrippedRest, Column} = strip_horizontal_space(Rest, Scope#elixir_tokenizer.column),
  IndentedScope = Scope#elixir_tokenizer{indentation=Column-1},
  tokenize(StrippedRest, Line + 1, Column, IndentedScope, Tokens).

strip_horizontal_space([H | T], Counter) when ?is_horizontal_space(H) ->
  strip_horizontal_space(T, Counter + 1);
strip_horizontal_space(T, Counter) ->
  {T, Counter}.

tokenize_dot(T, Line, Column, DotInfo, Scope, Tokens) ->
  case strip_horizontal_space(T, 0) of
    {[$# | R], _} ->
      case tokenize_comment(R, [$#]) of
        {error, Char} ->
          error_comment(Char, [$# | R], Line, Column, Scope, Tokens);

        {Rest, Comment} ->
          preserve_comments(Line, Column, Tokens, Comment, Rest, Scope),
          tokenize_dot(Rest, Line, Scope#elixir_tokenizer.column, DotInfo, Scope, Tokens)
      end;
    {"\r\n" ++ Rest, _} ->
      tokenize_dot(Rest, Line + 1, Scope#elixir_tokenizer.column, DotInfo, Scope, Tokens);
    {"\n" ++ Rest, _} ->
      tokenize_dot(Rest, Line + 1, Scope#elixir_tokenizer.column, DotInfo, Scope, Tokens);
    {Rest, Length} ->
      handle_dot([$. | Rest], Line, Column + Length, DotInfo, Scope, Tokens)
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
    {ok, NewLine, NewColumn, Parts, Rest, NewScope} ->
      case unescape_tokens(Parts, Line, Column, NewScope) of
        {ok, Unescaped} ->
          Token = {heredoc_type(H), {Line, Column, nil}, NewColumn - 4, Unescaped},
          tokenize(Rest, NewLine, NewColumn, NewScope, [Token | Tokens]);

        {error, Reason} ->
          error(Reason, Rest, Scope, Tokens)
      end;

    {error, Reason} ->
      error(Reason, [H, H, H] ++ T, Scope, Tokens)
  end.

handle_strings(T, Line, Column, H, Scope, Tokens) ->
  case elixir_interpolation:extract(Line, Column, Scope, true, T, H) of
    {error, Reason} ->
      interpolation_error(Reason, [H | T], Scope, Tokens, " (for string starting at line ~B)", [Line], Line, Column-1, [H], [H]);

    {NewLine, NewColumn, Parts, [$: | Rest], InterScope} when ?is_space(hd(Rest)) ->
      NewScope = case is_unnecessary_quote(Parts, InterScope) of
        true ->
          WarnMsg = io_lib:format(
            "found quoted keyword \"~ts\" but the quotes are not required. "
            "Note that keywords are always atoms, even when quoted. "
            "Similar to atoms, keywords made exclusively of ASCII "
            "letters, numbers, and underscores and not beginning with a "
            "number do not require quotes",
            [hd(Parts)]
          ),
          prepend_warning(Line, Column, WarnMsg, InterScope);

        false ->
          InterScope
      end,

      case unescape_tokens(Parts, Line, Column, NewScope) of
        {ok, [Part]} when is_binary(Part) ->
          case unsafe_to_atom(Part, Line, Column - 1, Scope) of
            {ok, Atom} ->
              Token = {kw_identifier, {Line, Column - 1, nil}, Atom},
              tokenize(Rest, NewLine, NewColumn + 1, NewScope, [Token | Tokens]);
            {error, Reason} ->
              error(Reason, Rest, NewScope, Tokens)
          end;

        {ok, Unescaped} ->
          Key = case Scope#elixir_tokenizer.existing_atoms_only of
            true  -> kw_identifier_safe;
            false -> kw_identifier_unsafe
          end,
          Token = {Key, {Line, Column - 1, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn + 1, NewScope, [Token | Tokens]);

        {error, Reason} ->
          error(Reason, Rest, NewScope, Tokens)
      end;

    {NewLine, NewColumn, Parts, Rest, NewScope} ->
      case unescape_tokens(Parts, Line, Column, NewScope) of
        {ok, Unescaped} ->
          Token = {string_type(H), {Line, Column - 1, nil}, Unescaped},
          tokenize(Rest, NewLine, NewColumn, NewScope, [Token | Tokens]);

        {error, Reason} ->
          error(Reason, Rest, NewScope, Tokens)
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
      NewScope =
        %% TODO: Remove these deprecations on Elixir v2.0
        case Op of
          '^^^' ->
            Msg = "^^^ is deprecated. It is typically used as xor but it has the wrong precedence, use Bitwise.bxor/2 instead",
            prepend_warning(Line, Column, Msg, Scope);

          '~~~' ->
            Msg = "~~~ is deprecated. Use Bitwise.bnot/1 instead for clarity",
            prepend_warning(Line, Column, Msg, Scope);

          '<|>' ->
            Msg = "<|> is deprecated. Use another pipe-like operator",
            prepend_warning(Line, Column, Msg, Scope);

          _ ->
            Scope
        end,

      Token = {Kind, {Line, Column, previous_was_eol(Tokens)}, Op},
      tokenize(Remaining, Line, Column + Length + Extra, NewScope, add_token_with_eol(Token, Tokens))
  end.

% ## Three Token Operators
handle_dot([$., T1, T2, T3 | Rest], Line, Column, DotInfo, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?xor_op3(T1, T2, T3); ?concat_op3(T1, T2, T3) ->
  handle_call_identifier(Rest, Line, Column, DotInfo, 3, [T1, T2, T3], Scope, Tokens);

% ## Two Token Operators
handle_dot([$., T1, T2 | Rest], Line, Column, DotInfo, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?concat_op(T1, T2); ?power_op(T1, T2);
    ?type_op(T1, T2) ->
  handle_call_identifier(Rest, Line, Column, DotInfo, 2, [T1, T2], Scope, Tokens);

% ## Single Token Operators
handle_dot([$., T | Rest], Line, Column, DotInfo, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T) ->
  handle_call_identifier(Rest, Line, Column, DotInfo, 1, [T], Scope, Tokens);

% ## Exception for .( as it needs to be treated specially in the parser
handle_dot([$., $( | Rest], Line, Column, DotInfo, Scope, Tokens) ->
  TokensSoFar = add_token_with_eol({dot_call_op, DotInfo, '.'}, Tokens),
  tokenize([$( | Rest], Line, Column, Scope, TokensSoFar);

handle_dot([$., H | T] = Original, Line, Column, DotInfo, Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column + 1, Scope, true, T, H) of
    {NewLine, NewColumn, [Part], Rest, InterScope} when is_list(Part) ->
      NewScope = case is_unnecessary_quote([Part], InterScope) of
        true ->
          WarnMsg = io_lib:format(
            "found quoted call \"~ts\" but the quotes are not required. "
            "Calls made exclusively of Unicode letters, numbers, and underscores "
            "and not beginning with a number do not require quotes",
            [Part]
          ),
          prepend_warning(Line, Column, WarnMsg, InterScope);

        false ->
          InterScope
      end,

      case unsafe_to_atom(Part, Line, Column, NewScope) of
        {ok, Atom} ->
          Token = check_call_identifier(Line, Column, Part, Atom, Rest),
          TokensSoFar = add_token_with_eol({'.', DotInfo}, Tokens),
          tokenize(Rest, NewLine, NewColumn, NewScope, [Token | TokensSoFar]);

        {error, Reason} ->
          error(Reason, Original, NewScope, Tokens)
      end;
    {_NewLine, _NewColumn, _Parts, Rest, NewScope} ->
      Message = "interpolation is not allowed when calling function/macro. Found interpolation in a call starting with: ",
      error({?LOC(Line, Column), Message, [H]}, Rest, NewScope, Tokens);
    {error, Reason} ->
      interpolation_error(Reason, Original, Scope, Tokens, " (for function name starting at line ~B)", [Line], Line, Column, [H], [H])
  end;

handle_dot([$. | Rest], Line, Column, DotInfo, Scope, Tokens) ->
  TokensSoFar = add_token_with_eol({'.', DotInfo}, Tokens),
  tokenize(Rest, Line, Column, Scope, TokensSoFar).

handle_call_identifier(Rest, Line, Column, DotInfo, Length, UnencodedOp, Scope, Tokens) ->
  Token = check_call_identifier(Line, Column, UnencodedOp, list_to_atom(UnencodedOp), Rest),
  TokensSoFar = add_token_with_eol({'.', DotInfo}, Tokens),
  tokenize(Rest, Line, Column + Length, Scope, [Token | TokensSoFar]).

% ## Ambiguous unary/binary operators tokens
% Keywords are not ambiguous operators
handle_space_sensitive_tokens([Sign, $:, Space | _] = String, Line, Column, Scope, Tokens) when ?dual_op(Sign), ?is_space(Space) ->
  tokenize(String, Line, Column, Scope, Tokens);

% But everything else, except other operators, are
handle_space_sensitive_tokens([Sign, NotMarker | T], Line, Column, Scope, [{identifier, _, _} = H | Tokens]) when
    ?dual_op(Sign), not(?is_space(NotMarker)), NotMarker =/= Sign, NotMarker =/= $/, NotMarker =/= $> ->
  Rest = [NotMarker | T],
  DualOpToken = {dual_op, {Line, Column, nil}, list_to_atom([Sign])},
  tokenize(Rest, Line, Column + 1, Scope, [DualOpToken, setelement(1, H, op_identifier) | Tokens]);

% Handle cursor completion
handle_space_sensitive_tokens([], Line, Column,
                              #elixir_tokenizer{cursor_completion=Cursor} = Scope,
                              [{identifier, Info, Identifier} | Tokens]) when Cursor /= false ->
  tokenize([$(], Line, Column+1, Scope, [{paren_identifier, Info, Identifier} | Tokens]);

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

is_unnecessary_quote([Part], Scope) when is_list(Part) ->
  case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(Part) of
    {identifier, _, [], _, true, Special} -> not lists:member(at, Special);
    _ -> false
  end;
is_unnecessary_quote(_Parts, _Scope) ->
  false.

unsafe_to_atom(Part, Line, Column, #elixir_tokenizer{}) when
    is_binary(Part) andalso byte_size(Part) > 255;
    is_list(Part) andalso length(Part) > 255 ->
  {error, {?LOC(Line, Column), "atom length must be less than system limit: ", elixir_utils:characters_to_list(Part)}};
unsafe_to_atom(Part, Line, Column, #elixir_tokenizer{static_atoms_encoder=StaticAtomsEncoder}) when
    is_function(StaticAtomsEncoder) ->
  Value = elixir_utils:characters_to_binary(Part),
  case StaticAtomsEncoder(Value, [{line, Line}, {column, Column}]) of
    {ok, Term} ->
      {ok, Term};
    {error, Reason} when is_binary(Reason) ->
      {error, {?LOC(Line, Column), elixir_utils:characters_to_list(Reason) ++ ": ", elixir_utils:characters_to_list(Part)}}
  end;
unsafe_to_atom(Binary, Line, Column, #elixir_tokenizer{existing_atoms_only=true}) when is_binary(Binary) ->
  try
    {ok, binary_to_existing_atom(Binary, utf8)}
  catch
    error:badarg -> {error, {?LOC(Line, Column), "unsafe atom does not exist: ", elixir_utils:characters_to_list(Binary)}}
  end;
unsafe_to_atom(Binary, _Line, _Column, #elixir_tokenizer{}) when is_binary(Binary) ->
  {ok, binary_to_atom(Binary, utf8)};
unsafe_to_atom(List, Line, Column, #elixir_tokenizer{existing_atoms_only=true}) when is_list(List) ->
  try
    {ok, list_to_existing_atom(List)}
  catch
    error:badarg -> {error, {?LOC(Line, Column), "unsafe atom does not exist: ", List}}
  end;
unsafe_to_atom(List, _Line, _Column, #elixir_tokenizer{}) when is_list(List) ->
  {ok, list_to_atom(List)}.

collect_modifiers([H | T], Buffer) when ?is_downcase(H) or ?is_upcase(H) or ?is_digit(H) ->
  collect_modifiers(T, [H | Buffer]);

collect_modifiers(Rest, Buffer) ->
  {Rest, lists:reverse(Buffer)}.

%% Heredocs

extract_heredoc_with_interpolation(Line, Column, Scope, Interpol, T, H) ->
  case extract_heredoc_header(T) of
    {ok, Headerless} ->
      %% We prepend a new line so we can transparently remove
      %% spaces later. This new line is removed by calling "tl"
      %% in the final heredoc body three lines below.
      case elixir_interpolation:extract(Line, Column, Scope, Interpol, [$\n|Headerless], [H,H,H]) of
        {NewLine, NewColumn, Parts0, Rest, InterScope} ->
          Indent = NewColumn - 4,
          Fun = fun(Part, Acc) -> extract_heredoc_indent(Part, Acc, Indent) end,
          {Parts1, {ShouldWarn, _}} = lists:mapfoldl(Fun, {false, Line}, Parts0),
          Parts2 = extract_heredoc_head(Parts1),
          NewScope = maybe_heredoc_warn(ShouldWarn, Column, InterScope, H),
          {ok, NewLine, NewColumn, tokens_to_binary(Parts2), Rest, NewScope};

        {error, Reason} ->
          {error, interpolation_format(Reason, " (for heredoc starting at line ~B)", [Line], Line, Column, [H, H, H], [H, H, H])}
      end;

    error ->
      Message = "heredoc allows only whitespace characters followed by a new line after opening ",
      {error, {?LOC(Line, Column + 3), io_lib:format(Message, []), [H, H, H]}}
  end.

extract_heredoc_header("\r\n" ++ Rest) ->
  {ok, Rest};
extract_heredoc_header("\n" ++ Rest) ->
  {ok, Rest};
extract_heredoc_header([H | T]) when ?is_horizontal_space(H) ->
  extract_heredoc_header(T);
extract_heredoc_header(_) ->
  error.

extract_heredoc_indent(Part, {Warned, Line}, Indent) when is_list(Part) ->
  extract_heredoc_indent(Part, [], Warned, Line, Indent);
extract_heredoc_indent({_, {EndLine, _, _}, _} = Part, {Warned, _Line}, _Indent) ->
  {Part, {Warned, EndLine}}.

extract_heredoc_indent([$\n | Rest], Acc, Warned, Line, Indent) ->
  {Trimmed, ShouldWarn} = trim_space(Rest, Indent),
  Warn = if ShouldWarn, not Warned -> Line + 1; true -> Warned end,
  extract_heredoc_indent(Trimmed, [$\n | Acc], Warn, Line + 1, Indent);
extract_heredoc_indent([Head | Rest], Acc, Warned, Line, Indent) ->
  extract_heredoc_indent(Rest, [Head | Acc], Warned, Line, Indent);
extract_heredoc_indent([], Acc, Warned, Line, _Indent) ->
  {lists:reverse(Acc), {Warned, Line}}.

trim_space(Rest, 0) -> {Rest, false};
trim_space([$\r, $\n | _] = Rest, _) -> {Rest, false};
trim_space([$\n | _] = Rest, _) -> {Rest, false};
trim_space([H | T], Spaces) when ?is_horizontal_space(H) -> trim_space(T, Spaces - 1);
trim_space([], _Spaces) -> {[], false};
trim_space(Rest, _Spaces) -> {Rest, true}.

maybe_heredoc_warn(false, _Column, Scope, _Marker) ->
  Scope;
maybe_heredoc_warn(Line, Column, Scope, Marker) ->
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

  prepend_warning(Line, Column, Msg, Scope).

extract_heredoc_head([[$\n|H]|T]) -> [H|T].

unescape_tokens(Tokens, Line, Column, #elixir_tokenizer{unescape=true}) ->
  case elixir_interpolation:unescape_tokens(Tokens) of
    {ok, Result} ->
      {ok, Result};

    {error, Message, Token} ->
      {error, {?LOC(Line, Column), Message ++ ". Syntax error after: ", Token}}
  end;
unescape_tokens(Tokens, _Line, _Column, #elixir_tokenizer{unescape=false}) ->
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
    when (E =:= $E) or (E =:= $e), ?is_digit(H), S =:= $+ orelse S =:= $- ->
  tokenize_number(T, [H, S, E | Acc], Length + 3, true);

%% Check if we have e followed by numbers (valid only for floats);
tokenize_number([E, H | T], Acc, Length, true)
    when (E =:= $E) or (E =:= $e), ?is_digit(H) ->
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
tokenize_comment([H | _Rest], _) when ?bidi(H) ->
  {error, H};
tokenize_comment([H | Rest], Acc) ->
  tokenize_comment(Rest, [H | Acc]);
tokenize_comment([], Acc) ->
  {[], lists:reverse(Acc)}.

error_comment(H, Comment, Line, Column, Scope, Tokens) ->
  Token = io_lib:format("\\u~4.16.0B", [H]),
  Reason = {?LOC(Line, Column), "invalid bidirectional formatting character in comment: ", Token},
  error(Reason, Comment, Scope, Tokens).

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
tokenize([H | T]) when ?is_downcase(H); H =:= $_ ->
  {Acc, Rest, Length, Special} = tokenize_continue(T, [H], 1, []),
  {identifier, lists:reverse(Acc), Rest, Length, true, Special};
tokenize(_List) ->
  {error, empty}.

tokenize_continue([$@ | T], Acc, Length, Special) ->
  tokenize_continue(T, [$@ | Acc], Length + 1, [at | lists:delete(at, Special)]);
tokenize_continue([$! | T], Acc, Length, Special) ->
  {[$! | Acc], T, Length + 1, [punctuation | Special]};
tokenize_continue([$? | T], Acc, Length, Special) ->
  {[$? | Acc], T, Length + 1, [punctuation | Special]};
tokenize_continue([H | T], Acc, Length, Special) when ?is_upcase(H); ?is_downcase(H); ?is_digit(H); H =:= $_ ->
  tokenize_continue(T, [H | Acc], Length + 1, Special);
tokenize_continue(Rest, Acc, Length, Special) ->
  {Acc, Rest, Length, Special}.

tokenize_identifier(String, Line, Column, Scope, MaybeKeyword) ->
  case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(String) of
    {Kind, Acc, Rest, Length, Ascii, Special} ->
      Keyword = MaybeKeyword andalso maybe_keyword(Rest),

      case keyword_or_unsafe_to_atom(Keyword, Acc, Line, Column, Scope) of
        {keyword, Atom, Type} ->
          {keyword, Atom, Type, Rest, Length};
        {ok, Atom} ->
          {Kind, Acc, Atom, Rest, Length, Ascii, Special};
        {error, _Reason} = Error ->
          Error
      end;

    {error, {mixed_script, Wrong, {Prefix, Suffix}}} ->
      WrongColumn = Column + length(Wrong) - 1,
      case suggest_simpler_unexpected_token_in_error(Wrong, Line, WrongColumn, Scope) of
        no_suggestion ->
          %% we append a pointer to more info if we aren't appending a suggestion
          MoreInfo = "\nSee https://hexdocs.pm/elixir/unicode-syntax.html for more information.",
          {error, {?LOC(Line, Column), {Prefix, Suffix ++ MoreInfo}, Wrong}};

        {_, {Location, _, SuggestionMessage}} = _SuggestionError ->
          {error, {Location, {Prefix, Suffix ++ SuggestionMessage}, Wrong}}
      end;

    {error, {unexpected_token, Wrong}} ->
      WrongColumn = Column + length(Wrong) - 1,
      case suggest_simpler_unexpected_token_in_error(Wrong, Line, WrongColumn, Scope) of
        no_suggestion ->
          [T | _] = lists:reverse(Wrong),
          case suggest_simpler_unexpected_token_in_error([T], Line, WrongColumn, Scope) of
            no_suggestion -> {unexpected_token, length(Wrong)};
            SuggestionError -> SuggestionError
          end;

        SuggestionError ->
          SuggestionError
      end;

    {error, empty} ->
      empty
  end.

%% heuristic: try nfkc; try confusability skeleton; try calling this again w/just failed codepoint
suggest_simpler_unexpected_token_in_error(Wrong, Line, WrongColumn, Scope) ->
  NFKC = unicode:characters_to_nfkc_list(Wrong),
  case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(NFKC) of
    {error, _Reason} ->
       ConfusableSkeleton = 'Elixir.String.Tokenizer.Security':confusable_skeleton(Wrong),
       case (Scope#elixir_tokenizer.identifier_tokenizer):tokenize(ConfusableSkeleton) of
         {_, Simpler, _, _, _, _} ->
           Message = suggest_change("Codepoint failed identifier tokenization, but a simpler form was found.",
                                    Wrong,
                                    "You could write the above in a similar way that is accepted by Elixir:",
                                    Simpler,
                                    "See https://hexdocs.pm/elixir/unicode-syntax.html for more information."),
           {error, {?LOC(Line, WrongColumn), "unexpected token: ", Message}};
         _other ->
           no_suggestion
       end;
    {_, _NFKC, _, _, _, _} ->
      Message = suggest_change("Elixir expects unquoted Unicode atoms, variables, and calls to use allowed codepoints and to be in NFC form.",
                               Wrong,
                               "You could write the above in a compatible format that is accepted by Elixir:",
                               NFKC,
                               "See https://hexdocs.pm/elixir/unicode-syntax.html for more information."),
          {error, {?LOC(Line, WrongColumn), "unexpected token: ", Message}}
    end.

suggest_change(Intro, WrongForm, Hint, HintedForm, Ending) ->
  WrongCodepoints = list_to_codepoint_hex(WrongForm),
  HintedCodepoints = list_to_codepoint_hex(HintedForm),
  io_lib:format("~ts\n\nGot:\n\n    \"~ts\" (code points~ts)\n\n"
                "Hint: ~ts\n\n    \"~ts\" (code points~ts)\n\n~ts",
                [Intro, WrongForm, WrongCodepoints, Hint, HintedForm, HintedCodepoints, Ending]).

maybe_keyword([]) -> true;
maybe_keyword([$:, $: | _]) -> true;
maybe_keyword([$: | _]) -> false;
maybe_keyword(_) -> true.

list_to_codepoint_hex(List) ->
  [io_lib:format(" 0x~5.16.0B", [Codepoint]) || Codepoint <- List].

tokenize_alias(Rest, Line, Column, Unencoded, Atom, Length, Ascii, Special, Scope, Tokens) ->
  if
    not Ascii or (Special /= []) ->
      Invalid = hd([C || C <- Unencoded, (C < $A) or (C > 127)]),
      Reason = {?LOC(Line, Column), invalid_character_error("alias (only ASCII characters, without punctuation, are allowed)", Invalid), Unencoded},
      error(Reason, Unencoded ++ Rest, Scope, Tokens);

    true ->
      AliasesToken = {alias, {Line, Column, Unencoded}, Atom},
      tokenize(Rest, Line, Column + Length, Scope, [AliasesToken | Tokens])
  end.

%% Check if it is a call identifier (paren | bracket | do)

check_call_identifier(Line, Column, Unencoded, Atom, [$( | _]) ->
  {paren_identifier, {Line, Column, Unencoded}, Atom};
check_call_identifier(Line, Column, Unencoded, Atom, [$[ | _]) ->
  {bracket_identifier, {Line, Column, Unencoded}, Atom};
check_call_identifier(Line, Column, Unencoded, Atom, _Rest) ->
  {identifier, {Line, Column, Unencoded}, Atom}.

add_token_with_eol({unary_op, _, _} = Left, T) -> [Left | T];
add_token_with_eol(Left, [{eol, _} | T]) -> [Left | T];
add_token_with_eol(Left, T) -> [Left | T].

previous_was_eol([{',', {_, _, Count}} | _]) when Count > 0 -> Count;
previous_was_eol([{';', {_, _, Count}} | _]) when Count > 0 -> Count;
previous_was_eol([{eol, {_, _, Count}} | _]) when Count > 0 -> Count;
previous_was_eol(_) -> nil.

%% Error handling

interpolation_error(Reason, Rest, Scope, Tokens, Extension, Args, Line, Column, Opening, Closing) ->
  error(interpolation_format(Reason, Extension, Args, Line, Column, Opening, Closing), Rest, Scope, Tokens).

interpolation_format({string, EndLine, EndColumn, Message, Token}, Extension, Args, Line, Column, Opening, Closing) ->
  Meta = [
    {opening_delimiter, list_to_atom(Opening)},
    {expected_delimiter, list_to_atom(Closing)},
    {line, Line},
    {column, Column},
    {end_line, EndLine},
    {end_column, EndColumn}
  ],
  {Meta, [Message, io_lib:format(Extension, Args)], Token};
interpolation_format({_, _, _} = Reason, _Extension, _Args, _Line, _Column, _Opening, _Closing) ->
  Reason.

%% Terminators

handle_terminator(Rest, _, _, Scope, {'(', {Line, Column, _}}, [{alias, _, Alias} | Tokens]) ->
  Reason =
    io_lib:format(
      "unexpected ( after alias ~ts. Function names and identifiers in Elixir "
      "start with lowercase characters or underscore. For example:\n\n"
      "    hello_world()\n"
      "    _starting_with_underscore()\n"
      "    numb3rs_are_allowed()\n"
      "    may_finish_with_question_mark?()\n"
      "    may_finish_with_exclamation_mark!()\n\n"
      "Unexpected token: ",
      [Alias]
    ),

  error({?LOC(Line, Column), Reason, ["("]}, atom_to_list(Alias) ++ [$( | Rest], Scope, Tokens);
handle_terminator(Rest, Line, Column, #elixir_tokenizer{terminators=none} = Scope, Token, Tokens) ->
  tokenize(Rest, Line, Column, Scope, [Token | Tokens]);
handle_terminator(Rest, Line, Column, Scope, Token, Tokens) ->
  #elixir_tokenizer{terminators=Terminators} = Scope,

  case check_terminator(Token, Terminators, Scope) of
    {error, Reason} ->
      error(Reason, atom_to_list(element(1, Token)) ++ Rest, Scope, Tokens);
    {ok, New} ->
      tokenize(Rest, Line, Column, New, [Token | Tokens])
  end.

check_terminator({Start, Meta}, Terminators, Scope)
    when Start == '('; Start == '['; Start == '{'; Start == '<<' ->
  Indentation = Scope#elixir_tokenizer.indentation,
  {ok, Scope#elixir_tokenizer{terminators=[{Start, Meta, Indentation} | Terminators]}};

check_terminator({Start, Meta}, Terminators, Scope) when Start == 'fn'; Start == 'do' ->
  Indentation = Scope#elixir_tokenizer.indentation,

  NewScope =
    case Terminators of
      %% If the do is indented equally or less than the previous do, it may be a missing end error!
      [{Start, _, PreviousIndentation} = Previous | _] when Indentation =< PreviousIndentation ->
        Scope#elixir_tokenizer{mismatch_hints=[Previous | Scope#elixir_tokenizer.mismatch_hints]};

      _ ->
        Scope
    end,

  {ok, NewScope#elixir_tokenizer{terminators=[{Start, Meta, Indentation} | Terminators]}};

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

  {ok, NewScope#elixir_tokenizer{terminators=Terminators}};

check_terminator({End, {EndLine, EndColumn, _}}, [{Start, {StartLine, StartColumn, _}, _} | Terminators], Scope)
    when End == 'end'; End == ')'; End == ']'; End == '}'; End == '>>' ->
  case terminator(Start) of
    End ->
      {ok, Scope#elixir_tokenizer{terminators=Terminators}};

    ExpectedEnd ->
      Meta = [
        {line, StartLine},
        {column, StartColumn},
        {end_line, EndLine},
        {end_column, EndColumn},
        {error_type, mismatched_delimiter},
        {opening_delimiter, Start},
        {closing_delimiter, End},
        {expected_delimiter, ExpectedEnd}
     ],
     {error, {Meta, unexpected_token_or_reserved(End), [atom_to_list(End)]}}
  end;

check_terminator({'end', {Line, Column, _}}, [], #elixir_tokenizer{mismatch_hints=Hints}) ->
  Suffix =
    case lists:keyfind('end', 1, Hints) of
      {'end', HintLine, _Identation} ->
        io_lib:format("\n~ts the \"end\" on line ~B may not have a matching \"do\" "
                      "defined before it (based on indentation)", [elixir_errors:prefix(hint), HintLine]);
      false ->
        ""
    end,

  {error, {?LOC(Line, Column), {"unexpected reserved word: ", Suffix}, "end"}};

check_terminator({End, {Line, Column, _}}, [], _Scope)
    when End == ')'; End == ']'; End == '}'; End == '>>' ->
  {error, {?LOC(Line, Column), "unexpected token: ", atom_to_list(End)}};

check_terminator(_, _, Scope) ->
  {ok, Scope}.

unexpected_token_or_reserved('end') -> "unexpected reserved word: ";
unexpected_token_or_reserved(_) -> "unexpected token: ".

missing_terminator_hint(Start, End, #elixir_tokenizer{mismatch_hints=Hints}) ->
  case lists:keyfind(Start, 1, Hints) of
    {Start, {HintLine, _, _}, _} ->
      io_lib:format("\n~ts it looks like the \"~ts\" on line ~B does not have a matching \"~ts\"",
                    [elixir_errors:prefix(hint), Start, HintLine, End]);
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

keyword_or_unsafe_to_atom(true, "fn", _Line, _Column, _Scope) -> {keyword, 'fn', terminator};
keyword_or_unsafe_to_atom(true, "do", _Line, _Column, _Scope) -> {keyword, 'do', terminator};
keyword_or_unsafe_to_atom(true, "end", _Line, _Column, _Scope) -> {keyword, 'end', terminator};
keyword_or_unsafe_to_atom(true, "true", _Line, _Column, _Scope) -> {keyword, 'true', token};
keyword_or_unsafe_to_atom(true, "false", _Line, _Column, _Scope) -> {keyword, 'false', token};
keyword_or_unsafe_to_atom(true, "nil", _Line, _Column, _Scope) -> {keyword, 'nil', token};

keyword_or_unsafe_to_atom(true, "not", _Line, _Column, _Scope) -> {keyword, 'not', unary_op};
keyword_or_unsafe_to_atom(true, "and", _Line, _Column, _Scope) -> {keyword, 'and', and_op};
keyword_or_unsafe_to_atom(true, "or", _Line, _Column, _Scope) -> {keyword, 'or', or_op};
keyword_or_unsafe_to_atom(true, "when", _Line, _Column, _Scope) -> {keyword, 'when', when_op};
keyword_or_unsafe_to_atom(true, "in", _Line, _Column, _Scope) -> {keyword, 'in', in_op};

keyword_or_unsafe_to_atom(true, "after", _Line, _Column, _Scope) -> {keyword, 'after', block};
keyword_or_unsafe_to_atom(true, "else", _Line, _Column, _Scope) -> {keyword, 'else', block};
keyword_or_unsafe_to_atom(true, "catch", _Line, _Column, _Scope) -> {keyword, 'catch', block};
keyword_or_unsafe_to_atom(true, "rescue", _Line, _Column, _Scope) -> {keyword, 'rescue', block};

keyword_or_unsafe_to_atom(_, Part, Line, Column, Scope) ->
  unsafe_to_atom(Part, Line, Column, Scope).

tokenize_keyword(terminator, Rest, Line, Column, Atom, Length, Scope, Tokens) ->
  case tokenize_keyword_terminator(Line, Column, Atom, Tokens) of
    {ok, [Check | T]} ->
      handle_terminator(Rest, Line, Column + Length, Scope, Check, T);
    {error, Message, Token} ->
      error({?LOC(Line, Column), Message, Token}, Token ++ Rest, Scope, Tokens)
  end;

tokenize_keyword(token, Rest, Line, Column, Atom, Length, Scope, Tokens) ->
  Token = {Atom, {Line, Column, nil}},
  tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);

tokenize_keyword(block, Rest, Line, Column, Atom, Length, Scope, Tokens) ->
  Token = {block_identifier, {Line, Column, nil}, Atom},
  tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);

tokenize_keyword(Kind, Rest, Line, Column, Atom, Length, Scope, Tokens) ->
  NewTokens =
    case strip_horizontal_space(Rest, 0) of
      {[$/ | _], _} ->
        [{identifier, {Line, Column, nil}, Atom} | Tokens];

      _ ->
        case {Kind, Tokens} of
          {in_op, [{unary_op, NotInfo, 'not'} | T]} ->
            add_token_with_eol({in_op, NotInfo, 'not in'}, T);

          {_, _} ->
            add_token_with_eol({Kind, {Line, Column, previous_was_eol(Tokens)}, Atom}, Tokens)
        end
    end,

  tokenize(Rest, Line, Column + Length, Scope, NewTokens).

tokenize_sigil([$~ | T], Line, Column, Scope, Tokens) ->
  case tokenize_sigil_name(T, [], Line, Column + 1, Scope, Tokens) of
    {ok, Name, Rest, NewLine, NewColumn, NewScope, NewTokens} ->
      tokenize_sigil_contents(Rest, Name, NewLine, NewColumn, NewScope, NewTokens);

    {error, Message, Token} ->
      Reason = {?LOC(Line, Column), Message, Token},
      error(Reason, T, Scope, Tokens)
  end.

% A one-letter sigil is ok both as upcase as well as downcase.
tokenize_sigil_name([S | T], [], Line, Column, Scope, Tokens) when ?is_downcase(S) ->
  tokenize_lower_sigil_name(T, [S], Line, Column + 1, Scope, Tokens);
tokenize_sigil_name([S | T], [], Line, Column, Scope, Tokens) when ?is_upcase(S) ->
    tokenize_upper_sigil_name(T, [S], Line, Column + 1, Scope, Tokens).

tokenize_lower_sigil_name([S | _T] = Original, [_ | _] = NameAcc, _Line, _Column, _Scope, _Tokens) when ?is_downcase(S) ->
  SigilName = lists:reverse(NameAcc) ++ Original,
  {error, sigil_name_error(), [$~] ++ SigilName};
tokenize_lower_sigil_name(T, NameAcc, Line, Column, Scope, Tokens) ->
  {ok, lists:reverse(NameAcc), T, Line, Column, Scope, Tokens}.

% If we have an uppercase letter, we keep tokenizing the name.
% A digit is allowed but an uppercase letter or digit must proceed it.
tokenize_upper_sigil_name([S | T], NameAcc, Line, Column, Scope, Tokens) when ?is_upcase(S); ?is_digit(S) ->
  tokenize_upper_sigil_name(T, [S | NameAcc], Line, Column + 1, Scope, Tokens);
% With a lowercase letter and a non-empty NameAcc we return an error.
tokenize_upper_sigil_name([S | _T] = Original, [_ | _] = NameAcc, _Line, _Column, _Scope, _Tokens) when ?is_downcase(S) ->
  SigilName = lists:reverse(NameAcc) ++ Original,
  {error,  sigil_name_error(), [$~] ++ SigilName};
% We finished the letters, so the name is over.
tokenize_upper_sigil_name(T, NameAcc, Line, Column, Scope, Tokens) ->
  {ok, lists:reverse(NameAcc), T, Line, Column, Scope, Tokens}.

sigil_name_error() ->
  "invalid sigil name, it should be either a one-letter lowercase letter or an " ++
  "uppercase letter optionally followed by uppercase letters and digits, got: ".

tokenize_sigil_contents([H, H, H | T] = Original, [S | _] = SigilName, Line, Column, Scope, Tokens)
    when ?is_quote(H) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, ?is_downcase(S), T, H) of
    {ok, NewLine, NewColumn, Parts, Rest, NewScope} ->
      Indentation = NewColumn - 4,
      add_sigil_token(SigilName, Line, Column, NewLine, NewColumn, Parts, Rest, NewScope, Tokens, Indentation, <<H, H, H>>);

    {error, Reason} ->
      error(Reason, [$~] ++ SigilName ++ Original, Scope, Tokens)
  end;

tokenize_sigil_contents([H | T] = Original, [S | _] = SigilName, Line, Column, Scope, Tokens)
    when ?is_sigil(H) ->
  case elixir_interpolation:extract(Line, Column + 1, Scope, ?is_downcase(S), T, sigil_terminator(H)) of
    {NewLine, NewColumn, Parts, Rest, NewScope} ->
      Indentation = nil,
      add_sigil_token(SigilName, Line, Column, NewLine, NewColumn, tokens_to_binary(Parts), Rest, NewScope, Tokens, Indentation, <<H>>);

    {error, Reason} ->
      Sigil = [$~, S, H],
      Message = " (for sigil ~ts starting at line ~B)",
      interpolation_error(Reason, [$~] ++ SigilName ++ Original, Scope, Tokens, Message, [Sigil, Line], Line, Column, [H], [sigil_terminator(H)])
  end;

tokenize_sigil_contents([H | _] = Original, SigilName, Line, Column, Scope, Tokens) ->
  MessageString =
    "\"~ts\" (column ~p, code point U+~4.16.0B). The available delimiters are: "
    "//, ||, \"\", '', (), [], {}, <>",
  Message = io_lib:format(MessageString, [[H], Column, H]),
  ErrorColumn = Column - 1 - length(SigilName),
  error({?LOC(Line, ErrorColumn), "invalid sigil delimiter: ", Message}, [$~] ++ SigilName ++ Original, Scope, Tokens);

% Incomplete sigil.
tokenize_sigil_contents([], _SigilName, Line, Column, Scope, Tokens) ->
  tokenize([], Line, Column, Scope, Tokens).

add_sigil_token(SigilName, Line, Column, NewLine, NewColumn, Parts, Rest, Scope, Tokens, Indentation, Delimiter) ->
  TokenColumn = Column - 1 - length(SigilName),
  MaybeEncoded = case SigilName of
    % Single-letter sigils present no risk of atom exhaustion (limited possibilities)
    [_Char] -> {ok, list_to_atom("sigil_" ++ SigilName)};
    _ -> unsafe_to_atom("sigil_" ++ SigilName, Line, TokenColumn, Scope)
  end,
  case MaybeEncoded of
    {ok, Atom} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      Token = {sigil, {Line, TokenColumn, nil}, Atom, Parts, Modifiers, Indentation, Delimiter},
      NewColumnWithModifiers = NewColumn + length(Modifiers),
      tokenize(Final, NewLine, NewColumnWithModifiers, Scope, [Token | Tokens]);

    {error, Reason} ->
      error(Reason, Rest, Scope, Tokens)
  end.

%% Fail early on invalid do syntax. For example, after
%% most keywords, after comma and so on.
tokenize_keyword_terminator(DoLine, DoColumn, do, [{identifier, {Line, Column, Meta}, Atom} | T]) ->
  {ok, add_token_with_eol({do, {DoLine, DoColumn, nil}},
                          [{do_identifier, {Line, Column, Meta}, Atom} | T])};
tokenize_keyword_terminator(_Line, _Column, do, [{'fn', _} | _]) ->
  {error, invalid_do_with_fn_error("unexpected reserved word: "), "do"};
tokenize_keyword_terminator(Line, Column, do, Tokens) ->
  case is_valid_do(Tokens) of
    true  -> {ok, add_token_with_eol({do, {Line, Column, nil}}, Tokens)};
    false -> {error, invalid_do_error("unexpected reserved word: "), "do"}
  end;
tokenize_keyword_terminator(Line, Column, Atom, Tokens) ->
  {ok, [{Atom, {Line, Column, nil}} | Tokens]}.

is_valid_do([{Atom, _} | _]) ->
  case Atom of
    ','      -> false;
    ';'      -> false;
    'not'    -> false;
    'and'    -> false;
    'or'     -> false;
    'when'   -> false;
    'in'     -> false;
    'after'  -> false;
    'else'   -> false;
    'catch'  -> false;
    'rescue' -> false;
    _        -> true
  end;
is_valid_do(_) ->
  true.

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
  "where \"some_condition?\" is the first argument and the second argument is a keyword list.\n\n"
  "You may see this error if you forget a trailing comma before the \"do\" in a \"do\" block"}.

invalid_do_with_fn_error(Prefix) ->
  {Prefix, ". Anonymous functions are written as:\n\n"
  "    fn pattern -> expression end\n\nPlease remove the \"do\" keyword"}.

% TODO: Turn into an error on v2.0
maybe_warn_too_many_of_same_char([T | _] = Token, [T | _] = _Rest, Line, Column, Scope) ->
  Message = io_lib:format(
    "found \"~ts\" followed by \"~ts\", please use a space between \"~ts\" and the next \"~ts\"",
    [Token, [T], Token, [T]]
  ),
  prepend_warning(Line, Column, Message, Scope);
maybe_warn_too_many_of_same_char(_Token, _Rest, _Line, _Column, Scope) ->
  Scope.

%% TODO: Turn into an error on v2.0
maybe_warn_for_ambiguous_bang_before_equals(Kind, Unencoded, [$= | _], Line, Column, Scope) ->
  {What, Identifier} =
    case Kind of
      atom -> {"atom", [$: | Unencoded]};
      identifier -> {"identifier", Unencoded}
    end,

  case lists:last(Identifier) of
    Last when Last =:= $!; Last =:= $? ->
      Msg = io_lib:format("found ~ts \"~ts\", ending with \"~ts\", followed by =. "
                          "It is unclear if you mean \"~ts ~ts=\" or \"~ts =\". Please add "
                          "a space before or after ~ts to remove the ambiguity",
                          [What, Identifier, [Last], lists:droplast(Identifier), [Last], Identifier, [Last]]),
      prepend_warning(Line, Column, Msg, Scope);
    _ ->
      Scope
  end;
maybe_warn_for_ambiguous_bang_before_equals(_Kind, _Atom, _Rest, _Line, _Column, Scope) ->
  Scope.

prepend_warning(Line, Column, Msg, #elixir_tokenizer{warnings=Warnings} = Scope) ->
  Scope#elixir_tokenizer{warnings = [{{Line, Column}, Msg} | Warnings]}.

track_ascii(true, Scope) -> Scope;
track_ascii(false, Scope) -> Scope#elixir_tokenizer{ascii_identifiers_only=false}.

maybe_unicode_lint_warnings(_Ascii=false, Tokens, Warnings) ->
  'Elixir.String.Tokenizer.Security':unicode_lint_warnings(lists:reverse(Tokens)) ++ Warnings;
maybe_unicode_lint_warnings(_Ascii=true, _Tokens, Warnings) ->
  Warnings.

error(Reason, Rest, #elixir_tokenizer{warnings=Warnings}, Tokens) ->
  {error, Reason, Rest, Warnings, Tokens}.

%% Cursor handling

cursor_complete(Line, Column, Terminators) ->
  lists:mapfoldl(
    fun({Start, _, _}, AccColumn) ->
      End = terminator(Start),
      {{End, {Line, AccColumn, nil}}, AccColumn + length(erlang:atom_to_list(End))}
    end,
    Column,
    Terminators
  ).

add_cursor(_Line, Column, noprune, Terminators, Tokens) ->
  {Column, Terminators, Tokens};
add_cursor(Line, Column, prune_and_cursor, Terminators, Tokens) ->
  PrePrunedTokens = prune_identifier(Tokens),
  {PrunedTokens, PrunedTerminators} = prune_tokens(PrePrunedTokens, [], Terminators),
  CursorTokens = [
    {')', {Line, Column + 11, nil}},
    {'(', {Line, Column + 10, nil}},
    {paren_identifier, {Line, Column, nil}, '__cursor__'}
    | PrunedTokens
  ],
  {Column + 12, PrunedTerminators, CursorTokens}.

prune_identifier([{identifier, _, _} | Tokens]) -> Tokens;
prune_identifier(Tokens) -> Tokens.

%%% Any terminator needs to be closed
prune_tokens([{'end', _} | Tokens], Opener, Terminators) ->
  prune_tokens(Tokens, ['end' | Opener], Terminators);
prune_tokens([{')', _} | Tokens], Opener, Terminators) ->
  prune_tokens(Tokens, [')' | Opener], Terminators);
prune_tokens([{']', _} | Tokens], Opener, Terminators) ->
  prune_tokens(Tokens, [']' | Opener], Terminators);
prune_tokens([{'}', _} | Tokens], Opener, Terminators) ->
  prune_tokens(Tokens, ['}' | Opener], Terminators);
prune_tokens([{'>>', _} | Tokens], Opener, Terminators) ->
  prune_tokens(Tokens, ['>>' | Opener], Terminators);
%%% Close opened terminators
prune_tokens([{'fn', _} | Tokens], ['end' | Opener], Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
prune_tokens([{'do', _} | Tokens], ['end' | Opener], Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
prune_tokens([{'(', _} | Tokens], [')' | Opener], Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
prune_tokens([{'[', _} | Tokens], [']' | Opener], Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
prune_tokens([{'{', _} | Tokens], ['}' | Opener], Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
prune_tokens([{'<<', _} | Tokens], ['>>' | Opener], Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
%%% Handle anonymous functions
prune_tokens([{'(', _}, {capture_op, _, _} | Tokens], [], [{'(', _, _} | Terminators]) ->
  prune_tokens(Tokens, [], Terminators);
%%% or it is time to stop...
prune_tokens([{';', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'eol', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{',', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'fn', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'do', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'(', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'[', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'{', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{'<<', _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{identifier, _, _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{block_identifier, _, _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{kw_identifier, _, _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{kw_identifier_safe, _, _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{kw_identifier_unsafe, _, _} | _] = Tokens, [], Terminators) ->
  {Tokens, Terminators};
prune_tokens([{OpType, _, _} | _] = Tokens, [], Terminators)
  when OpType =:= comp_op; OpType =:= at_op; OpType =:= unary_op; OpType =:= and_op;
       OpType =:= or_op; OpType =:= arrow_op; OpType =:= match_op; OpType =:= in_op;
       OpType =:= in_match_op; OpType =:= type_op; OpType =:= dual_op; OpType =:= mult_op;
       OpType =:= power_op; OpType =:= concat_op; OpType =:= range_op; OpType =:= xor_op;
       OpType =:= pipe_op; OpType =:= stab_op; OpType =:= when_op; OpType =:= assoc_op;
       OpType =:= rel_op; OpType =:= ternary_op; OpType =:= capture_op; OpType =:= ellipsis_op ->
  {Tokens, Terminators};
%%% or we traverse until the end.
prune_tokens([_ | Tokens], Opener, Terminators) ->
  prune_tokens(Tokens, Opener, Terminators);
prune_tokens([], [], Terminators) ->
  {[], Terminators}.
