-module(elixir_tokenizer).
-include("elixir.hrl").
-export([tokenize/1]).
-import(elixir_interpolation, [unescape_chars/1, unescape_tokens/1]).

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

-define(LIST_TO_ATOM(I), case ExistingAtomsOnly of 
                true -> list_to_existing_atom(I);
                false -> list_to_atom(I)
              end).

-define(BINARY_TO_ATOM(I,E), case ExistingAtomsOnly of 
                true -> binary_to_existing_atom(I,E);
                false -> binary_to_atom(I,E)
              end).


tokenize(#elixir_tokenizer_context{} = Ctx) ->
  tokenize(Ctx, [], []).

tokenize(#elixir_tokenizer_context{ string = [] }, [], Tokens) ->
  { ok, lists:reverse(Tokens) };

tokenize(#elixir_tokenizer_context{ string = [] , line = EndLine}, [{ Start, StartLine }|_], _Tokens) ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~s (for \"~s\" starting at line ~B)", [End, Start, StartLine]),
  { error, { EndLine, Message, [] } };

% Base integers

tokenize(#elixir_tokenizer_context{ string = [$0,X,H|T], line = Line } = Ctx, Terminators, Tokens) when (X == $x orelse X == $X), ?is_hex(H) ->
  { Rest, Number } = tokenize_hex([H|T], []),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{number,Line,Number}|Tokens]);

tokenize(#elixir_tokenizer_context{ string = [$0,O,H|T], line = Line } = Ctx, Terminators, Tokens) when (O == $o orelse O == $O), ?is_octal(H) ->
  { Rest, Number } = tokenize_octal([H|T], []),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{number,Line,Number}|Tokens]);

tokenize(#elixir_tokenizer_context{ string = [$0,B,H|T], line = Line } = Ctx, Terminators, Tokens) when (B == $b orelse B == $B), ?is_bin(H) ->
  { Rest, Number } = tokenize_bin([H|T], []),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{number,Line,Number}|Tokens]);

% Comments

tokenize(#elixir_tokenizer_context{ string = [$#|String] } = Ctx, Terminators, Tokens) ->
  Rest = tokenize_comment(String),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, Tokens);

% Sigils

tokenize(#elixir_tokenizer_context{ string = [$%,S,H,H,H|T], line = Line, file = File } = Ctx, Terminators, Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, File, ?is_downcase(S), T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(Ctx#elixir_tokenizer_context{ string = Final }, Terminators, [{sigil,Line,S,Parts,Modifiers}|Tokens])
  end;

tokenize(#elixir_tokenizer_context{ string = [$%,S,H|T], line = Line, file = File } = Ctx, Terminators, Tokens) when not(?is_word(H)), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, File, ?is_downcase(S), T, sigil_terminator(H)) of
    { NewLine, Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(Ctx#elixir_tokenizer_context{ string = Final, line = NewLine }, Terminators, [{sigil,Line,S,Parts,Modifiers}|Tokens]);
    Error ->
      Sigil = [$%,S,H],
      interpolation_error(Error, " (for sigil ~s starting at line ~B)", [Sigil, Line])
  end;

% Char tokens

tokenize(#elixir_tokenizer_context{ string = [$?,$\\,H|T], line = Line } = Ctx, Terminators, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  tokenize(Ctx#elixir_tokenizer_context{ string = T }, Terminators, [{number,Line,Char}|Tokens]);

tokenize(#elixir_tokenizer_context{ string = [$?,Char|T], line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = T }, Terminators, [{number,Line,Char}|Tokens]);

% Dot identifier

tokenize(#elixir_tokenizer_context{ string = "..." ++ Rest, line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [tokenize_call_identifier(identifier, Line, '...', Rest)|Tokens]);

% Dot operators

% ## Containers
tokenize(#elixir_tokenizer_context{ string = ".<<>>" ++ Rest } = Ctx, Terminators, Tokens) ->
  handle_call_identifier(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, '<<>>', Tokens);

tokenize(#elixir_tokenizer_context{ string = [$.,T1,T2|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?container2(T1, T2) ->
  handle_call_identifier(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1, T2]), Tokens);

% ## Three Token Operators
tokenize(#elixir_tokenizer_context{ string = [$.,T1,T2,T3|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp3(T1, T2, T3); ?op3(T1, T2, T3) ->
  handle_call_identifier(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1, T2, T3]), Tokens);

% ## Two Token Operators
tokenize(#elixir_tokenizer_context{ string = [$.,T1,T2|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp2(T1, T2); ?op2(T1, T2) ->
  handle_call_identifier(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1, T2]),Tokens);

% ## Single Token Operators
tokenize(#elixir_tokenizer_context{ string = [$.,T|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp1(T); ?op1(T); T == $& ->
  handle_call_identifier(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T]), Tokens);

% Dot call

% ## Exception for .( as it needs to be treated specially in the parser
tokenize(#elixir_tokenizer_context{ string = [$.,$(|Rest], line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = [$(|Rest] }, Terminators, add_token_with_nl({dot_call_op,Line,'.'}, Tokens));

tokenize(#elixir_tokenizer_context{ string = [$.,H|T], line = Line, file = File, 
                                    existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, [Part], Rest } when is_binary(Part) ->
      Atom = ?BINARY_TO_ATOM(Part, utf8),
      tokenize(Ctx#elixir_tokenizer_context{ string =  Rest, line = NewLine }, Terminators,
        [tokenize_call_identifier(identifier, Line, Atom, Rest)|add_token_with_nl({'.',Line}, Tokens)]);
    Error ->
      interpolation_error(Error, " (for function name starting at line ~B)", [Line])
  end;

% Heredocs

tokenize(#elixir_tokenizer_context{ string = "\"\"\"" ++ T } = Ctx, Terminators, Tokens) -> 
  handle_heredocs(Ctx#elixir_tokenizer_context{ string = T }, Terminators, $", Tokens);
tokenize(#elixir_tokenizer_context{ string = "'''" ++ T } = Ctx, Terminators, Tokens)    -> 
  handle_heredocs(Ctx#elixir_tokenizer_context{ string = T }, Terminators, $', Tokens);

% Strings

tokenize(#elixir_tokenizer_context{ string = [$"|T] } = Ctx, Terminators, Tokens) -> 
  handle_strings(Ctx#elixir_tokenizer_context{ string = T }, Terminators, $", Tokens);
tokenize(#elixir_tokenizer_context{ string = [$'|T] } = Ctx, Terminators, Tokens) -> 
  handle_strings(Ctx#elixir_tokenizer_context{ string = T }, Terminators, $', Tokens);

% Atoms

tokenize(#elixir_tokenizer_context{ string = [$:,T|String], line = Line, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?is_upcase(T); ?is_downcase(T); T == $_ ->
  { Rest, Atom } = tokenize_atom([T|String], [], ExistingAtomsOnly),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{atom,Line,[Atom]}|Tokens]);

tokenize(#elixir_tokenizer_context{ string = [$:,H|T], line = Line, file = File } = Ctx, Terminators, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, Parts, Rest } -> tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = NewLine }, Terminators, [{atom,Line,unescape_tokens(Parts)}|Tokens]);
    Error ->
      interpolation_error(Error, " (for atom starting at line ~B)", [Line])
  end;

% Atom operators

% ## Containers
tokenize(#elixir_tokenizer_context{ string = ":<<>>" ++ Rest, line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{atom,Line,['<<>>']}|Tokens]);

tokenize(#elixir_tokenizer_context{ string = [$:,T1,T2|Rest], line = Line, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?container2(T1, T2) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest}, Terminators, [{atom,Line,[?LIST_TO_ATOM([T1,T2])]}|Tokens]);

% ## Three Token Operators
tokenize(#elixir_tokenizer_context{ string = [$:,T1,T2,T3|Rest], line = Line, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp3(T1, T2, T3); ?op3(T1, T2, T3)  ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{atom,Line,[?LIST_TO_ATOM([T1,T2,T3])]}|Tokens]);

% ## Two Token Operators
tokenize(#elixir_tokenizer_context{ string = [$:,T1,T2|Rest], line = Line, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp2(T1, T2); ?op2(T1, T2) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{atom,Line,[?LIST_TO_ATOM([T1,T2])]}|Tokens]);

% ## Single Token Operators
tokenize(#elixir_tokenizer_context{ string = [$:,T|Rest], line = Line, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp1(T); ?op1(T); T == $&; T == $. ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{atom,Line,[?LIST_TO_ATOM([T])]}|Tokens]);


% End of line

tokenize(#elixir_tokenizer_context{ string = ";" ++ Rest, line = Line } = Ctx, Terminators, []) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, eol(Line, $;, []));

tokenize(#elixir_tokenizer_context{ string = ";" ++ Rest, line = Line } = Ctx, Terminators, [Top|Tokens]) when element(1, Top) /= eol ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, eol(Line, $;, [Top|Tokens]));

tokenize(#elixir_tokenizer_context{ string = "\\\n" ++ Rest, line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = Line + 1 }, Terminators, Tokens);

tokenize(#elixir_tokenizer_context{ string = "\\\r\n" ++ Rest, line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = Line + 1 }, Terminators, Tokens);

tokenize(#elixir_tokenizer_context{ string = "\n" ++ Rest, line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = Line + 1 }, Terminators, eol(Line, $\n, Tokens));

tokenize(#elixir_tokenizer_context{ string = "\r\n" ++ Rest, line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = Line + 1 }, Terminators, eol(Line, $\n, Tokens));

% Stand-alone tokens

% ## &
tokenize(#elixir_tokenizer_context{ string = [$&,H|Rest], line = Line } = Ctx, Terminators, Tokens) when ?is_digit(H) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{'&', Line, [list_to_integer([H])]}|Tokens]);

% ## Comparison three token operators
tokenize(#elixir_tokenizer_context{ string = [T1,T2,T3|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp3(T1, T2, T3) ->
  handle_comp_op(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1,T2,T3]), Tokens);

% ## Three token operators
tokenize(#elixir_tokenizer_context{ string = [T1,T2,T3|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?op3(T1, T2, T3) ->
  handle_op(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1,T2,T3]), Tokens);

% ## Containers + punctuation tokens

tokenize(#elixir_tokenizer_context{ string = [T,T|Rest], line = Line, file = File, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when T == $<; T == $> ->
  Token = { ?LIST_TO_ATOM([T,T]), Line },
  case handle_terminator(File, Token, Terminators) of
    { error, _ } = Error -> Error;
    New -> tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, New, [Token|Tokens])
  end;

tokenize(#elixir_tokenizer_context{ string = [T|Rest], line = Line, file = File, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when T == $(;
    T == ${; T == $}; T == $[; T == $]; T == $); T == $, ->
  Token = { ?LIST_TO_ATOM([T]), Line },
  case handle_terminator(File, Token, Terminators) of
    { error, _ } = Error -> Error;
    New -> tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, New, [Token|Tokens])
  end;

% ## Comparison two token operators
tokenize(#elixir_tokenizer_context{ string = [T1,T2|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp2(T1, T2) ->
  handle_comp_op(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1,T2]), Tokens);

% ## Two Token Operators
tokenize(#elixir_tokenizer_context{ string = [T1,T2|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?op2(T1, T2) ->
  handle_op(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T1,T2]), Tokens);

% ## Comparison single token operators
tokenize(#elixir_tokenizer_context{ string = [T|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?comp1(T) ->
  handle_comp_op(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T]), Tokens);

% ## Single Token Operators
tokenize(#elixir_tokenizer_context{ string = [T|Rest], existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?op1(T) ->
  handle_op(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, ?LIST_TO_ATOM([T]), Tokens);

tokenize(#elixir_tokenizer_context{ string = [$.|Rest], line = Line } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, add_token_with_nl({'.', Line}, Tokens));

% Integers and floats

tokenize(#elixir_tokenizer_context{ string = [H|_] = String, line = Line } = Ctx, Terminators, Tokens) when ?is_digit(H) ->
  { Rest, Number } = tokenize_number(String, [], false),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{number,Line,Number}|Tokens]);

% Aliases

tokenize(#elixir_tokenizer_context{ string = [H|_] = String, line = Line, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, Tokens) when ?is_upcase(H) ->
  { Rest, Alias } = tokenize_identifier(String, [], false),
  Atom = ?LIST_TO_ATOM(Alias),

  { Final, Token } =
    case Rest of
      [$:|T] when hd(T) /= $: -> { T, { kw_identifier,Line,Atom } };
      _ -> { Rest, { '__aliases__', Line, [Atom] } }
    end,

  tokenize(Ctx#elixir_tokenizer_context{ string = Final }, Terminators, [Token|Tokens]);

% Identifier

tokenize(#elixir_tokenizer_context{ string = [H|_] = String, file = File, line = Line} = Ctx, Terminators, Tokens) when ?is_downcase(H); H == $_ ->
  { Rest, { Kind, _, Identifier } } = tokenize_any_identifier(Ctx#elixir_tokenizer_context{ string = String }, []),
  case handle_keyword(Line, Kind, Identifier, Tokens) of
    false ->
      tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{Kind,Line,Identifier}|Tokens]);
    [Check|T] ->
      case handle_terminator(File, Check, Terminators) of
        { error, _ } = Error -> Error;
        New -> tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, New, [Check|T])
      end
  end;

% Ambiguous unary/binary operators tokens

tokenize(#elixir_tokenizer_context{ string = [Space,Sign,NotMarker|T], line = Line } = Ctx, Terminators, [{Identifier,_,_} = H|Tokens]) when Sign == $+ orelse Sign == $-,
  Space == $\s orelse Space == $\t,
  NotMarker /= $\s, NotMarker /= $\t, NotMarker /= $\r,
  NotMarker /= $\n, NotMarker /= $:, NotMarker /= $(,
  NotMarker /= $+, NotMarker /= $-, NotMarker /= $>,
  Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{list_to_atom([Sign]),Line},setelement(1, H, op_identifier)|Tokens]);

% Spaces

tokenize(#elixir_tokenizer_context{ string = [T|Rest] } = Ctx, Terminators, Tokens) when T == $\s; T == $\r; T == $\t ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, Tokens);

tokenize(#elixir_tokenizer_context{ string = [{line,Line}|Rest] } = Ctx, Terminators, Tokens) ->
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = Line }, Terminators, Tokens);

tokenize(#elixir_tokenizer_context{ string = T, line = Line }, _Terminators, _Tokens) ->
  { error, { Line, "invalid token: ", until_eol(T) } }.

until_eol("\r\n" ++ _) -> [];
until_eol("\n" ++ _)   -> [];
until_eol([])          -> [];
until_eol([H|T])       -> [H|until_eol(T)].

%% Handlers

handle_heredocs(#elixir_tokenizer_context{ string = T, line = Line, file = File } = Ctx, Terminators, H, Tokens) ->
  case extract_heredoc_with_interpolation(Line, File, true, T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      Token = { string_type(H), Line, unescape_tokens(Parts) },
      tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [Token|Tokens])
  end.

handle_strings(#elixir_tokenizer_context{ string = T, line = Line, file = File, existing_atoms_only = ExistingAtomsOnly } = Ctx, Terminators, H, Tokens) ->
  case elixir_interpolation:extract(Line, File, true, T, H) of
    { NewLine, Parts, [$:|Rest] } when hd(Rest) /= $: ->
      case Parts of
        [Bin] when is_binary(Bin) ->
          Atom = ?BINARY_TO_ATOM(unescape_chars(Bin), utf8),
          tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = NewLine }, Terminators, [{kw_identifier,Line,Atom}|Tokens]);
        _ ->
          { error, { Line, "invalid interpolation in key", [$"|T] } }
      end;
    { NewLine, Parts, Rest } ->
      Token = { string_type(H),Line,unescape_tokens(Parts) },
      tokenize(Ctx#elixir_tokenizer_context{ string = Rest, line = NewLine }, Terminators, [Token|Tokens]);
    Error ->
      interpolation_error(Error, " (for string starting at line ~B)", [Line])
  end.

handle_comp_op(#elixir_tokenizer_context{ string = [$:|Rest], line = Line, file = File } = Ctx, Terminators, Op, Tokens) when hd(Rest) /= $: ->
  verify_kw_and_space(Line, File, Op, Rest),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{kw_identifier, Line, Op}|Tokens]);

handle_comp_op(#elixir_tokenizer_context{ string = _Rest, line = Line } = Ctx, Terminators, Op, Tokens) ->
  tokenize(Ctx, Terminators, add_token_with_nl({comp_op, Line, Op}, Tokens)).

handle_op(#elixir_tokenizer_context{ string = [$:|Rest], line = Line, file = File } = Ctx, Terminators, Op, Tokens) when hd(Rest) /= $: ->
  verify_kw_and_space(Line, File, Op, Rest),
  tokenize(Ctx#elixir_tokenizer_context{ string = Rest }, Terminators, [{kw_identifier, Line, Op}|Tokens]);

handle_op(#elixir_tokenizer_context{ string = _Rest, line = Line } = Ctx, Terminators, Op, Tokens) when ?unary_op(Op) ->
  tokenize(Ctx, Terminators, [{Op, Line}|Tokens]);

handle_op(#elixir_tokenizer_context{ string = _Rest, line = Line } = Ctx, Terminators, Op, Tokens) ->
  tokenize(Ctx, Terminators, add_token_with_nl({Op, Line}, Tokens)).

handle_call_identifier(#elixir_tokenizer_context{ string = Rest, line = Line } = Ctx, Terminators, Op, Tokens) ->
  Token = tokenize_call_identifier(identifier, Line, Op, Rest),
  tokenize(Ctx, Terminators, [Token|add_token_with_nl({'.',Line}, Tokens)]).

%% Helpers

eol(_Line, _Mod, [{',',_}|_] = Tokens)   -> Tokens;
eol(_Line, _Mod, [{eol,_,_}|_] = Tokens) -> Tokens;
eol(Line, Mod, Tokens) -> [{eol,Line,Mod}|Tokens].

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
tokenize_atom(String, Acc, ExistingAtomsOnly) ->
  { Rest, Identifier } = tokenize_identifier(String, Acc, atom),
  case Rest of
    [H|T] when H == $?; H == $! ->
      { T, ?ELIXIR_ATOM_CONCAT([Identifier, [H]]) };
    _ ->
      { Rest, ?LIST_TO_ATOM(Identifier) }
  end.

% Tokenize any identifier, handling kv, punctuated, paren, bracket and do identifiers.
tokenize_any_identifier(#elixir_tokenizer_context{ string = String, file = File, line = Line, existing_atoms_only = ExistingAtomsOnly }, Acc) ->
  { Rest, Identifier } = tokenize_identifier(String, Acc, false),
  case Rest of
    [H,$:|T] when H == $? orelse H == $!, hd(T) /= $: ->
      Atom = ?ELIXIR_ATOM_CONCAT([Identifier, [H]]),
      verify_kw_and_space(Line, File, Atom, T),
      { T, { kw_identifier, Line, Atom } };
    [H|T] when H == $?; H == $! ->
      Atom = ?ELIXIR_ATOM_CONCAT([Identifier, [H]]),
      { T, tokenize_call_identifier(punctuated_identifier, Line, Atom, T) };
    [$:|T] when hd(T) /= $: ->
      Atom = ?LIST_TO_ATOM(Identifier),
      verify_kw_and_space(Line, File, Atom, T),
      { T, { kw_identifier, Line, Atom } };
    _ ->
      { Rest, tokenize_call_identifier(identifier, Line, ?LIST_TO_ATOM(Identifier), Rest) }
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

verify_kw_and_space(_Line, _File, _Atom, [H|_]) when ?is_space(H) -> ok;
verify_kw_and_space(Line, File, Atom, _) ->
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

add_token_with_nl(Left, [{eol,_,$\n}|T]) -> [Left|T];
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

%% In case File is "__internal__", we don't
%% do any of the terminator checks

handle_terminator(<<"__internal__">>, _, []) -> [];
handle_terminator(_File, Token, Terminator) ->
  handle_terminator(Token, Terminator).

handle_terminator({ S, Line }, Terminators) when S == 'fn'; S == 'fn_paren' ->
  [{ fn, Line }|Terminators];

handle_terminator({ S, _ } = New, Terminators) when
    S == 'do';
    S == '(';
    S == '[';
    S == '{';
    S == '<<' ->
  [New|Terminators];

handle_terminator({ E, _ }, [{ S, _ }|Terminators]) when
    S == 'do', E == 'end';
    S == 'fn', E == 'end';
    S == '(',  E == ')';
    S == '[',  E == ']';
    S == '{',  E == '}';
    S == '<<', E == '>>' ->
  Terminators;

handle_terminator({ E, Line }, [{ Start, StartLine }|_]) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~s (for \"~s\" starting at line ~B)", [End, Start, StartLine]),
  { error, { Line, Message, [] } };

handle_terminator({ E, Line }, []) when
    E == 'end'; E == ')'; E == ']'; E == '}'; E == '>>' ->
  { error, { Line, "unexpected token: ", atom_to_list(E) } };

handle_terminator(_, Terminators) ->
  Terminators.

terminator('fn') -> 'end';
terminator('do') -> 'end';
terminator('(')  -> ')';
terminator('[')  -> ']';
terminator('{')  -> '}';
terminator('<<') -> '>>'.

% Keywords check
handle_keyword(Line, paren_identifier, fn, Tokens) ->
  [{ 'fn_paren', Line }|Tokens];

handle_keyword(Line, Identifier, Atom, Tokens) when Identifier ==  bracket_identifier; Identifier == paren_identifier ->
  case keyword(Atom) of
    true -> [{ Atom, Line }|Tokens];
    _    -> [{ Identifier, Line, Atom }|Tokens]
  end;

handle_keyword(Line, Identifier, Atom, Tokens) when Identifier ==  identifier; Identifier == do_identifier ->
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