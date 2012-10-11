-module(elixir_literal).
-export([translate/2]).
-import(elixir_translator, [translate_each/2, translate_args/2]).
-import(elixir_scope, [umergev/2, umergec/2]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

translate({ '<<>>', Line, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S, allow_tail) of
    error ->
      case S#elixir_scope.context of
        assign ->
          build_bitstr(fun elixir_translator:translate_each/2, Args, Line, S);
        _ ->
          { TArgs, { SC, SV } } = build_bitstr(fun elixir_translator:translate_arg/2, Args, Line, { S, S }),
          { TArgs, umergec(SV, SC) }
      end;
    Else -> Else
  end;

translate({ '{}', Line, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S) of
    error ->
      { TArgs, SE } = translate_args(Args, S),
      { {tuple, Line, TArgs}, SE };
    Else -> Else
  end;

translate({ '[]', _Line, [] }, S) ->
  { { nil, 0 }, S };

translate({ '[]', _Line, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S, allow_tail) of
    error ->
      [RTail|RArgs] = lists:reverse(Args),

      case RTail of
        {'|',_,[Left,Right]} ->
          Exprs = [Left|RArgs],
          { Tail, ST } = translate_each(Right, S),
          ListS = umergec(S, ST);
        _ ->
          Exprs = [RTail|RArgs],
          Tail = { nil, 0 },
          ST = S,
          ListS = S
      end,

      { FExprs, FS } = case S#elixir_scope.context of
        assign ->
          elixir_tree_helpers:build_reverse_list(fun elixir_translator:translate_each/2, Exprs, 0, ListS, Tail);
        _ ->
          { TArgs, { SC, SV } } =
            elixir_tree_helpers:build_reverse_list(fun elixir_translator:translate_arg/2, Exprs, 0, { ListS, ListS }, Tail),
          { TArgs, umergec(SV, SC) }
      end,

      { FExprs, umergev(ST, FS) };
    Else -> Else
  end;

translate({ Left, Right }, S) ->
  translate({ '{}', 0, [Left, Right]}, S);

translate(Args, S) when is_list(Args) ->
  translate({ '[]', 0, Args }, S);

translate(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.
  
%% Helpers

% Build a bitstring taking into accounts the following types:
%
% * If a bitstring or a list is given, we just append its items
% * If '|' is given, extract the bitstring information
% * All the other types are simply translated and handled with Erlang's default
%
build_bitstr(Fun, Exprs, Line, S) ->
  { Final, FinalS } = build_bitstr_each(Fun, Exprs, Line, S, []),
  { { bin, Line, lists:reverse(Final) }, FinalS }.

build_bitstr_each(_Fun, [], _Line, S, Acc) ->
  { Acc, S };

build_bitstr_each(Fun, [H|T], Line, S, Acc) when is_list(H) ->
  { NewAcc, NewS } = build_bitstr_each(Fun, H, Line, S, Acc),
  build_bitstr_each(Fun, T, Line, NewS, NewAcc);

build_bitstr_each(Fun, [H|T], Line, S, Acc) when is_bitstring(H) ->
  { bin, _, Elements } = elixir_tree_helpers:abstract_syntax(H),
  NewAcc = lists:foldl(fun(Element, FinalAcc) -> [Element|FinalAcc] end, Acc, Elements),
  build_bitstr_each(Fun, T, Line, S, NewAcc);

build_bitstr_each(Fun, [{'::',_,[H,V]}|T], Line, S, Acc) ->
  { Expr, NS } = Fun(H, S),

  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  case NS of
    { ES, _ } -> [];                       %% translate_arg,  no assigns
    _ -> ES = NS#elixir_scope{context=nil} %% translate_each, revert assigns
  end,

  { Size, Types } = extract_bit_values(Line, V, ES),
  build_bitstr_each(Fun, T, Line, NS, [{ bin_element, Line, Expr, Size, Types }|Acc]);

%% Deprecated syntax
build_bitstr_each(Fun, [{'|',_,[H,V]}|T], Line, S, Acc) ->
  { Expr, NS } = Fun(H, S),

  %% Just variables defined outside the binary can be accounted on subparts
  case NS of
    { ES, _ } -> [];
    ES -> []
  end,

  elixir_errors:deprecation(Line, ES#elixir_scope.file, "Old bitstring syntax with | is deprecated, use the new one with :: instead"),

  %% Assigns can be made in subparts
  { Int, Types } = extract_bin_values(Line, V, default, [], ES#elixir_scope{context=nil}),

  Final = case Types of
    [] -> default;
    _  -> lists:reverse(Types)
  end,

  build_bitstr_each(Fun, T, Line, NS, [{ bin_element, Line, Expr, Int, Final }|Acc]);

build_bitstr_each(Fun, [H|T], Line, S, Acc) ->
  { Expr, NS } = Fun(H, S),
  build_bitstr_each(Fun, T, Line, NS, [{ bin_element, Line, Expr, default, default }|Acc]).

%% Extra bitstring specifiers

extract_bit_values(Line, V, S) when is_list(V) ->
  extract_bit_values(Line, V, default, [], S);

extract_bit_values(Line, V, S) ->
  extract_bit_values(Line, [V], S).

extract_bit_values(Line, [{ Value, _CallLine, Atom }|T] = All, Size, Types, S)
    when is_atom(Value) andalso (is_atom(Atom) orelse Atom == []) ->
  case extract_bit_type(Value, Types) of
    { error, unknown } ->
      handle_unknown_specifier(Line, All, Size, Types, S);
    NewTypes when is_list(NewTypes) ->
      extract_bit_values(Line, T, Size, NewTypes, S)
  end;

extract_bit_values(Line, [{ Value, CallLine, [_] = Args }|T] = All, Size, Types, S) when is_atom(Value) ->
  { TArgs, _ } = elixir_translator:translate_args(Args, S),
  case extract_bit_type_or_size(Value, TArgs, Size, Types) of
    { error, unknown } ->
      handle_unknown_specifier(Line, All, Size, Types, S);
    { error, Msg } ->
      elixir_errors:syntax_error(CallLine, S#elixir_scope.file, Msg);
    { NewSize, NewTypes } ->
      extract_bit_values(Line, T, NewSize, NewTypes, S)
  end;

extract_bit_values(Line, [{ '|', _, [Left, Right] }], Size, Types, S) ->
  Expanded = 'Elixir.Macro':expand(Right, elixir_scope:to_ex_env({ Line, S })),
  extract_bit_values(Line, [Left|join_expansion(Expanded,[])], Size, Types, S);

extract_bit_values(Line, [_|_] = All, Size, Types, S) ->
  handle_unknown_specifier(Line, All, Size, Types, S);

extract_bit_values(_Line, [], Size, [], _S) ->
  { Size, default };

extract_bit_values(_Line, [], Size, Types, _S) ->
  { Size, lists:reverse(Types) }.

extract_bit_type(Value, Types) when
    Value == binary; Value == integer; Value == float; Value == bitstring;
    Value == bytes; Value == bits;
    Value == utf8; Value == utf16; Value == utf32;
    Value == signed; Value == unsigned;
    Value == big; Value == little; Value == native ->
  [Value|Types];

extract_bit_type(_Value, _Types) ->
  { error, unknown }.

extract_bit_type_or_size(size, [{ Kind, _, _ } = Arg], default, Types) when Kind == var; Kind == integer ->
  { Arg, Types };

extract_bit_type_or_size(size, _Args, default, _Types) ->
  { error, "size in bitstring expects an integer or a variable as argument" };

extract_bit_type_or_size(size, _Args, _Other, _Types) ->
  { error, "duplicated size definition for bitstring" };

extract_bit_type_or_size(unit, [{ integer, _, Arg }], Size, Types) ->
  { Size, [{ unit, Arg }|Types] };

extract_bit_type_or_size(unit, _Args, _Other, _Types) ->
  { error, "unit in bitstring expects an integer as argument" };

extract_bit_type_or_size(_Value, _Args, _Other, _Types) ->
  { error, unknown }.

handle_unknown_specifier(Line, [H|T], Size, Types, S) ->
  case 'Elixir.Macro':expand(H, elixir_scope:to_ex_env({ Line, S })) of
    H ->
      elixir_errors:syntax_error(Line, S#elixir_scope.file, "unknown bitstring specifier ~s", ['Elixir.Macro':to_binary(H)]);
    E ->
      extract_bit_values(Line, join_expansion(E,T), Size, Types, S)
  end.

join_expansion({ '__block__', _, [Expanded] }, Tail) -> join_expansion(Expanded, Tail);
join_expansion(Expanded, Tail) when is_list(Expanded) -> Expanded ++ Tail;
join_expansion(Expanded, Tail) -> [Expanded|Tail].

%% Deprecated specifiers

extract_bin_values(Line, { '-', _Line, [Left, Right] }, Int, Types, S) ->
  { LInt, LTypes } = extract_bin_values(Line, Left, Int, Types, S),
  extract_bin_values(Line, Right, LInt, LTypes, S);

extract_bin_values(Line, Value, default, Types, _S) when is_integer(Value) ->
  { { integer, Line, Value }, Types };

extract_bin_values(_Line, { Value, _, Atom } = Expr, default, Types, S) when is_atom(Value), is_atom(Atom) ->
  Translated = element(1, elixir_translator:translate_each(Expr, S)),
  { Translated, Types };

extract_bin_values(Line, Value, _Int, _Types, S) when is_integer(Value) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "duplicated size specifier ~p in <<>>", [Value]);

extract_bin_values(_Line, Value, Int, Types, _S) when is_atom(Value); is_tuple(Value), tuple_size(Value) == 2 ->
  { Int, [Value|Types] };

extract_bin_values(Line, _Value, _Int, _Types, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "invalid specifier for <<>>").