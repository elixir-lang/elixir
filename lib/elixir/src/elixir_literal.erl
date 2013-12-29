%% Handle translation of Elixir literals to Erlang AST.
-module(elixir_literal).
-export([translate/2, expand_bitstr/3]).
-import(elixir_translator, [translate_each/2, translate_args/2]).
-include("elixir.hrl").

%% TODO: Move most of those contents to translation

translate({ '<<>>', Meta, Args }, S) when is_list(Args) ->
  case S#elixir_scope.context of
    match ->
      build_bitstr(fun elixir_translator:translate_each/2, Args, Meta, S);
    _ ->
      build_bitstr(fun(X, Acc) -> elixir_translator:translate_arg(X, Acc, S) end, Args, Meta, S)
  end;

translate({ '{}', Meta, Args }, S) when is_list(Args) ->
  { TArgs, SE } = translate_args(Args, S),
  { { tuple, ?line(Meta), TArgs }, SE };

translate({ Left, Right }, S) ->
  translate({ '{}', [], [Left, Right]}, S);

translate([], S) ->
  { { nil, 0 }, S };

translate([_|_] = Args, S) ->
  [RTail|RArgs] = lists:reverse(Args),

  case RTail of
    { '|', _, [Left,Right] } ->
      RExprs = [Left|RArgs],
      TailFun = fun(ST) -> translate_each(Right, ST) end;
    _ ->
      RExprs = [RTail|RArgs],
      TailFun = fun(ST) -> { { nil, 0 }, ST } end
  end,

  { Exprs, SE } = translate_args(lists:reverse(RExprs), S),
  { Tail, ST }  = TailFun(SE),
  { elixir_utils:list_to_cons(0, Exprs, Tail), ST };

translate(Other, S) ->
  { elixir_utils:elixir_to_erl(0, Other, S), S }.

%% Helpers

expand_bitstr(Meta, Args, E) ->
  case E#elixir_env.context of
    match ->
      { EArgs, EA } = expand_bitstr(fun elixir_exp:expand/2, Args, [], E),
      { { '<<>>', Meta, EArgs }, EA };
    _ ->
      { EArgs, { EC, EV } } = expand_bitstr(fun elixir_exp:expand_arg/2, Args, [], { E, E }),
      { { '<<>>', Meta, EArgs }, elixir_env:mergea(EV, EC) }
  end.

expand_bitstr(_Fun, [], Acc, E) ->
  { lists:reverse(Acc), E };
expand_bitstr(Fun, [{'::',Meta,[Left,Right]}|T], Acc, E) ->
  { ELeft, EL } = Fun(Left, E),

  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  case E of
    { ER, _ } -> ok;                    %% expand_arg,  no assigns
    _ -> ER = E#elixir_env{context=nil} %% expand_each, revert assigns
  end,

  ERight = expand_bit_info(Meta, Right, ER),
  expand_bitstr(Fun, T, [{'::',Meta,[ELeft,ERight]}|Acc], EL);

expand_bitstr(Fun, [H|T], Acc, E) ->
  { Expr, ES } = Fun(H, E),
  expand_bitstr(Fun, T, [Expr|Acc], ES).

%% Expand bit info

expand_bit_info(Meta, Info, E) when is_list(Info) ->
  expand_bit_info(Meta, Info, default, [], E);

expand_bit_info(Meta, Info, E) ->
  expand_bit_info(Meta, [Info], E).

expand_bit_info(Meta, [{ Expr, ExprMeta, Args }|T], Size, Types, E) when is_atom(Expr) ->
  EArgs = if is_atom(Args) -> []; is_list(Args) -> Args end,
  case expand_bit_type_or_size(Expr, EArgs) of
    type ->
      expand_bit_info(Meta, T, Size, [{ Expr, [], EArgs }|Types], E);
    size ->
      case Size of
        default -> ok;
        _ -> elixir_errors:compile_error(Meta, E#elixir_env.file, "duplicated size definition in bitstring")
      end,
      expand_bit_info(Meta, T, { Expr, [], EArgs }, Types, E);
    none ->
      handle_unknown_bit_info(Meta, { Expr, ExprMeta, EArgs }, T, Size, Types, E)
  end;

expand_bit_info(Meta, [Int|T], Size, Types, E) when is_integer(Int) ->
  expand_bit_info(Meta, [{ size, [], [Int] }|T], Size, Types, E);

expand_bit_info(Meta, [Expr|_], _Size, _Types, E) ->
  elixir_errors:compile_error(Meta, E#elixir_env.file,
    "unknown bitstring specifier ~ts", ['Elixir.Kernel':inspect(Expr)]);

expand_bit_info(_Meta, [], Size, Types, _) ->
  case Size of
    default -> lists:reverse(Types);
    _ -> [Size|lists:reverse(Types)]
  end.

expand_bit_type_or_size(binary, [])    -> type;
expand_bit_type_or_size(integer, [])   -> type;
expand_bit_type_or_size(float, [])     -> type;
expand_bit_type_or_size(bitstring, []) -> type;
expand_bit_type_or_size(bytes, [])     -> type;
expand_bit_type_or_size(bits, [])      -> type;
expand_bit_type_or_size(utf8, [])      -> type;
expand_bit_type_or_size(utf16, [])     -> type;
expand_bit_type_or_size(utf32, [])     -> type;
expand_bit_type_or_size(signed, [])    -> type;
expand_bit_type_or_size(unsigned, [])  -> type;
expand_bit_type_or_size(big, [])       -> type;
expand_bit_type_or_size(little, [])    -> type;
expand_bit_type_or_size(native, [])    -> type;
expand_bit_type_or_size(unit, [_])     -> type;
expand_bit_type_or_size(size, [_])     -> size;
expand_bit_type_or_size(_, _)          -> none.

handle_unknown_bit_info(Meta, { _, ExprMeta, _ } = Expr, T, Size, Types, E) ->
  case 'Elixir.Macro':expand(Expr, elixir_env:env_to_ex({ ?line(ExprMeta), E })) of
    Expr ->
      elixir_errors:compile_error(ExprMeta, E#elixir_env.file,
        "unknown bitstring specifier ~ts", ['Elixir.Macro':to_string(Expr)]);
    Other ->
      List = case is_list(Other) of true -> Other; false -> [Other] end,
      expand_bit_info(Meta, List ++ T, Size, Types, E)
  end.

%% Helpers

build_bitstr(Fun, Exprs, Meta, S) ->
  { Final, FinalS } = build_bitstr_each(Fun, Exprs, Meta, S, []),
  { { bin, ?line(Meta), lists:reverse(Final) }, FinalS }.

build_bitstr_each(_Fun, [], _Meta, S, Acc) ->
  { Acc, S };

build_bitstr_each(Fun, [{'::',_,[H,V]}|T], Meta, S, Acc) ->
  { Size, Types } = extract_bit_info(Meta, V, S#elixir_scope{context=nil}),
  build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types);

build_bitstr_each(Fun, [H|T], Meta, S, Acc) ->
  build_bitstr_each(Fun, T, Meta, S, Acc, H, default, default).

build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types) ->
  { Expr, NS } = Fun(H, S),

  AllowString = types_allow_string(Types),
  AllowSplice = types_allow_splice(Types),
  AllowAny    = (AllowString orelse AllowSplice) andalso (Size == default),

  case AllowAny andalso Expr of
    { bin, _, [{ bin_element, 0, { string, 0, String }, default, default }] } when AllowString ->
      build_bitstr_each(Fun, T, Meta, NS, [{ bin_element, ?line(Meta), { string, 0, String }, Size, Types }|Acc]);
    { bin, _, Elements } when AllowSplice ->
      build_bitstr_each(Fun, T, Meta, NS, lists:reverse(Elements) ++ Acc);
    { cons, _, _, _ } = Cons ->
      build_bitstr_each(Fun, T, Meta, NS, rehash_cons(Cons, Size, Types, []) ++ Acc);
    { nil, _ } ->
      build_bitstr_each(Fun, T, Meta, NS, Acc);
    _ ->
      build_bitstr_each(Fun, T, Meta, NS, [{ bin_element, ?line(Meta), Expr, Size, Types }|Acc])
  end.

rehash_cons({ nil, _ }, _Size, _Types, Acc) -> Acc;
rehash_cons({ cons, Line, Left, Right }, Size, Types, Acc) ->
  rehash_cons(Right, Size, Types, [{ bin_element, Line, Left, Size, Types }|Acc]).

types_allow_string([End|T]) when End == little; End == big -> types_allow_string(T);
types_allow_string([UTF|T]) when UTF == utf8; UTF == utf16; UTF == utf32 -> types_allow_string(T);
types_allow_string([]) -> true;
types_allow_string(_) -> false.

types_allow_splice(default) -> true;
types_allow_splice([bytes]) -> true;
types_allow_splice([binary]) -> true;
types_allow_splice([bits]) -> true;
types_allow_splice([bitstring]) -> true;
types_allow_splice(_) -> false.

%% Extra bitstring specifiers

extract_bit_info(Meta, [{ size, _, [Arg] }|T], S) ->
  case elixir_translator:translate_each(Arg, S) of
    { { Kind, _, _ } = Size, _ } when Kind == integer; Kind == var ->
      { Size, extract_bit_type(Meta, T, S) };
    _ ->
      elixir_errors:compile_error(Meta, S#elixir_scope.file,
        "size in bitstring expects an integer or a variable as argument, got: ~ts", ['Elixir.Macro':to_string(Arg)])
  end;
extract_bit_info(Meta, T, S) ->
  { default, extract_bit_type(Meta, T, S) }.

extract_bit_type(Meta, [{ unit, _, [Arg] }|T], S) when is_integer(Arg) ->
  [{ unit, Arg }|extract_bit_type(Meta, T, S)];
extract_bit_type(Meta, [{ unit, _, [Arg] }|_], S) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file,
    "unit in bitstring expects an integer as argument, got: ~ts", ['Elixir.Macro':to_string(Arg)]);
extract_bit_type(Meta, [{ Other, _, [] }|T], S) ->
  [Other|extract_bit_type(Meta, T, S)];
extract_bit_type(_Meta, [], _S) ->
  [].
