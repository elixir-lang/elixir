-module(elixir_bitstring).
-export([translate/3, expand/3, has_size/1]).
-include("elixir.hrl").

%% Expansion

expand(Meta, Args, E) ->
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
  ListArgs = if is_atom(Args) -> []; is_list(Args) -> Args end,
  case expand_bit_type_or_size(Expr, ListArgs) of
    type ->
      { EArgs, EE } = elixir_exp:expand_args(ListArgs, E),
      expand_bit_info(Meta, T, Size, [{ Expr, [], EArgs }|Types], EE);
    size ->
      case Size of
        default -> ok;
        _ -> elixir_errors:compile_error(Meta, E#elixir_env.file, "duplicated size definition in bitstring")
      end,
      { EArgs, EE } = elixir_exp:expand_args(ListArgs, E),
      expand_bit_info(Meta, T, { Expr, [], EArgs }, Types, EE);
    none ->
      handle_unknown_bit_info(Meta, { Expr, ExprMeta, ListArgs }, T, Size, Types, E)
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

%% Translation

has_size({ bin, _, Elements }) ->
  not lists:any(fun({ bin_element, _Line, _Expr, Size, Types }) ->
    (Types /= default) andalso (Size == default) andalso
      lists:any(fun(X) -> lists:member(X, Types) end,
                [bits, bytes, bitstring, binary])
  end, Elements).

translate(Meta, Args, S) ->
  case S#elixir_scope.context of
    match ->
      build_bitstr(fun elixir_translator:translate/2, Args, Meta, S);
    _ ->
      build_bitstr(fun(X, Acc) -> elixir_translator:translate_arg(X, Acc, S) end, Args, Meta, S)
  end.

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

build_bitstr_each(Fun, T, Meta, S, Acc, H, default, Types) when is_binary(H) ->
  Element =
    case types_allow_splice(Types, []) of
      true ->
        %% See explanation in elixir_utils:elixir_to_erl/1 to know
        %% why we can simply convert the binary to a list.
        { bin_element, ?line(Meta), { string, 0, binary_to_list(H) }, default, default };
      false ->
        case types_require_conversion(Types) of
          true ->
            { bin_element, ?line(Meta), { string, 0, elixir_utils:characters_to_list(H) }, default, Types };
          false ->
            elixir_errors:compile_error(Meta, S#elixir_scope.file, "invalid types for literal string in <<>>. "
              "Accepted types are: little, big, utf8, utf16, utf32, bits, bytes, binary, bitstring")
        end
    end,

  build_bitstr_each(Fun, T, Meta, S, [Element|Acc]);

build_bitstr_each(_Fun, _T, Meta, S, _Acc, H, _Size, _Types) when is_binary(H) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file, "size is not supported for literal string in <<>>");

build_bitstr_each(_Fun, _T, Meta, S, _Acc, H, _Size, _Types) when is_list(H); is_atom(H) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file, "invalid literal ~ts in <<>>",
    ['Elixir.Macro':to_string(H)]);

build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types) ->
  { Expr, NS } = Fun(H, S),

  case Expr of
    { bin, _, Elements } ->
      case (Size == default) andalso types_allow_splice(Types, Elements) of
        true  -> build_bitstr_each(Fun, T, Meta, NS, lists:reverse(Elements) ++ Acc);
        false -> build_bitstr_each(Fun, T, Meta, NS, [{ bin_element, ?line(Meta), Expr, Size, Types }|Acc])
      end;
    _ ->
      build_bitstr_each(Fun, T, Meta, NS, [{ bin_element, ?line(Meta), Expr, Size, Types }|Acc])
  end.

types_require_conversion([End|T]) when End == little; End == big -> types_require_conversion(T);
types_require_conversion([UTF|T]) when UTF == utf8; UTF == utf16; UTF == utf32 -> types_require_conversion(T);
types_require_conversion([]) -> true;
types_require_conversion(_) -> false.

types_allow_splice([bytes], Elements)  -> is_byte_size(Elements, 0);
types_allow_splice([binary], Elements) -> is_byte_size(Elements, 0);
types_allow_splice([bits], _)          -> true;
types_allow_splice([bitstring], _)     -> true;
types_allow_splice(default, _)         -> true;
types_allow_splice(_, _)               -> false.

is_byte_size([Element|T], Acc) ->
  case elem_size(Element) of
    {unknown, Unit} when Unit rem 8 == 0 -> is_byte_size(T, Acc);
    {unknown, _Unit} -> false;
    {Size, Unit} -> is_byte_size(T, Size*Unit + Acc)
  end;
is_byte_size([], Size) ->
  Size rem 8 == 0.

elem_size({bin_element, _, _, default, _})              -> {0, 0};
elem_size({bin_element, _, _, {integer,_,Size}, Types}) -> {Size, unit_size(Types, 1)};
elem_size({bin_element, _, _, _Size, Types})            -> {unknown, unit_size(Types, 1)}.

unit_size([binary|T], _)       -> unit_size(T, 8);
unit_size([{unit, Size}|_], _) -> Size;
unit_size([_|T], Guess)        -> unit_size(T, Guess);
unit_size([], Guess)           -> Guess.

%% Extra bitstring specifiers

extract_bit_info(Meta, [{ size, _, [Arg] }|T], S) ->
  case elixir_translator:translate(Arg, S) of
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
