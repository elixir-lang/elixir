-module(elixir_bitstring).
-export([translate/3, expand/3, has_size/1]).
-include("elixir.hrl").

%% Expansion

expand(Meta, Args, E) ->
  case ?m(E, context) of
    match ->
      {EArgs, EA} = expand_bitstr(fun elixir_exp:expand/2, Args, [], E),
      {{'<<>>', Meta, EArgs}, EA};
    _ ->
      {EArgs, {EC, EV}} = expand_bitstr(fun elixir_exp:expand_arg/2, Args, [], {E, E}),
      {{'<<>>', Meta, EArgs}, elixir_env:mergea(EV, EC)}
  end.

expand_bitstr(_Fun, [], Acc, E) ->
  {lists:reverse(Acc), E};
expand_bitstr(Fun, [{'::', Meta, [Left, Right]}|T], Acc, E) ->
  {ELeft, EL} = Fun(Left, E),

  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  case E of
    {ER, _} -> ok;               %% expand_arg,  no assigns
    _ -> ER = E#{context := nil} %% expand_each, revert assigns
  end,

  ERight = expand_bit_info(Meta, Right, ER),
  expand_bitstr(Fun, T, [{'::', Meta, [ELeft, ERight]}|Acc], EL);

expand_bitstr(Fun, [H|T], Acc, E) ->
  {Expr, ES} = Fun(H, E),
  expand_bitstr(Fun, T, [Expr|Acc], ES).

%% Expand bit info

expand_bit_info(Meta, Info, E) ->
  expand_bit_info(Meta, unpack_bit_info(Info, []), default, [], E).

expand_bit_info(Meta, [{size, _, [_]=Args}|T], Size, Types, E) ->
  case Size of
    default ->
      {[EArg], EE} = elixir_exp:expand_args(Args, E),

      case EArg of
        {Var, _, Context} when is_atom(Var) and is_atom(Context) ->
          ok;
        _ when is_integer(EArg) ->
          ok;
        _ ->
          elixir_errors:compile_error(Meta, ?m(E, file),
            "size in bitstring expects an integer or a variable as argument, got: ~ts",
            ['Elixir.Macro':to_string(EArg)])
      end,

      expand_bit_info(Meta, T, {size, [], [EArg]}, Types, EE);
    _ ->
      elixir_errors:compile_error(Meta, ?m(E, file),
        "duplicated size definition in bitstring")
  end;

expand_bit_info(Meta, [{Expr, ExprMeta, Args}|T], Size, Types, E) when is_atom(Expr) ->
  case expand_bit_type(Expr, Args) of
    type ->
      {EArgs, EE} = elixir_exp:expand_args(Args, E),
      validate_bit_type_args(Meta, Expr, EArgs, EE),
      expand_bit_info(Meta, T, Size, [{Expr, [], EArgs}|Types], EE);
    none ->
      handle_unknown_bit_info(Meta, {Expr, ExprMeta, Args}, T, Size, Types, E)
  end;

expand_bit_info(Meta, [Expr|_], _Size, _Types, E) ->
  elixir_errors:compile_error(Meta, ?m(E, file),
    "unknown bitstring specifier ~ts", ['Elixir.Kernel':inspect(Expr)]);

expand_bit_info(Meta, [], Size, Types, _) ->
  [H|T] = case Size of
    default -> lists:reverse(Types);
    _       -> lists:reverse(Types, [Size])
  end,
  lists:foldl(fun(I, Acc) -> {'-', Meta, [Acc, I]} end, H, T).

expand_bit_type(binary, [])    -> type;
expand_bit_type(integer, [])   -> type;
expand_bit_type(float, [])     -> type;
expand_bit_type(bitstring, []) -> type;
expand_bit_type(bytes, [])     -> type;
expand_bit_type(bits, [])      -> type;
expand_bit_type(utf8, [])      -> type;
expand_bit_type(utf16, [])     -> type;
expand_bit_type(utf32, [])     -> type;
expand_bit_type(signed, [])    -> type;
expand_bit_type(unsigned, [])  -> type;
expand_bit_type(big, [])       -> type;
expand_bit_type(little, [])    -> type;
expand_bit_type(native, [])    -> type;
expand_bit_type(unit, [_])     -> type;
expand_bit_type(_, _)          -> none.

validate_bit_type_args(Meta, unit, [Unit], E) when not is_integer(Unit) ->
  elixir_errors:compile_error(Meta, ?m(E, file),
    "unit in bitstring expects an integer as argument, got: ~ts",
    ['Elixir.Macro':to_string(Unit)]);
validate_bit_type_args(_Meta, _Expr, _Args, _E) ->
  ok.

handle_unknown_bit_info(Meta, Expr, T, Size, Types, E) ->
  case 'Elixir.Macro':expand(Expr, elixir_env:linify({?line(Meta), E})) of
    Expr ->
      elixir_errors:compile_error(Meta, ?m(E, file),
        "unknown bitstring specifier ~ts", ['Elixir.Macro':to_string(Expr)]);
    Info ->
      expand_bit_info(Meta, unpack_bit_info(Info, []) ++ T, Size, Types, E)
  end.

unpack_bit_info({'-', _, [H, T]}, Acc) ->
  unpack_bit_info(H, unpack_bit_info(T, Acc));
unpack_bit_info({'*', _, [{'_', _, Atom}, Unit]}, Acc) when is_atom(Atom) and is_integer(Unit) ->
  [{unit, [], [Unit]}|Acc];
unpack_bit_info({'*', _, [Size, Unit]}, Acc) when is_integer(Size) and is_integer(Unit) ->
  [{size, [], [Size]}, {unit, [], [Unit]}|Acc];
unpack_bit_info(Size, Acc) when is_integer(Size) ->
  [{size, [], [Size]}|Acc];
unpack_bit_info({Expr, Meta, Args}, Acc) when is_atom(Expr) ->
  ListArgs = if is_atom(Args) -> []; is_list(Args) -> Args end,
  [{Expr, Meta, ListArgs}|Acc];
unpack_bit_info(Other, Acc) ->
  [Other|Acc].

%% Translation

has_size({bin, _, Elements}) ->
  not lists:any(fun({bin_element, _Line, _Expr, Size, Types}) ->
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
  {Final, FinalS} = build_bitstr_each(Fun, Exprs, Meta, S, []),
  {{bin, ?ann(Meta), lists:reverse(Final)}, FinalS}.

build_bitstr_each(_Fun, [], _Meta, S, Acc) ->
  {Acc, S};

build_bitstr_each(Fun, [{'::', _, [H, V]}|T], Meta, S, Acc) ->
  {Size, Types} = extract_bit_info(V, S#elixir_scope{context=nil}),
  build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types);

build_bitstr_each(Fun, [H|T], Meta, S, Acc) ->
  build_bitstr_each(Fun, T, Meta, S, Acc, H, default, default).

build_bitstr_each(Fun, T, Meta, S, Acc, H, default, Types) when is_binary(H) ->
  Element =
    case types_allow_splice(Types, []) of
      true ->
        %% See explanation in elixir_utils:elixir_to_erl/1 to know
        %% why we can simply convert the binary to a list.
        {bin_element, ?ann(Meta), {string, 0, binary_to_list(H)}, default, default};
      false ->
        case types_require_conversion(Types) of
          true ->
            {bin_element, ?ann(Meta), {string, 0, elixir_utils:characters_to_list(H)}, default, Types};
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
  {Expr, NS} = Fun(H, S),

  case Expr of
    {bin, _, Elements} ->
      case (Size == default) andalso types_allow_splice(Types, Elements) of
        true  -> build_bitstr_each(Fun, T, Meta, NS, lists:reverse(Elements, Acc));
        false -> build_bitstr_each(Fun, T, Meta, NS, [{bin_element, ?ann(Meta), Expr, Size, Types}|Acc])
      end;
    _ ->
      build_bitstr_each(Fun, T, Meta, NS, [{bin_element, ?ann(Meta), Expr, Size, Types}|Acc])
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
elem_size({bin_element, _, _, {integer, _, Size}, Types}) -> {Size, unit_size(Types, 1)};
elem_size({bin_element, _, _, _Size, Types})            -> {unknown, unit_size(Types, 1)}.

unit_size([binary|T], _)       -> unit_size(T, 8);
unit_size([bytes|T], _)        -> unit_size(T, 8);
unit_size([{unit, Size}|_], _) -> Size;
unit_size([_|T], Guess)        -> unit_size(T, Guess);
unit_size([], Guess)           -> Guess.

%% Extra bitstring specifiers

extract_bit_info({'-', _, [L, {size, _, [Size]}]}, S) ->
  {extract_bit_size(Size, S), extract_bit_type(L, [])};
extract_bit_info({size, _, [Size]}, S) ->
  {extract_bit_size(Size, S), []};
extract_bit_info(L, _S) ->
  {default, extract_bit_type(L, [])}.

extract_bit_size(Size, S) ->
  {TSize, _} = elixir_translator:translate(Size, S),
  TSize.

extract_bit_type({'-', _, [L, R]}, Acc) ->
  extract_bit_type(L, extract_bit_type(R, Acc));
extract_bit_type({unit, _, [Arg]}, Acc) ->
  [{unit, Arg}|Acc];
extract_bit_type({Other, _, []}, Acc) ->
  [Other|Acc].
