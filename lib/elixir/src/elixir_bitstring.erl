-module(elixir_bitstring).
-export([expand/4, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

expand(Meta, Args, E, RequireSize) ->
  case ?key(E, context) of
    match ->
      {EArgs, EA} = expand(Meta, fun elixir_expand:expand/2, Args, [], E, RequireSize),
      {{'<<>>', Meta, EArgs}, EA};
    _ ->
      {EArgs, {EC, EV}} = expand(Meta, fun elixir_expand:expand_arg/2, Args, [], {E, E}, RequireSize),
      {{'<<>>', Meta, EArgs}, elixir_env:mergea(EV, EC)}
  end.

expand(_BitstrMeta, _Fun, [], Acc, E, _RequireSize) ->
  {lists:reverse(Acc), E};
expand(BitstrMeta, Fun, [{'::', Meta, [Left, Right]} | T], Acc, E, RequireSize) ->
  {ELeft, EL} = expand_expr(Meta, Left, Fun, E),

  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  {ER, MatchSize} =
    case E of
      {EExtracted, _} -> {EExtracted, false};          %% expand_arg,  no assigns
      _               -> {E#{context := nil}, T /= []} %% expand, revert assigns
    end,

  ERight = expand_specs(expr_type(ELeft), Meta, Right, ER, RequireSize or MatchSize),
  expand(BitstrMeta, Fun, T, [{'::', Meta, [ELeft, ERight]} | Acc], EL, RequireSize);
expand(BitstrMeta, Fun, [{_, Meta, _} = H | T], Acc, E, RequireSize) ->
  {Expr, ES} = expand_expr(Meta, H, Fun, E),
  expand(BitstrMeta, Fun, T, [wrap_expr(Expr) | Acc], ES, RequireSize);
expand(Meta, Fun, [H | T], Acc, E, RequireSize) ->
  {Expr, ES} = expand_expr(Meta, H, Fun, E),
  expand(Meta, Fun, T, [wrap_expr(Expr) | Acc], ES, RequireSize).

wrap_expr(Expr) ->
  case expr_type(Expr) of
    bitstring ->
      {'::', [], [Expr, {bitstring, [], []}]};
    binary ->
      {'::', [], [Expr, {binary, [], []}]};
    float ->
      {'::', [], [Expr, {float, [], []}]};
    _ ->
      {'::', [], [Expr, {integer, [], []}]}
  end.

expr_type(Integer) when is_integer(Integer) -> integer;
expr_type(Float) when is_float(Float) -> float;
expr_type(Binary) when is_binary(Binary) -> binary;
expr_type({'<<>>', _, _}) -> bitstring;
expr_type(_) -> default.

%% Expands the expression of a bitstring, that is, the LHS of :: or
%% an argument of the bitstring (such as "foo" in "<<foo>>").

expand_expr(Meta, {{'.', M1, ['Elixir.Kernel', to_string]}, M2, [Arg]}, Fun, E) ->
  case expand_expr(Meta, Arg, Fun, E) of
    {Bin, _} = Pair when is_binary(Bin) -> Pair;
    {EArg, EE} -> {{{'.', M1, ['Elixir.Kernel', to_string]}, M2, [EArg]}, EE}
  end;
expand_expr(Meta, Component, Fun, E) ->
  case Fun(Component, E) of
    {EComponent, _} when is_list(EComponent); is_atom(EComponent) ->
      ErrorE = env_for_error(E),
      form_error(Meta, ?key(ErrorE, file), ?MODULE, {invalid_literal, EComponent});
    {_, _} = Expanded ->
      Expanded
  end.

env_for_error({E, _}) -> E;
env_for_error(E) -> E.

%% Expands and normalizes types of a bitstring.

expand_specs(ExprType, Meta, Info, E, RequireSize) ->
  Default =
    #{size => default,
      unit => default,
      sign => default,
      type => default,
      endianess => default},
  #{size := Size, unit := Unit, type := Type, endianess := Endianess, sign := Sign} =
    expand_each_spec(Meta, unpack_specs(Info, []), Default, E),
  MergedType = type(Meta, ExprType, Type, E),
  validate_size_required(Meta, RequireSize, ExprType, MergedType, Size, E),
  SizeAndUnit = size_and_unit(Meta, ExprType, Size, Unit, E),
  [H | T] = build_spec(Meta, Size, Unit, MergedType, Endianess, Sign, SizeAndUnit, E),
  lists:foldl(fun(I, Acc) -> {'-', Meta, [Acc, I]} end, H, T).

type(_, default, default, _) ->
  integer;
type(_, ExprType, default, _) ->
  ExprType;
type(_, binary, Type, _) when Type == binary; Type == bitstring; Type == utf8; Type == utf16; Type == utf32 ->
  Type;
type(_, bitstring, Type, _) when Type == binary; Type == bitstring ->
  Type;
type(_, integer, Type, _) when Type == integer; Type == float; Type == utf8; Type == utf16; Type == utf32 ->
  Type;
type(_, float, Type, _) when Type == float ->
  Type;
type(_, default, Type, _) ->
  Type;
type(Meta, Other, Value, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bittype_mismatch, Value, Other, type}).

expand_each_spec(Meta, [{Expr, _, Args} = H | T], Map, E) when is_atom(Expr) ->
  case validate_spec(Expr, Args) of
    {Key, Arg} ->
      {Value, EE} = expand_spec_arg(Arg, E),
      validate_spec_arg(Meta, Key, Value, EE),

      case maps:get(Key, Map) of
        default -> ok;
        Value -> ok;
        Other -> form_error(Meta, ?key(E, file), ?MODULE, {bittype_mismatch, Value, Other, Key})
      end,

      expand_each_spec(Meta, T, maps:put(Key, Value, Map), EE);
    none ->
      case 'Elixir.Macro':expand(H, elixir_env:linify({?line(Meta), E})) of
        H ->
          form_error(Meta, ?key(E, file), ?MODULE, {undefined_bittype, H});
        NewTypes ->
          expand_each_spec(Meta, unpack_specs(NewTypes, []) ++ T, Map, E)
      end
  end;
expand_each_spec(Meta, [Expr | _], _Map, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {undefined_bittype, Expr});
expand_each_spec(_Meta, [], Map, _E) ->
  Map.

unpack_specs({'-', _, [H, T]}, Acc) ->
  unpack_specs(H, unpack_specs(T, Acc));
unpack_specs({'*', _, [{'_', _, Atom}, Unit]}, Acc) when is_atom(Atom) and is_integer(Unit) ->
  [{unit, [], [Unit]} | Acc];
unpack_specs({'*', _, [Size, Unit]}, Acc) when is_integer(Size) and is_integer(Unit) ->
  [{size, [], [Size]}, {unit, [], [Unit]} | Acc];
unpack_specs(Size, Acc) when is_integer(Size) ->
  [{size, [], [Size]} | Acc];
unpack_specs({Expr, Meta, Args}, Acc) when is_atom(Expr) ->
  ListArgs = if is_atom(Args) -> []; is_list(Args) -> Args end,
  [{Expr, Meta, ListArgs} | Acc];
unpack_specs(Other, Acc) ->
  [Other | Acc].

validate_spec(big, [])       -> {endianess, big};
validate_spec(little, [])    -> {endianess, little};
validate_spec(native, [])    -> {endianess, native};
validate_spec(size, [Size])  -> {size, Size};
validate_spec(unit, [Unit])  -> {unit, Unit};
validate_spec(integer, [])   -> {type, integer};
validate_spec(float, [])     -> {type, float};
validate_spec(binary, [])    -> {type, binary};
validate_spec(bytes, [])     -> {type, binary};
validate_spec(bitstring, []) -> {type, bitstring};
validate_spec(bits, [])      -> {type, bitstring};
validate_spec(utf8, [])      -> {type, utf8};
validate_spec(utf16, [])     -> {type, utf16};
validate_spec(utf32, [])     -> {type, utf32};
validate_spec(signed, [])    -> {sign, signed};
validate_spec(unsigned, [])  -> {sign, unsigned};
validate_spec(_, _)          -> none.

expand_spec_arg(Expr, E) when is_atom(Expr); is_integer(Expr) -> {Expr, E};
expand_spec_arg(Expr, E) -> elixir_expand:expand(Expr, E).

validate_spec_arg(Meta, size, Value, E) ->
  case Value of
    {Var, _, Context} when is_atom(Var) and is_atom(Context) -> ok;
    _ when is_integer(Value) -> ok;
    _ -> form_error(Meta, ?key(E, file), ?MODULE, {bad_size_argument, Value})
  end;
validate_spec_arg(Meta, unit, Value, E) when not is_integer(Value) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bad_unit_argument, Value});
validate_spec_arg(_Meta, _Key, _Value, _E) ->
  ok.

validate_size_required(Meta, true, default, Type, default, E) when Type == binary; Type == bitstring ->
  form_error(Meta, ?key(E, file), ?MODULE, unsized_binary);
validate_size_required(_, _, _, _, _, _) ->
  ok.

size_and_unit(Meta, bitstring, Size, Unit, E) when Size /= default; Unit /= default ->
  form_error(Meta, ?key(E, file), ?MODULE, bittype_literal_bitstring);
size_and_unit(Meta, binary, Size, Unit, E) when Size /= default; Unit /= default ->
  form_error(Meta, ?key(E, file), ?MODULE, bittype_literal_string);
size_and_unit(_Meta, _ExprType, Size, Unit, _E) ->
  add_arg(unit, Unit, add_arg(size, Size, [])).

add_arg(_Key, default, Spec) -> Spec;
add_arg(Key, Arg, Spec) -> [{Key, [], [Arg]} | Spec].

build_spec(Meta, Size, Unit, Type, Endianess, Sign, Spec, E) when Type == utf8; Type == utf16; Type == utf32 ->
  if
    Size /= default; Unit /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_utf);
    Sign /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_signed);
    true ->
      add_spec(Type, add_spec(Endianess, Spec))
  end;

build_spec(Meta, _Size, Unit, Type, _Endianess, Sign, Spec, E) when Type == binary; Type == bitstring ->
  if
    Type == bitstring, Unit /= default, Unit /= 1 ->
      form_error(Meta, ?key(E, file), ?MODULE, {bittype_mismatch, Unit, 1, unit});
    Sign /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_signed);
    true ->
      %% Endianess is supported but has no effect, so we just ignore it.
      add_spec(Type, Spec)
  end;

build_spec(Meta, Size, Unit, Type, Endianess, Sign, Spec, E) when Type == integer; Type == float ->
  NumberSize = number_size(Size, Unit),
  if
    Type == float, is_integer(NumberSize), NumberSize /= 32, NumberSize /= 64 ->
      form_error(Meta, ?key(E, file), ?MODULE, {bittype_float_size, NumberSize});
    Size == default, Unit /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_unit);
    true ->
      add_spec(Type, add_spec(Endianess, add_spec(Sign, Spec)))
  end.

number_size(default, _) -> default;
number_size(Size, default) -> Size;
number_size(Size, Unit) -> Size * Unit.

add_spec(default, Spec) -> Spec;
add_spec(Key, Spec) -> [{Key, [], []} | Spec].

format_error(unsized_binary) ->
  "a binary field without size is only allowed at the end of a binary pattern "
  "and never allowed in binary generators";
format_error(bittype_literal_bitstring) ->
  "literal <<>> in bitstring supports only type specifiers, which must be one of: "
    "binary or bitstring";
format_error(bittype_literal_string) ->
  "literal string in bitstring supports only endianess and type specifiers, which must be one of: "
    "little, big, native, utf8, utf16, utf32, bits, bytes, binary or bitstring";
format_error(bittype_utf) ->
  "size and unit are not supported on utf types";
format_error(bittype_signed) ->
  "signed and unsigned specifiers are supported only on integer and float types";
format_error(bittype_unit) ->
  "integer and float types require a size specifier if the unit specifier is given";
format_error({bittype_float_size, Other}) ->
  io_lib:format("float requires size*unit to be 32 or 64 (default), got: ~p", [Other]);
format_error({invalid_literal, Literal}) ->
  io_lib:format("invalid literal ~ts in <<>>", ['Elixir.Macro':to_string(Literal)]);
format_error({undefined_bittype, Expr}) ->
  io_lib:format("unknown bitstring specifier: ~ts", ['Elixir.Macro':to_string(Expr)]);
format_error({bittype_mismatch, Val1, Val2, Where}) ->
  io_lib:format("conflicting ~ts specification for bit field: \"~p\" and \"~p\"", [Where, Val1, Val2]);
format_error({bad_unit_argument, Unit}) ->
  io_lib:format("unit in bitstring expects an integer as argument, got: ~ts",
                ['Elixir.Macro':to_string(Unit)]);
format_error({bad_size_argument, Size}) ->
  io_lib:format("size in bitstring expects an integer or a variable as argument, got: ~ts",
                ['Elixir.Macro':to_string(Size)]).
