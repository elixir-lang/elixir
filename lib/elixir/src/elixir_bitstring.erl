-module(elixir_bitstring).
-export([expand/4, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

expand(Meta, Args, E, RequireSize) ->
  case ?key(E, context) of
    match ->
      {EArgs, Alignment, EE} =
        expand(Meta, fun elixir_expand:expand/2, Args, [], E, E, 0, RequireSize),

      case find_match(EArgs) of
        false ->
          {{'<<>>', [{alignment, Alignment} | Meta], EArgs}, EE};
        Match ->
          form_error(Meta, ?key(EE, file), ?MODULE, {nested_match, Match})
      end;
    _ ->
      {EArgs, Alignment, {_EC, EV}} =
        expand(Meta, fun elixir_expand:expand_arg/2, Args, [], {E, E}, E, 0, RequireSize),
      {{'<<>>', [{alignment, Alignment} | Meta], EArgs}, EV}
  end.

expand(_BitstrMeta, _Fun, [], Acc, E, _OriginalE, Alignment, _RequireSize) ->
  {lists:reverse(Acc), Alignment, E};
expand(BitstrMeta, Fun, [{'::', Meta, [Left, Right]} | T], Acc, E, OriginalE, Alignment, RequireSize) ->
  {ELeft, EL} = expand_expr(Meta, Left, Fun, E),

  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  {EM, MatchSize} =
    case EL of
      %% expand_arg, no assigns
      {EC1, _} -> {EC1, false};

      %% expand, revert assigns
      _ -> {EL#{context := nil, prematch_vars := raise}, T /= []}
    end,

  EType = expr_type(ELeft),
  {ERight, EAlignment, ES} = expand_specs(EType, Meta, Right, EM, OriginalE, RequireSize or MatchSize),

  EE =
    case EL of
      %% no assigns, vars are kept separately
      {EC2, EV2} -> {EC2, elixir_env:mergev(EV2, ES)};

      %% copy only vars on top
      _ -> elixir_env:mergea(ES, EL)
    end,

  EAcc =
    %% If the Etype is a bitstring (which implies a literal <<>>)
    %% and we have no further modifiers other than binary or bitstring,
    %% we can attempt to merge the inner <<>> into the outer one.
    case ERight of
      {binary, _, []} when EType == bitstring  ->
        case byte_parts(ELeft) of
          {ok, Parts} -> lists:reverse(Parts, Acc);
          error -> prepend_unless_bitstring_in_match(EType, Meta, ELeft, ERight, Acc, E)
        end;
      {bitstring, _, []} when EType == bitstring ->
        lists:reverse(element(3, ELeft), Acc);
      _ ->
        prepend_unless_bitstring_in_match(EType, Meta, ELeft, ERight, Acc, E)
    end,

  expand(BitstrMeta, Fun, T, EAcc, EE, OriginalE, alignment(Alignment, EAlignment), RequireSize);
expand(BitstrMeta, Fun, [{_, Meta, _} = H | T], Acc, E, OriginalE, Alignment, RequireSize) ->
  {Expr, ES} = expand_expr(Meta, H, Fun, E),
  {EAcc, EAlignment} = wrap_expr(Expr, Acc),
  expand(BitstrMeta, Fun, T, EAcc, ES, OriginalE, alignment(Alignment, EAlignment), RequireSize);
expand(Meta, Fun, [H | T], Acc, E, OriginalE, Alignment, RequireSize) ->
  {Expr, ES} = expand_expr(Meta, H, Fun, E),
  {EAcc, EAlignment} = wrap_expr(Expr, Acc),
  expand(Meta, Fun, T, EAcc, ES, OriginalE, alignment(Alignment, EAlignment), RequireSize).

prepend_unless_bitstring_in_match(Type, Meta, Left, Right, Acc, E) ->
  Expr = {'::', Meta, [Left, Right]},

  case E of
    #{context := match} when Type == bitstring ->
      form_error(Meta, ?key(E, file), ?MODULE, {unaligned_bitstring_in_match, Expr});
    _ ->
      [Expr | Acc]
  end.

byte_parts({'<<>>', Meta, Parts}) ->
  case lists:keyfind(alignment, 1, Meta) of
    {alignment, 0} -> {ok, Parts};
    _ -> error
  end.

wrap_expr({'<<>>', Meta, Entries}, Acc) ->
  %% A literal bitstring can always be merged into the outer one
  %% when the bitstring specifications are not present.
  {_, Alignment} = lists:keyfind(alignment, 1, Meta),
  {lists:reverse(Entries, Acc), Alignment};
wrap_expr(Expr, Acc) ->
  Node =
    case expr_type(Expr) of
      binary ->
        {'::', [], [Expr, {binary, [], []}]};
      float ->
        {'::', [], [Expr, {float, [], []}]};
      integer ->
        {'::', [], [Expr, {integer, [], []}]};
      default ->
        {'::', [], [Expr, {integer, [], []}]}
    end,
  {[Node | Acc], 0}.

expr_type(Integer) when is_integer(Integer) -> integer;
expr_type(Float) when is_float(Float) -> float;
expr_type(Binary) when is_binary(Binary) -> binary;
expr_type({'<<>>', _, _}) -> bitstring;
expr_type(_) -> default.

%% Handling of alignment

alignment(Left, Right) when is_integer(Left), is_integer(Right) -> (Left + Right) rem 8;
alignment(_, _) -> unknown.

compute_alignment(_, Size, Unit) when is_integer(Size), is_integer(Unit) -> (Size * Unit) rem 8;
compute_alignment(default, Size, Unit) -> compute_alignment(integer, Size, Unit);
compute_alignment(integer, default, Unit) -> compute_alignment(integer, 8, Unit);
compute_alignment(integer, Size, default) -> compute_alignment(integer, Size, 1);
compute_alignment(bitstring, Size, default) -> compute_alignment(bitstring, Size, 1);
compute_alignment(binary, Size, default) -> compute_alignment(binary, Size, 8);
compute_alignment(binary, _, _) -> 0;
compute_alignment(float, _, _) -> 0;
compute_alignment(utf32, _, _) -> 0;
compute_alignment(utf16, _, _) -> 0;
compute_alignment(utf8, _, _) -> 0;
compute_alignment(_, _, _) -> unknown.

%% Expands the expression of a bitstring, that is, the LHS of :: or
%% an argument of the bitstring (such as "foo" in "<<foo>>").

expand_expr(_, {{'.', M1, [Mod, to_string]}, M2, [Arg]}, Fun, E)
    when Mod == 'Elixir.Kernel'; Mod == 'Elixir.String.Chars' ->
  case Fun(Arg, E) of
    {EBin, EE} when is_binary(EBin) -> {EBin, EE};
    {EArg, EE} -> {{{'.', M1, ['Elixir.String.Chars', to_string]}, M2, [EArg]}, EE}
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

expand_specs(ExprType, Meta, Info, E, OriginalE, RequireSize) ->
  Default =
    #{size => default,
      unit => default,
      sign => default,
      type => default,
      endianness => default},
  {#{size := Size, unit := Unit, type := Type, endianness := Endianness, sign := Sign}, ES} =
    expand_each_spec(Meta, unpack_specs(Info, []), Default, E, OriginalE),
  MergedType = type(Meta, ExprType, Type, E),
  validate_size_required(Meta, RequireSize, ExprType, MergedType, Size, ES),
  SizeAndUnit = size_and_unit(Meta, ExprType, Size, Unit, ES),
  Alignment = compute_alignment(MergedType, Size, Unit),
  [H | T] = build_spec(Meta, Size, Unit, MergedType, Endianness, Sign, SizeAndUnit, ES),
  {lists:foldl(fun(I, Acc) -> {'-', Meta, [Acc, I]} end, H, T), Alignment, ES}.

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

expand_each_spec(Meta, [{Expr, _, Args} = H | T], Map, E, OriginalE) when is_atom(Expr) ->
  case validate_spec(Expr, Args) of
    {Key, Arg} ->
      {Value, EE} = expand_spec_arg(Arg, E),
      validate_spec_arg(Meta, Key, Value, EE, OriginalE),

      case maps:get(Key, Map) of
        default -> ok;
        Value -> ok;
        Other -> form_error(Meta, ?key(E, file), ?MODULE, {bittype_mismatch, Value, Other, Key})
      end,

      expand_each_spec(Meta, T, maps:put(Key, Value, Map), EE, OriginalE);
    none ->
      case 'Elixir.Macro':expand(H, elixir_env:linify({?line(Meta), E})) of
        H ->
          form_error(Meta, ?key(E, file), ?MODULE, {undefined_bittype, H});
        NewTypes ->
          expand_each_spec(Meta, unpack_specs(NewTypes, []) ++ T, Map, E, OriginalE)
      end
  end;
expand_each_spec(Meta, [Expr | _], _Map, E, _OriginalE) ->
  form_error(Meta, ?key(E, file), ?MODULE, {undefined_bittype, Expr});
expand_each_spec(_Meta, [], Map, E, _OriginalE) ->
  {Map, E}.

unpack_specs({'-', _, [H, T]}, Acc) ->
  unpack_specs(H, unpack_specs(T, Acc));
unpack_specs({'*', _, [{'_', _, Atom}, Unit]}, Acc) when is_atom(Atom) ->
  [{unit, [], [Unit]} | Acc];
unpack_specs({'*', _, [Size, Unit]}, Acc) ->
  [{size, [], [Size]}, {unit, [], [Unit]} | Acc];
unpack_specs(Size, Acc) when is_integer(Size) ->
  [{size, [], [Size]} | Acc];
unpack_specs({Expr, Meta, Args}, Acc) when is_atom(Expr) ->
  ListArgs = if is_atom(Args) -> []; is_list(Args) -> Args end,
  [{Expr, Meta, ListArgs} | Acc];
unpack_specs(Other, Acc) ->
  [Other | Acc].

validate_spec(big, [])       -> {endianness, big};
validate_spec(little, [])    -> {endianness, little};
validate_spec(native, [])    -> {endianness, native};
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

validate_spec_arg(Meta, size, Value, E, OriginalE) ->
  case Value of
    {Var, VarMeta, Context} when is_atom(Var) and is_atom(Context) ->
      Tuple = {Var, elixir_utils:var_context(VarMeta, Context)},

      case is_valid_spec_arg_var(Tuple, E, OriginalE) of
        true -> ok;
        false -> form_error(Meta, ?key(E, file), ?MODULE, {undefined_var_in_spec, Value})
      end;

    _ when is_integer(Value) ->
      ok;

    _ ->
      form_error(Meta, ?key(E, file), ?MODULE, {bad_size_argument, Value})
  end;
validate_spec_arg(Meta, unit, Value, E, _OriginalE) when not is_integer(Value) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bad_unit_argument, Value});
validate_spec_arg(_Meta, _Key, _Value, _E, _OriginalE) ->
  ok.

is_valid_spec_arg_var(Var, E, #{context := match} = OriginalE) ->
  case ?key(OriginalE, prematch_vars) of
    #{Var := _} ->
      true;
    _ ->
      maps:is_key(Var, ?key(E, current_vars)) andalso
        not maps:is_key(Var, ?key(OriginalE, current_vars))
  end;
is_valid_spec_arg_var(_Var, _E, _OriginalE) -> true.

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

build_spec(Meta, Size, Unit, Type, Endianness, Sign, Spec, E) when Type == utf8; Type == utf16; Type == utf32 ->
  if
    Size /= default; Unit /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_utf);
    Sign /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_signed);
    true ->
      add_spec(Type, add_spec(Endianness, Spec))
  end;

build_spec(Meta, _Size, Unit, Type, _Endianness, Sign, Spec, E) when Type == binary; Type == bitstring ->
  if
    Type == bitstring, Unit /= default, Unit /= 1 ->
      form_error(Meta, ?key(E, file), ?MODULE, {bittype_mismatch, Unit, 1, unit});
    Sign /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_signed);
    true ->
      %% Endianness is supported but has no effect, so we just ignore it.
      add_spec(Type, Spec)
  end;

build_spec(Meta, Size, Unit, Type, Endianness, Sign, Spec, E) when Type == integer; Type == float ->
  NumberSize = number_size(Size, Unit),
  if
    Type == float, is_integer(NumberSize), NumberSize /= 32, NumberSize /= 64 ->
      form_error(Meta, ?key(E, file), ?MODULE, {bittype_float_size, NumberSize});
    Size == default, Unit /= default ->
      form_error(Meta, ?key(E, file), ?MODULE, bittype_unit);
    true ->
      add_spec(Type, add_spec(Endianness, add_spec(Sign, Spec)))
  end.

number_size(Size, default) when is_integer(Size) -> Size;
number_size(Size, Unit) when is_integer(Size) -> Size * Unit;
number_size(Size, _) -> Size.

add_spec(default, Spec) -> Spec;
add_spec(Key, Spec) -> [{Key, [], []} | Spec].

find_match([{'=', _, [_Left, _Right]} = Expr | _Rest]) ->
  Expr;
find_match([{_, _, Args} | Rest]) when is_list(Args) ->
  case find_match(Args) of
    false -> find_match(Rest);
    Match -> Match
  end;
find_match([_Arg | Rest]) ->
  find_match(Rest);
find_match([]) ->
  false.

format_error({unaligned_bitstring_in_match, Expr}) ->
  Message =
    "cannot verify size of binary expression in match. "
    "If you are concatenating two binaries or nesting a binary inside a bitstring, "
    "you need to make sure the size of all fields in the binary expression are known. "
    "The following examples are invalid:\n\n"
    "    \"foo\" <> <<field, rest::bits>>\n"
    "    <<\"foo\", <<field, rest::bitstring>>::binary>>\n\n"
    "They are invalid because there is a bits/bitstring component of unknown size given "
    "as argument. Those examples could be fixed as:\n\n"
    "    \"foo\" <> <<field, rest::binary>>\n"
    "    <<\"foo\", <<field, rest::bitstring>>::bitstring>>\n\n"
    "Got: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error(unsized_binary) ->
  "a binary field without size is only allowed at the end of a binary pattern "
    "and never allowed in binary generators";
format_error(bittype_literal_bitstring) ->
  "literal <<>> in bitstring supports only type specifiers, which must be one of: "
    "binary or bitstring";
format_error(bittype_literal_string) ->
  "literal string in bitstring supports only endianness and type specifiers, which must be one of: "
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
                ['Elixir.Macro':to_string(Size)]);
format_error({nested_match, Expr}) ->
  Message =
    "cannot pattern match inside a bitstring "
      "that is already in match, got: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error({undefined_var_in_spec, Var}) ->
  Message =
    "undefined variable \"~ts\" in bitstring segment. If the size of the binary is a "
      "variable, the variable must be defined prior to its use in the binary/bitstring match "
      "itself, or outside the pattern match",
  io_lib:format(Message, ['Elixir.Macro':to_string(Var)]).
