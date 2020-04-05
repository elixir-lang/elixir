-module(elixir_bitstring).
-export([expand/4, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

expand_match(Expr, {E, OriginalE}) ->
  {EExpr, EE} = elixir_expand:expand(Expr, E),
  {EExpr, {EE, OriginalE}}.

expand(Meta, Args, E, RequireSize) ->
  case ?key(E, context) of
    match ->
      {EArgs, Alignment, {EA, _}} =
        expand(Meta, fun expand_match/2, Args, [], {E, E}, 0, RequireSize),

      case find_match(EArgs) of
        false ->
          {{'<<>>', [{alignment, Alignment} | Meta], EArgs}, EA};
        Match ->
          form_error(Meta, EA, ?MODULE, {nested_match, Match})
      end;
    _ ->
      PairE = {elixir_env:prepare_write(E), E},

      {EArgs, Alignment, {EA, _}} =
        expand(Meta, fun elixir_expand:expand_arg/2, Args, [], PairE, 0, RequireSize),

      {{'<<>>', [{alignment, Alignment} | Meta], EArgs}, elixir_env:close_write(EA, E)}
  end.

expand(_BitstrMeta, _Fun, [], Acc, E, Alignment, _RequireSize) ->
  {lists:reverse(Acc), Alignment, E};
expand(BitstrMeta, Fun, [{'::', Meta, [Left, Right]} | T], Acc, E, Alignment, RequireSize) ->
  {ELeft, {EL, OriginalE}} = expand_expr(Meta, Left, Fun, E),

  MatchOrRequireSize = RequireSize or is_match_size(T, EL),
  EType = expr_type(ELeft),
  {ERight, EAlignment, ES} = expand_specs(EType, Meta, Right, EL, OriginalE, MatchOrRequireSize),

  EAcc = concat_or_prepend_bitstring(Meta, ELeft, ERight, Acc, ES, MatchOrRequireSize),
  expand(BitstrMeta, Fun, T, EAcc, {ES, OriginalE}, alignment(Alignment, EAlignment), RequireSize);
expand(BitstrMeta, Fun, [H | T], Acc, E, Alignment, RequireSize) ->
  Meta = extract_meta(H, BitstrMeta),
  {ELeft, {ES, OriginalE}} = expand_expr(Meta, H, Fun, E),

  MatchOrRequireSize = RequireSize or is_match_size(T, ES),
  EType = expr_type(ELeft),
  ERight = infer_spec(EType, Meta),

  InferredMeta = [{inferred_bitstring_spec, true} | Meta],
  EAcc = concat_or_prepend_bitstring(InferredMeta, ELeft, ERight, Acc, ES, MatchOrRequireSize),
  expand(Meta, Fun, T, EAcc, {ES, OriginalE}, Alignment, RequireSize).

extract_meta({_, Meta, _}, _) -> Meta;
extract_meta(_, Meta) -> Meta.

%% Variables defined outside the binary can be accounted
%% on subparts, however we can't assign new variables.
is_match_size([_ | _], #{context := match}) -> true;
is_match_size(_, _) -> false.

expr_type(Integer) when is_integer(Integer) -> integer;
expr_type(Float) when is_float(Float) -> float;
expr_type(Binary) when is_binary(Binary) -> binary;
expr_type({'<<>>', _, _}) -> bitstring;
expr_type(_) -> default.

infer_spec(bitstring, Meta) -> {bitstring, Meta, []};
infer_spec(binary, Meta) -> {binary, Meta, []};
infer_spec(float, Meta) -> {float, Meta, []};
infer_spec(integer, Meta) -> {integer, Meta, []};
infer_spec(default, Meta) -> {integer, Meta, []}.

concat_or_prepend_bitstring(_Meta, {'<<>>', _, []}, _ERight, Acc, _E, _RequireSize) ->
  Acc;
concat_or_prepend_bitstring(Meta, {'<<>>', PartsMeta, Parts} = ELeft, ERight, Acc, E, RequireSize) ->
  case E of
    #{context := match} when RequireSize ->
      case lists:last(Parts) of
        {'::', SpecMeta, [Bin, {binary, _, []}]} when not is_binary(Bin) ->
          form_error(SpecMeta, E, ?MODULE, unsized_binary);

        {'::', SpecMeta, [_, {bitstring, _, []}]} ->
          form_error(SpecMeta, E, ?MODULE, unsized_binary);

        _ ->
          ok
      end;
    _ ->
      ok
  end,

  case ERight of
    {binary, _, []} ->
      {alignment, Alignment} = lists:keyfind(alignment, 1, PartsMeta),

      if
        Alignment == 0 ->
          lists:reverse(Parts, Acc);

        is_integer(Alignment) ->
          form_error(Meta, E, ?MODULE, {unaligned_binary, ELeft});

        true ->
          [{'::', Meta, [ELeft, ERight]} | Acc]
      end;
    {bitstring, _, []} ->
      lists:reverse(Parts, Acc)
  end;
concat_or_prepend_bitstring(Meta, ELeft, ERight, Acc, _E, _RequireSize) ->
  [{'::', Meta, [ELeft, ERight]} | Acc].

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
%% If we are inside a match/guard, we inline interpolations explicitly,
%% otherwise they are inlined by elixir_rewrite.erl.

expand_expr(_Meta, {{'.', _, [Mod, to_string]}, _, [Arg]} = AST, Fun, {#{context := Context}, _} = E)
    when Context /= nil, (Mod == 'Elixir.Kernel') orelse (Mod == 'Elixir.String.Chars') ->
  case Fun(Arg, E) of
    {EBin, EE} when is_binary(EBin) -> {EBin, EE};
    _ -> Fun(AST, E) % Let it raise
  end;
expand_expr(Meta, Component, Fun, E) ->
  case Fun(Component, E) of
    {EComponent, {ErrorE, _}} when is_list(EComponent); is_atom(EComponent) ->
      form_error(Meta, ErrorE, ?MODULE, {invalid_literal, EComponent});
    {_, _} = Expanded ->
      Expanded
  end.

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
  form_error(Meta, E, ?MODULE, {bittype_mismatch, Value, Other, type}).

expand_each_spec(Meta, [{Expr, _, Args} = H | T], Map, E, OriginalE) when is_atom(Expr) ->
  case validate_spec(Expr, Args) of
    {Key, Arg} ->
      {Value, EE} = expand_spec_arg(Arg, E, OriginalE),
      validate_spec_arg(Meta, Key, Value, EE, OriginalE),

      case maps:get(Key, Map) of
        default -> ok;
        Value -> ok;
        Other -> form_error(Meta, E, ?MODULE, {bittype_mismatch, Value, Other, Key})
      end,

      expand_each_spec(Meta, T, maps:put(Key, Value, Map), EE, OriginalE);
    none ->
      case 'Elixir.Macro':expand(H, elixir_env:linify({?line(Meta), E})) of
        H ->
          form_error(Meta, E, ?MODULE, {undefined_bittype, H});
        NewTypes ->
          expand_each_spec(Meta, unpack_specs(NewTypes, []) ++ T, Map, E, OriginalE)
      end
  end;
expand_each_spec(Meta, [Expr | _], _Map, E, _OriginalE) ->
  form_error(Meta, E, ?MODULE, {undefined_bittype, Expr});
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

expand_spec_arg(Expr, E, _OriginalE) when is_atom(Expr); is_integer(Expr) ->
  {Expr, E};
expand_spec_arg(Expr, #{context := match} = E, _OriginalE) ->
  {EExpr, EE} = elixir_expand:expand(Expr, E#{context := nil, prematch_vars := raise}),
  {EExpr, EE#{context := match, prematch_vars := ?key(E, prematch_vars)}};
expand_spec_arg(Expr, E, OriginalE) ->
  elixir_expand:expand(Expr, elixir_env:reset_read(E, OriginalE)).

validate_spec_arg(Meta, size, Value, E, OriginalE) ->
  case Value of
    {Var, VarMeta, Context} when is_atom(Var) and is_atom(Context) ->
      Tuple = {Var, elixir_utils:var_context(VarMeta, Context)},

      case is_valid_spec_arg_var(Tuple, E, OriginalE) of
        true -> ok;
        false -> form_error(Meta, E, ?MODULE, {undefined_var_in_spec, Value})
      end;

    _ when is_integer(Value) ->
      ok;

    _ ->
      form_error(Meta, E, ?MODULE, {bad_size_argument, Value})
  end;
validate_spec_arg(Meta, unit, Value, E, _OriginalE) when not is_integer(Value) ->
  form_error(Meta, E, ?MODULE, {bad_unit_argument, Value});
validate_spec_arg(_Meta, _Key, _Value, _E, _OriginalE) ->
  ok.

is_valid_spec_arg_var(Var, E, #{context := match} = OriginalE) ->
  case OriginalE of
    #{prematch_vars := {#{Var := _}, _}} -> true;
    _ -> is_var(Var, E) andalso not is_var(Var, OriginalE)
  end;
is_valid_spec_arg_var(_Var, _E, _OriginalE) ->
  true.

is_var(Var, #{current_vars := {Read, _}}) ->
  maps:is_key(Var, Read).

validate_size_required(Meta, true, default, Type, default, E) when Type == binary; Type == bitstring ->
  form_error(Meta, E, ?MODULE, unsized_binary);
validate_size_required(_, _, _, _, _, _) ->
  ok.

size_and_unit(Meta, bitstring, Size, Unit, E) when Size /= default; Unit /= default ->
  form_error(Meta, E, ?MODULE, bittype_literal_bitstring);
size_and_unit(Meta, binary, Size, Unit, E) when Size /= default; Unit /= default ->
  form_error(Meta, E, ?MODULE, bittype_literal_string);
size_and_unit(_Meta, _ExprType, Size, Unit, _E) ->
  add_arg(unit, Unit, add_arg(size, Size, [])).

add_arg(_Key, default, Spec) -> Spec;
add_arg(Key, Arg, Spec) -> [{Key, [], [Arg]} | Spec].

build_spec(Meta, Size, Unit, Type, Endianness, Sign, Spec, E) when Type == utf8; Type == utf16; Type == utf32 ->
  if
    Size /= default; Unit /= default ->
      form_error(Meta, E, ?MODULE, bittype_utf);
    Sign /= default ->
      form_error(Meta, E, ?MODULE, bittype_signed);
    true ->
      add_spec(Type, add_spec(Endianness, Spec))
  end;

build_spec(Meta, _Size, Unit, Type, _Endianness, Sign, Spec, E) when Type == binary; Type == bitstring ->
  if
    Type == bitstring, Unit /= default, Unit /= 1 ->
      form_error(Meta, E, ?MODULE, {bittype_mismatch, Unit, 1, unit});
    Sign /= default ->
      form_error(Meta, E, ?MODULE, bittype_signed);
    true ->
      %% Endianness is supported but has no effect, so we just ignore it.
      add_spec(Type, Spec)
  end;

build_spec(Meta, Size, Unit, Type, Endianness, Sign, Spec, E) when Type == integer; Type == float ->
  NumberSize = number_size(Size, Unit),
  if
    Type == float, is_integer(NumberSize), NumberSize /= 32, NumberSize /= 64 ->
      form_error(Meta, E, ?MODULE, {bittype_float_size, NumberSize});
    Size == default, Unit /= default ->
      form_error(Meta, E, ?MODULE, bittype_unit);
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

format_error({unaligned_binary, Expr}) ->
  Message = "expected ~ts to be a binary but its number of bits is not divisible by 8",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error(unsized_binary) ->
  "a binary field without size is only allowed at the end of a binary pattern, "
  "at the right side of binary concatenation and and never allowed in binary generators. "
  "The following examples are invalid:\n\n"
  "    rest <> \"foo\"\n"
  "    <<rest::binary, \"foo\">>\n\n"
  "They are invalid because there is a bits/bitstring component not at the end. "
  "However, the \"reverse\" would work:\n\n"
  "    \"foo\" <> rest\n"
  "    <<\"foo\", rest::binary>>\n\n";
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
