-module(elixir_bitstring).
-export([expand/5, format_error/1, validate_spec/2]).
-import(elixir_errors, [function_error/4, file_error/4]).
-include("elixir.hrl").

expand_match(Expr, {S, OriginalS}, E) ->
  {EExpr, SE, EE} = elixir_expand:expand(Expr, S, E),
  {EExpr, {SE, OriginalS}, EE}.

expand(Meta, Args, S, E, RequireSize) ->
  case ?key(E, context) of
    match ->
      {EArgs, Alignment, {SA, _}, EA} =
        expand(Meta, fun expand_match/3, Args, [], {S, S}, E, 0, RequireSize),

      case find_match(EArgs) of
        false -> ok;
        Match -> file_error(Meta, EA, ?MODULE, {nested_match, Match})
      end,

      {{'<<>>', [{alignment, Alignment} | Meta], EArgs}, SA, EA};
    _ ->
      PairS = {elixir_env:prepare_write(S), S},

      {EArgs, Alignment, {SA, _}, EA} =
        expand(Meta, fun elixir_expand:expand_arg/3, Args, [], PairS, E, 0, RequireSize),

      {{'<<>>', [{alignment, Alignment} | Meta], EArgs}, elixir_env:close_write(SA, S), EA}
  end.

expand(_BitstrMeta, _Fun, [], Acc, S, E, Alignment, _RequireSize) ->
  {lists:reverse(Acc), Alignment, S, E};
expand(BitstrMeta, Fun, [{'::', Meta, [Left, Right]} | T], Acc, S, E, Alignment, RequireSize) ->
  {ELeft, {SL, OriginalS}, EL} = expand_expr(Left, Fun, S, E),

  MatchOrRequireSize = RequireSize or is_match_size(T, EL),
  EType = expr_type(ELeft),
  ExpectSize = case ELeft of
    _ when not MatchOrRequireSize -> optional;
    {'^', _, [{_, _, _}]} -> {infer, ELeft};
    _ -> required
  end,
  {ERight, EAlignment, SS, ES} = expand_specs(EType, Meta, Right, SL, OriginalS, EL, ExpectSize),

  EAcc = concat_or_prepend_bitstring(Meta, ELeft, ERight, Acc, ES, MatchOrRequireSize),
  expand(BitstrMeta, Fun, T, EAcc, {SS, OriginalS}, ES, alignment(Alignment, EAlignment), RequireSize);
expand(BitstrMeta, Fun, [H | T], Acc, S, E, Alignment, RequireSize) ->
  Meta = extract_meta(H, BitstrMeta),
  {ELeft, {SS, OriginalS}, ES} = expand_expr(H, Fun, S, E),

  MatchOrRequireSize = RequireSize or is_match_size(T, ES),
  EType = expr_type(ELeft),
  ERight = infer_spec(EType, Meta),

  InferredMeta = [{inferred_bitstring_spec, true} | Meta],
  EAcc = concat_or_prepend_bitstring(InferredMeta, ELeft, ERight, Acc, ES, MatchOrRequireSize),
  expand(Meta, Fun, T, EAcc, {SS, OriginalS}, ES, Alignment, RequireSize).

extract_meta({_, Meta, _}, _) -> Meta;
extract_meta(_, Meta) -> Meta.

%% Variables defined outside the binary can be accounted
%% on subparts; however, we can't assign new variables.
is_match_size([_ | _], #{context := match}) -> true;
is_match_size(_, _) -> false.

expr_type(Integer) when is_integer(Integer) -> integer;
expr_type(Float) when is_float(Float) -> float;
expr_type(Binary) when is_binary(Binary) -> binary;
expr_type({'<<>>', _, _}) -> bitstring;
expr_type(_) -> default.

infer_spec(bitstring, Meta) -> {bitstring, Meta, nil};
infer_spec(binary, Meta) -> {binary, Meta, nil};
infer_spec(float, Meta) -> {float, Meta, nil};
infer_spec(integer, Meta) -> {integer, Meta, nil};
infer_spec(default, Meta) -> {integer, Meta, nil}.

concat_or_prepend_bitstring(_Meta, {'<<>>', _, []}, _ERight, Acc, _E, _RequireSize) ->
  Acc;
concat_or_prepend_bitstring(Meta, {'<<>>', PartsMeta, Parts} = ELeft, ERight, Acc, E, RequireSize) ->
  case E of
    #{context := match} when RequireSize ->
      case lists:last(Parts) of
        {'::', SpecMeta, [Bin, {binary, _, nil}]} when not is_binary(Bin) ->
          function_error(SpecMeta, E, ?MODULE, unsized_binary);

        {'::', SpecMeta, [_, {bitstring, _, nil}]} ->
          function_error(SpecMeta, E, ?MODULE, unsized_binary);

        _ ->
          ok
      end;
    _ ->
      ok
  end,

  case ERight of
    {binary, _, nil} ->
      {alignment, Alignment} = lists:keyfind(alignment, 1, PartsMeta),

      if
        is_integer(Alignment) ->
          (Alignment /= 0) andalso function_error(Meta, E, ?MODULE, {unaligned_binary, ELeft}),
          lists:reverse(Parts, Acc);

        true ->
          [{'::', Meta, [ELeft, ERight]} | Acc]
      end;
    {bitstring, _, nil} ->
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

expand_expr({{'.', _, [Mod, to_string]}, _, [Arg]} = AST, Fun, S, #{context := Context} = E)
    when Context /= nil, (Mod == 'Elixir.Kernel') orelse (Mod == 'Elixir.String.Chars') ->
  case Fun(Arg, S, E) of
    {EBin, SE, EE} when is_binary(EBin) -> {EBin, SE, EE};
    _ -> Fun(AST, S, E) % Let it raise
  end;
expand_expr(Component, Fun, S, E) ->
  Fun(Component, S, E).

%% Expands and normalizes types of a bitstring.

expand_specs(ExprType, Meta, Info, S, OriginalS, E, ExpectSize) ->
  Default =
    #{size => default,
      unit => default,
      sign => default,
      type => default,
      endianness => default},
  {#{size := Size, unit := Unit, type := Type, endianness := Endianness, sign := Sign}, SS, ES} =
    expand_each_spec(Meta, unpack_specs(Info, []), Default, S, OriginalS, E),

  MergedType = type(Meta, ExprType, Type, E),
  validate_size_required(Meta, ExpectSize, ExprType, MergedType, Size, ES),
  SizeAndUnit = size_and_unit(Meta, ExprType, Size, Unit, ES),
  Alignment = compute_alignment(MergedType, Size, Unit),

  MaybeInferredSize = case {ExpectSize, MergedType, SizeAndUnit} of
    {{infer, PinnedVar}, binary, []} -> [{size, Meta, [{{'.', Meta, [erlang, byte_size]}, Meta, [PinnedVar]}]}];
    {{infer, PinnedVar}, bitstring, []} -> [{size, Meta, [{{'.', Meta, [erlang, bit_size]}, Meta, [PinnedVar]}]}];
    _ -> SizeAndUnit
  end,

  [H | T] = build_spec(Meta, Size, Unit, MergedType, Endianness, Sign, MaybeInferredSize, ES),
  {lists:foldl(fun(I, Acc) -> {'-', Meta, [Acc, I]} end, H, T), Alignment, SS, ES}.

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
type(Meta, Other, Type, E) ->
  function_error(Meta, E, ?MODULE, {bittype_mismatch, Type, Other, type}),
  Type.

expand_each_spec(Meta, [{Expr, MetaE, Args} = H | T], Map, S, OriginalS, E) when is_atom(Expr) ->
  case validate_spec(Expr, Args) of
    {Key, Arg} ->
      case Args of
        [] ->
          elixir_errors:file_warn(Meta, E, ?MODULE, {parens_bittype, Expr});
        _ -> ok
      end,
      {Value, SE, EE} = expand_spec_arg(Arg, S, OriginalS, E),
      validate_spec_arg(Meta, Key, Value, SE, OriginalS, EE),

      case maps:get(Key, Map) of
        default -> ok;
        Value -> ok;
        Other -> function_error(Meta, E, ?MODULE, {bittype_mismatch, Value, Other, Key})
      end,

      expand_each_spec(Meta, T, maps:put(Key, Value, Map), SE, OriginalS, EE);

    none ->
      HA = case Args of
        nil ->
          elixir_errors:file_warn(Meta, E, ?MODULE, {unknown_bittype, Expr}),
          {Expr, MetaE, []};
        _ -> H
      end,

      case 'Elixir.Macro':expand(HA, E#{line := ?line(Meta)}) of
        HA ->
          function_error(Meta, E, ?MODULE, {undefined_bittype, H}),
          expand_each_spec(Meta, T, Map, S, OriginalS, E);

        NewTypes ->
          expand_each_spec(Meta, unpack_specs(NewTypes, []) ++ T, Map, S, OriginalS, E)
      end
  end;
expand_each_spec(Meta, [Expr | Tail], Map, S, OriginalS, E) ->
  function_error(Meta, E, ?MODULE, {undefined_bittype, Expr}),
  expand_each_spec(Meta, Tail, Map, S, OriginalS, E);
expand_each_spec(_Meta, [], Map, S, _OriginalS, E) ->
  {Map, S, E}.

unpack_specs({'-', _, [H, T]}, Acc) ->
  unpack_specs(H, unpack_specs(T, Acc));
unpack_specs({'*', _, [{'_', _, Atom}, Unit]}, Acc) when is_atom(Atom) ->
  [{unit, [], [Unit]} | Acc];
unpack_specs({'*', _, [Size, Unit]}, Acc) ->
  [{size, [], [Size]}, {unit, [], [Unit]} | Acc];
unpack_specs(Size, Acc) when is_integer(Size) ->
  [{size, [], [Size]} | Acc];
unpack_specs({Expr, Meta, Args}, Acc) when is_atom(Expr) ->
  ListArgs = if is_atom(Args) -> nil; is_list(Args) -> Args end,
  [{Expr, Meta, ListArgs} | Acc];
unpack_specs(Other, Acc) ->
  [Other | Acc].

validate_spec(Spec, [])       -> validate_spec(Spec, nil);
validate_spec(big, nil)       -> {endianness, big};
validate_spec(little, nil)    -> {endianness, little};
validate_spec(native, nil)    -> {endianness, native};
validate_spec(size, [Size])   -> {size, Size};
validate_spec(unit, [Unit])   -> {unit, Unit};
validate_spec(integer, nil)   -> {type, integer};
validate_spec(float, nil)     -> {type, float};
validate_spec(binary, nil)    -> {type, binary};
validate_spec(bytes, nil)     -> {type, binary};
validate_spec(bitstring, nil) -> {type, bitstring};
validate_spec(bits, nil)      -> {type, bitstring};
validate_spec(utf8, nil)      -> {type, utf8};
validate_spec(utf16, nil)     -> {type, utf16};
validate_spec(utf32, nil)     -> {type, utf32};
validate_spec(signed, nil)    -> {sign, signed};
validate_spec(unsigned, nil)  -> {sign, unsigned};
validate_spec(_, _)           -> none.

expand_spec_arg(Expr, S, _OriginalS, E) when is_atom(Expr); is_integer(Expr) ->
  {Expr, S, E};
expand_spec_arg(Expr, S, OriginalS, #{context := match} = E) ->
  %% We can only access variables that are either on prematch or not in original
  #elixir_ex{prematch={PreRead, PreCycle, _} = OldPre} = S,
  #elixir_ex{vars={OriginalRead, _}} = OriginalS,
  NewPre = {PreRead, PreCycle, {bitsize, OriginalRead}},
  {EExpr, SE, EE} = elixir_expand:expand(Expr, S#elixir_ex{prematch=NewPre}, E#{context := guard}),
  {EExpr, SE#elixir_ex{prematch=OldPre}, EE#{context := match}};
expand_spec_arg(Expr, S, OriginalS, E) ->
  elixir_expand:expand(Expr, elixir_env:reset_read(S, OriginalS), E).

validate_spec_arg(Meta, unit, Value, _S, _OriginalS, E) when not is_integer(Value) ->
  function_error(Meta, E, ?MODULE, {bad_unit_argument, Value}),
  ok;
validate_spec_arg(_Meta, _Key, _Value, _S, _OriginalS, _E) ->
  ok.

validate_size_required(Meta, required, default, Type, default, E) when Type == binary; Type == bitstring ->
  function_error(Meta, E, ?MODULE, unsized_binary),
  ok;
validate_size_required(_, _, _, _, _, _) ->
  ok.

size_and_unit(Meta, bitstring, Size, Unit, E) when Size /= default; Unit /= default ->
  function_error(Meta, E, ?MODULE, bittype_literal_bitstring),
  [];
size_and_unit(Meta, binary, Size, Unit, E) when Size /= default; Unit /= default ->
  function_error(Meta, E, ?MODULE, bittype_literal_string),
  [];
size_and_unit(_Meta, _ExprType, Size, Unit, _E) ->
  add_arg(unit, Unit, add_arg(size, Size, [])).

add_arg(_Key, default, Spec) -> Spec;
add_arg(Key, Arg, Spec) -> [{Key, [], [Arg]} | Spec].

build_spec(Meta, Size, Unit, Type, Endianness, Sign, Spec, E) when Type == utf8; Type == utf16; Type == utf32 ->
  if
    Size /= default; Unit /= default ->
      function_error(Meta, E, ?MODULE, bittype_utf);
    Sign /= default ->
      function_error(Meta, E, ?MODULE, bittype_signed);
    true ->
      ok
  end,
  add_spec(Type, add_spec(Endianness, Spec));

build_spec(Meta, _Size, Unit, Type, _Endianness, Sign, Spec, E) when Type == binary; Type == bitstring ->
  if
    Type == bitstring, Unit /= default, Unit /= 1 ->
      function_error(Meta, E, ?MODULE, {bittype_mismatch, Unit, 1, unit});
    Sign /= default ->
      function_error(Meta, E, ?MODULE, bittype_signed);
    true ->
      %% Endianness is supported but has no effect, so we just ignore it.
      ok
  end,
  add_spec(Type, Spec);

build_spec(Meta, Size, Unit, Type, Endianness, Sign, Spec, E) when Type == integer; Type == float ->
  NumberSize = number_size(Size, Unit),
  if
    Type == float, is_integer(NumberSize) ->
      case valid_float_size(NumberSize) of
        true ->
          add_spec(Type, add_spec(Endianness, add_spec(Sign, Spec)));
        false ->
          function_error(Meta, E, ?MODULE, {bittype_float_size, NumberSize}),
          []
      end;
    Size == default, Unit /= default ->
      function_error(Meta, E, ?MODULE, bittype_unit),
      [];
    true ->
      add_spec(Type, add_spec(Endianness, add_spec(Sign, Spec)))
  end.

number_size(Size, default) when is_integer(Size) -> Size;
number_size(Size, Unit) when is_integer(Size) -> Size * Unit;
number_size(Size, _) -> Size.

valid_float_size(16) -> true;
valid_float_size(32) -> true;
valid_float_size(64) -> true;
valid_float_size(_) -> false.

add_spec(default, Spec) -> Spec;
add_spec(Key, Spec) -> [{Key, [], nil} | Spec].

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
  "at the right side of binary concatenation and never allowed in binary generators. "
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
  io_lib:format("float requires size*unit to be 16, 32, or 64 (default), got: ~p", [Other]);
format_error({undefined_bittype, Expr}) ->
  io_lib:format("unknown bitstring specifier: ~ts", ['Elixir.Macro':to_string(Expr)]);
format_error({unknown_bittype, Name}) ->
  io_lib:format("bitstring specifier \"~ts\" does not exist and is being expanded to \"~ts()\","
                " please use parentheses to remove the ambiguity.\n"
                "You may run \"mix format --migrate\" to fix this warning automatically.", [Name, Name]);
format_error({parens_bittype, Name}) ->
    io_lib:format("extra parentheses on a bitstring specifier \"~ts()\" have been deprecated. "
    "Please remove the parentheses: \"~ts\".\n"
    "You may run \"mix format --migrate\" to fix this warning automatically."
    ,
    [Name, Name]);
format_error({bittype_mismatch, Val1, Val2, Where}) ->
  io_lib:format("conflicting ~ts specification for bit field: \"~p\" and \"~p\"", [Where, Val1, Val2]);
format_error({bad_unit_argument, Unit}) ->
  io_lib:format("unit in bitstring expects an integer as argument, got: ~ts",
                ['Elixir.Macro':to_string(Unit)]);
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
