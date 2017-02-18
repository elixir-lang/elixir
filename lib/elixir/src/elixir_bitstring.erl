-module(elixir_bitstring).
-export([translate/3, expand/3, has_size/1, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% Expansion

expand(Meta, Args, E) ->
  case ?m(E, context) of
    match ->
      {EArgs, EA} = expand_bitstr(Meta, fun elixir_exp:expand/2, Args, [], E),
      {{'<<>>', Meta, EArgs}, EA};
    _ ->
      {EArgs, {EC, EV}} = expand_bitstr(Meta, fun elixir_exp:expand_arg/2, Args, [], {E, E}),
      {{'<<>>', Meta, EArgs}, elixir_env:mergea(EV, EC)}
  end.

expand_bitstr(_BitstrMeta, _Fun, [], Acc, E) ->
  {lists:reverse(Acc), E};
expand_bitstr(BitstrMeta, Fun, [{'::', Meta, [Left, Right]} | T], Acc, E) ->
  {ELeft, EL} = expand_bitstr_component(Meta, Left, Fun, E),

  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  ER = case E of
    {EExtracted, _} -> EExtracted;        %% expand_arg,  no assigns
    _               -> E#{context := nil} %% expand_each, revert assigns
  end,

  ERight = expand_bit_info(ELeft, Meta, Right, ER),
  expand_bitstr(BitstrMeta, Fun, T, [{'::', Meta, [ELeft, ERight]} | Acc], EL);
expand_bitstr(BitstrMeta, Fun, [{_, Meta, _} = H | T], Acc, E) ->
  {Expr, ES} = expand_bitstr_component(Meta, H, Fun, E),
  expand_bitstr(BitstrMeta, Fun, T, [Expr | Acc], ES);
expand_bitstr(Meta, Fun, [H | T], Acc, E) ->
  {Expr, ES} = expand_bitstr_component(Meta, H, Fun, E),
  expand_bitstr(Meta, Fun, T, [Expr | Acc], ES).

%% Expands a "component" of the bitstring, that is, either the LHS of a :: or an
%% argument of the bitstring (such as "foo" in "<<foo>>").
expand_bitstr_component(Meta, Component, Fun, E) ->
  case Fun(Component, E) of
    {EComponent, _} when is_list(EComponent); is_atom(EComponent) ->
      ErrorE = env_for_error(E),
      form_error(Meta, ?m(ErrorE, file), ?MODULE, {invalid_literal, EComponent});
    {_, _} = Expanded ->
      Expanded
  end.

env_for_error({E, _}) -> E;
env_for_error(E) -> E.

%% Expand bit info

expand_bit_info(Left, Meta, Info, E) ->
  expand_bit_info(Left, Meta, unpack_bit_info(Info, []), default, [], E).

expand_bit_info(Left, Meta, [{size, _, [_] = Args} | T], Size, Types, E) ->
  case {Left, Size} of
    %% If we see a "size" part but the LHS of :: is a literal binary, then we can
    %% safely raise; we're sure that this is the first size we encountered
    %% (default) otherwise we would have raised before.
    {Bin, default} when is_binary(Bin) ->
      form_error(Meta, ?m(E, file), ?MODULE, size_for_literal_string);
    {_, default} ->
      {[EArg], EE} = elixir_exp:expand_args(Args, E),

      case EArg of
        {Var, _, Context} when is_atom(Var) and is_atom(Context) ->
          ok;
        _ when is_integer(EArg) ->
          ok;
        _ ->
          form_error(Meta, ?m(E, file), ?MODULE, {bad_bitsize, EArg})
      end,

      expand_bit_info(Left, Meta, T, {size, [], [EArg]}, Types, EE);
    _ ->
      form_error(Meta, ?m(E, file), ?MODULE, duplicated_size_definition)
 end;

expand_bit_info(Left, Meta, [{Expr, ExprMeta, Args} | T], Size, Types, E) when is_atom(Expr) ->
  case expand_bit_type(Expr, Args) of
    type ->
      {EArgs, EE} = elixir_exp:expand_args(Args, E),
      validate_bit_type_args(Meta, Expr, EArgs, EE),
      validate_bit_type_if_literal_bin(Left, Meta, Expr, E),
      expand_bit_info(Left, Meta, T, Size, [{Expr, [], EArgs} | Types], EE);
    none ->
      handle_unknown_bit_info(Left, Meta, {Expr, ExprMeta, Args}, T, Size, Types, E)
  end;

expand_bit_info(_Left, Meta, [Expr | _], _Size, _Types, E) ->
  form_error(Meta, ?m(E, file), ?MODULE, {undefined_bittype, Expr});
expand_bit_info(_Left, Meta, [], Size, Types, _) ->
  [H | T] = case Size of
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
  form_error(Meta, ?m(E, file), ?MODULE, {bad_unit_argument, Unit});
validate_bit_type_args(_Meta, _Expr, _Args, _E) ->
  ok.

validate_bit_type_if_literal_bin(Left, Meta, Type, E) when is_binary(Left) ->
  case valid_bit_type_for_literal_bin(Type) of
    true ->
      ok;
    false ->
      form_error(Meta, ?m(E, file), ?MODULE, invalid_type_for_literal_string)
  end;
validate_bit_type_if_literal_bin(_Left, _Meta, _Type, _E) ->
  ok.

valid_bit_type_for_literal_bin(little) -> true;
valid_bit_type_for_literal_bin(big) -> true;
valid_bit_type_for_literal_bin(utf8) -> true;
valid_bit_type_for_literal_bin(utf16) -> true;
valid_bit_type_for_literal_bin(utf32) -> true;
valid_bit_type_for_literal_bin(bytes) -> true;
valid_bit_type_for_literal_bin(binary) -> true;
valid_bit_type_for_literal_bin(bits) -> true;
valid_bit_type_for_literal_bin(bitstring) -> true;
valid_bit_type_for_literal_bin(default) -> true;
valid_bit_type_for_literal_bin(_) -> false.

handle_unknown_bit_info(Left, Meta, Expr, T, Size, Types, E) ->
  case 'Elixir.Macro':expand(Expr, elixir_env:linify({?line(Meta), E})) of
    Expr ->
      form_error(Meta, ?m(E, file), ?MODULE, {undefined_bittype, Expr});
    Info ->
      expand_bit_info(Left, Meta, unpack_bit_info(Info, []) ++ T, Size, Types, E)
  end.

unpack_bit_info({'-', _, [H, T]}, Acc) ->
  unpack_bit_info(H, unpack_bit_info(T, Acc));
unpack_bit_info({'*', _, [{'_', _, Atom}, Unit]}, Acc) when is_atom(Atom) and is_integer(Unit) ->
  [{unit, [], [Unit]} | Acc];
unpack_bit_info({'*', _, [Size, Unit]}, Acc) when is_integer(Size) and is_integer(Unit) ->
  [{size, [], [Size]}, {unit, [], [Unit]} | Acc];
unpack_bit_info(Size, Acc) when is_integer(Size) ->
  [{size, [], [Size]} | Acc];
unpack_bit_info({Expr, Meta, Args}, Acc) when is_atom(Expr) ->
  ListArgs = if is_atom(Args) -> []; is_list(Args) -> Args end,
  [{Expr, Meta, ListArgs} | Acc];
unpack_bit_info(Other, Acc) ->
  [Other | Acc].

format_error({invalid_literal, Literal}) ->
  io_lib:format("invalid literal ~ts in <<>>", ['Elixir.Macro':to_string(Literal)]);
format_error(size_for_literal_string) ->
  "size is not supported for literal string in <<>>";
format_error({bad_bitsize, Expr}) ->
  io_lib:format("size in bitstring expects an integer or a variable as argument, got: ~ts",
                ['Elixir.Macro':to_string(Expr)]);
format_error(duplicated_size_definition) ->
  "duplicated size definition in bitstring";
format_error({bad_unit_argument, Unit}) ->
  io_lib:format("unit in bitstring expects an integer as argument, got: ~ts",
                ['Elixir.Macro':to_string(Unit)]);
format_error(invalid_type_for_literal_string) ->
  "invalid types for literal string in <<>>. Accepted types are: "
    "little, big, utf8, utf16, utf32, bits, bytes, binary, bitstring";
format_error({undefined_bittype, Expr}) ->
  io_lib:format("unknown bitstring specifier ~ts", ['Elixir.Macro':to_string(Expr)]).

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

build_bitstr_each(Fun, [{'::', _, [H, V]} | T], Meta, S, Acc) ->
  {Size, Types} = extract_bit_info(V, S#elixir_scope{context=nil}),
  build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types);

build_bitstr_each(Fun, [H | T], Meta, S, Acc) ->
  build_bitstr_each(Fun, T, Meta, S, Acc, H, default, default).

build_bitstr_each(Fun, T, Meta, S, Acc, H, default, Types) when is_binary(H) ->
  Element =
    case types_allow_splice(Types) of
      true ->
        %% See explanation in elixir_utils:elixir_to_erl/1 to know
        %% why we can simply convert the binary to a list.
        {bin_element, ?ann(Meta), {string, 0, binary_to_list(H)}, default, default};
      false ->
        %% The Types must require conversion at this point (for example, utf
        %% types).
        {bin_element, ?ann(Meta), {string, 0, elixir_utils:characters_to_list(H)}, default, Types}
    end,

  build_bitstr_each(Fun, T, Meta, S, [Element | Acc]);

build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types) ->
  {Expr, NS} = Fun(H, S),
  Splice = types_allow_splice(Types),

  case Expr of
    {bin, _, Elements} when Splice, Size == default, S#elixir_scope.context == match ->
      build_bitstr_each(Fun, T, Meta, NS, lists:reverse(Elements, Acc));
    {bin, _, _} when Types == default ->
      build_bitstr_each(Fun, T, Meta, NS, [{bin_element, ?ann(Meta), Expr, Size, [bitstring]} | Acc]);
    _ ->
      build_bitstr_each(Fun, T, Meta, NS, [{bin_element, ?ann(Meta), Expr, Size, Types} | Acc])
  end.

types_allow_splice([bytes])     -> true;
types_allow_splice([binary])    -> true;
types_allow_splice([bits])      -> true;
types_allow_splice([bitstring]) -> true;
types_allow_splice(default)     -> true;
types_allow_splice(_)           -> false.

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
  [{unit, Arg} | Acc];
extract_bit_type({Other, _, []}, Acc) ->
  [Other | Acc].
