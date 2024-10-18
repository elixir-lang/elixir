%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([get_line/1, generated/1,
  split_last/1, split_opts/1, noop/0, var_context/2,
  characters_to_list/1, characters_to_binary/1, relative_to_cwd/1,
  macro_name/1, returns_boolean/1, caller/4, meta_keep/1,
  read_file_type/1, read_file_type/2, read_link_type/1, read_posix_mtime_and_size/1,
  change_posix_time/2, change_universal_time/2, var_info/2,
  guard_op/2, guard_info/1, extract_splat_guards/1, extract_guards/1,
  erlang_comparison_op_to_elixir/1, erl_fa_to_elixir_fa/2, jaro_similarity/2]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

var_info(Name, Kind) when Kind == nil; is_integer(Kind) ->
  io_lib:format("\"~ts\"", [Name]);
var_info(Name, Kind) ->
  io_lib:format("\"~ts\" (context ~ts)", [Name, elixir_aliases:inspect(Kind)]).

guard_info(#elixir_ex{prematch={_, _, {bitsize, _}}}) -> "bitstring size specifier";
guard_info(_) -> "guard".

macro_name(Macro) ->
  list_to_atom("MACRO-" ++ atom_to_list(Macro)).

erl_fa_to_elixir_fa(Name, Arity) ->
  case atom_to_list(Name) of
    "MACRO-" ++ Rest -> {list_to_atom(Rest), Arity - 1};
    _ -> {Name, Arity}
  end.

guard_op('andalso', 2) ->
  true;
guard_op('orelse', 2) ->
  true;
guard_op(Op, Arity) ->
  try erl_internal:op_type(Op, Arity) of
    arith -> true;
    comp  -> true;
    bool  -> true;
    list  -> false;
    send  -> false
  catch
    _:_ -> false
  end.

erlang_comparison_op_to_elixir('/=') -> '!=';
erlang_comparison_op_to_elixir('=<') -> '<=';
erlang_comparison_op_to_elixir('=:=') -> '===';
erlang_comparison_op_to_elixir('=/=') -> '!==';
erlang_comparison_op_to_elixir(Other) -> Other.

var_context(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    {counter, Counter} -> Counter;
    false -> Kind
  end.

% Extract guards

extract_guards({'when', _, [Left, Right]}) -> {Left, extract_or_guards(Right)};
extract_guards(Else) -> {Else, []}.

extract_or_guards({'when', _, [Left, Right]}) -> [Left | extract_or_guards(Right)];
extract_or_guards(Term) -> [Term].

% Extract guards when multiple left side args are allowed.

extract_splat_guards([{'when', _, [_ | _] = Args}]) ->
  {Left, Right} = split_last(Args),
  {Left, extract_or_guards(Right)};
extract_splat_guards(Else) ->
  {Else, []}.

%% No-op function that can be used for stuff like preventing tail-call
%% optimization to kick in.
noop() ->
  ok.

split_last([])           -> {[], []};
split_last(List)         -> split_last(List, []).
split_last([H], Acc)     -> {lists:reverse(Acc), H};
split_last([H | T], Acc) -> split_last(T, [H | Acc]).

%% Useful to handle options similarly in `opts, do ... end` and `opts, do: ...`.
split_opts(Args) ->
  case elixir_utils:split_last(Args) of
    {OuterCases, OuterOpts} when is_list(OuterOpts) ->
      case elixir_utils:split_last(OuterCases) of
        {InnerCases, InnerOpts} when is_list(InnerOpts) ->
          {InnerCases, InnerOpts ++ OuterOpts};
        _ ->
          {OuterCases, OuterOpts}
      end;
    _ ->
      {Args, []}
  end.

read_file_type(File) ->
  read_file_type(File, []).

read_file_type(File, Opts) ->
  case file:read_file_info(File, [{time, posix} | Opts]) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

read_link_type(File) ->
  case file:read_link_info(File) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

read_posix_mtime_and_size(File) ->
  case file:read_file_info(File, [raw, {time, posix}]) of
    {ok, #file_info{mtime=Mtime, size=Size}} -> {ok, Mtime, Size};
    {error, _} = Error -> Error
  end.

change_posix_time(Name, Time) when is_integer(Time) ->
  file:write_file_info(Name, #file_info{mtime=Time}, [raw, {time, posix}]).

change_universal_time(Name, {{Y, M, D}, {H, Min, Sec}}=Time)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(Min), is_integer(Sec) ->
  file:write_file_info(Name, #file_info{mtime=Time}, [{time, universal}]).

relative_to_cwd(Path) ->
  try elixir_config:get(relative_paths) of
    true  -> 'Elixir.Path':relative_to_cwd(Path);
    false -> Path
  catch
    _:_ -> Path
  end.

characters_to_list(Data) when is_list(Data) ->
  Data;
characters_to_list(Data) ->
  case unicode:characters_to_list(Data) of
    Result when is_list(Result) -> Result;
    {error, Encoded, Rest} -> conversion_error(invalid, Encoded, Rest);
    {incomplete, Encoded, Rest} -> conversion_error(incomplete, Encoded, Rest)
  end.

characters_to_binary(Data) when is_binary(Data) ->
  Data;
characters_to_binary(Data) ->
  case unicode:characters_to_binary(Data) of
    Result when is_binary(Result) -> Result;
    {error, Encoded, Rest} -> conversion_error(invalid, Encoded, Rest);
    {incomplete, Encoded, Rest} -> conversion_error(incomplete, Encoded, Rest)
  end.

conversion_error(Kind, Encoded, Rest) ->
  error('Elixir.UnicodeConversionError':exception([{encoded, Encoded}, {rest, Rest}, {kind, Kind}])).

%% Returns the caller as a stacktrace entry.
caller(Line, File, nil, _) ->
  {elixir_compiler_0, '__FILE__', 1, stack_location(Line, File)};
caller(Line, File, Module, nil) ->
  {Module, '__MODULE__', 0, stack_location(Line, File)};
caller(Line, File, Module, {Name, Arity}) ->
  {Module, Name, Arity, stack_location(Line, File)}.

stack_location(Line, File) ->
  [{file, elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File))},
   {line, Line}].

get_line(Opts) when is_list(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    {line, Line} when is_integer(Line) -> Line;
    _ -> 0
  end.

generated([{generated, true} | _] = Meta) -> Meta;
generated(Meta) -> [{generated, true} | Meta].

%% Meta location.
%%
%% Macros add a file pair on location keep which we
%% should take into account for error reporting.
%%
%% Returns {binary, integer} on location keep or nil.

meta_keep(Meta) ->
  case lists:keyfind(keep, 1, Meta) of
    {keep, {File, Line} = Pair} when is_binary(File), is_integer(Line) ->
      Pair;
    _ ->
      nil
  end.

%% Boolean checks

returns_boolean(Bool) when is_boolean(Bool) -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_]}) when Op == 'not' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, _]}) when
  Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '==';  Op == '/='; Op == '=<';  Op == '>=';
  Op == '<';   Op == '>';  Op == '=:='; Op == '=/=' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, Right]}) when
  Op == 'andalso'; Op == 'orelse' ->
  returns_boolean(Right);

returns_boolean({{'.', _, [erlang, Fun]}, _, [_]}) when
  Fun == is_atom;   Fun == is_binary;   Fun == is_bitstring; Fun == is_boolean;
  Fun == is_float;  Fun == is_function; Fun == is_integer;   Fun == is_list;
  Fun == is_number; Fun == is_pid;      Fun == is_port;      Fun == is_reference;
  Fun == is_tuple;  Fun == is_map;      Fun == is_process_alive -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _]}) when
  Fun == is_map_key; Fun == is_function; Fun == is_record -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _, _]}) when
  Fun == function_exported; Fun == is_record -> true;

returns_boolean({'case', _, [_, [{do, Clauses}]]}) ->
  lists:all(fun
    ({'->', _, [_, Expr]}) -> returns_boolean(Expr)
  end, Clauses);

returns_boolean({'cond', _, [[{do, Clauses}]]}) ->
  lists:all(fun
    ({'->', _, [_, Expr]}) -> returns_boolean(Expr)
  end, Clauses);

returns_boolean({'__block__', _, Exprs}) ->
  returns_boolean(lists:last(Exprs));

returns_boolean(_) -> false.


% TODO: Remove me when we require Erlang/OTP 27+
% This is a polyfill for older versions, copying the code from
% https://github.com/erlang/otp/pull/7879
-spec jaro_similarity(String1, String2) -> Similarity when
      String1 :: unicode:chardata(),
      String2 :: unicode:chardata(),
      Similarity :: float(). %% Between +0.0 and 1.0
jaro_similarity(A0, B0) ->
    {A, ALen} = str_to_gcl_and_length(A0),
    {B, BLen} = str_to_indexmap(B0),
    Dist = max(ALen, BLen) div 2,
    {AM, BM} = jaro_match(A, B, -Dist, Dist, [], []),
    if
        ALen =:= 0 andalso BLen =:= 0 ->
            1.0;
        ALen =:= 0 orelse BLen =:= 0 ->
            0.0;
        AM =:= [] ->
            0.0;
        true ->
            {M,T} = jaro_calc_mt(AM, BM, 0, 0),
            (M/ALen + M/BLen + (M-T/2)/M) / 3
    end.

jaro_match([A|As], B0, Min, Max, AM, BM) ->
    case jaro_detect(maps:get(A, B0, []), Min, Max) of
        false ->
            jaro_match(As, B0, Min+1, Max+1, AM, BM);
        {J, Remain} ->
            B = B0#{A => Remain},
            jaro_match(As, B, Min+1, Max+1, [A|AM], add_rsorted({J,A},BM))
    end;
jaro_match(_A, _B, _Min, _Max, AM, BM) ->
    {AM, BM}.

jaro_detect([Idx|Rest], Min, Max) when Min < Idx, Idx < Max ->
    {Idx, Rest};
jaro_detect([Idx|Rest], Min, Max) when Idx < Max ->
    jaro_detect(Rest, Min, Max);
jaro_detect(_, _, _) ->
    false.

jaro_calc_mt([CharA|AM], [{_, CharA}|BM], M, T) ->
    jaro_calc_mt(AM, BM, M+1, T);
jaro_calc_mt([_|AM], [_|BM], M, T) ->
    jaro_calc_mt(AM, BM, M+1, T+1);
jaro_calc_mt([], [], M, T) ->
    {M, T}.


%% Returns GC list and length
str_to_gcl_and_length(S0) ->
    gcl_and_length(unicode_util:gc(S0), [], 0).

gcl_and_length([C|Str], Acc, N) ->
    gcl_and_length(unicode_util:gc(Str), [C|Acc], N+1);
gcl_and_length([], Acc, N) ->
    {lists:reverse(Acc), N};
gcl_and_length({error, Err}, _, _) ->
    error({badarg, Err}).

%% Returns GC map with index and length
str_to_indexmap(S) ->
    [M|L] = str_to_map(unicode_util:gc(S), 0),
    {M,L}.

str_to_map([], L) -> [#{}|L];
str_to_map([G | Gs], I) ->
    [M|L] = str_to_map(unicode_util:gc(Gs), I+1),
    [maps:put(G, [I | maps:get(G, M, [])], M)| L];
str_to_map({error,Error}, _) ->
    error({badarg, Error}).

%% Add in decreasing order
add_rsorted(A, [H|_]=BM) when A > H ->
    [A|BM];
add_rsorted(A, [H|BM]) ->
    [H|add_rsorted(A,BM)];
add_rsorted(A, []) ->
    [A].

