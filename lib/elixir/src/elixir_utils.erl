%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([get_line/1, get_line/2, get_file/2, generated/1,
  split_last/1, split_opts/1, noop/0, var_context/2,
  characters_to_list/1, characters_to_binary/1, relative_to_cwd/1,
  macro_name/1, returns_boolean/1, caller/4, meta_keep/1,
  read_file_type/1, read_file_type/2, read_link_type/1, read_posix_mtime_and_size/1,
  change_posix_time/2, change_universal_time/2, var_info/2,
  guard_op/2, guard_info/1, extract_splat_guards/1, extract_guards/1,
  erlang_comparison_op_to_elixir/1, erl_fa_to_elixir_fa/2, rand_shuffle/1]).
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
  file:write_file_info(Name, #file_info{mtime=Time}, [raw, {time, universal}]).

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

get_line(Meta, Env) when is_list(Meta) ->
  case lists:keyfind(line, 1, Meta) of
    {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
    false -> ?key(Env, line)
  end.

get_file(Meta, Env) when is_list(Meta) ->
  case lists:keyfind(file, 1, Meta) of
    {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
    false -> ?key(Env, file)
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

returns_boolean({{'.', _, [lists, member]}, _, [_, _]}) ->
  true;

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

%% TODO: Remove this when we require Erlang/OTP 29+
-if(?OTP_RELEASE >= 29).
rand_shuffle(List) -> rand:shuffle(List).
-else.
%% The code below is a copy of rand:shuffle/1 from Erlang/OTP 29,
%% with the is_integer/3 guards rewritten for earlier versions.
%%
%% Copyright Ericsson AB 2015-2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-define(BIT(Bits), (1 bsl (Bits))).
-define(MASK(Bits), (?BIT(Bits) - 1)).
-define(MASK(Bits, X), ((X) band ?MASK(Bits))).

rand_shuffle(List) when is_list(List) ->
    {AlgHandler, R0} =
        case get(rand_seed) of
            undefined -> rand:seed_s(default);
            Old -> Old
        end,
    [P0|S0] = shuffle_init_bitstream(R0, AlgHandler),
    {ShuffledList, _P1, [R1|_]=_S1} = shuffle_r(List, [], P0, S0),
    put(rand_seed, {AlgHandler, R1}),
    ShuffledList.

%% Random-split-and-shuffle algorithm suggested by Richard A. O'Keefe
%% on ErlangForums, as I interpreted it...  "basically a randomized
%% quicksort", shall we name it Quickshuffle?
%%
%% Randomly split the list in two lists, and recursively shuffle
%% the two smaller lists.
%%
%% How the algorithm works and why it is correct can be explained like this:
%%
%% The objective is, given a list of elements, to return a random
%% permutation of those elements so that every possible permutation
%% has the same probability to be returned.
%%
%% One of the two correct and bias free algorithms described on the Wikipedia
%% page for Fisher-Yates shuffling is to assign a random number to each
%% element in the list and order the elements according to the numbers.
%% For this to be correct the generated numbers must not have duplicates.
%%
%% This algorithm does that, but assigning a number and ordering
%% the elements is more or less the same step, which is taken
%% one binary bit at the time.
%%
%% It can be seen as, to each element, assign a fixpoint number
%% of infinite length starting with bit weight 1/2, continuing with 1/4,
%% and so on..., but reveal it incrementally.
%%
%% The list to shuffle is traversed, and a random bit is generated
%% for each element.  If it is a 0, the element is assigned the zero bit
%% by moving it to the head of the list Zero, and if it is a 1,
%% to the head of the list One.
%%
%% Then the list Zero is recursively shuffled onto the accumulator list Acc,
%% after that the list One.  By that all elements in Zero are ordered
%% before the ones in One, according to the generated numbers.
%% The order is actually not important as long as it is consistent.
%%
%% The algorithm recurses until the Zero or One list has length
%% 0 or 1, which is when the generated fixpoint number has no duplicate.
%% The fixpoint number in itself only exists in the guise of the
%% recursion call stack, that is whether an element belongs to list
%% Zero or One on each recursion level.
%% Here is the bare algorithm:
%%
%% quickshuffle([], Acc) -> Acc;
%% quickshuffle([X], Acc) -> [X | Acc];
%% quickshuffle([_|_] = L, Acc) ->
%%     {Zero, One} = quickshuffle_split(L, [], []),
%%     quickshuffle(One, quickshuffle(Zero, Acc)).
%%
%% quickshuffle_split([], Zero, One) ->
%%     {Zero, One};
%% quickshuffle_split([X | L], Zero, One) ->
%%     case random_bit() of
%%         0 -> quickshuffle_split(L, [X | Zero], One);
%%         1 -> quickshuffle_split(L, Zero, [X | One])
%%     end.
%%
%% As an optimization, since the algorithm is equivalent to its objective
%% to randomly permute a list, we can when reaching a small enough list
%% as in 3 or 2 instead do an explicit random permutation of the list.
%%
%% The `random_bit()` can be generated with small overhead by generating
%% a random word and cache it, then shift out one bit at the time.
%%
%% Also, it is faster to do a 4-way split by 2 bits instead of,
%% as described above, a 2-way split by 1 bit.

%% Leaf cases - random permutations for 0 .. 3 elements
shuffle_r([], Acc, P, S) ->
    {Acc, P, S};
shuffle_r([X], Acc, P, S) ->
    {[X | Acc], P, S};
shuffle_r([X, Y], Acc, P, S) ->
    shuffle_r_2(X, Acc, P, S, Y);
shuffle_r([X, Y, Z], Acc, P, S) ->
    shuffle_r_3(X, Acc, P, S, Y, Z);
%% General case - split and recursive shuffle
shuffle_r([_, _, _ | _] = List, Acc, P, S) ->
    %% P and S is bitstream cache and state
    shuffle_r(List, Acc, P, S, [], [], [], []).
%%
%% Split L into 4 random subsets
%%
shuffle_r([], Acc0, P0, S0, Zero, One, Two, Three) ->
    %% Split done, recursively shuffle the splitted lists onto Acc
    {Acc1, P1, S1} = shuffle_r(Zero, Acc0, P0, S0),
    {Acc2, P2, S2} = shuffle_r(One, Acc1, P1, S1),
    {Acc3, P3, S3} = shuffle_r(Two, Acc2, P2, S2),
    shuffle_r(Three, Acc3, P3, S3);
shuffle_r([X | L], Acc, P0, S, Zero, One, Two, Three)
  when is_integer(P0), ?BIT(2) =< P0, P0 =< ?MASK(59) ->
    P1 = P0 bsr 2,
    case ?MASK(2, P0) of
        0 -> shuffle_r(L, Acc, P1, S, [X | Zero], One, Two, Three);
        1 -> shuffle_r(L, Acc, P1, S, Zero, [X | One], Two, Three);
        2 -> shuffle_r(L, Acc, P1, S, Zero, One, [X | Two], Three);
        3 -> shuffle_r(L, Acc, P1, S, Zero, One, Two, [X | Three])
    end;
shuffle_r([_ | _] = L, Acc, _P, S0, Zero, One, Two, Three) ->
    [P|S1] = shuffle_new_bits(S0),
    shuffle_r(L, Acc, P, S1, Zero, One, Two, Three).

%% Permute 2 elements
shuffle_r_2(X, Acc, P, S, Y)
  when is_integer(P), ?BIT(1) =< P, P =< ?MASK(59) ->
    {case ?MASK(1, P) of
         0 -> [Y, X | Acc];
         1 -> [X, Y | Acc]
     end, P bsr 1, S};
shuffle_r_2(X, Acc, _P, S0, Y) ->
    [P|S1] = shuffle_new_bits(S0),
    shuffle_r_2(X, Acc, P, S1, Y).

%% Permute 3 elements
%%
%% Uses 3 random bits per iteration with a probability of 1/4
%% to reject and retry, which on average is 3 * 4/3
%% (infinite sum of (1/4)^k) = 4 bits per permutation
shuffle_r_3(X, Acc, P0, S, Y, Z)
  when is_integer(P0), ?BIT(3) =< P0, P0 =< ?MASK(59) ->
    P1 = P0 bsr 3,
    case ?MASK(3, P0) of
        0 -> {[Z, Y, X | Acc], P1, S};
        1 -> {[Y, Z, X | Acc], P1, S};
        2 -> {[Z, X, Y | Acc], P1, S};
        3 -> {[X, Z, Y | Acc], P1, S};
        4 -> {[Y, X, Z | Acc], P1, S};
        5 -> {[X, Y, Z | Acc], P1, S};
        _ -> % Reject and retry
            shuffle_r_3(X, Acc, P1, S, Y, Z)
    end;
shuffle_r_3(X, Acc, _P, S0, Y, Z) ->
    [P|S1] = shuffle_new_bits(S0),
    shuffle_r_3(X, Acc, P, S1, Y, Z).

%%
shuffle_init_bitstream(R, #{bits:=Bits, next:=Next} = AlgHandler) ->
    Mask = ?MASK(Bits),
    Shift = maps:get(weak_low_bits, AlgHandler, 0),
    shuffle_init_bitstream(R, Next, Shift, Mask);
shuffle_init_bitstream(R, #{max:=Mask, next:=Next}) ->
    %% Old spec - assume 2 weak low bits
    Shift = 2,
    shuffle_init_bitstream(R, Next, Shift, Mask).
%%
-dialyzer({no_improper_lists, shuffle_init_bitstream/4}).
shuffle_init_bitstream(R, Next, Shift, Mask0) ->
    Mask1 = Mask0 bsr Shift,    % Adjust mask for weak low bits
    Mask = ?MASK(58, Mask1),    % Limit the mask to avoid bignum
    P = 1,                      % Marker for out of random bits
    W = {Next,Shift,Mask},      % Generator
    S = [R|W],                  % Generator state
    [P|S].                      % Bit cash and state

-dialyzer({no_improper_lists, shuffle_new_bits/1}).
%%
shuffle_new_bits([R0|{Next,Shift,Mask}=W])
  when is_integer(Shift), 0 =< Shift, Shift =< 3,
       is_integer(Mask), 0 =< Mask, Mask =< ?MASK(58) ->
    case Next(R0) of
        {V, R1} when is_integer(V) ->
            %% Setting the top bit here provides the marker
            %% for when we are out of random bits: P =:= 1
            P = ((V bsr Shift) band Mask) bor (Mask + 1),
            S = [R1|W],
            [P|S]
    end.
-endif.
