%% Responsible handling Elixir partials with &1, &2 and so on.
-module(elixir_partials).
-export([handle/2, handle/3, is_sequential/1]).
-include("elixir.hrl").

handle(Original, S) ->
  handle(Original, S, default).

handle({ _, Meta, Args } = Original, S, Opt) when is_list(Args), S#elixir_scope.context /= match ->
  case convert(Args, S, Opt) of
    { Call, Def, SC } when Def /= [] ->
      Final = validate(Meta, Def, SC),
      Block = setelement(3, Original, Call),
      elixir_fn:fn(Meta, [{ Final, Meta, Block }], SC);
    _ -> error
  end;

handle(_Original, _S, _Opt) ->
  error.

validate(Meta, Def, S) ->
  validate(Meta, lists:sort(Def), 1, S).

validate(Meta, [{ Pos, Item }|T], Pos, S) ->
  [Item|validate(Meta, T, Pos + 1, S)];

validate(Meta, [{ Pos, _ }|_], Expected, S) ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file, "partial variable &~w cannot be defined without &~w", [Pos, Expected]);

validate(_Meta, [], _Pos, _S) ->
  [].

%% This function receives arguments and then checks
%% the args for partial application. It returns a tuple
%% with three elements where the first element is the list
%% of call args, the second the list of def args for the
%% function definition and the third one is the new scope.
convert(List, S, Opt) -> convert(List, S, Opt, [], []).

convert([{'|', Meta, [_, _] = Args}|T], S, allow_tail, CallAcc, DefAcc) ->
  { NewArgs, NewDef, NewS } = convert(Args, S, allow_tail, [], DefAcc),
  convert(T, NewS, allow_tail, [{ '|', Meta, NewArgs}|CallAcc], NewDef);

convert([{'&', Meta, [Pos]}|T], S, Opt, CallAcc, DefAcc) when is_integer(Pos) ->
  case lists:keyfind(Pos, 1, DefAcc) of
    false ->
      { Var, SC } = elixir_scope:build_ex_var(?line(Meta), S),
      convert(T, SC, Opt, [Var|CallAcc], [{Pos,Var}|DefAcc]);
    {Pos,Var} ->
      convert(T, S, Opt, [Var|CallAcc], DefAcc)
  end;

convert([H|T], S, Opt, CallAcc, DefAcc) ->
  convert(T, S, Opt, [H|CallAcc], DefAcc);

convert([], S, _Opt, CallAcc, DefAcc) ->
  { lists:reverse(CallAcc), lists:reverse(DefAcc), S }.

is_sequential(List) -> is_sequential(List, 1).

is_sequential([{ '&', _, [Int] }|T], Int) ->
  is_sequential(T, Int + 1);

is_sequential([], Int) when Int > 1 -> true;
is_sequential(_, _Int) -> false.