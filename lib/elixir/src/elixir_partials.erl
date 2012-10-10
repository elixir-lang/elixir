-module(elixir_partials).
-export([handle/2, handle/3, is_sequential/1]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

handle(Original, S) ->
  handle(Original, S, default).

handle({ _, Line, Args } = Original, S, Opt) when is_list(Args), S#elixir_scope.context /= assign ->
  case convert(Args, S, Opt) of
    { Call, Def, SC } when Def /= [] ->
      Final = validate(Line, Def, SC),
      Block = setelement(3, Original, Call),
      elixir_translator:translate_fn(Line, [{ Final, Block }], SC);
    _ -> error
  end;

handle(_Original, _S, _Opt) ->
  error.

validate(Line, Def, S) ->
  validate(Line, lists:sort(Def), 1, S).

validate(Line, [{ Pos, Item }|T], Pos, S) ->
  [Item|validate(Line, T, Pos + 1, S)];

validate(Line, [{ Pos, _ }|_], Expected, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "partial variable &~w cannot be defined without &~w", [Pos, Expected]);

validate(_Line, [], _Pos, _S) ->
  [].

%% This function receives arguments and then checks
%% the args for partial application. It returns a tuple
%% with three elements where the first element is the list
%% of call args, the second the list of def args for the
%% function definition and the third one is the new scope.
convert(List, S, Opt) -> convert(List, S, Opt, [], []).

convert([{'|', Line, [_, _] = Args}|T], S, allow_tail, CallAcc, DefAcc) ->
  { NewArgs, NewDef, NewS } = convert(Args, S, allow_tail, [], DefAcc),
  convert(T, NewS, allow_tail, [{ '|', Line, NewArgs}|CallAcc], NewDef);

convert([{'&', Line, [Pos]}|T], S, Opt, CallAcc, DefAcc) ->
  case lists:keyfind(Pos, 1, DefAcc) of
    false ->
      { Var, SC } = elixir_scope:build_ex_var(Line, S),
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