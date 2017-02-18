%% Handle default clauses for function definitions.
-module(elixir_def_defaults).
-export([expand/2, unpack/3]).
-include("elixir.hrl").

expand(Args, E) ->
  NoContext = E#{context := nil},
  lists:mapfoldl(fun
    ({'\\\\', Meta, [Left, Right]}, Acc) ->
      {ELeft, EL} = elixir_exp:expand(Left, Acc),
      {ERight, _} = elixir_exp:expand(Right, NoContext),
      {{'\\\\', Meta, [ELeft, ERight]}, EL};
    (Left, Acc) ->
      elixir_exp:expand(Left, Acc)
  end, E, Args).

unpack(Kind, Name, Args) ->
  unpack_each(Kind, Name, Args, [], []).

unpack_each(Kind, Name, [{'\\\\', Meta, [Expr, _]} | T] = List, Acc, Clauses) ->
  Base = build_match(Acc, []),
  {Args, Invoke} = extract_defaults(List, length(Base), [], []),
  Clause = {Base ++ Args, {super, Meta, Base ++ Invoke}},
  unpack_each(Kind, Name, T, [Expr | Acc], [Clause | Clauses]);
unpack_each(Kind, Name, [H | T], Acc, Clauses) ->
  unpack_each(Kind, Name, T, [H | Acc], Clauses);
unpack_each(_Kind, _Name, [], Acc, Clauses) ->
  {lists:reverse(Acc), lists:reverse(Clauses)}.

%% Extract default values from args following the current default clause.

extract_defaults([{'\\\\', _, [_Expr, Default]} | T], Counter, NewArgs, NewInvoke) ->
  extract_defaults(T, Counter, NewArgs, [Default | NewInvoke]);
extract_defaults([_ | T], Counter, NewArgs, NewInvoke) ->
  H = {list_to_atom([$x | integer_to_list(Counter)]), [], 'Elixir'},
  extract_defaults(T, Counter + 1, [H | NewArgs], [H | NewInvoke]);
extract_defaults([], _Counter, NewArgs, NewInvoke) ->
  {lists:reverse(NewArgs), lists:reverse(NewInvoke)}.

%% Build matches for all the previous argument until the current default clause.

build_match([], Acc) ->
  Acc;
build_match([_ | T], Acc) ->
  Var = {list_to_atom([$x | integer_to_list(length(T))]), [], 'Elixir'},
  build_match(T, [Var | Acc]).
