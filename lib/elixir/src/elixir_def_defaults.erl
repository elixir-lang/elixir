%% Handle default clauses for function definitions.
-module(elixir_def_defaults).
-export([unpack/5]).
-include("elixir.hrl").

unpack(Kind, Meta, Name, Args, E) ->
  Expanded = expand_defaults(Args, E#{context := nil}),
  unpack(Kind, Meta, Name, Expanded, [], []).

unpack(Kind, Meta, Name, [{'\\\\', DefaultMeta, [Expr, _]} | T] = List, Acc, Clauses) ->
  Base = match_defaults(Acc, length(Acc), []),
  {Args, Invoke} = extract_defaults(List, length(Base), [], []),
  Clause = {Meta, Base ++ Args, [], {super, DefaultMeta, Base ++ Invoke}},
  unpack(Kind, Meta, Name, T, [Expr | Acc], [Clause | Clauses]);
unpack(Kind, Meta, Name, [H | T], Acc, Clauses) ->
  unpack(Kind, Meta, Name, T, [H | Acc], Clauses);
unpack(_Kind, _Meta, _Name, [], Acc, Clauses) ->
  {lists:reverse(Acc), lists:reverse(Clauses)}.

%% Expand the right side of default arguments

expand_defaults([{'\\\\', Meta, [Expr, Default]} | Args], E) ->
  {ExpandedDefault, _} = elixir_exp:expand(Default, E),
  [{'\\\\', Meta, [Expr, ExpandedDefault]} | expand_defaults(Args, E)];
expand_defaults([Arg | Args], E) ->
  [Arg | expand_defaults(Args, E)];
expand_defaults([], _E) ->
  [].

%% Extract default values from args following the current default clause

extract_defaults([{'\\\\', _, [_Expr, Default]} | T], Counter, NewArgs, NewInvoke) ->
  extract_defaults(T, Counter, NewArgs, [Default | NewInvoke]);
extract_defaults([_ | T], Counter, NewArgs, NewInvoke) ->
  H = default_var(Counter),
  extract_defaults(T, Counter + 1, [H | NewArgs], [H | NewInvoke]);
extract_defaults([], _Counter, NewArgs, NewInvoke) ->
  {lists:reverse(NewArgs), lists:reverse(NewInvoke)}.

%% Build matches for all the previous argument until the current default clause

match_defaults([], 0, Acc) ->
  Acc;
match_defaults([_ | T], Counter, Acc) ->
  NewCounter = Counter - 1,
  match_defaults(T, NewCounter, [default_var(NewCounter) | Acc]).

%% Helpers

default_var(Counter) ->
  {list_to_atom([$x | integer_to_list(Counter)]), [{generated, true}], 'Elixir'}.
