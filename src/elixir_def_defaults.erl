-module(elixir_def_defaults).
-export([unpack/2]).
-include("elixir.hrl").

% Unpack default args from the given clause. Invoked by elixir_translate.
unpack(Name, Clause) ->
  { NewArgs, NewClauses } = unpack_each(Name, element(3, Clause), [], []),
  { setelement(3, Clause, NewArgs), NewClauses }.

%% Helpers

%% Unpack default from given args.
%% Returns the given arguments without their default
%% clauses and a list of clauses for the default calls.
unpack_each(Name, [{call, _, {atom, Line, '//'}, [Expr, _Default]}|T] = List, Acc, Clauses) ->
  { Args, Invoke } = build_default_arg(Acc, Line, [], []),
  Defaults = lists:map(fun extract_default/1, List),
  Clause = { clause, Line, Args, [], [
    { call, Line, {atom, Line, Name}, Invoke ++ Defaults }
  ]},
  unpack_each(Name, T, [Expr|Acc], [Clause|Clauses]);

unpack_each(Name, [H|T], Acc, Clauses) ->
  unpack_each(Name, T, [H|Acc], Clauses);

unpack_each(_Name, [], Acc, Clauses) ->
  { lists:reverse(Acc), lists:reverse(Clauses) }.

% Extract default values
extract_default({call, _, {atom, _,'//'}, [_Expr, Default]}) ->
  Default.

% Build default arguments for the function clause and for invocation.
build_default_arg([], _Line, Args, Invoke) -> { Args, Invoke };

build_default_arg([H|T], Line, Args, Invoke) ->
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", length(T)]) },
  build_default_arg(T, Line, [{match, Line, Var, prune_vars(H)}|Args], [Var|Invoke]).

% Remove any reference to vars from the given form.
prune_vars({var, Line, _}) ->
  { var, Line, '_' };

prune_vars(H) when is_tuple(H) ->
  list_to_tuple(lists:map(fun prune_vars/1, tuple_to_list(H)));

prune_vars(H) when is_list(H) ->
  lists:map(fun prune_vars/1, H);

prune_vars(H) -> H.