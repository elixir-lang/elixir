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
  { Args, Invoke } = build_match(Acc, Line, [], []),
  { NewArgs, NewInvoke } = extract_defaults(List, [], []),
  Clause = { clause, Line, Args ++ NewArgs, [], [
    { call, Line, {atom, Line, Name}, Invoke ++ NewInvoke }
  ]},
  unpack_each(Name, T, [Expr|Acc], [Clause|Clauses]);

unpack_each(Name, [H|T], Acc, Clauses) ->
  unpack_each(Name, T, [H|Acc], Clauses);

unpack_each(_Name, [], Acc, Clauses) ->
  { lists:reverse(Acc), lists:reverse(Clauses) }.

% Extract default values from args following the current default clause.

extract_defaults([{call, _, {atom, _,'//'}, [_Expr, Default]}|T], NewArgs, NewInvoke) ->
  extract_defaults(T, NewArgs, [Default|NewInvoke]);

extract_defaults([H|T], NewArgs, NewInvoke) ->
  extract_defaults(T, [H|NewArgs], [H|NewInvoke]);

extract_defaults([], NewArgs, NewInvoke) ->
  { lists:reverse(NewArgs), lists:reverse(NewInvoke) }.

% Build matches for all the previous argument until the current default clause.

build_match([], _Line, Args, Invoke) -> { Args, Invoke };

build_match([H|T], Line, Args, Invoke) ->
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", length(T)]) },
  build_match(T, Line, [{match, Line, Var, prune_vars(H)}|Args], [Var|Invoke]).

% Remove any reference to vars from the given form.

prune_vars({var, Line, _}) ->
  { var, Line, '_' };

prune_vars(H) when is_tuple(H) ->
  list_to_tuple(lists:map(fun prune_vars/1, tuple_to_list(H)));

prune_vars(H) when is_list(H) ->
  lists:map(fun prune_vars/1, H);

prune_vars(H) -> H.