% Handle default clauses for function definitions.
-module(elixir_def_defaults).
-export([unpack/3]).
-include("elixir.hrl").

% Unpack default args from the given clause. Invoked by elixir_translate.
unpack(Name, Args, S) ->
  unpack_each(Name, Args, [], [], S).

%% Helpers

%% Unpack default from given args.
%% Returns the given arguments without their default
%% clauses and a list of clauses for the default calls.
unpack_each(Name, [{'//', Line, [Expr, _]}|T] = List, Acc, Clauses, S) ->
  Base = build_match(Acc, Line, []),
  { Args, Invoke } = extract_defaults(List, [], []),

  Call = { { '.', Line, [{'__LOCAL__', Line, nil}, Name] }, Line, Base ++ Invoke },
  { Clause, _ } = elixir_clauses:assigns_block(Line,
    fun elixir_translator:translate/2, Base ++ Args, [Call], [], S),

  unpack_each(Name, T, [Expr|Acc], [Clause|Clauses], S);

unpack_each(Name, [H|T], Acc, Clauses, S) ->
  unpack_each(Name, T, [H|Acc], Clauses, S);

unpack_each(_Name, [], Acc, Clauses, _S) ->
  { lists:reverse(Acc), lists:reverse(Clauses) }.

% Extract default values from args following the current default clause.

extract_defaults([{'//', _, [_Expr, Default]}|T], NewArgs, NewInvoke) ->
  extract_defaults(T, NewArgs, [Default|NewInvoke]);

extract_defaults([H|T], NewArgs, NewInvoke) ->
  extract_defaults(T, [H|NewArgs], [H|NewInvoke]);

extract_defaults([], NewArgs, NewInvoke) ->
  { lists:reverse(NewArgs), lists:reverse(NewInvoke) }.

% Build matches for all the previous argument until the current default clause.

build_match([], _Line, Acc) -> Acc;

build_match([_|T], Line, Acc) ->
  Var = { ?ELIXIR_ATOM_CONCAT(["_EX", length(T)]), Line, nil },
  build_match(T, Line, [Var|Acc]).
