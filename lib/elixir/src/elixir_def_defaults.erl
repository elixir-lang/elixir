% Handle default clauses for function definitions.
-module(elixir_def_defaults).
-export([expand/2, unpack/4]).
-include("elixir.hrl").

expand(Args, E) ->
  lists:mapfoldl(fun
    ({ '\\\\', Meta, [Left, Right] }, Acc) ->
      { ELeft, EL } = elixir_exp:expand(Left, Acc),
      { ERight, _ } = elixir_exp:expand(Right, Acc#elixir_env{context=nil}),
      { { '\\\\', Meta, [ELeft, ERight] }, EL };
    (Left, Acc) ->
      elixir_exp:expand(Left, Acc)
  end, E, Args).

unpack(Kind, Name, Args, S) ->
  unpack_each(Kind, Name, Args, [], [], S).

%% Helpers

%% Unpack default from given args.
%% Returns the given arguments without their default
%% clauses and a list of clauses for the default calls.
unpack_each(Kind, Name, [{'\\\\', Line, [Expr, _]}|T] = List, Acc, Clauses, S) ->
  Base = build_match(Acc, Line, []),
  { Args, Invoke } = extract_defaults(List, Line, length(Base), [], []),

  { DefArgs, SA }  = elixir_clauses:match(fun elixir_translator:translate_args/2, Base ++ Args, S),
  { DefInvoke, _ } = elixir_translator:translate_args(Base ++ Invoke, SA),

  Call = { call, Line,
    { atom, Line, name_for_kind(Kind, Name) },
    DefInvoke
  },

  Clause = { clause, Line, DefArgs, [], [Call] },
  unpack_each(Kind, Name, T, [Expr|Acc], [Clause|Clauses], S);

unpack_each(Kind, Name, [H|T], Acc, Clauses, S) ->
  unpack_each(Kind, Name, T, [H|Acc], Clauses, S);

unpack_each(_Kind, _Name, [], Acc, Clauses, _S) ->
  { lists:reverse(Acc), lists:reverse(Clauses) }.

% Extract default values from args following the current default clause.

extract_defaults([{'\\\\', _, [_Expr, Default]}|T], Line, Counter, NewArgs, NewInvoke) ->
  extract_defaults(T, Line, Counter, NewArgs, [Default|NewInvoke]);

extract_defaults([_|T], Line, Counter, NewArgs, NewInvoke) ->
  H = { ?atom_concat(["_@D", Counter]), Line, nil },
  extract_defaults(T, Line, Counter + 1, [H|NewArgs], [H|NewInvoke]);

extract_defaults([], _Line, _Counter, NewArgs, NewInvoke) ->
  { lists:reverse(NewArgs), lists:reverse(NewInvoke) }.

% Build matches for all the previous argument until the current default clause.

build_match([], _Line, Acc) -> Acc;

build_match([_|T], Line, Acc) ->
  Var = { ?atom_concat(["_@D", length(T)]), Line, nil },
  build_match(T, Line, [Var|Acc]).

% Given the invoked function name based on the kind

name_for_kind(Kind, Name) when Kind == defmacro; Kind == defmacrop -> ?elixir_macro(Name);
name_for_kind(_Kind, Name) -> Name.