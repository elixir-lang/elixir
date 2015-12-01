% Handle default clauses for function definitions.
-module(elixir_def_defaults).
-export([expand/2, unpack/4]).
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

unpack(Kind, Name, Args, S) ->
  unpack_each(Kind, Name, Args, [], [], S).

%% Helpers

%% Unpack default from given args.
%% Returns the given arguments without their default
%% clauses and a list of clauses for the default calls.
unpack_each(Kind, Name, [{'\\\\', DefMeta, [Expr, _]}|T] = List, Acc, Clauses, S) ->
  Base = wrap_kind(Kind, build_match(Acc, [])),
  {Args, Invoke} = extract_defaults(List, length(Base), [], []),

  {DefArgs, SA}  = elixir_clauses:match(fun elixir_translator:translate_args/2, Base ++ Args, S),
  {DefInvoke, _} = elixir_translator:translate_args(Base ++ Invoke, SA),

  Ann = ?ann(DefMeta),

  Call = {call, Ann,
    {atom, Ann, name_for_kind(Kind, Name)},
    DefInvoke
  },

  Clause = {clause, Ann, DefArgs, [], [Call]},
  unpack_each(Kind, Name, T, [Expr|Acc], [Clause|Clauses], S);

unpack_each(Kind, Name, [H|T], Acc, Clauses, S) ->
  unpack_each(Kind, Name, T, [H|Acc], Clauses, S);

unpack_each(_Kind, _Name, [], Acc, Clauses, _S) ->
  {lists:reverse(Acc), lists:reverse(Clauses)}.

% Extract default values from args following the current default clause.

extract_defaults([{'\\\\', _, [_Expr, Default]}|T], Counter, NewArgs, NewInvoke) ->
  extract_defaults(T, Counter, NewArgs, [Default|NewInvoke]);

extract_defaults([_|T], Counter, NewArgs, NewInvoke) ->
  H = {elixir_utils:atom_concat(["x", Counter]), [], nil},
  extract_defaults(T, Counter + 1, [H|NewArgs], [H|NewInvoke]);

extract_defaults([], _Counter, NewArgs, NewInvoke) ->
  {lists:reverse(NewArgs), lists:reverse(NewInvoke)}.

% Build matches for all the previous argument until the current default clause.

build_match([], Acc) -> Acc;

build_match([_|T], Acc) ->
  Var = {elixir_utils:atom_concat(["x", length(T)]), [], nil},
  build_match(T, [Var|Acc]).

% Given the invoked function name based on the kind

wrap_kind(Kind, Args) when Kind == defmacro; Kind == defmacrop -> [{c, [], nil}|Args];
wrap_kind(_Kind, Args) -> Args.

name_for_kind(Kind, Name) when Kind == defmacro; Kind == defmacrop -> elixir_utils:macro_name(Name);
name_for_kind(_Kind, Name) -> Name.