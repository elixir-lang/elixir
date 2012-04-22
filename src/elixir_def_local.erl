%% Module responsible for local invocation of macros and functions.
-module(elixir_def_local).
-export([
  % table/1, build_table/1, delete_table/1, format_error/1
  macro_for/2,
  function_for/3
  ]).
-include("elixir.hrl").

macro_for(_Tuple, #elixir_scope{module=[]}) -> false;

macro_for(Tuple, #elixir_scope{module=Module}) ->
  case ets:lookup(elixir_def:table(Module), Tuple) of
    [{Tuple, Line, Kind, _, Clauses}] when Kind == defmacro; Kind == defmacrop ->
      RewrittenClauses = [rewrite_clause(Clause, Module) || Clause <- Clauses],
      Fun = { 'fun', Line, {clauses, lists:reverse(RewrittenClauses)} },
      { value, Result, _Binding } = erl_eval:exprs([Fun], []),
      Result;
    _ -> false
  end.

function_for(Module, Name, Arity) ->
  Tuple = { Name, Arity },
  case ets:lookup(elixir_def:table(Module), Tuple) of
    [{Tuple, Line, _, _, Clauses}] ->
      RewrittenClauses = [rewrite_clause(Clause, Module) || Clause <- Clauses],
      Fun = { 'fun', Line, {clauses, lists:reverse(RewrittenClauses)} },
      { value, Result, _Binding } = erl_eval:exprs([Fun], []),
      Result;
    _ ->
      [_|T] = erlang:get_stacktrace(),
      erlang:raise(error, undef, [{Module,Name,Arity,[]}|T])
  end.

%% Helpers

rewrite_clause({ call, Line, { atom, Line, _ } = Atom, Args }, Module) ->
  Remote = { remote, Line,
    { atom, Line, ?MODULE },
    { atom, Line, function_for }
  },
  Arity   = { integer, Line, length(Args) },
  FunCall = { call, Line, Remote, [{ atom, Line, Module }, Atom, Arity] },
  { call, Line, FunCall, Args };

rewrite_clause(Tuple, Module) when is_tuple(Tuple) ->
  list_to_tuple(rewrite_clause(tuple_to_list(Tuple), Module));

rewrite_clause(List, Module) when is_list(List) ->
  [rewrite_clause(Item, Module) || Item <- List];

rewrite_clause(Else, _) -> Else.