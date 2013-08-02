%% Module responsible for local invocation of macros and functions.
-module(elixir_def_local).
-export([macro_for/3, function_for/3]).
-include("elixir.hrl").

macro_for(_Tuple, _All, #elixir_scope{module=nil}) -> false;

macro_for(Tuple, All, #elixir_scope{module=Module}) ->
  try elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, Kind, Line, _, _, _, _ }, Clauses } when Kind == defmacro, Clauses /= [];
                                                        All, Kind == defmacrop, Clauses /= [] ->
      fun() -> get_function(Line, Module, Clauses) end;
    _ ->
      false
  catch
    error:badarg -> false
  end.

function_for(Module, Name, Arity) ->
  Tuple = { Name, Arity },
  case elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, _, Line, _, _, _, _ }, Clauses } when Clauses /= [] ->
      %% There is no need to record such calls
      %% since they come from funtions that were
      %% already analyzed at compilation time.
      get_function(Line, Module, Clauses);
    _ ->
      [_|T] = erlang:get_stacktrace(),
      erlang:raise(error, undef, [{Module,Name,Arity,[]}|T])
  end.

get_function(Line, Module, Clauses) ->
  RewrittenClauses = [rewrite_clause(Clause, Module) || Clause <- Clauses],
  Fun = { 'fun', Line, {clauses, RewrittenClauses } },
  { value, Result, _Binding } = erl_eval:exprs([Fun], []),
  Result.

rewrite_clause({ call, Line, { atom, Line, RawName }, Args }, Module) ->
  Remote = { remote, Line,
    { atom, Line, ?MODULE },
    { atom, Line, function_for }
  },

  %% If we have a macro, its arity in the table is
  %% actually one less than in the function call
  { Name, Arity } = case atom_to_list(RawName) of
    "MACRO-" ++ Rest -> { list_to_atom(Rest), length(Args) - 1 };
    _ -> { RawName, length(Args) }
  end,

  FunCall = { call, Line, Remote, [
    { atom, Line, Module }, { atom, Line, Name }, { integer, Line, Arity }
  ] },
  { call, Line, FunCall, Args };

rewrite_clause(Tuple, Module) when is_tuple(Tuple) ->
  list_to_tuple(rewrite_clause(tuple_to_list(Tuple), Module));

rewrite_clause(List, Module) when is_list(List) ->
  [rewrite_clause(Item, Module) || Item <- List];

rewrite_clause(Else, _) -> Else.