%% Module responsible for local invocation of macros and functions.
-module(elixir_def_local).
-export([
  macro_for/3,
  function_for/3,
  format_error/1,
  check_unused_local_macros/3
]).
-include("elixir.hrl").

%% Used by elixir_dispatch, returns false if no macro is found
macro_for(_Tuple, _All, nil) -> false;

macro_for(Tuple, All, Module) ->
  try ets:lookup(elixir_def:table(Module), Tuple) of
    [{Tuple, Kind, Line, _, _, _, _, Clauses}] when Kind == defmacro; All, Kind == defmacrop ->
      get_function(Line, Module, Clauses);
    _ ->
      false
  catch
    error:badarg -> false
  end.

%% Used on runtime by rewritten clauses, raises an error if function is not found
function_for(Module, Name, Arity) ->
  Tuple = { Name, Arity },
  case ets:lookup(elixir_def:table(Module), Tuple) of
    [{Tuple, _, Line, _, _, _, _, Clauses}] ->
      get_function(Line, Module, Clauses);
    _ ->
      [_|T] = erlang:get_stacktrace(),
      erlang:raise(error, undef, [{Module,Name,Arity,[]}|T])
  end.

%% Helpers

get_function(Line, Module, Clauses) ->
  RewrittenClauses = [rewrite_clause(Clause, Module) || Clause <- Clauses],
  Fun = { 'fun', Line, {clauses, lists:reverse(RewrittenClauses)} },
  { value, Result, _Binding } = erl_eval:exprs([Fun], []),
  Result.

%% TODO: Consider caching functions in a table for performance.
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

%% Error handling

check_unused_local_macros(File, Recorded, Defmacrop) ->
  [elixir_errors:handle_file_warning(File,
    { Line, ?MODULE, { unused_macro, Fun } }) || { Fun, Line, Check } <- Defmacrop,
      Check, not lists:member(Fun, Recorded)].

format_error({unused_macro,{Name, Arity}}) ->
  io_lib:format("macro ~s/~B is unused", [Name, Arity]).