%% Module responsible for local invocation of macros and functions.
-module(elixir_def_local).
-export([
  build_table/1,
  delete_table/1,
  record/4,
  macro_for/3,
  function_for/3,
  format_error/1,
  check_unused_local_macros/3
]).
-include("elixir.hrl").

%% Table

table(Module) -> ?ELIXIR_ATOM_CONCAT([l, Module]).

build_table(Module) ->
  ets:new(table(Module), [duplicate_bag, named_table, private]).

delete_table(Module) ->
  ets:delete(table(Module)).

record(_Line, _Tuple, _IsMacro, []) -> [];

record(Line, Tuple, IsMacro, Module) ->
  ets:insert(table(Module), { Tuple, Line, IsMacro }).

%% Reading

macro_for(_Tuple, _All, #elixir_scope{module=[]}) -> false;

macro_for(Tuple, All, #elixir_scope{module=Module}) ->
  case ets:lookup(elixir_def:table(Module), Tuple) of
    [{Tuple, Line, Kind, _, Clauses}] when Kind == defmacro; All, Kind == defmacrop ->
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
      % elixir_def_local:record(Line, Tuple, false, Module),
      RewrittenClauses = [rewrite_clause(Clause, Module) || Clause <- Clauses],
      Fun = { 'fun', Line, {clauses, lists:reverse(RewrittenClauses)} },
      { value, Result, _Binding } = erl_eval:exprs([Fun], []),
      Result;
    _ ->
      [_|T] = erlang:get_stacktrace(),
      erlang:raise(error, undef, [{Module,Name,Arity,[]}|T])
  end.

%% Helpers
%% TODO: Consider caching functions in a table for performance.

rewrite_clause({ call, Line, { atom, Line, RawName }, Args }, Module) ->
  Remote = { remote, Line,
    { atom, Line, ?MODULE },
    { atom, Line, function_for }
  },
  Arity   = { integer, Line, length(Args) },
  Name    = { atom, Line, rewrite_name(atom_to_list(RawName), RawName) },
  FunCall = { call, Line, Remote, [{ atom, Line, Module }, Name, Arity] },
  { call, Line, FunCall, Args };

rewrite_clause(Tuple, Module) when is_tuple(Tuple) ->
  list_to_tuple(rewrite_clause(tuple_to_list(Tuple), Module));

rewrite_clause(List, Module) when is_list(List) ->
  [rewrite_clause(Item, Module) || Item <- List];

rewrite_clause(Else, _) -> Else.

rewrite_name("MACRO_" ++ Rest, _) -> list_to_atom(Rest);
rewrite_name(_, Name) -> Name.

%% Error handling

check_unused_local_macros(Filename, Module, PMacros) ->
  Table = table(Module),
  [elixir_errors:handle_file_warning(Filename,
    { Line, ?MODULE, { unused_macro, Fun } }) || { Fun, Line } <- PMacros, not ets:member(Table, Fun)].

format_error({unused_macro,{Name, Arity}}) ->
  io_lib:format("macro ~s/~B is unused", [Name, Arity]);

format_error({runtime_macro,{Name, Arity}}) ->
  io_lib:format("macro ~s/~B is being invoked before it is defined", [Name, Arity]).