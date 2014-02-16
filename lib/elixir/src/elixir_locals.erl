%% Module responsible for tracking invocations of module calls.
-module(elixir_locals).
-export([
  setup/1, cleanup/1, cache_env/1, get_cached_env/1,
  record_local/2, record_local/3, record_import/4,
  record_definition/3, record_defaults/4,
  ensure_no_function_conflict/4, warn_unused_local/3, format_error/1
]).
-export([macro_for/3, local_for/3, local_for/4]).

-include("elixir.hrl").
-define(attr, '__locals_tracker').
-define(tracker, 'Elixir.Module.LocalsTracker').

macro_for(Module, Name, Arity) ->
  Tuple = { Name, Arity },
  try elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, Kind, Line, _, _, _, _ }, [_|_] = Clauses }
        when Kind == defmacro; Kind == defmacrop ->
      fun() -> get_function(Line, Module, Clauses) end;
    _ ->
      false
  catch
    error:badarg -> false
  end.

local_for(Module, Name, Arity) ->
  local_for(Module, Name, Arity, nil).
local_for(Module, Name, Arity, Given) ->
  Tuple = { Name, Arity },
  case elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, Kind, Line, _, _, _, _ }, [_|_] = Clauses }
        when Given == nil; Kind == Given ->
      get_function(Line, Module, Clauses);
    _ ->
      [_|T] = erlang:get_stacktrace(),
      erlang:raise(error, undef, [{Module,Name,Arity,[]}|T])
  end.

get_function(Line, Module, Clauses) ->
  RewrittenClauses = [rewrite_clause(Clause, Module) || Clause <- Clauses],
  Fun = { 'fun', Line, { clauses, RewrittenClauses } },
  { value, Result, _Binding } = erl_eval:exprs([Fun], []),
  Result.

rewrite_clause({ call, Line, { atom, Line, RawName }, Args }, Module) ->
  Remote = { remote, Line,
    { atom, Line, ?MODULE },
    { atom, Line, local_for }
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

%% TRACKING

setup(Module) ->
  case code:is_loaded(?tracker) of
    { file, _ } -> ets:insert(Module, { ?attr, ?tracker:start_link() });
    false -> ok
  end.

cleanup(Module) ->
  if_tracker(Module, fun(Pid) -> unlink(Pid), ?tracker:stop(Pid) end).

record_local(Tuple, Module) when is_atom(Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_local(Pid, Tuple),
    true
  end).
record_local(Tuple, _Module, Function)
  when Function == nil; Function == Tuple -> false;
record_local(Tuple, Module, Function) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_local(Pid, Function, Tuple),
    true
  end).

record_import(_Tuple, Receiver, Module, _Function)
  when Module == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module, Function) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_import(Pid, Function, Receiver, Tuple),
    true
  end).

record_definition(Tuple, Kind, Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_definition(Pid, Kind, Tuple),
    true
  end).

record_defaults(_Tuple, _Kind, _Module, 0) ->
  true;
record_defaults(Tuple, Kind, Module, Defaults) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_defaults(Pid, Kind, Tuple, Defaults),
    true
  end).

if_tracker(Module, Callback) ->
  try ets:lookup_element(Module, ?attr, 2) of
    Pid -> Callback(Pid)
  catch
    error:badarg -> false
  end.

%% CACHING

cache_env(#elixir_env{module=Module} = RE) ->
  E = RE#elixir_env{line=nil,vars=[]},
  try ets:lookup_element(Module, ?attr, 2) of
    Pid ->
      { Pid, ?tracker:cache_env(Pid, E) }
  catch
    error:badarg ->
      { Escaped, _ } = elixir_quote:escape(E, false),
      Escaped
  end;
cache_env(ExEnv) ->
  cache_env(elixir_env:ex_to_env(ExEnv)).

get_cached_env({Pid,Ref}) -> ?tracker:get_cached_env(Pid, Ref);
get_cached_env(Env) -> Env.

%% ERROR HANDLING

ensure_no_function_conflict(Meta, File, Module, AllDefined) ->
  if_tracker(Module, fun(Pid) ->
    [ begin
        elixir_errors:form_error(Meta, File, ?MODULE, { function_conflict, Error })
      end || Error <- ?tracker:collect_imports_conflicts(Pid, AllDefined) ]
  end),
  ok.

warn_unused_local(File, Module, Private) ->
  if_tracker(Module, fun(Pid) ->
    Args = [ { Fun, Kind, Defaults } ||
             { Fun, Kind, _Line, true, Defaults } <- Private],

    Unused = ?tracker:collect_unused_locals(Pid, Args),

    [ begin
        { _, _, Line, _, _ } = lists:keyfind(element(2, Error), 1, Private),
        elixir_errors:handle_file_warning(File, { Line, ?MODULE, Error })
      end || Error <- Unused ]
  end).

format_error({function_conflict,{Receivers, Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_aliases:inspect(hd(Receivers)), Name, Arity]);

format_error({unused_args,{Name, Arity}}) ->
  io_lib:format("default arguments in ~ts/~B are never used", [Name, Arity]);

format_error({unused_args,{Name, Arity},1}) ->
  io_lib:format("the first default argument in ~ts/~B is never used", [Name, Arity]);

format_error({unused_args,{Name, Arity},Count}) ->
  io_lib:format("the first ~B default arguments in ~ts/~B are never used", [Count, Name, Arity]);

format_error({unused_def,{Name, Arity},defp}) ->
  io_lib:format("function ~ts/~B is unused", [Name, Arity]);

format_error({unused_def,{Name, Arity},defmacrop}) ->
  io_lib:format("macro ~ts/~B is unused", [Name, Arity]).
