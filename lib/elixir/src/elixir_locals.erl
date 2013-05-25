%% Module responsible for local invocation of macros and functions.
-module(elixir_locals).
-export([
  setup/1, cleanup/1,
  record/2, record_root/2,
  macro_for/3, function_for/3,
  check_unused_local/3, format_error/1
]).
-include("elixir.hrl").

-define(attr, '__locals_tracker').
-define(tracker, 'Elixir.Kernel.LocalsTracker').

setup(Module) ->
  case code:ensure_loaded(?tracker) of
    { module, _ } -> ets:insert(Module, { ?attr, ?tracker:start_link() });
    { error, _ }  -> ok
  end.

cleanup(Module) ->
  if_tracker(Module, fun(Pid) -> ?tracker:stop(Pid) end).

record(Tuple, #elixir_scope{function=Function})
    when Function == nil; Function == Tuple -> false;
record(Tuple, #elixir_scope{module=Module, function=Function, function_kind=Kind}) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_definition(Pid, Kind, Function),
    ?tracker:add_dispatch(Pid, Function, Tuple),
    true
  end).

record_root(Module, Tuple) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_dispatch(Pid, root, Tuple),
    true
  end).

if_tracker(Module, Callback) ->
  case ets:lookup(Module, ?attr) of
    [{ ?attr, Pid }] -> Callback(Pid);
    _ -> false
  end.

%% Used by elixir_dispatch, returns false if no macro is found
macro_for(_Tuple, _All, #elixir_scope{module=nil}) -> false;

macro_for(Tuple, All, #elixir_scope{module=Module} = S) ->
  try elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, Kind, Line, _, _, _, _ }, Clauses } when Kind == defmacro; All, Kind == defmacrop ->
      record(Tuple, S),
      get_function(Line, Module, Clauses);
    _ ->
      false
  catch
    error:badarg -> false
  end.

%% Used on runtime by rewritten clauses, raises an error if function is not found
function_for(Module, Name, Arity) ->
  Tuple = { Name, Arity },
  case elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, _, Line, _, _, _, _ }, Clauses } ->
      %% There is no need to record such calls
      %% since they come from funtions that were
      %% already analyzed at compilation time.
      get_function(Line, Module, Clauses);
    _ ->
      [_|T] = erlang:get_stacktrace(),
      erlang:raise(error, undef, [{Module,Name,Arity,[]}|T])
  end.

%% Helpers

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

%% Error handling

check_unused_local(File, Module, Private) ->
  if_tracker(Module, fun(Pid) ->
    Args = [ { Fun, Kind, Defaults } ||
             { Fun, Kind, _Line, true, Defaults } <- Private],

    Unused = ?tracker:collect_unused(Pid, Args),

    [ begin
        { _, _, Line, _, _ } = lists:keyfind(element(2, Error), 1, Private),
        elixir_errors:handle_file_warning(File, { Line, ?MODULE, Error })
      end || Error <- Unused ]
  end).

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
