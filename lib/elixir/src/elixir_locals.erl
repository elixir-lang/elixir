%% Module responsible for local invocation of macros and functions.
-module(elixir_locals).
-export([
  setup/1, cleanup/1,
  record_local/2, record_import/3,
  record_warn/4,
  macro_for/3, function_for/3,
  ensure_no_import_conflict/4, ensure_all_imports_used/3,
  warn_unused_local/3, format_error/1
]).
-include("elixir.hrl").

-define(attr, '__locals_tracker').
-define(tracker, 'Elixir.Kernel.LocalsTracker').

%% RECORD

setup(Module) ->
  case code:ensure_loaded(?tracker) of
    { module, _ } -> ets:insert(Module, { ?attr, ?tracker:start_link() });
    { error, _ }  -> ok
  end.

cleanup(Module) ->
  if_tracker(Module, fun(Pid) -> ?tracker:stop(Pid) end).

record_local(Tuple, Module) when is_atom(Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_local(Pid, Tuple),
    true
  end);
record_local(Tuple, #elixir_scope{function=Function})
  when Function == nil; Function == Tuple -> false;
record_local(Tuple, #elixir_scope{module=Module, function=Function, function_kind=Kind}) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_definition(Pid, Kind, Function),
    ?tracker:add_local(Pid, Function, Tuple),
    true
  end).

record_warn(Ref, Warn, Line, Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_warnable(Pid, Ref, Warn, Line),
    true
  end).

record_import(_Tuple, Receiver, Module)
  when Module == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module) ->
  try
    Pid = ets:lookup_element(Module, ?attr, 2),
    ?tracker:add_import(Pid, Receiver, Tuple)
  catch
    error:badarg -> false
  end.

%% RETRIEVAL

macro_for(_Tuple, _All, #elixir_scope{module=nil}) -> false;

macro_for(Tuple, All, #elixir_scope{module=Module} = S) ->
  try elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, Kind, Line, _, _, _, _ }, Clauses } when Kind == defmacro; All, Kind == defmacrop ->
      record_local(Tuple, S),
      get_function(Line, Module, Clauses);
    _ ->
      false
  catch
    error:badarg -> false
  end.

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

%% HELPERS

if_tracker(Module, Callback) ->
  case ets:lookup(Module, ?attr) of
    [{ ?attr, Pid }] -> Callback(Pid);
    _ -> false
  end.

%% ERROR HANDLING

%% Ensure no import conflicts with any of the local definitions.

ensure_no_import_conflict(Meta, File, Module, AllDefined) ->
  if_tracker(Module, fun(Pid) ->
    [do_ensure_no_import_conflict(Meta, File, Pid, X) || X  <- AllDefined]
  end),
  ok.

do_ensure_no_import_conflict(Meta, File, Pid, { Name, Arity } = Tuple) ->
  Matches = ?tracker:imports_with_dispatch(Pid, Tuple),

  case Matches of
    []  -> ok;
    Key ->
      Error = { import_conflict, { hd(Key), Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Error)
  end.

%% Ensure all imports are used.

ensure_all_imports_used(_Line, File, Module) ->
  if_tracker(Module, fun(Pid) ->
    [ begin
        elixir_errors:handle_file_warning(File, { L, ?MODULE, { unused_import, M } })
      end || { M, L } <- ?tracker:collect_unused_imports(Pid)]
  end),
  ok.

%% Warn for unused/unreachable locals and default arguments.

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

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({ unused_import, Module }) ->
  io_lib:format("unused import ~ts", [elixir_errors:inspect(Module)]);

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
