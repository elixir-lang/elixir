%% Module responsible for local invocation of macros and functions.
-module(elixir_def_local).
-export([
  macro_for/3,
  function_for/3,
  format_error/1,
  check_unused_local/3
]).
-include("elixir.hrl").

%% Used by elixir_dispatch, returns false if no macro is found
macro_for(_Tuple, _All, nil) -> false;

macro_for(Tuple, All, Module) ->
  try elixir_def:lookup_definition(Module, Tuple) of
    { { Tuple, Kind, Line, _, _, _, _ }, Clauses } when Kind == defmacro; All, Kind == defmacrop ->
      elixir_import:record(Tuple, Module, Module),
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
      elixir_import:record(Tuple, Module, Module),
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

check_unused_local(File, Recorded, Private) ->
  [check_unused_local(Fun, Kind, Line, File, Defaults, Recorded) ||
    { Fun, Kind, Line, true, Defaults } <- Private].

check_unused_local(Fun, Kind, Line, File, 0, Recorded) ->
  not(lists:member(Fun, Recorded)) andalso
    elixir_errors:handle_file_warning(File, { Line, ?MODULE, { unused_def, Kind, Fun } });

check_unused_local({ _, Arity } = Fun, Kind, Line, File, Defaults, Recorded) when Defaults > 0 ->
  Min = Arity - Defaults,
  Max = Arity,

  Invoked = [A || { _, A } <- Recorded, A >= Min, A =< Max],

  case Invoked of
    [] ->
      elixir_errors:handle_file_warning(File, { Line, ?MODULE, { unused_def, Kind, Fun } });
    _ ->
      UnusedArgs = lists:min(Invoked) - Min,
      if
        UnusedArgs == 0 ->
          ok;
        UnusedArgs == Defaults ->
          elixir_errors:handle_file_warning(File, { Line, ?MODULE, { unused_args, Fun } });
        true ->
          elixir_errors:handle_file_warning(File, { Line, ?MODULE, { unused_args, Fun, UnusedArgs } })
      end
  end;

check_unused_local(_Fun, _Kind, _Line, _File, _Defaults, _Recorded) ->
  ok.

format_error({unused_def,defp,{Name, Arity}}) ->
  io_lib:format("function ~ts/~B is unused", [Name, Arity]);

format_error({unused_args,{Name, Arity}}) ->
  io_lib:format("default arguments in ~ts/~B are never used", [Name, Arity]);

format_error({unused_args,{Name, Arity},1}) ->
  io_lib:format("the first default argument in ~ts/~B is never used", [Name, Arity]);

format_error({unused_args,{Name, Arity},Count}) ->
  io_lib:format("the first ~B default arguments in ~ts/~B are never used", [Count, Name, Arity]);

format_error({unused_def,defmacrop,{Name, Arity}}) ->
  io_lib:format("macro ~ts/~B is unused", [Name, Arity]).
