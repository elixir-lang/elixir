%% Module responsible for handling imports and conflicts.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([calculate/6, format_error/1,
  ensure_no_import_conflict/4, ensure_no_local_conflict/4,
  build_table/1, delete_table/1, record/4]).
-include("elixir.hrl").

%% Create tables that are responsible to store
%% import and internal invocations.

import_table(Module)   -> ?ELIXIR_ATOM_CONCAT([i, Module]).

build_table(Module) ->
  ets:new(import_table(Module),   [set, named_table, private]).

delete_table(Module) ->
  ets:delete(import_table(Module)).

record(_Kind, _Tuple, _Receiver, #elixir_scope{module=[]}) ->
  [];

record(import, Tuple, Receiver, #elixir_scope{module=Module}) ->
  ets:insert(import_table(Module), { Tuple, Receiver }).

%% Update the old entry according to the optins given
%% and the values returned by fun.

calculate(_Line, Key, _Opts, Old, [], _S) ->
  keydelete(Key, Old);

calculate(Line, Key, Opts, Old, Available, S) ->
  Filename = S#elixir_scope.filename,
  All = keydelete(Key, Old),

  New = case orddict:find(only, Opts) of
    { ok, Only } ->
      case Only -- Key:module_info(exports) of
        [{Name,Arity}|_] ->
          Tuple = { invalid_import, { Key, Name, Arity } },
          elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
        _ -> intersection(Only, Available)
      end;
    error ->
      case orddict:find(except, Opts) of
        { ok, Except } -> difference(Except, Available);
        error -> Available
      end
  end,

  Final = New -- internal_funs(),

  case Final of
    [] -> All;
    _  ->
      ensure_no_conflicts(Line, Filename, Final, keydelete(Key, S#elixir_scope.macros)),
      ensure_no_conflicts(Line, Filename, Final, keydelete(Key, S#elixir_scope.functions)),
      ensure_no_in_erlang_macro_conflict(Line, Filename, Key, Final, internal_conflict),
      [{ Key, Final }|All]
  end.

%% Check if any of the locals defined conflicts with an invoked
%% Elixir "implemented in Erlang" macro. Checking if a local
%% conflicts with an import is automatically done by Erlang.

ensure_no_local_conflict(Line, Filename, Module, AllDefined) ->
  ensure_no_in_erlang_macro_conflict(Line, Filename, Module, AllDefined, local_conflict).

%% Find conlicts in the given list of functions with
%% the recorded set of imports.

ensure_no_import_conflict(Line, Filename, Module, AllDefined) ->
  Table = import_table(Module),
  Matches = [X || X <- AllDefined, ets:member(Table, X)],

  case Matches of
    [{Name,Arity}|_] ->
      Key = ets:lookup_element(Table, {Name, Arity }, 2),
      Tuple = { import_conflict, { Key, Name, Arity } },
      elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
    [] ->
      ok
  end.

%% Conflict helpers

%% Ensure the given functions don't clash with any
%% of Elixir non overridable macros.

ensure_no_in_erlang_macro_conflict(Line, Filename, Key, [{Name,Arity}|T], Reason) ->
  Values = lists:filter(fun({X,Y}) ->
    (Name == X) andalso ((Y == '*') orelse (Y == Arity))
  end, non_overridable_macros()),

  case Values /= [] of
    true  ->
      Tuple = { Reason, { Key, Name, Arity } },
      elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
    false -> ensure_no_in_erlang_macro_conflict(Line, Filename, Key, T, Reason)
  end;

ensure_no_in_erlang_macro_conflict(_Line, _Filename, _Key, [], _) -> ok.

%% Find conlicts in the given list of functions with the set of imports.
%% Used internally to ensure a newly imported fun or macro does not
%% conflict with an already imported set.

ensure_no_conflicts(Line, Filename, Functions, [{Key,Value}|T]) ->
  Filtered = lists:filter(fun(X) -> lists:member(X, Functions) end, Value),
  case Filtered of
    [{Name,Arity}|_] ->
      Tuple = { already_imported, { Key, Name, Arity } },
      elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
    [] ->
      ensure_no_conflicts(Line, Filename, Functions, T)
  end;

ensure_no_conflicts(_Line, _Filename, _Functions, _S) -> ok.

%% Error handling

format_error({already_imported,{Receiver, Name, Arity}}) ->
  io_lib:format("function ~s/~B already imported from ~s", [Name, Arity, Receiver]);

format_error({invalid_import,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~s.~s/~B because it doesn't exist", [Receiver, Name, Arity]);

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("imported ~s.~s/~B conflicts with local function", [Receiver, Name, Arity]);

format_error({local_conflict,{_, Name, Arity}}) ->
  io_lib:format("cannot define local ~s/~B because it conflicts with Elixir internal macros", [Name, Arity]);

format_error({internal_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~s.~s/~B because it conflicts with Elixir internal macros", [Receiver, Name, Arity]).

%% Deletes all the entries in the list with the given key.

keydelete(Key, List) ->
  lists:keydelete(Key, 1, List).

difference(Except, All) ->
  All -- Except.

intersection([H|T], All) ->
  case lists:member(H, All) of
    true  -> [H|intersection(T, All)];
    false -> intersection(T, All)
  end;

intersection([], _All) -> [].

%% Internal funs that are never imported etc.

internal_funs() ->
  [
    { module_info, 0 },
    { module_info, 1 },
    { '__info__', 1 },
    { '__using__', 1 }
  ].

%% Macros implemented in Erlang that are not overridable.

non_overridable_macros() ->
  [
    {'^',1},
    {'=',2},
    {'__OP__',2},
    {'__OP__',3},
    {'__BLOCK__','*'},
    {'__KVBLOCK__','*'},
    {'<<>>','*'},
    {'{}','*'},
    {'[]','*'},
    {'require',1},
    {'require',2},
    {'import',1},
    {'import',2},
    {'import',3},
    {'__MODULE__',0},
    {'__FILE__',0},
    {'__LINE__',0},
    {'__REF__',1},
    {'quote',1},
    {'quote',2},
    {'unquote',1},
    {'unquote_splicing',1},
    {'fn','*'},
    {'loop','*'},
    {'recur','*'},
    {'for','*'},
    {'bc','*'},
    {'lc','*'}
  ].