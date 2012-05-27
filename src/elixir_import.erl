%% Module responsible for handling imports and conflicts.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/5, recorded_locals/1, format_error/1,
  ensure_no_import_conflict/4, ensure_no_local_conflict/4,
  build_table/1, delete_table/1, record/4]).
-include("elixir.hrl").

table(Module) -> ?ELIXIR_ATOM_CONCAT([i, Module]).

build_table(Module) ->
  ets:new(table(Module), [set, named_table, public]).

delete_table(Module) ->
  ets:delete(table(Module)).

record(_Kind, _Tuple, _Receiver, #elixir_scope{module=nil}) ->
  [];

record(import, Tuple, Receiver, #elixir_scope{module=Module}) ->
  ets:insert(table(Module), { Tuple, Receiver }).

recorded_locals(Module) ->
  Table  = table(Module),
  Match  = { '$1', Module },
  Result = ets:match(Table, Match),
  ets:match_delete(Table, Match),
  lists:append(Result).

%% Update the scope to consider the imports for reference
%% based on the given options and selector.

import(Line, Ref, Opts, Selector, S) ->
  SF = case (Selector == all) or (Selector == functions) of
    false -> S;
    true  ->
      FunctionsFun = fun() -> get_functions(Ref) end,
      Functions = calculate(Line, Ref, Opts,
        S#elixir_scope.functions, FunctionsFun, S),
      S#elixir_scope{functions=Functions}
  end,

  SM = case (Selector == all) or (Selector == macros) of
    false -> SF;
    true  ->
      MacrosFun = fun() ->
        case Selector of
          all -> get_optional_macros(Ref);
          _ -> get_macros(Line, Ref, SF)
        end
      end,
      Macros = calculate(Line, Ref, Opts,
        SF#elixir_scope.macros, MacrosFun, SF),
      SF#elixir_scope{macros=Macros}
  end,

  SM.

%% IMPORT FUNCTION RELATED HELPERS

%% Calculates the imports based on only and except

calculate(Line, Key, Opts, Old, AvailableFun, S) ->
  Filename = S#elixir_scope.filename,
  All = keydelete(Key, Old),

  New = case orddict:find(only, Opts) of
    { ok, Only } ->
      case Only -- get_exports(Key) of
        [{Name,Arity}|_] ->
          Tuple = { invalid_import, { Key, Name, Arity } },
          elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
        _ -> intersection(Only, AvailableFun())
      end;
    error ->
      case orddict:find(except, Opts) of
        { ok, Except } ->
          case keyfind(Key, Old) of
            false -> AvailableFun() -- Except;
            {Key,ToRemove} -> ToRemove -- Except
          end;
        error -> AvailableFun()
      end
  end,

  %% Normalize the data before storing it
  Final = ordsets:from_list(New -- internal_funs()),

  case Final of
    [] -> All;
    _  ->
      ensure_no_conflicts(Line, Filename, Final, keydelete(Key, S#elixir_scope.macros)),
      ensure_no_conflicts(Line, Filename, Final, keydelete(Key, S#elixir_scope.functions)),
      ensure_no_in_erlang_macro_conflict(Line, Filename, Key, Final, internal_conflict),
      [{ Key, Final }|All]
  end.

%% Retrieve functions and macros from modules

get_exports(Module) ->
  try
    Module:'__info__'(functions) ++ Module:'__info__'(macros)
  catch
    error:undef -> Module:module_info(exports)
  end.

get_functions(Module) ->
  try
    Module:'__info__'(functions)
  catch
    error:undef -> Module:module_info(exports)
  end.

get_macros(Line, Module, S) ->
  try
    Module:'__info__'(macros)
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

get_optional_macros(Module)  ->
  case code:ensure_loaded(Module) of
    { module, Module } ->
      try
        Module:'__info__'(macros)
      catch
        error:undef -> []
      end;
    { error, _ } -> []
  end.

%% VALIDATION HELPERS

%% Check if any of the locals defined conflicts with an invoked
%% Elixir "implemented in Erlang" macro. Checking if a local
%% conflicts with an import is automatically done by Erlang.

ensure_no_local_conflict(Line, Filename, Module, AllDefined) ->
  ensure_no_in_erlang_macro_conflict(Line, Filename, Module, AllDefined, local_conflict).

%% Find conlicts in the given list of functions with
%% the recorded set of imports.

ensure_no_import_conflict(Line, Filename, Module, AllDefined) ->
  Table = table(Module),
  Matches = [X || X <- AllDefined, ets:member(Table, X)],

  case Matches of
    [{Name,Arity}|_] ->
      Key = ets:lookup_element(Table, {Name, Arity }, 2),
      Tuple = { import_conflict, { Key, Name, Arity } },
      elixir_errors:form_error(Line, Filename, ?MODULE, Tuple);
    [] ->
      ok
  end.

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

%% ERROR HANDLING

format_error({already_imported,{Receiver, Name, Arity}}) ->
  io_lib:format("function ~s/~B already imported from ~s", [Name, Arity, elixir_errors:inspect(Receiver)]);

format_error({invalid_import,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~s.~s/~B because it doesn't exist",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("imported ~s.~s/~B conflicts with local function",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({local_conflict,{_, Name, Arity}}) ->
  io_lib:format("cannot define local ~s/~B because it conflicts with Elixir internal macros", [Name, Arity]);

format_error({internal_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~s.~s/~B because it conflicts with Elixir internal macros",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({ no_macros, Module }) ->
  io_lib:format("could not load macros from module ~s", [elixir_errors:inspect(Module)]).

%% LIST HELPERS

keyfind(Key, List) ->
  lists:keyfind(Key, 1, List).

keydelete(Key, List) ->
  lists:keydelete(Key, 1, List).

intersection([H|T], All) ->
  case lists:member(H, All) of
    true  -> [H|intersection(T, All)];
    false -> intersection(T, All)
  end;

intersection([], _All) -> [].

%% INTROSPECTION

%% Internal funs that are never imported etc.

internal_funs() ->
  [
    { module_info, 0 },
    { module_info, 1 },
    { '__info__', 1 },
    { '__using__', 2 }
  ].

%% Macros implemented in Erlang that are not overridable.

non_overridable_macros() ->
  [
    {'^',1},
    {'=',2},
    {'__op__',2},
    {'__op__',3},
    {'__block__','*'},
    {'->','2'},
    {'<<>>','*'},
    {'{}','*'},
    {'[]','*'},
    {'require',1},
    {'require',2},
    {'import',1},
    {'import',2},
    {'import',3},
    {'__MAIN__',0},
    {'__ENV__',0},
    {'__CALLER__',0},
    {'__MODULE__',0},
    {'__FILE__',0},
    {'__ref__',1},
    {'quote',1},
    {'quote',2},
    {'unquote',1},
    {'unquote_splicing',1},
    {'fn','*'},
    {'loop','*'},
    {'recur','*'},
    {'super','*'},
    {'bc','*'},
    {'lc','*'}
  ].