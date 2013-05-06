%% Module responsible for handling imports and conflicts
%% in between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/5, recorded_locals/1, format_error/1,
  ensure_no_import_conflict/4, ensure_no_local_conflict/4,
  build_table/1, delete_table/1, record/3]).
-include("elixir.hrl").

table(Module) -> ?atom_concat([i, Module]).

build_table(Module) ->
  ets:new(table(Module), [set, named_table, public]).

delete_table(Module) ->
  ets:delete(table(Module)).

record(_Tuple, _Receiver, nil) ->
  false;

record(Tuple, Receiver, Module) ->
  try
    ets:insert(table(Module), { Tuple, Receiver })
  catch
    error:badarg -> false
  end.

recorded_locals(Module) ->
  Table  = table(Module),
  Match  = { '$1', Module },
  Result = ets:match(Table, Match),
  ets:match_delete(Table, Match),
  lists:append(Result).

%% Update the scope to consider the imports for aliases
%% based on the given options and selector.

import(Meta, Ref, Opts, Selector, S) ->
  IncludeAll = (Selector == all) or (Selector == default),

  SF = case IncludeAll or (Selector == functions) of
    false -> S;
    true  ->
      FunctionsFun = fun(K) -> remove_underscored(K andalso Selector, get_functions(Ref)) end,
      { Functions, TempF } = calculate(Meta, Ref, Opts,
        S#elixir_scope.functions, S#elixir_scope.macro_functions, FunctionsFun, S),
      S#elixir_scope{functions=Functions, macro_functions=TempF}
  end,

  SM = case IncludeAll or (Selector == macros) of
    false -> SF;
    true  ->
      MacrosFun = fun(K) ->
        case IncludeAll of
          true  -> remove_underscored(K andalso Selector, get_optional_macros(Ref));
          false -> get_macros(Meta, Ref, SF)
        end
      end,
      { Macros, TempM } = calculate(Meta, Ref, Opts,
        SF#elixir_scope.macros, SF#elixir_scope.macro_macros, MacrosFun, SF),
      SF#elixir_scope{macros=Macros, macro_macros=TempM}
  end,

  SM.

%% IMPORT FUNCTION RELATED HELPERS

%% Calculates the imports based on only and except

calculate(Meta, Key, Opts, Old, Temp, AvailableFun, S) ->
  File = S#elixir_scope.file,

  New = case keyfind(only, Opts) of
    { only, RawOnly } ->
      Only = expand_fun_arity(Meta, only, RawOnly, S),
      case Only -- get_exports(Key) of
        [{Name,Arity}|_] ->
          Tuple = { invalid_import, { Key, Name, Arity } },
          elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
        _ ->
          intersection(Only, AvailableFun(false))
      end;
    false ->
      case keyfind(except, Opts) of
        false -> AvailableFun(true);
        { except, [] } -> AvailableFun(true);
        { except, RawExcept } ->
          Except = expand_fun_arity(Meta, except, RawExcept, S),
          case keyfind(Key, Old) of
            false -> AvailableFun(true) -- Except;
            {Key,OldImports} -> OldImports -- Except
          end
      end
  end,

  %% Normalize the data before storing it
  Set   = ordsets:from_list(New),
  Final = remove_internals(Set),

  case Final of
    [] -> { keydelete(Key, Old), keydelete(Key, Temp) };
    _  ->
      ensure_no_in_erlang_macro_conflict(Meta, File, Key, Final, internal_conflict),
      { [{ Key, Final }|keydelete(Key, Old)], [{ Key, Final }|keydelete(Key, Temp)] }
  end.

%% Ensure we are expanding macros and stuff

expand_fun_arity(Meta, Kind, Value, S) ->
  { TValue, _S } = elixir_translator:translate_each(Value, S),
  cons_to_keywords(Meta, Kind, TValue, S).

cons_to_keywords(Meta, Kind, { cons, _, Left, { nil, _ } }, S) ->
  [tuple_to_fun_arity(Meta, Kind, Left, S)];

cons_to_keywords(Meta, Kind, { cons, _, Left, Right }, S) ->
  [tuple_to_fun_arity(Meta, Kind, Left, S)|cons_to_keywords(Meta, Kind, Right, S)];

cons_to_keywords(Meta, Kind, _, S) ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file,
    "invalid value for :~ts, expected a list with functions and arities", [Kind]).

tuple_to_fun_arity(_Meta, _Kind, { tuple, _, [{ atom, _, Atom }, { integer, _, Integer }] }, _S) ->
  { Atom, Integer };

tuple_to_fun_arity(Meta, Kind, _, S) ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file,
    "invalid value for :~ts, expected a list with functions and arities", [Kind]).

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

get_macros(Meta, Module, S) ->
  try
    Module:'__info__'(macros)
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, Tuple)
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

ensure_no_local_conflict(Meta, File, Module, AllDefined) ->
  ensure_no_in_erlang_macro_conflict(Meta, File, Module, AllDefined, local_conflict).

%% Find conlicts in the given list of functions with
%% the recorded set of imports.

ensure_no_import_conflict(Meta, File, Module, AllDefined) ->
  Table = table(Module),
  Matches = [X || X <- AllDefined, ets:member(Table, X)],

  case Matches of
    [{Name,Arity}|_] ->
      Key = ets:lookup_element(Table, {Name, Arity }, 2),
      Tuple = { import_conflict, { Key, Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
    [] ->
      ok
  end.

%% Ensure the given functions don't clash with any
%% of Elixir non overridable macros.

ensure_no_in_erlang_macro_conflict(Meta, File, Key, [{Name,Arity}|T], Reason) ->
  Values = lists:filter(fun({X,Y}) ->
    (Name == X) andalso ((Y == '*') orelse (Y == Arity))
  end, non_overridable_macros()),

  case Values /= [] of
    true  ->
      Tuple = { Reason, { Key, Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
    false -> ensure_no_in_erlang_macro_conflict(Meta, File, Key, T, Reason)
  end;

ensure_no_in_erlang_macro_conflict(_Meta, _File, _Key, [], _) -> ok.

%% ERROR HANDLING

format_error({already_imported,{Receiver, Name, Arity}}) ->
  io_lib:format("function ~ts/~B already imported from ~ts", [Name, Arity, elixir_errors:inspect(Receiver)]);

format_error({invalid_import,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it doesn't exist",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({local_conflict,{_, Name, Arity}}) ->
  io_lib:format("cannot define local ~ts/~B because it conflicts with Elixir special forms", [Name, Arity]);

format_error({internal_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it conflicts with Elixir special forms",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({ no_macros, Module }) ->
  io_lib:format("could not load macros from module ~ts", [elixir_errors:inspect(Module)]).

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

remove_underscored(default, List) -> remove_underscored(List);
remove_underscored(_, List)       -> List.

remove_underscored([{ Name, _ } = H|T]) when Name < a  ->
  case atom_to_list(Name) of
    [$_, $_, _, $_, $_] -> [H|remove_underscored(T)];
    "_" ++ _            -> remove_underscored(T);
    _                   -> [H|remove_underscored(T)]
  end;

remove_underscored(T) ->
  T.

remove_internals(Set) ->
  ordsets:del_element({ module_info, 1 },
    ordsets:del_element({ module_info, 0 }, Set)).

%% Macros implemented in Erlang that are not overridable.

non_overridable_macros() ->
  [
    {'^',1},
    {'=',2},
    {'__op__',2},
    {'__op__',3},
    {'__ambiguousop__','*'},
    {'__scope__',2},
    {'__block__','*'},
    {'->','2'},
    {'<<>>','*'},
    {'{}','*'},
    {'[]','*'},
    {'alias',1},
    {'alias',2},
    {'require',1},
    {'require',2},
    {'import',1},
    {'import',2},
    {'import',3},
    {'__ENV__',0},
    {'__CALLER__',0},
    {'__MODULE__',0},
    {'__FILE__',0},
    {'__DIR__',0},
    {'__aliases__','*'},
    {'quote',1},
    {'quote',2},
    {'unquote',1},
    {'unquote_splicing',1},
    {'fn','*'},
    {'super','*'},
    {'super?',0},
    {'bc','*'},
    {'lc','*'},
    {'var!',1},
    {'var!',2},
    {'alias!',1}
  ].
