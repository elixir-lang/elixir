%% Module responsible for handling imports and conflicts
%% in between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/5, format_error/1,
  ensure_no_import_conflict/4, ensure_no_local_conflict/4,
  ensure_all_imports_used/3,
  build_table/1, delete_table/1, record/3]).
-include("elixir.hrl").

%% This table keeps:
%%
%% * Invoked imports and requires in the format
%%   { { Name, Arity }, Module }
%
%% * The current warn status imports and requires
%%   in the format { Module, Line :: integer }
%%
table(Module) -> ?atom_concat([i, Module]).

build_table(Module) ->
  ets:new(table(Module), [bag, named_table, public]).

delete_table(Module) ->
  ets:delete(table(Module)).

record(_Tuple, _Receiver, nil) -> false;
record(Tuple, Receiver, Module) ->
  try
    ets:insert(table(Module), { Tuple, Receiver })
  catch
    error:badarg -> false
  end.

record_warn(_Meta, _Ref, _Opts, #elixir_scope{module=nil}) -> false;
record_warn(Meta, Ref, Opts, S) ->
  Table = table(S#elixir_scope.module),
  ets:delete(Table, Ref),

  Warn =
    case keyfind(warn, Opts) of
      { warn, false } -> false;
      { warn, true } -> true;
      false -> not lists:keymember(context, 1, Meta)
    end,

  Warn andalso ets:insert(Table, { Ref, ?line(Meta) }).

%% IMPORT HELPERS

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

  record_warn(Meta, Ref, Opts, S),
  SM.

%% Calculates the imports based on only and except

calculate(Meta, Key, Opts, Old, Temp, AvailableFun, S) ->
  File = S#elixir_scope.file,

  New = case keyfind(only, Opts) of
    { only, Only } ->
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
        { except, Except } ->
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
    [] -> { keydelete(Key, Old), if_quoted(Meta, Temp, fun(Value) -> keydelete(Key, Value) end) };
    _  ->
      ensure_no_special_form_conflict(Meta, File, Key, Final, internal_conflict),
      { [{ Key, Final }|keydelete(Key, Old)],
        if_quoted(Meta, Temp, fun(Value) -> [{ Key, Final }|keydelete(Key, Value)] end) }
  end.

if_quoted(Meta, Temp, Callback) ->
  case lists:keyfind(context, 1, Meta) of
    { context, Context } ->
      Current = case orddict:find(Context, Temp) of
        { ok, Value } -> Value;
        error -> []
      end,
      orddict:store(Context, Callback(Current), Temp);
    _ ->
      Temp
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
  ensure_no_special_form_conflict(Meta, File, Module, AllDefined, local_conflict).

%% Find conlicts in the given list of functions with
%% the recorded set of imports.

ensure_no_import_conflict(Meta, File, Module, AllDefined) ->
  Table = table(Module),
  [ensure_no_import_conflict(Meta, File, Module, Table, X) || X  <- AllDefined],
  ok.

ensure_no_import_conflict(Meta, File, Module, Table, { Name, Arity } = Tuple) ->
  RawMatches = ets:match(Table, { Tuple, '$1' }),
  Matches = [X || X <- lists:append(RawMatches), not is_boolean(X), X /= Module],

  case Matches of
    []  -> ok;
    Key ->
      Error = { import_conflict, { hd(Key), Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Error)
  end.

%% Ensure all imports are used by checking all
%% enabled warnings in the table with the imported
%% function calls.

ensure_all_imports_used(_Line, File, Module) ->
  Table = table(Module),
  [ begin
      elixir_errors:handle_file_warning(File, { L, ?MODULE, { unused_import, M } })
    end || { M, L } <- ets:select(Table, module_line_spec()),
           ets:match(Table, { '$1', M }) == []].

module_line_spec() ->
  [{ { '$1', '$2' }, [{ is_integer, '$2' }], ['$_'] }].

%% Ensure the given functions don't clash with any
%% of Elixir non overridable macros.

ensure_no_special_form_conflict(Meta, File, Key, [{Name,Arity}|T], Reason) ->
  Values = lists:filter(fun({X,Y}) ->
    (Name == X) andalso ((Y == '*') orelse (Y == Arity))
  end, special_form()),

  case Values /= [] of
    true  ->
      Tuple = { Reason, { Key, Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
    false -> ensure_no_special_form_conflict(Meta, File, Key, T, Reason)
  end;

ensure_no_special_form_conflict(_Meta, _File, _Key, [], _) -> ok.

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

format_error({ unused_import, Module }) ->
  io_lib:format("unused import ~ts", [elixir_errors:inspect(Module)]);

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

%% Macros implemented in Erlang that are not importable.

special_form() ->
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
