%% Module responsible for handling imports and conflicts
%% in between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/4, special_form/2, format_error/1]).
-include("elixir.hrl").

%% IMPORT

import(Meta, Ref, Opts, S) ->
  SI =
    case keyfind(only, Opts) of
      { only, functions } ->
        import_functions(Meta, Ref, Opts, S);
      { only, macros } ->
        import_macros(true, Meta, Ref, Opts, S);
      { only, List } when is_list(List) ->
        import_macros(false, Meta, Ref, Opts, import_functions(Meta, Ref, Opts, S));
      false ->
        import_macros(false, Meta, Ref, Opts, import_functions(Meta, Ref, Opts, S))
    end,
  record_warn(Meta, Ref, Opts, SI),
  SI.

import_functions(Meta, Ref, Opts, S) ->
  { Functions, Temp } = calculate(Meta, Ref, Opts, S#elixir_scope.functions,
    S#elixir_scope.macro_functions, fun() -> get_functions(Ref) end, S),
  S#elixir_scope{functions=Functions, macro_functions=Temp}.

import_macros(Force, Meta, Ref, Opts, S) ->
  Existing = fun() ->
    case Force of
      true  -> get_macros(Meta, Ref, S);
      false -> get_optional_macros(Ref)
    end
  end,
  { Macros, Temp } = calculate(Meta, Ref, Opts, S#elixir_scope.macros,
    S#elixir_scope.macro_macros, Existing, S),
  S#elixir_scope{macros=Macros, macro_macros=Temp}.

record_warn(_Meta, _Ref, _Opts, #elixir_scope{module=nil}) -> false;
record_warn(Meta, Ref, Opts, #elixir_scope{module=Module}) ->
  Warn =
    case keyfind(warn, Opts) of
      { warn, false } -> false;
      { warn, true } -> true;
      false -> not lists:keymember(context, 1, Meta)
    end,
  elixir_tracker:record_warn(Ref, Warn, ?line(Meta), Module).

%% Calculates the imports based on only and except

calculate(Meta, Key, Opts, Old, Temp, Existing, S) ->
  New = case keyfind(only, Opts) of
    { only, Only } when is_list(Only) ->
      case Only -- get_exports(Key) of
        [{Name,Arity}|_] ->
          Tuple = { invalid_import, { Key, Name, Arity } },
          elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, Tuple);
        _ ->
          intersection(Only, Existing())
      end;
    _ ->
      case keyfind(except, Opts) of
        false -> remove_underscored(Existing());
        { except, [] } -> remove_underscored(Existing());
        { except, Except } when is_list(Except) ->
          case keyfind(Key, Old) of
            false -> remove_underscored(Existing()) -- Except;
            {Key,OldImports} -> OldImports -- Except
          end
      end
  end,

  %% Normalize the data before storing it
  Set   = ordsets:from_list(New),
  Final = remove_internals(Set),

  case Final of
    [] ->
      { keydelete(Key, Old),
        if_quoted(Meta, Temp, fun(Value) -> keydelete(Key, Value) end) };
    _  ->
      ensure_no_special_form_conflict(Meta, S#elixir_scope.file, Key, Final),
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

ensure_no_special_form_conflict(Meta, File, Key, [{Name,Arity}|T]) ->
  case special_form(Name, Arity) of
    true  ->
      Tuple = { special_form_conflict, { Key, Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
    false ->
      ensure_no_special_form_conflict(Meta, File, Key, T)
  end;

ensure_no_special_form_conflict(_Meta, _File, _Key, []) -> ok.

%% ERROR HANDLING

format_error({invalid_import,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it doesn't exist",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({special_form_conflict,{Receiver, Name, Arity}}) ->
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

%% Internal funs that are never imported etc.

remove_underscored(List) ->
  lists:filter(fun({ Name, _ }) ->
    case atom_to_list(Name) of
      "_" ++ _ -> false;
      _ -> true
    end
  end, List).

remove_internals(Set) ->
  ordsets:del_element({ module_info, 1 },
    ordsets:del_element({ module_info, 0 }, Set)).

%% Special forms

special_form('&',1) -> true;
special_form('^',1) -> true;
special_form('=',2) -> true;
special_form('__op__',2) -> true;
special_form('__op__',3) -> true;
special_form('__scope__',2) -> true;
special_form('__block__',_) -> true;
special_form('->',_) -> true;
special_form('<<>>',_) -> true;
special_form('{}',_) -> true;
special_form('[]',_) -> true;
special_form('alias',1) -> true;
special_form('alias',2) -> true;
special_form('require',1) -> true;
special_form('require',2) -> true;
special_form('import',1) -> true;
special_form('import',2) -> true;
special_form('__ENV__',0) -> true;
special_form('__CALLER__',0) -> true;
special_form('__MODULE__',0) -> true;
special_form('__FILE__',0) -> true;
special_form('__DIR__',0) -> true;
special_form('__aliases__',_) -> true;
special_form('quote',1) -> true;
special_form('quote',2) -> true;
special_form('unquote',1) -> true;
special_form('unquote_splicing',1) -> true;
special_form('fn',_) -> true;
special_form('super',_) -> true;
special_form('super?',0) -> true;
special_form('bc',_) -> true;
special_form('lc',_) -> true;
special_form('var!',1) -> true;
special_form('var!',2) -> true;
special_form('alias!',1) -> true;
special_form(_, _) -> false.
