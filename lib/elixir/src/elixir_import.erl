%% Module responsible for handling imports and conflicts
%% in between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/4, special_form/2, format_error/1]).
-include("elixir.hrl").

%% IMPORT

import(Meta, Ref, Opts, E) ->
  Res =
    case keyfind(only, Opts) of
      { only, functions } ->
        { import_functions(Meta, Ref, Opts, E),
          E#elixir_env.macros };
      { only, macros } ->
        { E#elixir_env.functions,
          import_macros(true, Meta, Ref, Opts, E) };
      { only, List } when is_list(List) ->
        { import_functions(Meta, Ref, Opts, E),
          import_macros(false, Meta, Ref, Opts, E) };
      false ->
        { import_functions(Meta, Ref, Opts, E),
          import_macros(false, Meta, Ref, Opts, E) }
    end,

  record_warn(Meta, Ref, Opts, E),
  Res.

import_functions(Meta, Ref, Opts, E) ->
  calculate(Meta, Ref, Opts, E#elixir_env.functions, E, fun() -> get_functions(Ref) end).

import_macros(Force, Meta, Ref, Opts, E) ->
  calculate(Meta, Ref, Opts, E#elixir_env.macros, E, fun() ->
    case Force of
      true  -> get_macros(Meta, Ref, E);
      false -> get_optional_macros(Ref)
    end
  end).

record_warn(Meta, Ref, Opts, E) ->
  Warn =
    case keyfind(warn, Opts) of
      { warn, false } -> false;
      { warn, true } -> true;
      false -> not lists:keymember(context, 1, Meta)
    end,
  elixir_lexical:record_import(Ref, ?line(Meta), Warn, E#elixir_env.lexical_tracker).

%% Calculates the imports based on only and except

calculate(Meta, Key, Opts, Old, E, Existing) ->
  New = case keyfind(only, Opts) of
    { only, Only } when is_list(Only) ->
      case Only -- get_exports(Key) of
        [{Name,Arity}|_] ->
          Tuple = { invalid_import, { Key, Name, Arity } },
          elixir_errors:form_error(Meta, E#elixir_env.file, ?MODULE, Tuple);
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
    [] -> keydelete(Key, Old);
    _  ->
      ensure_no_special_form_conflict(Meta, E#elixir_env.file, Key, Final),
      [{ Key, Final }|keydelete(Key, Old)]
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

get_macros(Meta, Module, E) ->
  try
    Module:'__info__'(macros)
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Meta, E#elixir_env.file, ?MODULE, Tuple)
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
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({special_form_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it conflicts with Elixir special forms",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({ no_macros, Module }) ->
  io_lib:format("could not load macros from module ~ts", [elixir_aliases:inspect(Module)]).

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

special_form('&', 1) -> true;
special_form('^', 1) -> true;
special_form('=', 2) -> true;
special_form('%', 2) -> true;
special_form('__op__', 2) -> true;
special_form('__op__', 3) -> true;
special_form('__block__', _) -> true;
special_form('->', _) -> true;
special_form('<<>>', _) -> true;
special_form('{}', _) -> true;
special_form('%{}', _) -> true;
special_form('alias', 1) -> true;
special_form('alias', 2) -> true;
special_form('require', 1) -> true;
special_form('require', 2) -> true;
special_form('import', 1) -> true;
special_form('import', 2) -> true;
special_form('__ENV__', 0) -> true;
special_form('__CALLER__', 0) -> true;
special_form('__MODULE__', 0) -> true;
special_form('__DIR__', 0) -> true;
special_form('__aliases__', _) -> true;
special_form('quote', 1) -> true;
special_form('quote', 2) -> true;
special_form('unquote', 1) -> true;
special_form('unquote_splicing', 1) -> true;
special_form('fn', _) -> true;
special_form('super', _) -> true;
special_form('for', _) -> true;
special_form('bc', _) -> true;
special_form('lc', _) -> true;
special_form('case', 2) -> true;
special_form('try', 2) -> true;
special_form('receive', 1) -> true;
special_form(_, _) -> false.
