%% Module responsible for handling imports and conflicts
%% between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/4, special_form/2, format_error/1]).
-include("elixir.hrl").

import(Meta, Ref, Opts, E) ->
  {Functions, Macros, Added} =
    case keyfind(only, Opts) of
      {only, functions} ->
        {Added1, Funs} = import_functions(Meta, Ref, Opts, E),
        {Funs, keydelete(Ref, ?m(E, macros)), Added1};
      {only, macros} ->
        {Added2, Macs} = import_macros(true, Meta, Ref, Opts, E),
        {keydelete(Ref, ?m(E, functions)), Macs, Added2};
      {only, List} when is_list(List) ->
        {Added1, Funs} = import_functions(Meta, Ref, Opts, E),
        {Added2, Macs} = import_macros(false, Meta, Ref, Opts, E),
        {Funs, Macs, Added1 or Added2};
      false ->
        {Added1, Funs} = import_functions(Meta, Ref, Opts, E),
        {Added2, Macs} = import_macros(false, Meta, Ref, Opts, E),
        {Funs, Macs, Added1 or Added2}
    end,

  record_warn(Meta, Ref, Opts, Added, E),
  {Functions, Macros}.

import_functions(Meta, Ref, Opts, E) ->
  calculate(Meta, Ref, Opts, ?m(E, functions), ?m(E, file), fun() ->
    get_functions(Ref)
  end).

import_macros(Force, Meta, Ref, Opts, E) ->
  calculate(Meta, Ref, Opts, ?m(E, macros), ?m(E, file), fun() ->
    case Force of
      true  -> get_macros(Meta, Ref, E);
      false -> get_optional_macros(Ref)
    end
  end).

record_warn(Meta, Ref, Opts, Added, E) ->
  Warn =
    case keyfind(warn, Opts) of
      {warn, false} -> false;
      {warn, true} -> true;
      false -> not lists:keymember(context, 1, Meta)
    end,

  case keyfind(only, Opts) of
    {only, List} when is_list(List) ->
      [elixir_lexical:record_import({Ref, Name, Arity}, ?line(Meta), Added and Warn, ?m(E, lexical_tracker)) || {Name, Arity} <- List];

    _ ->
      elixir_lexical:record_import(Ref, ?line(Meta), Added and Warn, ?m(E, lexical_tracker))
  end.

%% Calculates the imports based on only and except

calculate(Meta, Key, Opts, Old, File, Existing) ->
  New = case keyfind(only, Opts) of
    {only, Only} when is_list(Only) ->
      ok = ensure_keyword_list(Meta, File, Only, only),
      case keyfind(except, Opts) of
        false -> ok;
        _ ->
          elixir_errors:compile_error(Meta, File,
            ":only and :except can only be given together to import"
            " when :only is either :functions or :macros")
      end,
      case Only -- get_exports(Key) of
        [{Name, Arity}|_] ->
          Tuple = {invalid_import, {Key, Name, Arity}},
          elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
        _ ->
          intersection(Only, Existing())
      end;
    _ ->
      case keyfind(except, Opts) of
        false -> remove_underscored(Existing());
        {except, Except} when is_list(Except) ->
          ok = ensure_keyword_list(Meta, File, Except, except),
          case keyfind(Key, Old) of
            false -> remove_underscored(Existing()) -- Except;
            {Key, OldImports} -> OldImports -- Except
          end
      end
  end,

  %% Normalize the data before storing it
  Set   = ordsets:from_list(New),
  Final = remove_internals(Set),

  case Final of
    [] ->
      {false, keydelete(Key, Old)};
    _  ->
      ensure_no_special_form_conflict(Meta, File, Key, Final),
      {true, [{Key, Final}|keydelete(Key, Old)]}
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
      Tuple = {no_macros, Module},
      elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, Tuple)
  end.

get_optional_macros(Module)  ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      try
        Module:'__info__'(macros)
      catch
        error:undef -> []
      end;
    {error, _} -> []
  end.

%% VALIDATION HELPERS

ensure_no_special_form_conflict(Meta, File, Key, [{Name, Arity}|T]) ->
  case special_form(Name, Arity) of
    true  ->
      Tuple = {special_form_conflict, {Key, Name, Arity}},
      elixir_errors:form_error(Meta, File, ?MODULE, Tuple);
    false ->
      ensure_no_special_form_conflict(Meta, File, Key, T)
  end;

ensure_no_special_form_conflict(_Meta, _File, _Key, []) -> ok.

ensure_keyword_list(_Meta, _File, [], _Kind) -> ok;

ensure_keyword_list(Meta, File, [{Key, _} | Rest], Kind) when is_atom(Key) ->
  ensure_keyword_list(Meta, File, Rest, Kind);

ensure_keyword_list(Meta, File, _Other, Kind) ->
  elixir_errors:compile_error(Meta, File, "invalid :~s option for import, expected a keyword list", [Kind]).

%% ERROR HANDLING

format_error({invalid_import, {Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it doesn't exist",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({special_form_conflict, {Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it conflicts with Elixir special forms",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({no_macros, Module}) ->
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
  lists:filter(fun({Name, _}) ->
    case atom_to_list(Name) of
      "_" ++ _ -> false;
      _ -> true
    end
  end, List).

remove_internals(Set) ->
  ordsets:del_element({module_info, 1},
    ordsets:del_element({module_info, 0}, Set)).

%% Special forms

special_form('&', 1) -> true;
special_form('^', 1) -> true;
special_form('=', 2) -> true;
special_form('%', 2) -> true;
special_form('::', 2) -> true;
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
special_form('with', _) -> true;
special_form('cond', 1) -> true;
special_form('case', 2) -> true;
special_form('try', 2) -> true;
special_form('receive', 1) -> true;
special_form(_, _) -> false.
