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
        {Funs, keydelete(Ref, ?key(E, macros)), Added1};
      {only, macros} ->
        {Added2, Macs} = import_macros(true, Meta, Ref, Opts, E),
        {keydelete(Ref, ?key(E, functions)), Macs, Added2};
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
  calculate(Meta, Ref, Opts, ?key(E, functions), ?key(E, file), fun() ->
    get_functions(Ref)
  end).

import_macros(Force, Meta, Ref, Opts, E) ->
  calculate(Meta, Ref, Opts, ?key(E, macros), ?key(E, file), fun() ->
    case fetch_macros(Ref) of
      {ok, Macros} ->
        Macros;
      error when Force ->
        elixir_errors:form_error(Meta, E, ?MODULE, {no_macros, Ref});
      error ->
        []
    end
  end).

record_warn(Meta, Ref, Opts, Added, E) ->
  Warn =
    case keyfind(warn, Opts) of
      {warn, false} -> false;
      {warn, true} -> true;
      false -> not lists:keymember(context, 1, Meta)
    end,

  Only =
    case keyfind(only, Opts) of
      {only, List} when is_list(List) -> List;
      _ -> []
    end,

  elixir_lexical:record_import(Ref, Only, ?line(Meta), Added and Warn, ?key(E, lexical_tracker)).

%% Calculates the imports based on only and except

calculate(Meta, Key, Opts, Old, File, Existing) ->
  New = case keyfind(only, Opts) of
    {only, Only} when is_list(Only) ->
      ensure_keyword_list(Meta, File, Only, only),
      ensure_no_duplicates(Meta, File, Only, only),
      case keyfind(except, Opts) of
        false ->
          ok;
        _ ->
          elixir_errors:form_error(Meta, File, ?MODULE, only_and_except_given)
      end,
      case Only -- get_exports(Key) of
        [{Name, Arity} | _] ->
          elixir_errors:form_error(Meta, File, ?MODULE, {invalid_import, {Key, Name, Arity}});
        _ ->
          intersection(Only, Existing())
      end;
    _ ->
      case keyfind(except, Opts) of
        false ->
          remove_underscored(Existing());
        {except, Except} when is_list(Except) ->
          ensure_keyword_list(Meta, File, Except, except),
          ensure_no_duplicates(Meta, File, Except, except),
          %% We are not checking existence of exports listed in :except option
          %% on purpose: to support backwards compatible code.
          %% For example, "import String, except: [trim: 1]"
          %% should work across all Elixir versions.
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
      {true, [{Key, Final} | keydelete(Key, Old)]}
  end.

%% Retrieve functions and macros from modules

get_exports(Module) ->
  get_functions(Module) ++ get_macros(Module).

get_functions(Module) ->
  try
    Module:'__info__'(functions)
  catch
    error:undef -> Module:module_info(exports)
  end.

get_macros(Module) ->
  case fetch_macros(Module) of
    {ok, Macros} ->
      Macros;
    error ->
      []
  end.

fetch_macros(Module) ->
  try
    {ok, Module:'__info__'(macros)}
  catch
    error:undef -> error
  end.

%% VALIDATION HELPERS

ensure_no_special_form_conflict(Meta, File, Key, [{Name, Arity} | T]) ->
  case special_form(Name, Arity) of
    true  ->
      elixir_errors:form_error(Meta, File, ?MODULE, {special_form_conflict, {Key, Name, Arity}});
    false ->
      ensure_no_special_form_conflict(Meta, File, Key, T)
  end;

ensure_no_special_form_conflict(_Meta, _File, _Key, []) -> ok.

ensure_keyword_list(_Meta, _File, [], _Kind) -> ok;

ensure_keyword_list(Meta, File, [{Key, Value} | Rest], Kind) when is_atom(Key), is_integer(Value) ->
  ensure_keyword_list(Meta, File, Rest, Kind);

ensure_keyword_list(Meta, File, _Other, Kind) ->
  elixir_errors:form_error(Meta, File, ?MODULE, {invalid_option, Kind}).

ensure_no_duplicates(Meta, File, Option, Kind) ->
  lists:foldl(fun({Name, Arity}, Acc) ->
    case lists:member({Name, Arity}, Acc) of
      true ->
        elixir_errors:form_error(Meta, File, ?MODULE, {duplicated_import, {Kind, Name, Arity}});
      false ->
        [{Name, Arity} | Acc]
    end
  end, [], Option).

%% ERROR HANDLING

format_error(only_and_except_given) ->
  ":only and :except can only be given together to import "
  "when :only is either :functions or :macros";

format_error({duplicated_import, {Option, Name, Arity}}) ->
  io_lib:format("invalid :~s option for import, ~ts/~B is duplicated", [Option, Name, Arity]);

format_error({invalid_import, {Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it is undefined or private",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({invalid_option, Option}) ->
  Message = "invalid :~s option for import, expected a keyword list with integer values",
  io_lib:format(Message, [Option]);

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

intersection([H | T], All) ->
  case lists:member(H, All) of
    true  -> [H | intersection(T, All)];
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
special_form('__STACKTRACE__', 0) -> true;
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
special_form('try', 1) -> true;
special_form('receive', 1) -> true;
special_form(_, _) -> false.
