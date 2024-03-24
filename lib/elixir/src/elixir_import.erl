%% Module responsible for handling imports and conflicts
%% between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/5, special_form/2, format_error/1]).
-compile(inline_list_funcs).
-include("elixir.hrl").

import(Meta, Ref, Opts, E, Trace) ->
  case import_only_except(Meta, Ref, Opts, E) of
    {Functions, Macros, Added} ->
      Trace andalso elixir_env:trace({import, [{imported, Added} | Meta], Ref, Opts}, E),
      E#{functions := Functions, macros := Macros};

    {error, Reason} ->
      elixir_errors:file_error(Meta, E, ?MODULE, Reason)
  end.

import_only_except(Meta, Ref, Opts, E) ->
  MaybeOnly = lists:keyfind(only, 1, Opts),

  case lists:keyfind(except, 1, Opts) of
    false ->
      import_only_except(Meta, Ref, MaybeOnly, false, E);

    {except, DupExcept} when is_list(DupExcept) ->
      case ensure_keyword_list(DupExcept, except, Meta, E) of
        ok ->
          Except = ensure_no_duplicates(DupExcept, except, Meta, E),
          import_only_except(Meta, Ref, MaybeOnly, Except, E);

        {error, Reason} ->
          {error, Reason}
      end;

    {except, Other} ->
      {error, {invalid_option, except, Other}}
  end.

import_only_except(Meta, Ref, MaybeOnly, Except, E) ->
  case MaybeOnly of
    {only, functions} ->
      {Added1, _Used1, Funs} = import_functions(Meta, Ref, Except, E),
      {Funs, keydelete(Ref, ?key(E, macros)), Added1};

    {only, macros} ->
      {Added2, _Used2, Macs} = import_macros(true, Meta, Ref, Except, E),
      {keydelete(Ref, ?key(E, functions)), Macs, Added2};

    {only, sigils} ->
      {Added1, _Used1, Funs} = import_sigil_functions(Meta, Ref, Except, E),
      {Added2, _Used2, Macs} = import_sigil_macros(Meta, Ref, Except, E),
      {Funs, Macs, Added1 or Added2};

    {only, DupOnly} when is_list(DupOnly) ->
      case ensure_keyword_list(DupOnly, only, Meta, E) of
        ok when Except =:= false ->
          Only = ensure_no_duplicates(DupOnly, only, Meta, E),
          {Added1, Used1, Funs} = import_listed_functions(Meta, Ref, Only, E),
          {Added2, Used2, Macs} = import_listed_macros(Meta, Ref, Only, E),
          [elixir_errors:file_warn(Meta, E, ?MODULE, {invalid_import, {Ref, Name, Arity}}) ||
            {Name, Arity} <- (Only -- Used1) -- Used2],
          {Funs, Macs, Added1 or Added2};

        ok ->
          {error, only_and_except_given};

        {error, Reason} ->
          {error, Reason}
      end;

    {only, Other} ->
      {error, {invalid_option, only, Other}};

    false ->
      {Added1, _Used1, Funs} = import_functions(Meta, Ref, Except, E),
      {Added2, _Used2, Macs} = import_macros(false, Meta, Ref, Except, E),
      {Funs, Macs, Added1 or Added2}
  end.

import_listed_functions(Meta, Ref, Only, E) ->
  New = intersection(Only, get_functions(Ref)),
  calculate_key(Meta, Ref, ?key(E, functions), New, E).

import_listed_macros(Meta, Ref, Only, E) ->
  New = intersection(Only, get_macros(Meta, Ref, false, E)),
  calculate_key(Meta, Ref, ?key(E, macros), New, E).

import_functions(Meta, Ref, Except, E) ->
  calculate_except(Meta, Ref, Except, ?key(E, functions), E, fun() ->
    get_functions(Ref)
  end).

import_macros(Force, Meta, Ref, Except, E) ->
  calculate_except(Meta, Ref, Except, ?key(E, macros), E, fun() ->
    get_macros(Meta, Ref, Force, E)
  end).

import_sigil_functions(Meta, Ref, Except, E) ->
  calculate_except(Meta, Ref, Except, ?key(E, functions), E, fun() ->
    filter_sigils(get_functions(Ref))
  end).

import_sigil_macros(Meta, Ref, Except, E) ->
  calculate_except(Meta, Ref, Except, ?key(E, macros), E, fun() ->
    filter_sigils(get_macros(Meta, Ref, false, E))
  end).

calculate_except(Meta, Key, false, Old, E, Existing) ->
  New = remove_underscored(Existing()),
  calculate_key(Meta, Key, Old, New, E);

calculate_except(Meta, Key, Except, Old, E, Existing) ->
  %% We are not checking existence of exports listed in :except
  %% option on purpose: to support backwards compatible code.
  %% For example, "import String, except: [trim: 1]"
  %% should work across all Elixir versions.
  New =
    case lists:keyfind(Key, 1, Old) of
      false -> remove_underscored(Existing()) -- Except;
      {Key, OldImports} -> OldImports -- Except
    end,
  calculate_key(Meta, Key, Old, New, E).

calculate_key(Meta, Key, Old, New, E) ->
  case ordsets:from_list(New) of
    [] ->
      {false, [], keydelete(Key, Old)};
    Set  ->
      FinalSet = ensure_no_special_form_conflict(Set, Key, Meta, E),
      {true, FinalSet, [{Key, FinalSet} | keydelete(Key, Old)]}
  end.

%% Retrieve functions and macros from modules

get_functions(Module) ->
  try
    Module:'__info__'(functions)
  catch
    error:undef -> remove_internals(Module:module_info(exports))
  end.

get_macros(Meta, Module, Force, E) ->
  case fetch_macros(Module) of
    {ok, Macros} ->
      Macros;
    error when Force ->
      elixir_errors:file_error(Meta, E, ?MODULE, {no_macros, Module});
    error ->
      []
  end.

fetch_macros(Module) ->
  try
    {ok, Module:'__info__'(macros)}
  catch
    error:undef -> error
  end.

filter_sigils(Funs) ->
  lists:filter(fun is_sigil/1, Funs).

is_sigil({Name, 2}) ->
  case atom_to_list(Name) of
    "sigil_" ++ Letters ->
      case Letters of
        [L] when L >= $a, L =< $z -> true;
        [] -> false;
        Letters -> lists:all(fun(L) -> L >= $A andalso L =< $Z end, Letters)
      end;
    _ ->
      false
  end;
is_sigil(_) ->
  false.

%% VALIDATION HELPERS

ensure_no_special_form_conflict(Set, Key, Meta, E) ->
  lists:filter(fun({Name, Arity}) ->
    case special_form(Name, Arity) of
      true  ->
        elixir_errors:file_warn(Meta, E, ?MODULE, {special_form_conflict, {Key, Name, Arity}}),
        false;
      false ->
        true
    end
  end, Set).

ensure_keyword_list([], _Kind, _Meta, _E) -> ok;

ensure_keyword_list([{Key, Value} | Rest], Kind, Meta, E) when is_atom(Key), is_integer(Value) ->
  ensure_keyword_list(Rest, Kind, Meta, E);

ensure_keyword_list(_Other, Kind, Meta, E) ->
  elixir_errors:file_error(Meta, E, ?MODULE, {invalid_option, Kind}).

ensure_no_duplicates(Option, Kind, Meta, E) ->
  lists:foldl(fun({Name, Arity}, Acc) ->
    case lists:member({Name, Arity}, Acc) of
      true ->
        elixir_errors:file_warn(Meta, E, ?MODULE, {duplicated_import, {Kind, Name, Arity}}),
        Acc;
      false ->
        [{Name, Arity} | Acc]
    end
  end, [], Option).

%% ERROR HANDLING

format_error(only_and_except_given) ->
  ":only and :except can only be given together to import "
  "when :only is :functions, :macros, or :sigils";

format_error({duplicated_import, {Option, Name, Arity}}) ->
  io_lib:format("invalid :~s option for import, ~ts/~B is duplicated", [Option, Name, Arity]);

format_error({invalid_import, {Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it is undefined or private",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({invalid_option, Option}) ->
  Message = "invalid :~s option for import, expected a keyword list with integer values",
  io_lib:format(Message, [Option]);

format_error({invalid_option, only, Value}) ->
  Message = "invalid :only option for import, expected value to be an atom :functions, :macros"
  ", or a list literal, got: ~s",
  io_lib:format(Message, ['Elixir.Macro':to_string(Value)]);

format_error({invalid_option, except, Value}) ->
  Message = "invalid :except option for import, expected value to be a list literal, got: ~s",
  io_lib:format(Message, ['Elixir.Macro':to_string(Value)]);

format_error({special_form_conflict, {Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it conflicts with Elixir special forms, the import has been discarded",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({no_macros, Module}) ->
  io_lib:format("could not load macros from module ~ts", [elixir_aliases:inspect(Module)]).

%% LIST HELPERS

keydelete(Key, List) ->
  lists:keydelete(Key, 1, List).

intersection([H | T], All) ->
  case lists:member(H, All) of
    true  -> [H | intersection(T, All)];
    false -> intersection(T, All)
  end;

intersection([], _All) -> [].

%% Internal funs that are never imported, and the like

remove_underscored(List) ->
  lists:filter(fun({Name, _}) ->
    case atom_to_list(Name) of
      "_" ++ _ -> false;
      _ -> true
    end
  end, List).

remove_internals(Set) ->
  Set -- [{behaviour_info, 1}, {module_info, 1}, {module_info, 0}].

%% Special forms

special_form('&', 1) -> true;
special_form('^', 1) -> true;
special_form('=', 2) -> true;
special_form('%', 2) -> true;
special_form('|', 2) -> true;
special_form('.', 2) -> true;
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
