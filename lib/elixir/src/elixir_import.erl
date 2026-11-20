%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

%% Module responsible for handling imports and conflicts
%% between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/6, import/7, special_form/2,
         record/4, ensure_no_local_conflict/3,
         format_error/1]).
-compile(inline_list_funcs).
-include("elixir.hrl").

import(Meta, Ref, Opts, E, Warn, Trace) ->
  import(Meta, Ref, Opts, E, Warn, Trace, fun Ref:'__info__'/1).

import(Meta, Ref, Opts, E, Warn, Trace, InfoCallback) ->
  case import_only_except(Meta, Ref, Opts, E, Warn, InfoCallback) of
    {Functions, Macros, Added} ->
      Trace andalso elixir_env:trace({import, Meta, Ref, Opts}, E),
      %% Keep a record of empty imports so a later :except cannot restore them.
      EFuns = case Added of
        true -> Functions;
        false -> [{Ref, []} | Functions]
      end,
      EI = E#{functions := EFuns, macros := Macros},
      {ok, Added, elixir_aliases:require(Meta, Ref, [{warn, false} | Opts], EI, Trace)};

    {error, Reason} ->
      {error, Reason}
  end.

import_only_except(Meta, Ref, Opts, E, Warn, InfoCallback) ->
  MaybeOnly = lists:keyfind(only, 1, Opts),

  case lists:keyfind(except, 1, Opts) of
    false ->
      import_only_except(Meta, Ref, MaybeOnly, false, E, Warn, InfoCallback);

    {except, DupExcept} when is_list(DupExcept) ->
      case ensure_keyword_list(DupExcept) of
        ok ->
          Except = ensure_no_duplicates(DupExcept, except, Meta, E, Warn),
          import_only_except(Meta, Ref, MaybeOnly, Except, E, Warn, InfoCallback);

        error ->
          {error, {invalid_option, except, DupExcept}}
      end;

    {except, Other} ->
      {error, {invalid_option, except, Other}}
  end.

import_only_except(Meta, Ref, MaybeOnly, Except, E, Warn, InfoCallback) ->
  case MaybeOnly of
    {only, Only} when Only =:= functions; Only =:= macros; Only =:= sigils ->
      import_selected(Meta, Ref, Only, Except, E, Warn, InfoCallback);

    {only, DupOnly} when is_list(DupOnly) ->
      case ensure_keyword_list(DupOnly) of
        ok when Except =:= false ->
          Only = ensure_no_duplicates(DupOnly, only, Meta, E, Warn),
          {Added1, Used1, Funs} = import_listed_functions(Meta, Ref, Only, E, Warn, InfoCallback),
          {Added2, Used2, Macs} = import_listed_macros(Meta, Ref, Only, E, Warn, InfoCallback),
          [Warn andalso elixir_errors:file_warn(Meta, E, ?MODULE, {invalid_import, {Ref, Name, Arity}}) ||
            {Name, Arity} <- (Only -- Used1) -- Used2],
          {Funs, Macs, Added1 or Added2};

        ok ->
          {error, only_and_except_given};

        error ->
          {error, {invalid_option, only, DupOnly}}
      end;

    {only, Other} ->
      {error, {invalid_option, only, Other}};

    false ->
      import_selected(Meta, Ref, false, Except, E, Warn, InfoCallback)
  end.

import_listed_functions(Meta, Ref, Only, E, Warn, InfoCallback) ->
  New = intersection(Only, get_functions(Ref, InfoCallback)),
  calculate_key(Meta, Ref, ?key(E, functions), New, E, Warn).

import_listed_macros(Meta, Ref, Only, E, Warn, InfoCallback) ->
  New = intersection(Only, get_macros(InfoCallback)),
  calculate_key(Meta, Ref, ?key(E, macros), New, E, Warn).

import_selected(Meta, Ref, Only, Except, E, Warn, InfoCallback) ->
  #{functions := OldFuns, macros := OldMacs} = E,
  {PreviousFuns, PreviousMacs, Exclusions} = case Except of
    false ->
      {false, false, []};
    _ when Only =/= false ->
      {false, false, Except};
    _ ->
      case {lists:keyfind(Ref, 1, OldFuns), lists:keyfind(Ref, 1, OldMacs)} of
        {false, false} -> {false, false, Except};
        {Funs, Macs} -> {import_entries(Funs), import_entries(Macs), Except}
      end
  end,
  NewFuns = select_imports(functions, Only, PreviousFuns, Exclusions,
    fun() -> get_functions(Ref, InfoCallback) end),
  {Added1, _, Functions} = calculate_key(Meta, Ref, OldFuns, NewFuns, E, Warn),
  NewMacs = select_imports(macros, Only, PreviousMacs, Exclusions,
    fun() -> get_macros(InfoCallback) end),
  {Added2, _, Macros} = calculate_key(Meta, Ref, OldMacs, NewMacs, E, Warn),
  {Functions, Macros, Added1 or Added2}.

import_entries(false) -> [];
import_entries({_Ref, Entries}) -> Entries.

select_imports(functions, macros, _Previous, _Except, _Exports) -> [];
select_imports(macros, functions, _Previous, _Except, _Exports) -> [];
select_imports(_Kind, Only, Previous, Except, Exports) ->
  Available = case Previous of
    false -> remove_underscored(Exports());
    _ -> Previous
  end,
  Selected = case Only of
    sigils -> filter_sigils(Available);
    _ -> Available
  end,
  %% We are not checking existence of exports listed in :except
  %% option on purpose: to support backwards compatible code.
  %% For example, "import String, except: [trim: 1]"
  %% should work across all Elixir versions.
  Selected -- Except.

calculate_key(Meta, Key, Old, New, E, Warn) ->
  case ordsets:from_list(New) of
    [] ->
      {false, [], keydelete(Key, Old)};
    Set  ->
      FinalSet = ensure_no_special_form_conflict(Set, Key, Meta, E, Warn),
      {true, FinalSet, [{Key, FinalSet} | keydelete(Key, Old)]}
  end.

%% Record function calls for local conflicts

record(_Tuple, Receiver, Module, Function)
  when Function == nil; Module == Receiver -> false;
record(Tuple, Receiver, Module, _Function) ->
  try
    {Set, _Bag} = elixir_module:data_tables(Module),
    ets:insert(Set, {{import, Tuple}, Receiver}),
    true
  catch
    error:badarg -> false
  end.

ensure_no_local_conflict('Elixir.Kernel', _All, _E) ->
  ok;
ensure_no_local_conflict(Module, AllDefinitions, E) ->
  {Set, _} = elixir_module:data_tables(Module),

  [try
     Receiver = ets:lookup_element(Set, {import, Pair}, 2),
     elixir_errors:module_error(Meta, E, ?MODULE, {import_conflict, Receiver, Pair})
   catch
    error:badarg -> false
   end || {Pair, _, Meta, _} <- AllDefinitions].

%% Retrieve functions and macros from modules

get_functions(Module, InfoCallback) ->
  try
    InfoCallback(functions)
  catch
    error:undef -> remove_internals(Module:module_info(exports))
  end.

get_macros(InfoCallback) ->
  try
    InfoCallback(macros)
  catch
    error:undef -> []
  end.

filter_sigils(Funs) ->
  lists:filter(fun is_sigil/1, Funs).

is_sigil({Name, 2}) ->
  case atom_to_list(Name) of
    "sigil_" ++ Letters ->
      case Letters of
        [L] when L >= $a, L =< $z -> true;
        [] -> false;
        [H|T] when H >= $A, H =< $Z ->
              lists:all(fun(L) -> (L >= $0 andalso L =< $9)
                                  orelse (L>= $A andalso L =< $Z)
                        end, T);
        _ -> false
      end;
    _ ->
      false
  end;
is_sigil(_) ->
  false.

%% VALIDATION HELPERS\

ensure_keyword_list([]) ->
  ok;
ensure_keyword_list([{Key, Value} | Rest]) when is_atom(Key), is_integer(Value) ->
  ensure_keyword_list(Rest);
ensure_keyword_list(_Other) ->
  error.

ensure_no_special_form_conflict(Set, Key, Meta, E, Warn) ->
  lists:filter(fun({Name, Arity}) ->
    case special_form(Name, Arity) of
      true  ->
        Warn andalso elixir_errors:file_warn(Meta, E, ?MODULE, {special_form_conflict, {Key, Name, Arity}}),
        false;
      false ->
        true
    end
  end, Set).

ensure_no_duplicates(Option, Kind, Meta, E, Warn) ->
  lists:foldl(fun({Name, Arity}, Acc) ->
    case lists:member({Name, Arity}, Acc) of
      true ->
        Warn andalso elixir_errors:file_warn(Meta, E, ?MODULE, {duplicated_import, {Kind, Name, Arity}}),
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

format_error({invalid_option, only, Value}) ->
  Message = "invalid :only option for import, expected value to be an atom :functions, :macros, :sigils"
  ", or a literal keyword list of function names with arity as values, got: ~s",
  io_lib:format(Message, ['Elixir.Macro':to_string(Value)]);

format_error({invalid_option, except, Value}) ->
  Message = "invalid :except option for import, expected value to be a literal keyword list of function names with arity as values, got: ~s",
  io_lib:format(Message, ['Elixir.Macro':to_string(Value)]);

format_error({special_form_conflict, {Receiver, Name, Arity}}) ->
  io_lib:format("cannot import ~ts.~ts/~B because it conflicts with Elixir special forms, the import has been discarded",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({no_macros, Module}) ->
  io_lib:format("could not load macros from module ~ts", [elixir_aliases:inspect(Module)]);

format_error({import_conflict, Receiver, {Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_aliases:inspect(Receiver), Name, Arity]).

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
special_form('__aliases__', _) -> true;
special_form('__block__', _) -> true;
special_form('__cursor__', _) -> true;
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
