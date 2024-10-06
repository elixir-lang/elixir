%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([dispatch_import/6, dispatch_require/7,
  require_function/5, import_function/4,
  expand_import/7, expand_require/6, check_deprecated/6,
  default_functions/0, default_macros/0, default_requires/0,
  find_import/4, find_imports/3, format_error/1]).
-include("elixir.hrl").
-import(ordsets, [is_element/2]).
-define(kernel, 'Elixir.Kernel').
-define(application, 'Elixir.Application').

default_functions() ->
  [{?kernel, elixir_imported_functions()}].
default_macros() ->
  [{?kernel, elixir_imported_macros()}].
default_requires() ->
  ['Elixir.Application', 'Elixir.Kernel'].

%% This is used by elixir_quote. Note we don't record the
%% import locally because at that point there is no
%% ambiguity.
find_import(Meta, Name, Arity, E) ->
  Tuple = {Name, Arity},

  case find_import_by_name_arity(Meta, Tuple, [], E) of
    {function, Receiver} ->
      elixir_env:trace({imported_function, Meta, Receiver, Name, Arity}, E),
      Receiver;
    {macro, Receiver} ->
      elixir_env:trace({imported_macro, Meta, Receiver, Name, Arity}, E),
      Receiver;
    {ambiguous, _} = Ambiguous ->
      elixir_errors:file_error(Meta, E, ?MODULE, {import, Ambiguous, Name, Arity});
    _ ->
      false
  end.

find_imports(Meta, Name, E) ->
  Funs = ?key(E, functions),
  Macs = ?key(E, macros),

  Acc0 = #{},
  Acc1 = find_imports_by_name(Funs, Acc0, Name, Meta, E),
  Acc2 = find_imports_by_name(Macs, Acc1, Name, Meta, E),

  lists:sort(maps:to_list(Acc2)).

%% Function retrieval

import_function(Meta, Name, Arity, E) ->
  Tuple = {Name, Arity},
  case find_import_by_name_arity(Meta, Tuple, [], E) of
    {function, Receiver} ->
      elixir_env:trace({imported_function, Meta, Receiver, Name, Arity}, E),
      elixir_locals:record_import(Tuple, Receiver, ?key(E, module), ?key(E, function)),
      remote_function(Meta, Receiver, Name, Arity, E);
    {macro, _Receiver} ->
      false;
    {import, Receiver} ->
      require_function(Meta, Receiver, Name, Arity, E);
    {ambiguous, _} = Ambiguous ->
      elixir_errors:file_error(Meta, E, ?MODULE, {import, Ambiguous, Name, Arity});
    false ->
      case elixir_import:special_form(Name, Arity) of
        true ->
          false;
        false ->
          Function = ?key(E, function),

          case (Function /= nil) andalso (Function /= Tuple) andalso
                elixir_def:local_for(Meta, Name, Arity, [defmacro, defmacrop], E) of
            false ->
              elixir_env:trace({local_function, Meta, Name, Arity}, E),
              elixir_locals:record_local(Tuple, ?key(E, module), ?key(E, function), Meta, false),
              {local, Name, Arity};
            _ ->
              false
          end
      end
  end.

require_function(Meta, Receiver, Name, Arity, E) ->
  Required = is_element(Receiver, ?key(E, requires)),

  case is_macro(Name, Arity, Receiver, Required) of
    true  -> false;
    false ->
      elixir_env:trace({remote_function, Meta, Receiver, Name, Arity}, E),
      remote_function(Meta, Receiver, Name, Arity, E)
  end.

remote_function(Meta, Receiver, Name, Arity, E) ->
  check_deprecated(function, Meta, Receiver, Name, Arity, E),

  case elixir_rewrite:inline(Receiver, Name, Arity) of
    {AR, AN} -> {remote, AR, AN, Arity};
    false    -> {remote, Receiver, Name, Arity}
  end.

%% Dispatches

dispatch_import(Meta, Name, Args, S, E, Callback) ->
  Arity = length(Args),

  AllowLocals =
    %% If we are inside a function, we support reading from locals.
    case E of
      #{function := {N, A}} when Name =/= N; Arity =/= A -> true;
      _ -> false
    end,

  case expand_import(Meta, Name, Arity, E, [], AllowLocals, true) of
    {macro, Receiver, Expander} ->
      check_deprecated(macro, Meta, Receiver, Name, Arity, E),
      Caller = {?line(Meta), S, E},
      expand_quoted(Meta, Receiver, Name, Arity, Expander(Args, Caller), S, E);
    {function, Receiver, NewName} ->
      case elixir_rewrite:inline(Receiver, NewName, Arity) of
        {AR, AN} ->
          Callback({AR, AN});
        false ->
          check_deprecated(function, Meta, Receiver, Name, Arity, E),
          Callback({Receiver, NewName})
      end;
    not_found ->
      Callback(local);
    Error ->
      elixir_errors:file_error(Meta, E, ?MODULE, {import, Error, Name, Arity})
  end.

dispatch_require(Meta, Receiver, Name, Args, S, E, Callback) when is_atom(Receiver) ->
  Arity = length(Args),

  case elixir_rewrite:inline(Receiver, Name, Arity) of
    {AR, AN} ->
      elixir_env:trace({remote_function, Meta, Receiver, Name, Arity}, E),
      Callback(AR, AN);
    false ->
      case expand_require(Meta, Receiver, Name, Arity, E, true) of
        {macro, Receiver, Expander} ->
          check_deprecated(macro, Meta, Receiver, Name, Arity, E),
          Caller = {?line(Meta), S, E},
          expand_quoted(Meta, Receiver, Name, Arity, Expander(Args, Caller), S, E);
        error ->
          check_deprecated(function, Meta, Receiver, Name, Arity, E),
          elixir_env:trace({remote_function, Meta, Receiver, Name, Arity}, E),
          Callback(Receiver, Name)
      end
  end;

dispatch_require(_Meta, Receiver, Name, _Args, _S, _E, Callback) ->
  Callback(Receiver, Name).

%% Macros expansion

expand_import(Meta, Name, Arity, E, Extra, AllowLocals, Trace) ->
  Tuple = {Name, Arity},
  Module = ?key(E, module),
  Dispatch = find_import_by_name_arity(Meta, Tuple, Extra, E),

  case Dispatch of
    {ambiguous, Ambiguous} ->
      {ambiguous, Ambiguous};

    {import, _} ->
      do_expand_import(Dispatch, Meta, Name, Arity, Module, E, Trace);

    _ ->
      Local = AllowLocals andalso elixir_def:local_for(Meta, Name, Arity, [defmacro, defmacrop], E),

      case Dispatch of
        %% There is a local and an import. This is a conflict unless
        %% the receiver is the same as module (happens on bootstrap).
        {_, Receiver} when Local /= false, Receiver /= Module ->
          {conflict, Receiver};

        %% There is no local. Dispatch the import.
        _ when Local == false ->
          do_expand_import(Dispatch, Meta, Name, Arity, Module, E, Trace);

        %% Dispatch to the local.
        _ ->
          Trace andalso begin
            elixir_env:trace({local_macro, Meta, Name, Arity}, E),
            elixir_locals:record_local(Tuple, Module, ?key(E, function), Meta, true)
          end,
          {macro, Module, expander_macro_fun(Meta, Local, Module, Name, E)}
      end
  end.

do_expand_import(Result, Meta, Name, Arity, Module, E, Trace) ->
  case Result of
    {function, Receiver} ->
      Trace andalso begin
        elixir_env:trace({imported_function, Meta, Receiver, Name, Arity}, E),
        elixir_locals:record_import({Name, Arity}, Receiver, Module, ?key(E, function))
      end,
      {function, Receiver, Name};
    {macro, Receiver} ->
      Trace andalso begin
        elixir_env:trace({imported_macro, Meta, Receiver, Name, Arity}, E),
        elixir_locals:record_import({Name, Arity}, Receiver, Module, ?key(E, function))
      end,
      {macro, Receiver, expander_macro_named(Meta, Receiver, Name, Arity, E)};
    {import, Receiver} ->
      case expand_require(true, Meta, Receiver, Name, Arity, E, Trace) of
        {macro, _, _} = Response -> Response;
        error -> {function, Receiver, Name}
      end;
    false when Module == ?kernel ->
      case elixir_rewrite:inline(Module, Name, Arity) of
        {AR, AN} -> {function, AR, AN};
        false -> not_found
      end;
    false ->
      not_found
  end.

expand_require(Meta, Receiver, Name, Arity, E, Trace) ->
  Required = (Receiver == ?key(E, module))
    orelse (lists:keyfind(required, 1, Meta) == {required, true})
    orelse is_element(Receiver, ?key(E, requires)),

  expand_require(Required, Meta, Receiver, Name, Arity, E, Trace).

expand_require(Required, Meta, Receiver, Name, Arity, E, Trace) ->
  case is_macro(Name, Arity, Receiver, Required) of
    true ->
      Trace andalso elixir_env:trace({remote_macro, Meta, Receiver, Name, Arity}, E),
      {macro, Receiver, expander_macro_named(Meta, Receiver, Name, Arity, E)};
    false ->
      error
  end.

%% Expansion helpers

expander_macro_fun(Meta, Fun, Receiver, Name, E) ->
  fun(Args, Caller) -> expand_macro_fun(Meta, Fun, Receiver, Name, Args, Caller, E) end.

expander_macro_named(Meta, Receiver, Name, Arity, E) ->
  ProperName  = elixir_utils:macro_name(Name),
  ProperArity = Arity + 1,
  Fun         = fun Receiver:ProperName/ProperArity,
  fun(Args, Caller) -> expand_macro_fun(Meta, Fun, Receiver, Name, Args, Caller, E) end.

expand_macro_fun(Meta, Fun, Receiver, Name, Args, Caller, E) ->
  try
    apply(Fun, [Caller | Args])
  catch
    Kind:Reason:Stacktrace ->
      Arity = length(Args),
      MFA  = {Receiver, elixir_utils:macro_name(Name), Arity+1},
      Info = [{Receiver, Name, Arity, [{file, "expanding macro"}]}, caller(?line(Meta), E)],
      erlang:raise(Kind, Reason, prune_stacktrace(Stacktrace, MFA, Info, {ok, Caller}))
  end.

expand_quoted(Meta, Receiver, Name, Arity, Quoted, S, E) ->
  Next = elixir_module:next_counter(?key(E, module)),

  try
    ToExpand = elixir_quote:linify_with_context_counter(Meta, {Receiver, Next}, Quoted),
    elixir_expand:expand(ToExpand, S, E)
  catch
    Kind:Reason:Stacktrace ->
      MFA  = {Receiver, elixir_utils:macro_name(Name), Arity+1},
      Info = [{Receiver, Name, Arity, [{file, "expanding macro"}]}, caller(?line(Meta), E)],
      erlang:raise(Kind, Reason, prune_stacktrace(Stacktrace, MFA, Info, error))
  end.

caller(Line, E) ->
  elixir_utils:caller(Line, ?key(E, file), ?key(E, module), ?key(E, function)).

%% Helpers

find_imports_by_name([{Mod, Imports} | ModImports], Acc, Name, Meta, E) ->
  NewAcc = find_imports_by_name(Name, Imports, Acc, Mod, Meta, E),
  find_imports_by_name(ModImports, NewAcc, Name, Meta, E);
find_imports_by_name([], Acc, _Name, _Meta, _E) ->
  Acc.

find_imports_by_name(Name, [{Name, Arity} | Imports], Acc, Mod, Meta, E) ->
  case Acc of
    #{Arity := OtherMod} ->
      Error = {import, {ambiguous, [Mod, OtherMod]}, Name, Arity},
      elixir_errors:file_error(Meta, E, ?MODULE, Error);

    #{} ->
      find_imports_by_name(Name, Imports, Acc#{Arity => Mod}, Mod, Meta, E)
  end;
find_imports_by_name(Name, [{ImportName, _} | Imports], Acc, Mod, Meta, E) when Name > ImportName ->
  find_imports_by_name(Name, Imports, Acc, Mod, Meta, E);
find_imports_by_name(_Name, _Imports, Acc, _Mod, _Meta, _E) ->
  Acc.

find_import_by_name_arity(Meta, {_Name, Arity} = Tuple, Extra, E) ->
  case is_import(Meta, Arity) of
    {import, _} = Import ->
      Import;
    false ->
      Funs = ?key(E, functions),
      Macs = Extra ++ ?key(E, macros),
      FunMatch = find_import_by_name_arity(Tuple, Funs),
      MacMatch = find_import_by_name_arity(Tuple, Macs),

      case {FunMatch, MacMatch} of
        {[], [Receiver]} -> {macro, Receiver};
        {[Receiver], []} -> {function, Receiver};
        {[], []} -> false;
        _ -> {ambiguous, FunMatch ++ MacMatch}
      end
  end.

find_import_by_name_arity(Tuple, List) ->
  [Receiver || {Receiver, Set} <- List, is_element(Tuple, Set)].

is_import(Meta, Arity) ->
  case lists:keyfind(imports, 1, Meta) of
    {imports, [_ | _] = Imports} ->
      case lists:keyfind(context, 1, Meta) of
        {context, _} ->
          case lists:keyfind(Arity, 1, Imports) of
            {Arity, Receiver} -> {import, Receiver};
            false -> false
          end;
        false -> false
      end;
    _ -> false
  end.

% %% We've reached the macro wrapper fun, skip it with the rest
prune_stacktrace([{_, _, [Caller | _], _} | _], _MFA, Info, {ok, Caller}) ->
  Info;
%% We've reached the invoked macro, skip it
prune_stacktrace([{M, F, A, _} | _], {M, F, A}, Info, _E) ->
  Info;
%% We've reached the elixir_dispatch internals, skip it with the rest
prune_stacktrace([{Mod, _, _, _} | _], _MFA, Info, _E) when Mod == elixir_dispatch; Mod == elixir_exp ->
  Info;
prune_stacktrace([H | T], MFA, Info, E) ->
  [H | prune_stacktrace(T, MFA, Info, E)];
prune_stacktrace([], _MFA, Info, _E) ->
  Info.

%% ERROR HANDLING

format_error({import, {conflict, Receiver}, Name, Arity}) ->
  io_lib:format("call to local macro ~ts/~B conflicts with imported ~ts.~ts/~B, "
    "please rename the local macro or remove the conflicting import",
    [Name, Arity, elixir_aliases:inspect(Receiver), Name, Arity]);
format_error({import, {ambiguous, [Mod1, Mod2 | _]}, Name, Arity}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call is ambiguous",
    [Name, Arity, elixir_aliases:inspect(Mod1), elixir_aliases:inspect(Mod2)]);
format_error({compile_env, Name, Arity}) ->
  io_lib:format("Application.~s/~B is discouraged in the module body, use Application.compile_env/3 instead", [Name, Arity]);
format_error({deprecated, Mod, '__using__', 1, Message}) ->
  io_lib:format("use ~s is deprecated. ~s", [elixir_aliases:inspect(Mod), Message]);
format_error({deprecated, Mod, Fun, Arity, Message}) ->
  io_lib:format("~s.~s/~B is deprecated. ~s",[elixir_aliases:inspect(Mod), Fun, Arity, Message]).

%% INTROSPECTION

is_macro(_Name, _Arity, _Module, false) ->
  false;
is_macro(Name, Arity, Receiver, true) ->
  try Receiver:'__info__'(macros) of
    Macros -> is_element({Name, Arity}, Macros)
  catch
    error:_ -> false
  end.

%% Deprecations checks only happen at the module body,
%% so in there we can try to at least load the module.
get_deprecations(Receiver) ->
  case code:ensure_loaded(Receiver) of
    {module, Receiver} -> get_info(Receiver, deprecated);
    _ -> []
  end.

get_info(Receiver, Key) ->
  case erlang:function_exported(Receiver, '__info__', 1) of
    true ->
      try
        Receiver:'__info__'(Key)
      catch
        error:_ -> []
      end;
    false ->
      []
  end.

elixir_imported_functions() ->
  try
    ?kernel:'__info__'(functions)
  catch
    error:undef -> []
  end.

elixir_imported_macros() ->
  try
    ?kernel:'__info__'(macros)
  catch
    error:undef -> []
  end.

check_deprecated(_, _, erlang, _, _, _) -> ok;
check_deprecated(_, _, elixir_def, _, _, _) -> ok;
check_deprecated(_, _, elixir_module, _, _, _) -> ok;
check_deprecated(_, _, ?kernel, _, _, _) -> ok;
check_deprecated(Kind, Meta, ?application, Name, Arity, E) ->
  case E of
    #{module := Module, function := nil}
    when (Module /= nil) or (Kind == macro), (Name == get_env) orelse (Name == fetch_env) orelse (Name == 'fetch_env!') ->
      elixir_errors:file_warn(Meta, E, ?MODULE, {compile_env, Name, Arity});

    _ ->
      ok
  end;
check_deprecated(Kind, Meta, Receiver, Name, Arity, E) ->
  %% Any compile time behavior cannot be verified by the runtime group pass.
  case ((?key(E, function) == nil) or (Kind == macro)) andalso get_deprecations(Receiver) of
    [_ | _] = Deprecations ->
      case lists:keyfind({Name, Arity}, 1, Deprecations) of
        {_, Message} ->
          elixir_errors:file_warn(Meta, E, ?MODULE, {deprecated, Receiver, Name, Arity, Message});

        false ->
          false
      end;

    _ ->
      ok
  end.
