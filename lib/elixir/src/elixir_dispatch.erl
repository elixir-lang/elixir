%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([dispatch_import/5, dispatch_require/6,
  require_function/5, import_function/4,
  expand_import/6, expand_require/5,
  default_functions/0, default_macros/0, default_requires/0,
  find_import/4, format_error/1]).
-include("elixir.hrl").
-import(ordsets, [is_element/2]).
-define(kernel, 'Elixir.Kernel').

default_functions() ->
  [{?kernel, elixir_imported_functions()}].
default_macros() ->
  [{?kernel, elixir_imported_macros()}].
default_requires() ->
  ['Elixir.Kernel', 'Elixir.Kernel.Typespec'].

%% This is used by elixir_quote. Note we don't record the
%% import locally because at that point there is no
%% ambiguity.
find_import(Meta, Name, Arity, E) ->
  Tuple = {Name, Arity},

  case find_dispatch(Meta, Tuple, [], E) of
    {function, Receiver} ->
      elixir_lexical:record_import(Receiver, Name, Arity, ?m(E, function), ?line(Meta), ?m(E, lexical_tracker)),
      %% elixir_locals:record_import(Tuple, Receiver, ?m(E, module), ?m(E, function)),
      Receiver;
    {macro, Receiver} ->
      elixir_lexical:record_import(Receiver, Name, Arity, nil, ?line(Meta), ?m(E, lexical_tracker)),
      %% elixir_locals:record_import(Tuple, Receiver, ?m(E, module), ?m(E, function)),
      Receiver;
    _ ->
      false
  end.

%% Function retrieval

import_function(Meta, Name, Arity, E) ->
  Tuple = {Name, Arity},
  case find_dispatch(Meta, Tuple, [], E) of
    {function, Receiver} ->
      elixir_lexical:record_import(Receiver, Name, Arity, ?m(E, function), ?line(Meta), ?m(E, lexical_tracker)),
      elixir_locals:record_import(Tuple, Receiver, ?m(E, module), ?m(E, function)),
      remote_function(Meta, Receiver, Name, Arity, E);
    {macro, _Receiver} ->
      false;
    {import, Receiver} ->
      require_function(Meta, Receiver, Name, Arity, E);
    false ->
      case elixir_import:special_form(Name, Arity) of
        true  -> false;
        false ->
          elixir_locals:record_local(Tuple, ?m(E, module), ?m(E, function)),
          {local, Name, Arity}
      end
  end.

require_function(Meta, Receiver, Name, Arity, E) ->
  Required = is_element(Receiver, ?m(E, requires)),
  case is_element({Name, Arity}, get_macros(Receiver, Required)) of
    true  -> false;
    false ->
      elixir_lexical:record_remote(Receiver, ?m(E, function), ?m(E, lexical_tracker)),
      remote_function(Meta, Receiver, Name, Arity, E)
  end.

remote_function(Meta, Receiver, Name, Arity, E) ->
  check_deprecation(Meta, Receiver, Name, Arity, E),

  case elixir_rewrite:inline(Receiver, Name, Arity) of
    {AR, AN} -> {remote, AR, AN, Arity};
    false    -> {remote, Receiver, Name, Arity}
  end.

%% Dispatches

dispatch_import(Meta, Name, Args, E, Callback) ->
  Arity = length(Args),
  case expand_import(Meta, {Name, Arity}, Args, E, [], false) of
    {ok, Receiver, Quoted} ->
      expand_quoted(Meta, Receiver, Name, Arity, Quoted, E);
    {ok, Receiver, NewName, NewArgs} ->
      elixir_exp:expand({{'.', [], [Receiver, NewName]}, Meta, NewArgs}, E);
    error ->
      Callback()
  end.

dispatch_require(Meta, Receiver, Name, Args, E, Callback) when is_atom(Receiver) ->
  Arity = length(Args),

  case elixir_rewrite:inline(Receiver, Name, Arity) of
    {AR, AN} ->
      Callback(AR, AN, Args);
    false ->
      case expand_require(Meta, Receiver, {Name, Arity}, Args, E) of
        {ok, Receiver, Quoted} -> expand_quoted(Meta, Receiver, Name, Arity, Quoted, E);
        error -> Callback(Receiver, Name, Args)
      end
  end;

dispatch_require(_Meta, Receiver, Name, Args, _E, Callback) ->
  Callback(Receiver, Name, Args).

%% Macros expansion

expand_import(Meta, {Name, Arity} = Tuple, Args, E, Extra, External) ->
  Module      = ?m(E, module),
  Dispatch    = find_dispatch(Meta, Tuple, Extra, E),
  Function    = ?m(E, function),
  AllowLocals = External orelse ((Function /= nil) andalso (Function /= Tuple)),
  Local       = AllowLocals andalso elixir_locals:macro_for(Module, Name, Arity),

  case Dispatch of
    %% In case it is an import, we dispatch the import.
    {import, _} ->
      do_expand_import(Meta, Tuple, Args, Module, E, Dispatch);

    %% There is a local and an import. This is a conflict unless
    %% the receiver is the same as module (happens on bootstrap).
    {_, Receiver} when Local /= false, Receiver /= Module ->
      Error = {macro_conflict, {Receiver, Name, Arity}},
      elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, Error);

    %% There is no local. Dispatch the import.
    _ when Local == false ->
      do_expand_import(Meta, Tuple, Args, Module, E, Dispatch);

    %% Dispatch to the local.
    _ ->
      elixir_locals:record_local(Tuple, Module, Function),
      {ok, Module, expand_macro_fun(Meta, Local(), Module, Name, Args, E)}
  end.

do_expand_import(Meta, {Name, Arity} = Tuple, Args, Module, E, Result) ->
  case Result of
    {function, Receiver} ->
      elixir_lexical:record_import(Receiver, Name, Arity, ?m(E, function), ?line(Meta), ?m(E, lexical_tracker)),
      elixir_locals:record_import(Tuple, Receiver, Module, ?m(E, function)),
      {ok, Receiver, Name, Args};
    {macro, Receiver} ->
      check_deprecation(Meta, Receiver, Name, Arity, E),
      elixir_lexical:record_import(Receiver, Name, Arity, nil, ?line(Meta), ?m(E, lexical_tracker)),
      elixir_locals:record_import(Tuple, Receiver, Module, ?m(E, function)),
      {ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, E)};
    {import, Receiver} ->
      case expand_require([{required, true} | Meta], Receiver, Tuple, Args, E) of
        {ok, _, _} = Response -> Response;
        error -> {ok, Receiver, Name, Args}
      end;
    false when Module == ?kernel ->
      case elixir_rewrite:inline(Module, Name, Arity) of
        {AR, AN} -> {ok, AR, AN, Args};
        false -> error
      end;
    false ->
      error
  end.

expand_require(Meta, Receiver, {Name, Arity} = Tuple, Args, E) ->
  check_deprecation(Meta, Receiver, Name, Arity, E),
  Required = (Receiver == ?m(E, module)) orelse is_element(Receiver, ?m(E, requires)) orelse required(Meta),

  case is_element(Tuple, get_macros(Receiver, Required)) of
    true when Required ->
      elixir_lexical:record_remote(Receiver, Name, Arity, nil, ?line(Meta), ?m(E, lexical_tracker)),
      {ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, E)};
    true ->
      Info = {unrequired_module, {Receiver, Name, length(Args), ?m(E, requires)}},
      elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, Info);
    false ->
      error
  end.

%% Expansion helpers

expand_macro_fun(Meta, Fun, Receiver, Name, Args, E) ->
  Line = ?line(Meta),
  EArg = {Line, E},

  try
    apply(Fun, [EArg | Args])
  catch
    Kind:Reason ->
      Arity = length(Args),
      MFA  = {Receiver, elixir_utils:macro_name(Name), Arity+1},
      Info = [{Receiver, Name, Arity, [{file, "expanding macro"}]}, caller(Line, E)],
      erlang:raise(Kind, Reason, prune_stacktrace(erlang:get_stacktrace(), MFA, Info, EArg))
  end.

expand_macro_named(Meta, Receiver, Name, Arity, Args, E) ->
  ProperName  = elixir_utils:macro_name(Name),
  ProperArity = Arity + 1,
  Fun         = fun Receiver:ProperName/ProperArity,
  expand_macro_fun(Meta, Fun, Receiver, Name, Args, E).

expand_quoted(Meta, Receiver, Name, Arity, Quoted, E) ->
  Line = ?line(Meta),
  Next = erlang:unique_integer(),

  try
    elixir_exp:expand(
      elixir_quote:linify_with_context_counter(Line, {Receiver, Next}, Quoted),
      E)
  catch
    Kind:Reason ->
      MFA  = {Receiver, elixir_utils:macro_name(Name), Arity+1},
      Info = [{Receiver, Name, Arity, [{file, "expanding macro"}]}, caller(Line, E)],
      erlang:raise(Kind, Reason, prune_stacktrace(erlang:get_stacktrace(), MFA, Info, nil))
  end.

caller(Line, E) ->
  elixir_utils:caller(Line, ?m(E, file), ?m(E, module), ?m(E, function)).

%% Helpers

required(Meta) ->
  lists:keyfind(required, 1, Meta) == {required, true}.

find_dispatch(Meta, Tuple, Extra, E) ->
  case is_import(Meta) of
    {import, _} = Import ->
      Import;
    false ->
      Funs = ?m(E, functions),
      Macs = Extra ++ ?m(E, macros),
      FunMatch = find_dispatch(Tuple, Funs),
      MacMatch = find_dispatch(Tuple, Macs),

      case {FunMatch, MacMatch} of
        {[], [Receiver]} -> {macro, Receiver};
        {[Receiver], []} -> {function, Receiver};
        {[], []} -> false;
        _ ->
          {Name, Arity} = Tuple,
          [First, Second | _] = FunMatch ++ MacMatch,
          Error = {ambiguous_call, {First, Second, Name, Arity}},
          elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, Error)
      end
  end.

find_dispatch(Tuple, List) ->
  [Receiver || {Receiver, Set} <- List, is_element(Tuple, Set)].

is_import(Meta) ->
  case lists:keyfind(import, 1, Meta) of
    {import, _} = Import ->
      case lists:keyfind(context, 1, Meta) of
        {context, _} -> Import;
        false -> false
      end;
    false -> false
  end.

% %% We've reached the macro wrapper fun, skip it with the rest
prune_stacktrace([{_, _, [E | _], _} | _], _MFA, Info, E) ->
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

format_error({unrequired_module, {Receiver, Name, Arity, _Required}}) ->
  Module = elixir_aliases:inspect(Receiver),
  io_lib:format("you must require ~ts before invoking the macro ~ts.~ts/~B",
    [Module, Module, Name, Arity]);
format_error({macro_conflict, {Receiver, Name, Arity}}) ->
  io_lib:format("call to local macro ~ts/~B conflicts with imported ~ts.~ts/~B, "
    "please rename the local macro or remove the conflicting import",
    [Name, Arity, elixir_aliases:inspect(Receiver), Name, Arity]);
format_error({ambiguous_call, {Mod1, Mod2, Name, Arity}}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call is ambiguous",
    [Name, Arity, elixir_aliases:inspect(Mod1), elixir_aliases:inspect(Mod2)]).

%% INTROSPECTION

%% Do not try to get macros from Erlang. Speeds up compilation a bit.
get_macros(erlang, _) -> [];

get_macros(Receiver, false) ->
  case code:is_loaded(Receiver) of
    {file, _} ->
      try
        Receiver:'__info__'(macros)
      catch
        error:undef -> []
      end;
    false -> []
  end;

get_macros(Receiver, true) ->
  case code:ensure_loaded(Receiver) of
    {module, Receiver} ->
      try
        Receiver:'__info__'(macros)
      catch
        error:undef -> []
      end;
    {error, _} -> []
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

check_deprecation(Meta, Receiver, Name, Arity, #{file := File}) ->
  case deprecation(Receiver, Name, Arity) of
    false -> ok;
    Message ->
      Warning = deprecation_message(Receiver, Name, Arity, Message),
      elixir_errors:warn(?line(Meta), File, Warning)
  end.

deprecation_message(Receiver, '__using__', _Arity, Message) ->
  Warning = io_lib:format("use ~s is deprecated", [elixir_aliases:inspect(Receiver)]),
  deprecation_message(Warning, Message);

deprecation_message(Receiver, Name, Arity, Message) ->
  Warning = io_lib:format("~s.~s/~B is deprecated",
                          [elixir_aliases:inspect(Receiver), Name, Arity]),
  deprecation_message(Warning, Message).

deprecation_message(Warning, Message) ->
  case Message of
    true -> Warning;
    Message -> Warning ++ ", " ++ Message
  end.

deprecation('Elixir.HashDict', _, _) ->
  "use maps and the Map module instead";
deprecation('Elixir.HashSet', _, _) ->
  "use the MapSet module instead";
deprecation('Elixir.Dict', _, _) ->
  "use the Map module for working with maps or the Keyword module for working with keyword lists";
deprecation('Elixir.Set', _, _) ->
  "use the MapSet module for working with sets";

deprecation(_, _, _) ->
  false.
