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

-define(atom, 'Elixir.Atom').
-define(float, 'Elixir.Float').
-define(io, 'Elixir.IO').
-define(integer, 'Elixir.Integer').
-define(kernel, 'Elixir.Kernel').
-define(list, 'Elixir.List').
-define(map, 'Elixir.Map').
-define(node, 'Elixir.Node').
-define(port, 'Elixir.Port').
-define(process, 'Elixir.Process').
-define(string, 'Elixir.String').
-define(system, 'Elixir.System').
-define(tuple, 'Elixir.Tuple').

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
      elixir_lexical:record_import(Receiver, ?m(E, lexical_tracker)),
      %% elixir_locals:record_import(Tuple, Receiver, ?m(E, module), ?m(E, function)),
      Receiver;
    {macro, Receiver} ->
      elixir_lexical:record_import(Receiver, ?m(E, lexical_tracker)),
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
      elixir_lexical:record_import(Receiver, ?m(E, lexical_tracker)),
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
  case is_element({Name, Arity}, get_optional_macros(Receiver)) of
    true  -> false;
    false -> remote_function(Meta, Receiver, Name, Arity, E)
  end.

remote_function(Meta, Receiver, Name, Arity, E) ->
  check_deprecation(Meta, Receiver, Name, Arity, E),

  elixir_lexical:record_remote(Receiver, ?m(E, lexical_tracker)),
  case inline(Receiver, Name, Arity) of
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

  case rewrite(Receiver, Name, Args, Arity) of
    {ok, AR, AN, AA} ->
      Callback(AR, AN, AA);
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
      elixir_lexical:record_import(Receiver, ?m(E, lexical_tracker)),
      elixir_locals:record_import(Tuple, Receiver, Module, ?m(E, function)),

      case rewrite(Receiver, Name, Args, Arity) of
        {ok, _, _, _} = Res -> Res;
        false -> {ok, Receiver, Name, Args}
      end;
    {macro, Receiver} ->
      check_deprecation(Meta, Receiver, Name, Arity, E),
      elixir_lexical:record_import(Receiver, ?m(E, lexical_tracker)),
      elixir_locals:record_import(Tuple, Receiver, Module, ?m(E, function)),
      {ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, E)};
    {import, Receiver} ->
      case expand_require([{require,false}|Meta], Receiver, Tuple, Args, E) of
        {ok, _, _} = Response -> Response;
        error -> {ok, Receiver, Name, Args}
      end;
    false when Module == ?kernel ->
      case rewrite(Module, Name, Args, Arity) of
        {ok, _, _, _} = Res -> Res;
        false -> error
      end;
    false ->
      error
  end.

expand_require(Meta, Receiver, {Name, Arity} = Tuple, Args, E) ->
  check_deprecation(Meta, Receiver, Name, Arity, E),
  Module = ?m(E, module),

  case is_element(Tuple, get_optional_macros(Receiver)) of
    true ->
      Requires = ?m(E, requires),
      case (Receiver == Module) orelse is_element(Receiver, Requires) orelse skip_require(Meta) of
        true  ->
          elixir_lexical:record_remote(Receiver, ?m(E, lexical_tracker)),
          {ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, E)};
        false ->
          Info = {unrequired_module, {Receiver, Name, length(Args), Requires}},
          elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, Info)
      end;
    false ->
      error
  end.

%% Expansion helpers

expand_macro_fun(Meta, Fun, Receiver, Name, Args, E) ->
  Line = ?line(Meta),
  EArg = {Line, E},

  try
    apply(Fun, [EArg|Args])
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
  Next = elixir_counter:next(),

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

caller(Line, #{module := nil} = E) ->
  {elixir_compiler, '__FILE__', 2, location(Line, E)};
caller(Line, #{module := Module, function := nil} = E) ->
  {Module, '__MODULE__', 0, location(Line, E)};
caller(Line, #{module := Module, function := {Name, Arity}} = E) ->
  {Module, Name, Arity, location(Line, E)}.

location(Line, E) ->
  [{file, elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(?m(E, file)))},
   {line, Line}].

%% Helpers

skip_require(Meta) ->
  lists:keyfind(require, 1, Meta) == {require, false}.

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
          [First, Second|_] = FunMatch ++ MacMatch,
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
        false ->
          false
      end;
    false ->
      false
  end.

% %% We've reached the macro wrapper fun, skip it with the rest
prune_stacktrace([{_, _, [E|_], _}|_], _MFA, Info, E) ->
  Info;
%% We've reached the invoked macro, skip it
prune_stacktrace([{M, F, A, _}|_], {M, F, A}, Info, _E) ->
  Info;
%% We've reached the elixir_dispatch internals, skip it with the rest
prune_stacktrace([{Mod, _, _, _}|_], _MFA, Info, _E) when Mod == elixir_dispatch; Mod == elixir_exp ->
  Info;
prune_stacktrace([H|T], MFA, Info, E) ->
  [H|prune_stacktrace(T, MFA, Info, E)];
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
get_optional_macros(erlang) -> [];

get_optional_macros(Receiver) ->
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

rewrite(?atom, to_string, [Arg], _) ->
  {ok, erlang, atom_to_binary, [Arg, utf8]};
rewrite(?kernel, elem, [Tuple, Index], _) ->
  {ok, erlang, element, [increment(Index), Tuple]};
rewrite(?kernel, put_elem, [Tuple, Index, Value], _) ->
  {ok, erlang, setelement, [increment(Index), Tuple, Value]};
rewrite(?map, 'has_key?', [Map, Key], _) ->
  {ok, maps, is_key, [Key, Map]};
rewrite(?map, fetch, [Map, Key], _) ->
  {ok, maps, find, [Key, Map]};
rewrite(?map, put, [Map, Key, Value], _) ->
  {ok, maps, put, [Key, Value, Map]};
rewrite(?map, delete, [Map, Key], _) ->
  {ok, maps, remove, [Key, Map]};
rewrite(?process, monitor, [Arg], _) ->
  {ok, erlang, monitor, [process, Arg]};
rewrite(?string, to_atom, [Arg], _) ->
  {ok, erlang, binary_to_atom, [Arg, utf8]};
rewrite(?string, to_existing_atom, [Arg], _) ->
  {ok, erlang, binary_to_existing_atom, [Arg, utf8]};
rewrite(?tuple, insert_at, [Tuple, Index, Term], _) ->
  {ok, erlang, insert_element, [increment(Index), Tuple, Term]};
rewrite(?tuple, delete_at, [Tuple, Index], _) ->
  {ok, erlang, delete_element, [increment(Index), Tuple]};
rewrite(?tuple, duplicate, [Data, Size], _) ->
  {ok, erlang, make_tuple, [Size, Data]};

rewrite(Receiver, Name, Args, Arity) ->
  case inline(Receiver, Name, Arity) of
    {AR, AN} -> {ok, AR, AN, Args};
    false    -> false
  end.

increment(Number) when is_number(Number) ->
  Number + 1;
increment(Other) ->
  {{'.', [], [erlang, '+']}, [], [Other, 1]}.

inline(?atom, to_char_list, 1) -> {erlang, atom_to_list};
inline(?io, iodata_length, 1) -> {erlang, iolist_size};
inline(?io, iodata_to_binary, 1) -> {erlang, iolist_to_binary};
inline(?integer, to_string, 1) -> {erlang, integer_to_binary};
inline(?integer, to_string, 2) -> {erlang, integer_to_binary};
inline(?integer, to_char_list, 1) -> {erlang, integer_to_list};
inline(?integer, to_char_list, 2) -> {erlang, integer_to_list};
inline(?float, to_string, 1) -> {erlang, float_to_binary};
inline(?float, to_char_list, 1) -> {erlang, float_to_list};
inline(?list, to_atom, 1) -> {erlang, list_to_atom};
inline(?list, to_existing_atom, 1) -> {erlang, list_to_existing_atom};
inline(?list, to_float, 1) -> {erlang, list_to_float};
inline(?list, to_integer, 1) -> {erlang, list_to_integer};
inline(?list, to_integer, 2) -> {erlang, list_to_integer};
inline(?list, to_tuple, 1) -> {erlang, list_to_tuple};

inline(?kernel, '+', 2) -> {erlang, '+'};
inline(?kernel, '-', 2) -> {erlang, '-'};
inline(?kernel, '+', 1) -> {erlang, '+'};
inline(?kernel, '-', 1) -> {erlang, '-'};
inline(?kernel, '*', 2) -> {erlang, '*'};
inline(?kernel, '/', 2) -> {erlang, '/'};
inline(?kernel, '++', 2) -> {erlang, '++'};
inline(?kernel, '--', 2) -> {erlang, '--'};
inline(?kernel, 'not', 1) -> {erlang, 'not'};
inline(?kernel, '<', 2) -> {erlang, '<'};
inline(?kernel, '>', 2) -> {erlang, '>'};
inline(?kernel, '<=', 2) -> {erlang, '=<'};
inline(?kernel, '>=', 2) -> {erlang, '>='};
inline(?kernel, '==', 2) -> {erlang, '=='};
inline(?kernel, '!=', 2) -> {erlang, '/='};
inline(?kernel, '===', 2) -> {erlang, '=:='};
inline(?kernel, '!==', 2) -> {erlang, '=/='};
inline(?kernel, abs, 1) -> {erlang, abs};
inline(?kernel, apply, 2) -> {erlang, apply};
inline(?kernel, apply, 3) -> {erlang, apply};
inline(?kernel, binary_part, 3) -> {erlang, binary_part};
inline(?kernel, bit_size, 1) -> {erlang, bit_size};
inline(?kernel, byte_size, 1) -> {erlang, byte_size};
inline(?kernel, 'div', 2) -> {erlang, 'div'};
inline(?kernel, exit, 1) -> {erlang, exit};
inline(?kernel, hd, 1) -> {erlang, hd};
inline(?kernel, is_atom, 1) -> {erlang, is_atom};
inline(?kernel, is_binary, 1) -> {erlang, is_binary};
inline(?kernel, is_bitstring, 1) -> {erlang, is_bitstring};
inline(?kernel, is_boolean, 1) -> {erlang, is_boolean};
inline(?kernel, is_float, 1) -> {erlang, is_float};
inline(?kernel, is_function, 1) -> {erlang, is_function};
inline(?kernel, is_function, 2) -> {erlang, is_function};
inline(?kernel, is_integer, 1) -> {erlang, is_integer};
inline(?kernel, is_list, 1) -> {erlang, is_list};
inline(?kernel, is_map, 1) -> {erlang, is_map};
inline(?kernel, is_number, 1) -> {erlang, is_number};
inline(?kernel, is_pid, 1) -> {erlang, is_pid};
inline(?kernel, is_port, 1) -> {erlang, is_port};
inline(?kernel, is_reference, 1) -> {erlang, is_reference};
inline(?kernel, is_tuple, 1) -> {erlang, is_tuple};
inline(?kernel, length, 1) -> {erlang, length};
inline(?kernel, make_ref, 0) -> {erlang, make_ref};
inline(?kernel, map_size, 1) -> {erlang, map_size};
inline(?kernel, max, 2) -> {erlang, max};
inline(?kernel, min, 2) -> {erlang, min};
inline(?kernel, node, 0) -> {erlang, node};
inline(?kernel, node, 1) -> {erlang, node};
inline(?kernel, 'rem', 2) -> {erlang, 'rem'};
inline(?kernel, round, 1) -> {erlang, round};
inline(?kernel, self, 0) -> {erlang, self};
inline(?kernel, send, 2) -> {erlang, send};
inline(?kernel, spawn, 1) -> {erlang, spawn};
inline(?kernel, spawn, 3) -> {erlang, spawn};
inline(?kernel, spawn_link, 1) -> {erlang, spawn_link};
inline(?kernel, spawn_link, 3) -> {erlang, spawn_link};
inline(?kernel, spawn_monitor, 1) -> {erlang, spawn_monitor};
inline(?kernel, spawn_monitor, 3) -> {erlang, spawn_monitor};
inline(?kernel, throw, 1) -> {erlang, throw};
inline(?kernel, tl, 1) -> {erlang, tl};
inline(?kernel, trunc, 1) -> {erlang, trunc};
inline(?kernel, tuple_size, 1) -> {erlang, tuple_size};

inline(?map, keys, 1) -> {maps, keys};
inline(?map, merge, 2) -> {maps, merge};
inline(?map, size, 1) -> {maps, size};
inline(?map, values, 1) -> {maps, values};
inline(?map, to_list, 1) -> {maps, to_list};

inline(?node, spawn, 2) -> {erlang, spawn};
inline(?node, spawn, 3) -> {erlang, spawn_opt};
inline(?node, spawn, 4) -> {erlang, spawn};
inline(?node, spawn, 5) -> {erlang, spawn_opt};
inline(?node, spawn_link, 2) -> {erlang, spawn_link};
inline(?node, spawn_link, 4) -> {erlang, spawn_link};

inline(?process, exit, 2) -> {erlang, exit};
inline(?process, spawn, 2) -> {erlang, spawn_opt};
inline(?process, spawn, 4) -> {erlang, spawn_opt};
inline(?process, demonitor, 1) -> {erlang, demonitor};
inline(?process, demonitor, 2) -> {erlang, demonitor};
inline(?process, link, 1) -> {erlang, link};
inline(?process, unlink, 1) -> {erlang, unlink};

inline(?port, open, 2) -> {erlang, open_port};
inline(?port, call, 3) -> {erlang, port_call};
inline(?port, close, 1) -> {erlang, port_close};
inline(?port, command, 2) -> {erlang, port_command};
inline(?port, command, 3) -> {erlang, port_command};
inline(?port, connect, 2) -> {erlang, port_connect};
inline(?port, control, 3) -> {erlang, port_control};
inline(?port, info, 1) -> {erlang, port_info};
inline(?port, info, 2) -> {erlang, port_info};
inline(?port, list, 0) -> {erlang, ports};

inline(?string, to_float, 1) -> {erlang, binary_to_float};
inline(?string, to_integer, 1) -> {erlang, binary_to_integer};
inline(?string, to_integer, 2) -> {erlang, binary_to_integer};
inline(?system, stacktrace, 0) -> {erlang, get_stacktrace};
inline(?tuple, to_list, 1) -> {erlang, tuple_to_list};

inline(_, _, _) -> false.

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

deprecation(_, _, _) ->
  false.
