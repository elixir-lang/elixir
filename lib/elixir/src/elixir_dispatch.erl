%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([dispatch_import/5, dispatch_require/6,
  require_function/5, import_function/4,
  expand_import/5, expand_require/5,
  default_functions/0, default_macros/0, default_requires/0,
  find_import/4, format_error/1]).
-include("elixir.hrl").
-import(ordsets, [is_element/2]).

%% Module macros
-define(kernel, 'Elixir.Kernel').
-define(node, 'Elixir.Node').
-define(process, 'Elixir.Process').
-define(system, 'Elixir.System').

default_functions() ->
  [ { ?kernel, elixir_imported_functions() } ].
default_macros() ->
  [ { ?kernel, elixir_imported_macros() } ].
default_requires() ->
  [ 'Elixir.Kernel', 'Elixir.Kernel.Typespec' ].

find_import(Meta, Name, Arity, E) ->
  Tuple = { Name, Arity },

  case find_dispatch(Meta, Tuple, [], E) of
    { function, Receiver } ->
      elixir_lexical:record_import(Receiver, E#elixir_env.lexical_tracker),
      elixir_locals:record_import(Tuple, Receiver, E#elixir_env.module, E#elixir_env.function),
      Receiver;
    { macro, Receiver } ->
      elixir_lexical:record_import(Receiver, E#elixir_env.lexical_tracker),
      elixir_locals:record_import(Tuple, Receiver, E#elixir_env.module, E#elixir_env.function),
      Receiver;
    _ ->
      false
  end.

%% Function retrieval

import_function(Meta, Name, Arity, E) ->
  Tuple = { Name, Arity },
  case find_dispatch(Meta, Tuple, [], E) of
    { function, Receiver } ->
      elixir_lexical:record_import(Receiver, E#elixir_env.lexical_tracker),
      elixir_locals:record_import(Tuple, Receiver, E#elixir_env.module, E#elixir_env.function),
      remote_function(Receiver, Name, Arity, E);
    { macro, _Receiver } ->
      false;
    { import, Receiver } ->
      require_function(Meta, Receiver, Name, Arity, E);
    false ->
      case elixir_import:special_form(Name, Arity) of
        true  -> false;
        false ->
          elixir_locals:record_local(Tuple, E#elixir_env.module, E#elixir_env.function),
          { local, Name, Arity }
      end
  end.

require_function(_Meta, Receiver, Name, Arity, E) ->
  case is_element({ Name, Arity }, get_optional_macros(Receiver)) of
    true  -> false;
    false -> remote_function(Receiver, Name, Arity, E)
  end.

remote_function(Receiver, Name, Arity, E) ->
  elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
  case inline(Receiver, Name, Arity) of
    { AR, AN } -> { remote, AR, AN, Arity };
    false      -> { remote, Receiver, Name, Arity }
  end.

%% Dispatches

dispatch_import(Meta, Name, Args, E, Callback) ->
  Arity = length(Args),
  case expand_import(Meta, { Name, Arity }, Args, E, []) of
    { ok, Receiver, Quoted } ->
      expand_quoted(Meta, Receiver, Name, Arity, Quoted, E);
    { ok, Receiver } ->
      elixir_exp:expand({ { '.', [], [Receiver, Name] }, Meta, Args }, E);
    error ->
      Callback()
  end.

dispatch_require(Meta, Receiver, Name, Args, E, Callback) when is_atom(Receiver) ->
  Arity = length(Args),

  case rewrite(Receiver, Name, Args, Arity) of
    { ok, AR, AN, AA } ->
      Callback(AR, AN, AA);
    false ->
      case expand_require(Meta, Receiver, { Name, Arity }, Args, E) of
        { ok, Receiver, Quoted } -> expand_quoted(Meta, Receiver, Name, Arity, Quoted, E);
        error -> Callback(Receiver, Name, Args)
      end
  end;

dispatch_require(_Meta, Receiver, Name, Args, _E, Callback) ->
  Callback(Receiver, Name, Args).

%% Macros expansion

expand_import(Meta, { Name, Arity } = Tuple, Args, E, Extra) ->
  Module   = E#elixir_env.module,
  Dispatch = find_dispatch(Meta, Tuple, Extra, E),
  Function = E#elixir_env.function,
  Local    = (Function /= nil) andalso (Function /= Tuple) andalso
              elixir_locals:macro_for(Module, Name, Arity),

  case Dispatch of
    %% In case it is an import, we dispatch the import.
    { import, _ } ->
      do_expand_import(Meta, Tuple, Args, Module, E, Dispatch);

    %% There is a local and an import. This is a conflict unless
    %% the receiver is the same as module (happens on bootstrap).
    { _, Receiver } when Local /= false, Receiver /= Module ->
      Error = { macro_conflict, { Receiver, Name, Arity } },
      elixir_errors:form_error(Meta, E#elixir_env.file, ?MODULE, Error);

    %% There is no local. Dispatch the import.
    _ when Local == false ->
      do_expand_import(Meta, Tuple, Args, Module, E, Dispatch);

    %% Dispatch to the local.
    _ ->
      elixir_locals:record_local(Tuple, Module, Function),
      { ok, Module, expand_macro_fun(Meta, Local(), Module, Name, Args, E) }
  end.

do_expand_import(Meta, { Name, Arity } = Tuple, Args, Module, E, Result) ->
  case Result of
    { function, Receiver } ->
      elixir_lexical:record_import(Receiver, E#elixir_env.lexical_tracker),
      elixir_locals:record_import(Tuple, Receiver, Module, E#elixir_env.function),
      case rewrite(Receiver, Name, Args, Arity) of
        { ok, AR, AN, AA } ->
          { ok, AR, { { '.', [], [AR, AN] }, [], AA } };
        false ->
          { ok, Receiver }
      end;
    { macro, Receiver } ->
      elixir_lexical:record_import(Receiver, E#elixir_env.lexical_tracker),
      elixir_locals:record_import(Tuple, Receiver, Module, E#elixir_env.function),
      { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, E) };
    { import, Receiver } ->
      case expand_require([{require,false}|Meta], Receiver, Tuple, Args, E) of
        { ok, _, _ } = Response -> Response;
        error -> { ok, Receiver }
      end;
    false when Module == ?kernel ->
      case rewrite(Module, Name, Args, Arity) of
        { ok, AR, AN, AA } ->
          { ok, AR, { { '.', [], [AR, AN] }, [], AA } };
        false ->
          error
      end;
    false ->
      error
  end.

expand_require(Meta, Receiver, { Name, Arity } = Tuple, Args, E) ->
  Module = E#elixir_env.module,

  case is_element(Tuple, get_optional_macros(Receiver)) of
    true ->
      Requires = E#elixir_env.requires,
      case (Receiver == Module) orelse is_element(Receiver, Requires) orelse skip_require(Meta) of
        true  ->
          elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
          { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, E) };
        false ->
          Info = { unrequired_module, { Receiver, Name, length(Args), Requires } },
          elixir_errors:form_error(Meta, E#elixir_env.file, ?MODULE, Info)
      end;
    false ->
      error
  end.

%% Expansion helpers

expand_macro_fun(Meta, Fun, Receiver, Name, Args, E) ->
  Line = ?line(Meta),
  EArg = { Line, E },

  try
    apply(Fun, [EArg|Args])
  catch
    Kind:Reason ->
      Info = [{ Receiver, Name, length(Args), location(Line, E) }, mfa(Line, E)],
      erlang:raise(Kind, Reason, prune_stacktrace(Info, erlang:get_stacktrace(), EArg))
  end.

expand_macro_named(Meta, Receiver, Name, Arity, Args, E) ->
  ProperName  = ?elixir_macro(Name),
  ProperArity = Arity + 1,
  Fun         = fun Receiver:ProperName/ProperArity,
  expand_macro_fun(Meta, Fun, Receiver, Name, Args, E).

expand_quoted(Meta, Receiver, Name, Arity, Quoted, E) ->
  Line = ?line(Meta),
  Next = elixir_counter:next(),

  try
    elixir_exp:expand(
      elixir_quote:linify_with_context_counter(Line, { Receiver, Next }, Quoted),
      E)
  catch
    Kind:Reason ->
      Info = [{ Receiver, Name, Arity, location(Line, E) }, mfa(Line, E)],
      erlang:raise(Kind, Reason, prune_stacktrace(Info, erlang:get_stacktrace(), nil))
  end.

mfa(Line, #elixir_env{module=nil} = E) ->
  { elixir_compiler, '__FILE__', 2, location(Line, E) };
mfa(Line, #elixir_env{module=Module, function=nil} = E) ->
  { Module, '__MODULE__', 0, location(Line, E) };
mfa(Line, #elixir_env{module=Module, function={ Name, Arity }} = E) ->
  { Module, Name, Arity, location(Line, E) }.

location(Line, E) ->
  [{ file, elixir_utils:characters_to_list(E#elixir_env.file) }, { line, Line }].

%% Helpers

skip_require(Meta) ->
  lists:keyfind(require, 1, Meta) == { require, false }.

find_dispatch(Meta, Tuple, Extra, E) ->
  case is_import(Meta) of
    { import, _ } = Import ->
      Import;
    false ->
      Funs = E#elixir_env.functions,
      Macs = Extra ++ E#elixir_env.macros,
      FunMatch = find_dispatch(Tuple, Funs),
      MacMatch = find_dispatch(Tuple, Macs),

      case { FunMatch, MacMatch } of
        { [], [Receiver] } -> { macro, Receiver };
        { [Receiver], [] } -> { function, Receiver };
        { [], [] } -> false;
        _ ->
          { Name, Arity } = Tuple,
          [First, Second|_] = FunMatch ++ MacMatch,
          Error = { ambiguous_call, { First, Second, Name, Arity } },
          elixir_errors:form_error(Meta, E#elixir_env.file, ?MODULE, Error)
      end
  end.

find_dispatch(Tuple, List) ->
  [Receiver || { Receiver, Set } <- List, is_element(Tuple, Set)].

is_import(Meta) ->
  case lists:keyfind(import, 1, Meta) of
    { import, _ } = Import ->
      case lists:keyfind(context, 1, Meta) of
        { context, _ } -> Import;
        false ->
          false
      end;
    false ->
      false
  end.

%% We've reached the invoked macro, skip it with the rest
prune_stacktrace(Info, [{ _, _, [E|_], _ }|_], E) ->
  Info;
%% We've reached the elixir_dispatch internals, skip it with the rest
prune_stacktrace(Info, [{ Mod, _, _, _ }|_], _) when Mod == elixir_dispatch; Mod == elixir_exp ->
  Info;
prune_stacktrace(Info, [H|T], E) ->
  [H|prune_stacktrace(Info, T, E)];
prune_stacktrace(Info, [], _) ->
  Info.

%% ERROR HANDLING

format_error({ unrequired_module, { Receiver, Name, Arity, _Required }}) ->
  Module = elixir_aliases:inspect(Receiver),
  io_lib:format("you must require ~ts before invoking the macro ~ts.~ts/~B",
    [Module, Module, Name, Arity]);
format_error({ macro_conflict, { Receiver, Name, Arity } }) ->
  io_lib:format("call to local macro ~ts/~B conflicts with imported ~ts.~ts/~B, "
    "please rename the local macro or remove the conflicting import",
    [Name, Arity, elixir_aliases:inspect(Receiver), Name, Arity]);
format_error({ ambiguous_call, { Mod1, Mod2, Name, Arity }}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call is ambiguous",
    [Name, Arity, elixir_aliases:inspect(Mod1), elixir_aliases:inspect(Mod2)]).

%% INTROSPECTION

%% Do not try to get macros from Erlang. Speeds up compilation a bit.
get_optional_macros(erlang) -> [];

get_optional_macros(Receiver) ->
  case code:ensure_loaded(Receiver) of
    { module, Receiver } ->
      try
        Receiver:'__info__'(macros)
      catch
        error:undef -> []
      end;
    { error, _ } -> []
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

rewrite(?kernel, atom_to_binary, [Arg], 1) ->
  { ok, erlang, atom_to_binary, [Arg, utf8] };
rewrite(?kernel, binary_to_atom, [Arg], 1) ->
  { ok, erlang, binary_to_atom, [Arg, utf8] };
rewrite(?kernel, binary_to_existing_atom, [Arg], 1) ->
  { ok, erlang, binary_to_existing_atom, [Arg, utf8] };
rewrite(?kernel, elem, [Tuple, Index], 2) ->
  { ok, erlang, element, [increment(Index), Tuple] };
rewrite(?kernel, set_elem, [Tuple, Index, Value], 2) ->
  { ok, erlang, setelement, [increment(Index), Tuple, Value] };
rewrite(?process, monitor, [Arg], 1) ->
  { ok, erlang, monitor, [process, Arg] };
rewrite(Receiver, Name, Args, Arity) ->
  case inline(Receiver, Name, Arity) of
    { AR, AN } -> { ok, AR, AN, Args };
    false      -> false
  end.

increment(Number) when is_number(Number) ->
  Number + 1;
increment(Other) ->
  { { '.', [], [erlang, '+'] }, [], [Other, 1] }.

inline(?kernel, '+', 2) -> { erlang, '+' };
inline(?kernel, '-', 2) -> { erlang, '-' };
inline(?kernel, '+', 1) -> { erlang, '+' };
inline(?kernel, '-', 1) -> { erlang, '-' };
inline(?kernel, '*', 2) -> { erlang, '*' };
inline(?kernel, '/', 2) -> { erlang, '/' };
inline(?kernel, '++', 2) -> { erlang, '++' };
inline(?kernel, '--', 2) -> { erlang, '--' };
inline(?kernel, 'xor', 2) -> { erlang, 'xor' };
inline(?kernel, 'not', 1) -> { erlang, 'not' };
inline(?kernel, '<', 2) -> { erlang, '<' };
inline(?kernel, '>', 2) -> { erlang, '>' };
inline(?kernel, '<=', 2) -> { erlang, '=<' };
inline(?kernel, '>=', 2) -> { erlang, '>=' };
inline(?kernel, '==', 2) -> { erlang, '==' };
inline(?kernel, '!=', 2) -> { erlang, '/=' };
inline(?kernel, '===', 2) -> { erlang, '=:=' };
inline(?kernel, '!==', 2) -> { erlang, '=/=' };
inline(?kernel, abs, 1) -> { erlang, abs };
inline(?kernel, apply, 2) -> { erlang, apply };
inline(?kernel, apply, 3) -> { erlang, apply };
inline(?kernel, atom_to_list, 1) -> { erlang, atom_to_list };
inline(?kernel, binary_part, 3) -> { erlang, binary_part };
inline(?kernel, binary_to_float, 1) -> { erlang, binary_to_float };
inline(?kernel, binary_to_float, 2) -> { erlang, binary_to_float };
inline(?kernel, binary_to_integer, 1) -> { erlang, binary_to_integer };
inline(?kernel, binary_to_integer, 2) -> { erlang, binary_to_integer };
inline(?kernel, bit_size, 1) -> { erlang, bit_size };
inline(?kernel, bitstring_to_list, 1) -> { erlang, bitstring_to_list };
inline(?kernel, byte_size, 1) -> { erlang, byte_size };
inline(?kernel, 'div', 2) -> { erlang, 'div' };
inline(?kernel, exit, 1) -> { erlang, exit };
inline(?kernel, float_to_binary, 1) -> { erlang, float_to_binary };
inline(?kernel, float_to_list, 1) -> { erlang, float_to_list };
inline(?kernel, hd, 1) -> { erlang, hd };
inline(?kernel, integer_to_binary, 1) -> { erlang, integer_to_binary };
inline(?kernel, integer_to_binary, 2) -> { erlang, integer_to_binary };
inline(?kernel, integer_to_list, 1) -> { erlang, integer_to_list };
inline(?kernel, integer_to_list, 2) -> { erlang, integer_to_list };
inline(?kernel, iolist_size, 1) -> { erlang, iolist_size };
inline(?kernel, iolist_to_binary, 1) -> { erlang, iolist_to_binary };
inline(?kernel, is_atom, 1) -> { erlang, is_atom };
inline(?kernel, is_binary, 1) -> { erlang, is_binary };
inline(?kernel, is_bitstring, 1) -> { erlang, is_bitstring };
inline(?kernel, is_boolean, 1) -> { erlang, is_boolean };
inline(?kernel, is_float, 1) -> { erlang, is_float };
inline(?kernel, is_function, 1) -> { erlang, is_function };
inline(?kernel, is_function, 2) -> { erlang, is_function };
inline(?kernel, is_integer, 1) -> { erlang, is_integer };
inline(?kernel, is_list, 1) -> { erlang, is_list };
inline(?kernel, is_map, 1) -> { erlang, is_map };
inline(?kernel, is_number, 1) -> { erlang, is_number };
inline(?kernel, is_pid, 1) -> { erlang, is_pid };
inline(?kernel, is_port, 1) -> { erlang, is_port };
inline(?kernel, is_reference, 1) -> { erlang, is_reference };
inline(?kernel, is_tuple, 1) -> { erlang, is_tuple };
inline(?kernel, length, 1) -> { erlang, length };
inline(?kernel, list_to_atom, 1) -> { erlang, list_to_atom };
inline(?kernel, list_to_bitstring, 1) -> { erlang, list_to_bitstring };
inline(?kernel, list_to_existing_atom, 1) -> { erlang, list_to_existing_atom };
inline(?kernel, list_to_float, 1) -> { erlang, list_to_float };
inline(?kernel, list_to_integer, 1) -> { erlang, list_to_integer };
inline(?kernel, list_to_integer, 2) -> { erlang, list_to_integer };
inline(?kernel, list_to_tuple, 1) -> { erlang, list_to_tuple };
inline(?kernel, make_ref, 0) -> { erlang, make_ref };
inline(?kernel, map_size, 1) -> { erlang, map_size };
inline(?kernel, max, 2) -> { erlang, max };
inline(?kernel, min, 2) -> { erlang, min };
inline(?kernel, node, 0) -> { erlang, node };
inline(?kernel, node, 1) -> { erlang, node };
inline(?kernel, 'rem', 2) -> { erlang, 'rem' };
inline(?kernel, round, 1) -> { erlang, round };
inline(?kernel, self, 0) -> { erlang, self };
inline(?kernel, send, 2) -> { erlang, send };
inline(?kernel, size, 1) -> { erlang, size };
inline(?kernel, spawn, 1) -> { erlang, spawn };
inline(?kernel, spawn, 3) -> { erlang, spawn };
inline(?kernel, spawn_link, 1) -> { erlang, spawn_link };
inline(?kernel, spawn_link, 3) -> { erlang, spawn_link };
inline(?kernel, throw, 1) -> { erlang, throw };
inline(?kernel, tl, 1) -> { erlang, tl };
inline(?kernel, trunc, 1) -> { erlang, trunc };
inline(?kernel, tuple_size, 1) -> { erlang, tuple_size };
inline(?kernel, tuple_to_list, 1) -> { erlang, tuple_to_list };

inline(?process, exit, 2) -> { erlang, exit };
inline(?process, spawn, 1) -> { erlang, spawn };
inline(?process, spawn, 2) -> { erlang, spawn_opt };
inline(?process, spawn, 3) -> { erlang, spawn };
inline(?process, spawn, 4) -> { erlang, spawn_opt };
inline(?process, spawn_link, 1) -> { erlang, spawn_link };
inline(?process, spawn_link, 3) -> { erlang, spawn_link };
inline(?process, spawn_monitor, 1) -> { erlang, spawn_monitor };
inline(?process, spawn_monitor, 3) -> { erlang, spawn_monitor };
inline(?process, demonitor, 1) -> { erlang, demonitor };
inline(?process, demonitor, 2) -> { erlang, demonitor };
inline(?process, link, 1) -> { erlang, link };
inline(?process, unlink, 1) -> { erlang, unlink };

inline(?node, spawn, 2) -> { erlang, spawn };
inline(?node, spawn, 3) -> { erlang, spawn_opt };
inline(?node, spawn, 4) -> { erlang, spawn };
inline(?node, spawn, 5) -> { erlang, spawn_opt };
inline(?node, spawn_link, 2) -> { erlang, spawn_link };
inline(?node, spawn_link, 4) -> { erlang, spawn_link };
inline(?node, spawn_monitor, 2) -> { erlang, spawn_monitor };
inline(?node, spawn_monitor, 4) -> { erlang, spawn_monitor };

inline(?system, stacktrace, 0) -> { erlang, get_stacktrace };

inline(_, _, _) -> false.
