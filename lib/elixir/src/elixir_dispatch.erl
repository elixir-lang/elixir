%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([default_macros/0, default_functions/0, default_requires/0,
  dispatch_require/6, dispatch_import/5,
  require_function/5, import_function/4,
  expand_import/9, expand_require/8, find_import/4,
  format_error/1, in_erlang_functions/0, in_erlang_macros/0]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

-import(ordsets, [is_element/2]).
-define(BUILTIN, 'Elixir.Kernel').

default_functions() ->
  [ { ?BUILTIN, ordsets:union(in_elixir_functions(), in_erlang_functions()) } ].
default_macros() ->
  [ { ?BUILTIN, ordsets:union(in_elixir_macros(), in_erlang_macros()) } ].
default_requires() ->
  [ ?BUILTIN, 'Elixir.Kernel.Typespec' ].

find_import(Meta, Name, Arity, S) ->
  Tuple = { Name, Arity },

  case find_dispatch(Meta, Tuple, S#elixir_scope.functions, S#elixir_scope.macros, S#elixir_scope.file) of
    { function, Receiver } -> Receiver;
    { macro, Receiver } -> Receiver;
    nomatch -> false
  end.

%% Function retrieval

import_function(Meta, Name, Arity, S) ->
  Tuple = { Name, Arity },
  case find_dispatch(Meta, Tuple, S#elixir_scope.functions, S#elixir_scope.macros, S#elixir_scope.file) of
    { function, Receiver } ->
      elixir_import:record(import, Tuple, Receiver, S#elixir_scope.module),
      remote_function(Meta, Receiver, Name, Arity, S);
    { macro, _Receiver } ->
      false;
    nomatch ->
      { { 'fun', ?line(Meta), { function, Name, Arity } }, S }
  end.

require_function(Meta, Receiver, Name, Arity, S) ->
  Tuple = { Name, Arity },

  case is_element(Tuple, get_optional_macros(Receiver)) of
    true  -> false;
    false -> remote_function(Meta, Receiver, Name, Arity, S)
  end.

%% Function dispatch

dispatch_import(Meta, Name, Args, S, Callback) ->
  Module = S#elixir_scope.module,
  Arity  = length(Args),
  Tuple  = { Name, Arity },

  case find_dispatch(Meta, Tuple, S#elixir_scope.functions, S#elixir_scope.macros, S#elixir_scope.file) of
    { function, Receiver } ->
      elixir_import:record(import, Tuple, Receiver, Module),
      Endpoint = case (Receiver == ?BUILTIN) andalso is_element(Tuple, in_erlang_functions()) of
        true  -> erlang;
        false -> Receiver
      end,
      elixir_translator:translate_each({ { '.', Meta, [Endpoint, Name] }, Meta, Args }, S);
    Result ->
      case expand_import(Meta, Tuple, Args, Module, S#elixir_scope.function,
          S#elixir_scope.requires, S, Result) of
        { error, noexpansion } ->
          Callback();
        { error, internal } ->
          elixir_import:record(import, Tuple, ?BUILTIN, Module),
          elixir_macros:translate({ Name, Meta, Args }, S);
        { ok, _Receiver, Tree } ->
          translate_expansion(Meta, Tree, S)
      end
  end.

dispatch_require(Meta, Receiver, Name, Args, S, Callback) ->
  Module = S#elixir_scope.module,
  Arity  = length(Args),
  Tuple  = { Name, Arity },

  case (Receiver == ?BUILTIN) andalso is_element(Tuple, in_erlang_functions()) of
    true ->
      elixir_translator:translate_each({ { '.', Meta, [erlang, Name] }, Meta, Args }, S);
    false ->
      case expand_require(Meta, Receiver, Tuple, Args, Module,
          S#elixir_scope.function, S#elixir_scope.requires, S) of
        { error, noexpansion } ->
          Callback();
        { error, internal } ->
          elixir_macros:translate({ Name, Meta, Args }, S);
        { ok, Tree } ->
          translate_expansion(Meta, Tree, S)
      end
  end.

%% Macros expansion

expand_import(Meta, { Name, Arity } = Tuple, Args, Module, Function, Requires, SEnv, Result) ->
  case Result of
    { macro, ?BUILTIN } ->
      case is_element(Tuple, in_erlang_macros()) of
        true  -> { error, internal };
        false ->
          elixir_import:record(import, Tuple, ?BUILTIN, Module),
          { ok, ?BUILTIN, expand_macro_named(Meta, ?BUILTIN, Name, Arity, Args, Module, Requires, SEnv) }
      end;
    { macro, Receiver } ->
      elixir_import:record(import, Tuple, Receiver, Module),
      { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, Requires, SEnv) };
    _ ->
      Fun = (Function /= Tuple) andalso
        elixir_def_local:macro_for(Tuple, true, Module),
      case Fun of
        false -> { error, noexpansion };
        _ ->
          elixir_import:record(import, Tuple, Module, Module),
          { ok, Module, expand_macro_fun(Meta, Fun, Module, Name, Args, Module, Requires, SEnv) }
      end
  end.

expand_import(Meta, Tuple, Args, Module, Function, Requires, Functions, Macros, SEnv) ->
  Result = find_dispatch(Meta, Tuple, Functions, Macros, elixir_scope:filename(SEnv)),
  expand_import(Meta, Tuple, Args, Module, Function, Requires, SEnv, Result).

expand_require(Meta, ?BUILTIN, { Name, Arity } = Tuple, Args, Module, _Function, Requires, SEnv) ->
  case is_element(Tuple, in_erlang_macros()) of
    true  -> { error, internal };
    false ->
      case is_element(Tuple, in_elixir_macros()) of
        true  -> { ok, expand_macro_named(Meta, ?BUILTIN, Name, Arity, Args, Module, Requires, SEnv) };
        false -> { error, noexpansion }
      end
  end;

expand_require(Meta, Receiver, { Name, Arity } = Tuple, Args, Module, Function, Requires, SEnv) ->
  Fun = (Module == Receiver) andalso (Function /= Tuple) andalso
    elixir_def_local:macro_for(Tuple, false, Module),

  case Fun of
    false ->
      case is_element(Tuple, get_optional_macros(Receiver)) of
        true  -> { ok, expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, Requires, SEnv) };
        false -> { error, noexpansion }
      end;
    _ ->
      elixir_import:record(import, Tuple, Receiver, Module),
      { ok, expand_macro_fun(Meta, Fun, Receiver, Name, Args, Module, Requires, SEnv) }
  end.

%% Expansion helpers

expand_macro_fun(Meta, Fun, Receiver, Name, Args, Module, Requires, SEnv) ->
  case (Receiver == Module) or is_element(Receiver, Requires) or skip_requires(SEnv) of
    true  -> ok;
    false ->
      Tuple = { unrequired_module, { Receiver, Name, length(Args), Requires } },
      elixir_errors:form_error(Meta, elixir_scope:filename(SEnv), ?MODULE, Tuple)
  end,

  Line = ?line(Meta),
  SArg = {Line,SEnv},

  try
    apply(Fun, [SArg|Args])
  catch
    Kind:Reason ->
      Info = { Receiver, Name, length(Args), [{ file, elixir_scope:filename(SEnv) }, { line, Line }] },
      erlang:raise(Kind, Reason, munge_stacktrace(Info, erlang:get_stacktrace(), SArg))
  end.

expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, Requires, SEnv) ->
  ProperName  = ?elixir_macro(Name),
  ProperArity = Arity + 1,
  Fun         = fun Receiver:ProperName/ProperArity,
  expand_macro_fun(Meta, Fun, Receiver, Name, Args, Module, Requires, SEnv).

translate_expansion(Meta, Tree, S) ->
  { TR, TS } = elixir_translator:translate_each(
    elixir_quote:linify(?line(Meta), Tree),
    S#elixir_scope{
      check_clauses=false,
      macro_aliases=[] }
  ),
  { TR, TS#elixir_scope{
    check_clauses=S#elixir_scope.check_clauses,
    macro_aliases=merge_aliases(S#elixir_scope.macro_aliases, TS#elixir_scope.macro_aliases)
  } }.

merge_aliases(A1, A2) ->
  orddict:merge(fun(_K,_V1,V2) -> V2 end, A1, A2).

%% Helpers

skip_requires(#elixir_scope{check_requires=false}) -> true;
skip_requires(_) -> false.

find_dispatch(Meta, Tuple, Functions, Macros, File) ->
  FunMatch = find_dispatch(Tuple, Functions),
  MacMatch = find_dispatch(Tuple, Macros),

  case { FunMatch, MacMatch } of
    { [], [Receiver] } -> { macro, Receiver };
    { [Receiver], [] } -> { function, Receiver };
    { [], [] } -> nomatch;
    _ ->
      { Name, Arity } = Tuple,
      [First, Second|_] = FunMatch ++ MacMatch,
      Err = { ambiguous_call, { First, Second, Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Err)
  end.

find_dispatch(Tuple, List) ->
  [Receiver || { Receiver, Set } <- List, is_element(Tuple, Set)].

munge_stacktrace(Info, [{ _, _, [S|_], _ }|_], S) ->
  [Info];

munge_stacktrace(Info, [{ elixir_dispatch, expand_macro_fun, _, _ }|_], _) ->
  [Info];

munge_stacktrace(Info, [H|T], S) ->
  [H|munge_stacktrace(Info, T, S)];

munge_stacktrace(_, [], _) ->
  [].

%% ERROR HANDLING

format_error({ unrequired_module, { Receiver, Name, Arity, Required }}) ->
  String = string:join([elixir_errors:inspect(R) || R <- Required], ", "),
  io_lib:format("tried to invoke macro ~ts.~ts/~B but module was not required. Required: ~ts",
    [elixir_errors:inspect(Receiver), Name, Arity, String]);

format_error({ ambiguous_call, { Mod1, Mod2, Name, Arity }}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call is ambiguous",
    [Name, Arity, elixir_errors:inspect(Mod1), elixir_errors:inspect(Mod2)]).

%% INTROSPECTION

remote_function(Meta, Receiver, Name, Arity, S) ->
  Line  = ?line(Meta),

  Final =
    case Receiver == ?BUILTIN andalso is_element({ Name, Arity }, in_erlang_functions()) of
      true  -> erlang;
      false -> Receiver
    end,

  { { 'fun', Line, { function,
    { atom, Line, Final },
    { atom, Line, Name },
    { integer, Line, Arity}
  } }, S }.

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

%% Functions imported from Kernel module. Sorted on compilation.

in_elixir_functions() ->
  try
    ?BUILTIN:'__info__'(functions)
  catch
    error:undef -> []
  end.

%% Macros imported from Kernel module. Sorted on compilation.

in_elixir_macros() ->
  try
    ?BUILTIN:'__info__'(macros)
  catch
    error:undef -> []
  end.

%% Functions imported from Erlang module. MUST BE SORTED.
in_erlang_functions() ->
  [
    { abs, 1 },
    { atom_to_binary, 2 },
    { atom_to_list, 1 },
    { binary_part, 3 },
    { binary_to_atom, 2 },
    { binary_to_existing_atom, 2 },
    { binary_to_list, 1 },
    { binary_to_list, 3 },
    { binary_to_term, 1 },
    { binary_to_term, 2 },
    { bit_size, 1 },
    { bitstring_to_list, 1 },
    { byte_size, 1 },
    % { date, 0 },
    { exit, 1 },
    { float, 1 },
    { float_to_list, 1 },
    { hd, 1 },
    { integer_to_list, 1 },
    { integer_to_list, 2 },
    { iolist_size, 1 },
    { iolist_to_binary, 1 },
    { is_alive, 0 },
    { is_atom, 1 },
    { is_binary, 1 },
    { is_bitstring, 1 },
    { is_boolean, 1 },
    { is_float, 1 },
    { is_function, 1 },
    { is_function, 2 },
    { is_integer, 1 },
    { is_list, 1 },
    { is_number, 1 },
    { is_pid, 1 },
    { is_port, 1 },
    { is_reference, 1 },
    { is_tuple, 1 },
    { length, 1 },
    { list_to_atom, 1 },
    { list_to_binary, 1 },
    { list_to_bitstring, 1 },
    { list_to_existing_atom, 1 },
    { list_to_float, 1 },
    { list_to_integer, 1 },
    { list_to_integer, 2 },
    { list_to_pid, 1 },
    { list_to_tuple, 1 },
    { make_ref, 0 },
    { max, 2 },
    { min, 2 },
    { node, 0 },
    { node, 1 },
    % { now, 0 },
    { pid_to_list, 1 },
    { round, 1 },
    { self, 0 },
    { size, 1 },
    { spawn, 1 },
    { spawn, 3 },
    { spawn_link, 1 },
    { spawn_link, 3 },
    % { split_binary, 2 },
    { term_to_binary, 1 },
    { term_to_binary, 2 },
    { throw, 1 },
    % { time, 0 },
    { tl, 1 },
    { trunc, 1 },
    { tuple_size, 1 },
    { tuple_to_list, 1 }
  ].

%% Macros implemented in Erlang. MUST BE SORTED.
in_erlang_macros() ->
  [
    {'!',1},
    {'!=',2},
    {'!==',2},
    {'*',2},
    {'+',1},
    {'+',2},
    {'++',2},
    {'-',1},
    {'-',2},
    {'--',2},
    {'/',2},
    {'<',2},
    {'<-',2},
    {'<=',2},
    {'==',2},
    {'===',2},
    {'>',2},
    {'>=',2},
    {'@',1},
    {'and',2},
    {apply,2},
    {apply,3},
    {'case',2},
    {def,1},
    {def,2},
    {def,4},
    {defmacro,1},
    {defmacro,2},
    {defmacro,4},
    {defmacrop,1},
    {defmacrop,2},
    {defmacrop,4},
    {defmodule,2},
    {defp,1},
    {defp,2},
    {defp,4},
    {function,1},
    {function,2},
    {function,3},
    {in,2},
    {'not',1},
    {'or',2},
    {'receive',1},
    {'try',1},
    {'xor',2}
  ].
