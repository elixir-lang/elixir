%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([default_macros/0, default_functions/0, default_requires/0,
  dispatch_require/6, dispatch_import/5,
  require_function/5, import_function/4,
  expand_import/6, expand_require/6, find_import/4,
  format_error/1, in_erlang_functions/0, in_erlang_macros/0]).
-include("elixir.hrl").

-import(ordsets, [is_element/2]).
-define(builtin, 'Elixir.Kernel').

default_functions() ->
  [ { ?builtin, elixir_imported_functions() } ].
default_macros() ->
  [ { ?builtin, elixir_imported_macros() } ].
default_requires() ->
  [ ?builtin, 'Elixir.Kernel.Typespec', 'Elixir.Record' ].

find_import(Meta, Name, Arity, S) ->
  Tuple = { Name, Arity },

  case find_dispatch(Meta, Tuple, S) of
    { function, Receiver } ->
      elixir_tracker:record_import(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      Receiver;
    { macro, Receiver } ->
      elixir_tracker:record_import(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      Receiver;
    _ ->
      false
  end.

%% Function retrieval

import_function(Meta, Name, Arity, S) ->
  Tuple = { Name, Arity },
  case find_dispatch(Meta, Tuple, S) of
    { function, Receiver } ->
      elixir_tracker:record_import(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      remote_function(Meta, Receiver, Name, Arity, S);
    { macro, _Receiver } ->
      false;
    { import, Receiver } ->
      require_function(Meta, Receiver, Name, Arity, S);
    false ->
      case elixir_import:special_form(Name, Arity) of
        true  -> false;
        false ->
          elixir_tracker:record_local(Tuple, S#elixir_scope.module, S#elixir_scope.function),
          { { 'fun', ?line(Meta), { function, Name, Arity } }, S }
      end
  end.

require_function(Meta, Receiver, Name, Arity, S) ->
  Tuple = { Name, Arity },

  case is_element(Tuple, get_optional_macros(Receiver)) of
    true  -> false;
    false ->
      elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      remote_function(Meta, Receiver, Name, Arity, S)
  end.

%% Function dispatch

dispatch_import(Meta, Name, Args, S, Callback) ->
  Module = S#elixir_scope.module,
  Arity  = length(Args),
  Tuple  = { Name, Arity },

  case find_dispatch(Meta, Tuple, S) of
    { function, Receiver } ->
      elixir_tracker:record_import(Tuple, Receiver, Module, S#elixir_scope.function),
      Endpoint = case (Receiver == ?builtin) andalso is_element(Tuple, in_erlang_functions()) of
        true  -> erlang;
        false -> Receiver
      end,
      elixir_translator:translate_each({ { '.', Meta, [Endpoint, Name] }, Meta, Args }, S);
    { import, Receiver } ->
      elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      elixir_translator:translate_each({ { '.', Meta, [Receiver, Name] }, [{require,false}|Meta], Args }, S);
    Result ->
      case do_expand_import(Meta, Tuple, Args, Module, S, Result) of
        { error, noexpansion } ->
          Callback();
        { error, internal } ->
          elixir_tracker:record_import(Tuple, ?builtin, Module, S#elixir_scope.function),
          elixir_macros:translate({ Name, Meta, Args }, S);
        { ok, _Receiver, Tree } ->
          translate_expansion(Meta, Tree, S)
      end
  end.

dispatch_require(Meta, Receiver, Name, Args, S, Callback) ->
  Module = S#elixir_scope.module,
  Arity  = length(Args),
  Tuple  = { Name, Arity },

  case (Receiver == ?builtin) andalso is_element(Tuple, in_erlang_functions()) of
    true ->
      elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      { TArgs, SA } = elixir_translator:translate_args(Args, S),
      { ?wrap_call(?line(Meta), erlang, Name, TArgs), SA };
    false ->
      case expand_require(Meta, Receiver, Tuple, Args, Module, S) of
        { error, noexpansion } ->
          Callback();
        { error, internal } ->
          elixir_tracker:record_remote(Tuple, ?builtin, S#elixir_scope.module, S#elixir_scope.function),
          elixir_macros:translate({ Name, Meta, Args }, S);
        { ok, _Receiver, Tree } ->
          translate_expansion(Meta, Tree, S)
      end
  end.

%% Macros expansion

expand_import(Meta, Tuple, Args, Module, Extra, S) ->
  Result = find_dispatch(Meta, Tuple, Extra, S),
  do_expand_import(Meta, Tuple, Args, Module, S, Result).

do_expand_import(Meta, { Name, Arity } = Tuple, Args, Module, S, Result) ->
  Function = S#elixir_scope.function,

  Fun = (Function /= nil) andalso (Function /= Tuple) andalso
        elixir_def_local:macro_for(Tuple, true, S),

  case Fun of
    false ->
      do_expand_import_no_local(Meta, Tuple, Args, Module, S, Result);
    _ ->
      case Result of
        { Kind, Receiver } when Kind == import; Receiver == Module ->
          do_expand_import_no_local(Meta, Tuple, Args, Module, S, Result);
        { _, Receiver } ->
          Error = { macro_conflict, { Receiver, Name, Arity } },
          elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, Error);
        false ->
          elixir_tracker:record_local(Tuple, Module, S#elixir_scope.function),
          { ok, Module, expand_macro_fun(Meta, Fun(), Module, Name, Args, Module, S) }
      end
  end.

do_expand_import_no_local(Meta, { Name, Arity } = Tuple, Args, Module, S, Result) ->
  case Result of
    { macro, ?builtin } ->
      case is_element(Tuple, in_erlang_macros()) of
        true  -> { error, internal };
        false ->
          elixir_tracker:record_import(Tuple, ?builtin, Module, S#elixir_scope.function),
          { ok, ?builtin, expand_macro_named(Meta, ?builtin, Name, Arity, Args, Module, S) }
      end;
    { macro, Receiver } ->
      elixir_tracker:record_import(Tuple, Receiver, Module, S#elixir_scope.function),
      { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, S) };
    { import, Receiver } ->
      expand_require(Meta, Receiver, Tuple, Args, Module, S);
    _ ->
      { error, noexpansion }
  end.

expand_require(Meta, ?builtin, { Name, Arity } = Tuple, Args, Module, S) ->
  case is_element(Tuple, in_erlang_macros()) of
    true  -> { error, internal };
    false ->
      case is_element(Tuple, in_elixir_macros()) of
        true  ->
          elixir_tracker:record_remote(Tuple, ?builtin, S#elixir_scope.module, S#elixir_scope.function),
          { ok, ?builtin, expand_macro_named(Meta, ?builtin, Name, Arity, Args, Module, S) };
        false ->
          { error, noexpansion }
      end
  end;

expand_require(Meta, Receiver, { Name, Arity } = Tuple, Args, Module, S) ->
  Function = S#elixir_scope.function,

  Fun = (Module == Receiver) andalso (Function /= nil) andalso
        (Function /= Tuple) andalso elixir_def_local:macro_for(Tuple, false, S),

  case Fun of
    false ->
      case is_element(Tuple, get_optional_macros(Receiver)) of
        true  ->
          elixir_tracker:record_remote(Tuple, Receiver, Module, Function),
          { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, S) };
        false -> { error, noexpansion }
      end;
    _ ->
      elixir_tracker:record_local(Tuple, Module, Function),
      { ok, Receiver, expand_macro_fun(Meta, Fun(), Receiver, Name, Args, Module, S) }
  end.

%% Expansion helpers

expand_macro_fun(Meta, Fun, Receiver, Name, Args, Module, S) ->
  Requires = S#elixir_scope.requires,
  case (Receiver == Module) or is_element(Receiver, Requires) or skip_require(Meta) of
    true  -> ok;
    false ->
      Tuple = { unrequired_module, { Receiver, Name, length(Args), Requires } },
      elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, Tuple)
  end,

  Line = ?line(Meta),
  SArg = {Line,S},

  try
    apply(Fun, [SArg|Args])
  catch
    Kind:Reason ->
      Info = { Receiver, Name, length(Args), location(Line, S) },
      erlang:raise(Kind, Reason, munge_stacktrace(Info, erlang:get_stacktrace(), SArg))
  end.

expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, S) ->
  ProperName  = ?elixir_macro(Name),
  ProperArity = Arity + 1,
  Fun         = fun Receiver:ProperName/ProperArity,
  expand_macro_fun(Meta, Fun, Receiver, Name, Args, Module, S).

translate_expansion(Meta, Tree, S) ->
  Line = ?line(Meta),

  try
    elixir_translator:translate_each(
      elixir_quote:linify(Line, Tree),
      S
    )
  catch
    Kind:Reason ->
      erlang:raise(Kind, Reason, munge_stacktrace(mfa(Line, S), erlang:get_stacktrace(), nil))
  end.

mfa(Line, #elixir_scope{module=nil} = S) ->
  { elixir_compiler, '__FILE__', 2, location(Line, S) };

mfa(Line, #elixir_scope{module=Module,function=nil} = S) ->
  { Module, '__MODULE__', 0, location(Line, S) };

mfa(Line, #elixir_scope{module=Module,function={ Name, Arity }} = S) ->
  { Module, Name, Arity, location(Line, S) }.

location(Line, S) ->
  [{ file, S#elixir_scope.file }, { line, Line }].

%% Helpers

skip_require(Meta) ->
  lists:keyfind(require, 1, Meta) == { require, false }.

find_dispatch(Meta, Tuple, S) ->
  find_dispatch(Meta, Tuple, [], S).

find_dispatch(Meta, Tuple, Extra, S) ->
  case is_import(Meta, Tuple, S) of
    { import, _ } = Import ->
      Import;
    false ->
      Functions = S#elixir_scope.functions,
      Macros = Extra ++ S#elixir_scope.macros,
      File = S#elixir_scope.file,
      FunMatch = find_dispatch(Tuple, Functions),
      MacMatch = find_dispatch(Tuple, Macros),

      case { FunMatch, MacMatch } of
        { [], [Receiver] } -> { macro, Receiver };
        { [Receiver], [] } -> { function, Receiver };
        { [], [] } -> false;
        _ ->
          { Name, Arity } = Tuple,
          [First, Second|_] = FunMatch ++ MacMatch,
          Error = { ambiguous_call, { First, Second, Name, Arity } },
          elixir_errors:form_error(Meta, File, ?MODULE, Error)
      end
  end.

find_dispatch(Tuple, List) ->
  [Receiver || { Receiver, Set } <- List, is_element(Tuple, Set)].

is_import(Meta, Tuple, S) ->
  case lists:keyfind(import, 1, Meta) of
    { import, _ } = Import ->
      case lists:keyfind(context, 1, Meta) of
        { context, Context } ->
          not_an_import(Tuple, Context, S#elixir_scope.macro_functions)
            andalso not_an_import(Tuple, Context, S#elixir_scope.macro_macros)
            andalso Import;
        false ->
          false
      end;
    false ->
      false
  end.

not_an_import(Tuple, Context, Dict) ->
  case orddict:find(Context, Dict) of
    { ok, Pairs } ->
      not lists:any(fun({ _, List }) -> lists:member(Tuple, List) end, Pairs);
    error ->
      true
  end.

%% We've reached the invoked macro, skip it with the rest
munge_stacktrace(Info, [{ _, _, [S|_], _ }|_], S) ->
  [Info];

%% We've reached the elixir_dispatch internals, skip it with the rest
munge_stacktrace(Info, [{ elixir_dispatch, _, _, _ }|_], _) ->
  [Info];

munge_stacktrace(Info, [H|T], S) ->
  [H|munge_stacktrace(Info, T, S)];

munge_stacktrace(Info, [], _) ->
  [Info].

remote_function(Meta, Receiver, Name, Arity, S) ->
  Line  = ?line(Meta),

  Final =
    case (Receiver == ?builtin) andalso is_element({ Name, Arity }, in_erlang_functions()) of
      true  -> erlang;
      false -> Receiver
    end,

  { { 'fun', Line, { function,
    { atom, Line, Final },
    { atom, Line, Name },
    { integer, Line, Arity}
  } }, S }.


%% ERROR HANDLING

format_error({ unrequired_module, { Receiver, Name, Arity, Required }}) ->
  String = 'Elixir.Enum':join([elixir_errors:inspect(R) || R <- Required], ", "),
  io_lib:format("tried to invoke macro ~ts.~ts/~B but module was not required. Required: ~ts",
    [elixir_errors:inspect(Receiver), Name, Arity, String]);

format_error({ macro_conflict, { Receiver, Name, Arity } }) ->
  io_lib:format("call to local macro ~ts/~B conflicts with imported ~ts.~ts/~B",
    [Name, Arity, elixir_errors:inspect(Receiver), Name, Arity]);

format_error({ ambiguous_call, { Mod1, Mod2, Name, Arity }}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call is ambiguous",
    [Name, Arity, elixir_errors:inspect(Mod1), elixir_errors:inspect(Mod2)]).

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
    ?builtin:'__info__'(functions)
  catch
    error:undef -> in_erlang_functions()
  end.

elixir_imported_macros() ->
  try
    ?builtin:'__info__'(macros)
  catch
    error:undef -> in_erlang_macros()
  end.

%% Macros imported from Kernel module. Sorted on compilation.

in_elixir_macros() ->
  try
    ?builtin:'__info__'(macros)
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
    { binary_to_float, 1 },
    { binary_to_float, 2 },
    { binary_to_integer, 1 },
    { binary_to_integer, 2 },
    { binary_to_list, 1 },
    { binary_to_list, 3 },
    { binary_to_term, 1 },
    { binary_to_term, 2 },
    { bit_size, 1 },
    { bitstring_to_list, 1 },
    { byte_size, 1 },
    % { date, 0 },
    { exit, 1 },
    % { float, 1 },
    { float_to_binary, 1 },
    { float_to_list, 1 },
    { hd, 1 },
    { integer_to_binary, 1 },
    { integer_to_binary, 2 },
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
    {function,3},
    {in,2},
    {'not',1},
    {'or',2},
    {'receive',1},
    {'try',1},
    {'xor',2}
  ].
