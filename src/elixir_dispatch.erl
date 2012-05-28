%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([default_macros/0, default_functions/0, default_requires/0,
  dispatch_require/6, dispatch_imports/5,
  require_function/5, import_function/4,
  format_error/1]).
-include("elixir.hrl").
-import(ordsets, [is_element/2]).
-define(BUILTIN, '__MAIN__.Elixir.Builtin').

default_functions() ->
  [ { ?BUILTIN, ordsets:union(in_elixir_functions(), in_erlang_functions()) } ].
default_macros() ->
  [ { ?BUILTIN, ordsets:union(in_elixir_macros(), in_erlang_macros()) } ].
default_requires() ->
  [ ?BUILTIN ].

%% Function retrieval

import_function(Line, Name, Arity, S) ->
  Tuple = { Name, Arity },
  case find_dispatch(Tuple, S#elixir_scope.functions) of
    false ->
      case find_dispatch(Tuple, S#elixir_scope.macros) of
        false -> { { 'fun', Line, { function, Name, Arity } }, S };
        _ -> false
      end;
    Receiver ->
      elixir_import:record(import, Tuple, Receiver, S),
      remote_function(Line, Receiver, Name, Arity, S)
  end.

require_function(Line, Receiver, Name, Arity, S) ->
  Tuple = { Name, Arity },

  case is_element(Tuple, get_optional_macros(Receiver)) of
    true  -> false;
    false -> remote_function(Line, Receiver, Name, Arity, S)
  end.

%% Dispatch based on scope's imports

dispatch_imports(Line, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = { Name, Arity },
  case find_dispatch(Tuple, S#elixir_scope.functions) of
    false ->
      case find_dispatch(Tuple, S#elixir_scope.macros) of
        false ->
          Fun = (S#elixir_scope.function /= Tuple) andalso elixir_def_local:macro_for(Tuple, true, S),
          case Fun of
            false -> Callback();
            _ ->
              Receiver = S#elixir_scope.module,
              elixir_import:record(import, Tuple, Receiver, S),
              dispatch_macro_fun(Line, Fun, Receiver, Name, Arity, Args, S)
          end;
        ?BUILTIN ->
          elixir_import:record(import, Tuple, ?BUILTIN, S),
          dispatch_builtin_macro(Line, Tuple, Args, S);
        Receiver ->
          elixir_import:record(import, Tuple, Receiver, S),
          dispatch_macro(Line, Receiver, Name, Arity, Args, S)
      end;
    Receiver ->
      elixir_import:record(import, Tuple, Receiver, S),
      Endpoint = case (Receiver == ?BUILTIN) andalso is_element(Tuple, in_erlang_functions()) of
        true  -> erlang;
        false -> Receiver
      end,
      elixir_translator:translate_each({ { '.', Line, [Endpoint, Name] }, Line, Args }, S)
  end.

%% Dispatch based on scope's require

dispatch_require(Line, ?BUILTIN, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = {Name, Arity},

  case is_element(Tuple, in_erlang_functions()) of
    true ->
      elixir_translator:translate_each({ { '.', Line, [erlang, Name] }, Line, Args }, S);
    false ->
      case dispatch_builtin_macro(Line, Tuple, Args, S) of
        false -> Callback();
        Else -> Else
      end
  end;

dispatch_require(Line, Receiver, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = {Name, Arity},

  Fun = (S#elixir_scope.module == Receiver) andalso (S#elixir_scope.function /= Tuple) andalso
    elixir_def_local:macro_for(Tuple, false, S),

  case Fun of
    false ->
      case is_element(Tuple, get_optional_macros(Receiver)) of
        true  -> dispatch_macro(Line, Receiver, Name, Arity, Args, S);
        false -> Callback()
      end;
    _ ->
      elixir_import:record(import, Tuple, Receiver, S),
      dispatch_macro_fun(Line, Fun, Receiver, Name, Arity, Args, S)
  end.

%% HELPERS

dispatch_builtin_macro(Line, { Name, Arity } = Tuple, Args, S) ->
  case is_element(Tuple, in_erlang_macros()) of
    true  -> elixir_macros:translate_macro({ Name, Line, Args }, S);
    false ->
      case is_element(Tuple, in_elixir_macros()) of
        true -> dispatch_macro(Line, ?BUILTIN, Name, Arity, Args, S);
        false -> false
      end
  end.

dispatch_macro(Line, Receiver, Name, Arity, Args, S) ->
  %% Fix macro name and arity
  ProperName  = ?ELIXIR_MACRO(Name),
  ProperArity = Arity + 1,
  dispatch_macro_fun(Line, fun Receiver:ProperName/ProperArity, Receiver, Name, Arity, Args, S).

dispatch_macro_fun(Line, Fun, Receiver, Name, Arity, Args, S) ->
  ensure_required(Line, Receiver, Name, Arity, S),
  Tree = try
    apply(Fun, [{Line,S}|Args])
  catch
    Kind:Reason ->
      Info = { Receiver, Name, length(Args), [{ file, S#elixir_scope.filename }, { line, Line }] },
      erlang:raise(Kind, Reason, insert_before_dispatch_macro(Info, erlang:get_stacktrace()))
  end,
  NewS = S#elixir_scope{macro=[{Line,Receiver,Name,Arity}|S#elixir_scope.macro]},
  { TTree, TS } = elixir_translator:translate_each(elixir_quote:linify(Line, Tree), NewS),
  { TTree, TS#elixir_scope{macro=S#elixir_scope.macro} }.

find_dispatch(Tuple, [{ Name, Values }|T]) ->
  case is_element(Tuple, Values) of
    true  -> Name;
    false -> find_dispatch(Tuple, T)
  end;

find_dispatch(_Tuple, []) -> false.

%% Insert call site into backtrace right after dispatch macro

insert_before_dispatch_macro(Info, [{ elixir_dispatch, dispatch_macro_fun, _, _ }|_] = T) ->
  [Info|T];

insert_before_dispatch_macro(Info, [H|T]) ->
  [H|insert_before_dispatch_macro(Info, T)];

insert_before_dispatch_macro(_, []) ->
  [].

%% ERROR HANDLING

ensure_required(_Line, Receiver, _Name, _Arity, #elixir_scope{module=Receiver}) -> ok;
ensure_required(Line, Receiver, Name, Arity, S) ->
  Requires = S#elixir_scope.requires,
  case is_element(Receiver, Requires) of
    true  -> ok;
    false ->
      Tuple = { unrequired_module, { Receiver, Name, Arity, Requires } },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

format_error({ unrequired_module,{Receiver, Name, Arity, Required }}) ->
  io_lib:format("tried to invoke macro ~s.~s/~B but module was not required. Required: ~p",
    [elixir_errors:inspect(Receiver), Name, Arity, [elixir_errors:inspect(R) || R <- Required]]).

%% INTROSPECTION

remote_function(Line, Receiver, Name, Arity, S) ->
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

%% Functions imported from Elixit.Builtin module. Sorted on compilation.

in_elixir_functions() ->
  try
    ?BUILTIN:'__info__'(functions) -- [{'__info__',1}]
  catch
    error:undef -> []
  end.

%% Macros imported from Elixit.Builtin module. Sorted on compilation.

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
    % Those are allowed in guard clauses, so we need to bring them back.
    % { binary_part, 2 },
    % { binary_part, 3 },
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
    { halt, 0 },
    { halt, 1 },
    { halt, 2 },
    { hd, 1 },
    { integer_to_list, 1 },
    { integer_to_list, 2 },
    { iolist_size, 1 },
    { iolist_to_binary, 1 },
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
    {'not',1},
    {'or',2},
    {'receive',1},
    {'try',1},
    {'var!',1},
    {'xor',2}
  ].