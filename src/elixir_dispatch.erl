%% Helpers related to dispatching to imports.
-module(elixir_dispatch).
-export([get_functions/1, get_macros/3, get_optional_macros/1,
  default_macros/0, default_functions/0, default_requires/0,
  format_error/1, dispatch_require/6, dispatch_imports/5]).
-include("elixir.hrl").
-define(BUILTIN, '__MAIN__.Elixir.Builtin').

default_functions() ->
  [
    { ?BUILTIN, get_functions(?BUILTIN) },
    { erlang, in_erlang_functions() }
  ].
default_macros() ->
  [ { ?BUILTIN, get_optional_macros(?BUILTIN) } ].
default_requires() ->
  [ ?BUILTIN ].

%% Get macros/functions from the given module and
%% raise an exception if appropriated.

get_functions(?BUILTIN = Module) ->
  try
    ordsets:from_list((Module:module_info(exports) -- get_optional_macros(Module)) --
      [{module_info,0},{module_info,1},{'__info__',1}])
  catch
    error:undef -> []
  end;

get_functions(Module) ->
  ordsets:from_list(Module:module_info(exports) -- get_optional_macros(Module)).

get_macros(_Line, ?BUILTIN, _S) ->
  ordsets:from_list(get_optional_macros(?BUILTIN));

get_macros(Line, Module, S) ->
  try
    ordsets:from_list(Module:'__info__'(macros))
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

get_optional_macros(?BUILTIN) ->
  try
    ordsets:from_list(?BUILTIN:'__info__'(macros) ++ in_erlang_macros())
  catch
    error:undef -> ordsets:from_list(in_erlang_macros())
  end;

%% Do not try to get macros from Erlang. Speeds up compilation a bit.
get_optional_macros(erlang) -> [];

get_optional_macros(Receiver) ->
  try
    ordsets:from_list(Receiver:'__info__'(macros))
  catch
    error:undef -> []
  end.

%% Dispatch based on scope's imports

dispatch_imports(Line, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = { Name, Arity },
  case find_dispatch(Tuple, S#elixir_scope.functions) of
    nil ->
      case find_dispatch(Tuple, S#elixir_scope.macros) of
        nil -> Callback();
        Receiver ->
          elixir_import:record(import, Tuple, Receiver, S),
          dispatch_macro(Line, Receiver, Tuple, Args, S)
      end;
    Receiver ->
      elixir_import:record(import, Tuple, Receiver, S),
      elixir_translator:translate_each({ { '.', Line, [Receiver, Name] }, Line, Args }, S)
  end.

%% Dispatch based on scope's refer

dispatch_require(Line, Receiver, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = {Name, Arity},

  case lists:member(Tuple, get_optional_macros(Receiver)) of
    true  -> dispatch_macro(Line, Receiver, Tuple, Args, S);
    false -> Callback()
  end.

%% HELPERS

dispatch_macro(Line, ?BUILTIN = Receiver, { Name, Arity } = Tuple, Args, S) ->
  case lists:member(Tuple, in_erlang_macros()) of
    true  -> elixir_macros:translate_macro({ Name, Line, Args }, S);
    false -> dispatch_macro(Line, Receiver, Name, Arity, Args, S)
  end;

dispatch_macro(Line, Receiver, { Name, Arity }, Args, S) ->
  dispatch_macro(Line, Receiver, Name, Arity, Args, S).

dispatch_macro(Line, Receiver, Name, Arity, Args, S) ->
  ensure_required(Line, Receiver, Name, Arity, S),
  Tree = try
    apply(Receiver, Name, Args)
  catch
    Kind:Reason ->
      Info = { Receiver, Name, length(Args), [{ file, S#elixir_scope.filename }, { line, Line }] },
      erlang:raise(Kind, Reason, insert_before_dispatch_macro(Info, erlang:get_stacktrace()))
  end,
  NewS = S#elixir_scope{macro={Receiver,Name,Arity}},
  { TTree, TS } = elixir_translator:translate_each(elixir_quote:linify(Line, Tree), NewS),
  { TTree, TS#elixir_scope{macro=S#elixir_scope.macro} }.

find_dispatch(Tuple, [{ Name, Values }|T]) ->
  case ordsets:is_element(Tuple, Values) of
    true  -> Name;
    false -> find_dispatch(Tuple, T)
  end;

find_dispatch(_Tuple, []) -> nil.

%% Insert call site into backtrace right after dispatch macro

insert_before_dispatch_macro(Info, [{ elixir_dispatch, dispatch_macro, _, _ }|_] = T) ->
  [Info|T];

insert_before_dispatch_macro(Info, [H|T]) ->
  [H|insert_before_dispatch_macro(Info, T)];

insert_before_dispatch_macro(_, []) ->
  [].

%% ERROR HANDLING

ensure_required(Line, Receiver, Name, Arity, S) ->
  Requires = S#elixir_scope.requires,
  case ordsets:is_element(Receiver, Requires) of
    true  -> ok;
    false ->
      Tuple = { unrequired_module, { Receiver, Name, Arity, Requires } },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

format_error({ unrequired_module,{Receiver, Name, Arity, Required }}) ->
  io_lib:format("tried to invoke macro ~s.~s/~B but module was not required. Required: ~p", [Receiver, Name, Arity, Required]);

format_error({ no_macros, Module }) ->
  io_lib:format("could not load macros from module ~s", [Module]).

%% Implemented in Erlang.
in_erlang_functions() ->
  [
    { abs, 1 },
    { atom_to_binary, 2 },
    { atom_to_list, 1 },
    { binary_part, 2 },
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
    { exit, 2 },
    { float, 1 },
    { float_to_list, 1 },
    { halt, 0 },
    { halt, 1 },
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
    { spawn, 2 },
    { spawn, 3 },
    { spawn, 4 },
    { spawn_link, 1 },
    { spawn_link, 2 },
    { spawn_link, 3 },
    { spawn_link, 4 },
    { spawn_monitor, 1 },
    { spawn_monitor, 3 },
    { spawn_opt, 2 },
    { spawn_opt, 3 },
    { spawn_opt, 4 },
    { spawn_opt, 5 },
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

in_erlang_macros() ->
  [
    {'@',1},
    {'+',1},
    {'+',2},
    {'-',1},
    {'-',2},
    {'*',2},
    {'/',2},
    {'<-',2},
    {'++',2},
    {'--',2},
    {'<',2},
    {'>',2},
    {'<=',2},
    {'>=',2},
    {'==',2},
    {'!=',2},
    {'===',2},
    {'!==',2},
    {'access',2},
    {'apply',2},
    {'apply',3},
    {'not',1},
    {'and',2},
    {'or',2},
    {'xor',2},
    {'use',1},
    {'use',2},
    {'defmodule',2},
    {'defmodule',3},
    {'def',1},
    {'def',2},
    {'def',4},
    {'defp',1},
    {'defp',2},
    {'defp',4},
    {'defmacro',1},
    {'defmacro',2},
    {'defmacro',4},
    {'case',2},
    {'receive',1},
    {'try',1},
    {'var!',1}
  ].
