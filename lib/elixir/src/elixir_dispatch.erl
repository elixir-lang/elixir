%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([default_macros/0, default_functions/0, default_requires/0,
  dispatch_require/6, dispatch_import/5,
  require_function/5, import_function/4,
  expand_import/5, expand_require/5, find_import/4, format_error/1]).
-include("elixir.hrl").

-import(ordsets, [is_element/2]).
-define(builtin, 'Elixir.Kernel').

default_functions() ->
  [ { ?builtin, elixir_imported_functions() } ].
default_macros() ->
  [ { ?builtin, elixir_imported_macros() } ].
default_requires() ->
  [ 'Elixir.Integer', 'Elixir.Kernel', 'Elixir.Kernel.Typespec', 'Elixir.Record' ].

find_import(Meta, Name, Arity, S) ->
  Tuple = { Name, Arity },

  case find_dispatch(Meta, Tuple, S) of
    { function, Receiver } ->
      elixir_lexical:record_import(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_import(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      Receiver;
    { macro, Receiver } ->
      elixir_lexical:record_import(Receiver, S#elixir_scope.lexical_tracker),
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
      elixir_lexical:record_import(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_import(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      remote_function(Meta, Receiver, Name, Arity, S);
    { macro, _Receiver } ->
      false;
    { import, Receiver } ->
      elixir_lexical:record_remote(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
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
  case is_element({ Name, Arity }, get_optional_macros(Receiver)) of
    true  -> false;
    false -> remote_function(Meta, Receiver, Name, Arity, S)
  end.

remote_function(Meta, Receiver, Name, Arity, S) ->
  Tuple = { Name, Arity },

  Final =
    case (Receiver == ?builtin) andalso is_element(Tuple, in_erlang_functions()) of
      true  -> erlang;
      false -> Receiver
    end,

  Line = ?line(Meta),

  { { 'fun', Line, { function,
    { atom, Line, Final },
    { atom, Line, Name },
    { integer, Line, Arity}
  } }, S }.

%% Function dispatch

dispatch_import(Meta, Name, Args, S, Callback) ->
  case expand_import(Meta, { Name, length(Args) }, Args, S, []) of
    { ok, Receiver, Tree } ->
      translate_expansion(Meta, Receiver, Tree, S);
    { ok, Receiver } ->
      elixir_translator:translate_each({ { '.', Meta, [Receiver, Name] }, Meta, Args }, S);
    error ->
      Callback()
  end.

dispatch_require(Meta, Receiver, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = { Name, Arity },

  case (Receiver == ?builtin) andalso is_element(Tuple, in_erlang_functions()) of
    true ->
      elixir_lexical:record_remote(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function),
      { TArgs, SA } = elixir_translator:translate_args(Args, S),
      { ?wrap_call(?line(Meta), erlang, Name, TArgs), SA };
    false ->
      case expand_require(Meta, Receiver, Tuple, Args, S) of
        error ->
          Callback();
        { ok, Receiver, Tree } ->
          translate_expansion(Meta, Receiver, Tree, S)
      end
  end.

%% Macros expansion

expand_import(Meta, { Name, Arity } = Tuple, Args, S, Extra) ->
  Module   = S#elixir_scope.module,
  Dispatch = find_dispatch(Meta, Tuple, Extra, S),
  Function = S#elixir_scope.function,
  Local    = (Function /= nil) andalso (Function /= Tuple) andalso
              elixir_def_local:macro_for(Tuple, true, S),

  case Dispatch of
    %% In case it is an import, or the receive is the same as the
    %% current module (happens in bootstraping), we dispatch the import.
    { Kind, Receiver } when Kind == import; Receiver == Module ->
      do_expand_import(Meta, Tuple, Args, Module, S, Dispatch);

    %% There is a local and an import. This is a conflict.
    { _, Receiver } when Local /= false ->
      Error = { macro_conflict, { Receiver, Name, Arity } },
      elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, Error);

    %% There is no local. Dispatch the import.
    _ when Local == false ->
      do_expand_import(Meta, Tuple, Args, Module, S, Dispatch);

    %% Dispatch to the local.
    _ ->
      elixir_tracker:record_local(Tuple, Module, Function),
      { ok, Module, expand_macro_fun(Meta, Local(), Module, Name, Args, Module, S) }
  end.

do_expand_import(Meta, { Name, Arity } = Tuple, Args, Module, S, Result) ->
  case Result of
    { function, Receiver } ->
      elixir_lexical:record_import(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_import(Tuple, Receiver, Module, S#elixir_scope.function),
      Endpoint = case (Receiver == ?builtin) andalso is_element(Tuple, in_erlang_functions()) of
        true  -> erlang;
        false -> Receiver
      end,
      { ok, Endpoint };
    { macro, Receiver } ->
      elixir_lexical:record_import(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_import(Tuple, Receiver, Module, S#elixir_scope.function),
      { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, S) };
    { import, Receiver } ->
      case expand_require([{require,false}|Meta], Receiver, Tuple, Args, S) of
        { ok, _, _ } = Response -> Response;
        error -> { ok, Receiver }
      end;
    false ->
      error
  end.

expand_require(Meta, Receiver, { Name, Arity } = Tuple, Args, S) ->
  Module   = S#elixir_scope.module,
  Function = S#elixir_scope.function,

  case is_element(Tuple, get_optional_macros(Receiver)) of
    true  ->
      elixir_lexical:record_remote(Receiver, S#elixir_scope.lexical_tracker),
      elixir_tracker:record_remote(Tuple, Receiver, Module, Function),
      { ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, S) };
    false -> error
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
      erlang:raise(Kind, Reason, prune_stacktrace(Info, erlang:get_stacktrace(), SArg))
  end.

expand_macro_named(Meta, Receiver, Name, Arity, Args, Module, S) ->
  ProperName  = ?elixir_macro(Name),
  ProperArity = Arity + 1,
  Fun         = fun Receiver:ProperName/ProperArity,
  expand_macro_fun(Meta, Fun, Receiver, Name, Args, Module, S).

translate_expansion(Meta, Receiver, Tree, S) ->
  Line = ?line(Meta),
  New  = S#elixir_scope.macro_counter + 1,

  try
    elixir_translator:translate_each(
      elixir_quote:linify_with_context_counter(Line, { Receiver, New }, Tree),
      S#elixir_scope{macro_counter=New}
    )
  catch
    Kind:Reason ->
      erlang:raise(Kind, Reason, prune_stacktrace(mfa(Line, S), erlang:get_stacktrace(), nil))
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
  case is_import(Meta) of
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
prune_stacktrace(Info, [{ _, _, [S|_], _ }|_], S) ->
  [Info];

%% We've reached the elixir_dispatch internals, skip it with the rest
prune_stacktrace(Info, [{ elixir_dispatch, _, _, _ }|_], _) ->
  [Info];

prune_stacktrace(Info, [H|T], S) ->
  [H|prune_stacktrace(Info, T, S)];

prune_stacktrace(Info, [], _) ->
  [Info].

%% ERROR HANDLING

format_error({ unrequired_module, { Receiver, Name, Arity, _Required }}) ->
  Module = elixir_errors:inspect(Receiver),
  io_lib:format("you must require ~ts before invoking the macro ~ts.~ts/~B",
    [Module, Module, Name, Arity]);

format_error({ macro_conflict, { Receiver, Name, Arity } }) ->
  io_lib:format("call to local macro ~ts/~B conflicts with imported ~ts.~ts/~B, "
    "please rename the local macro or remove the conflicting import",
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
    error:undef -> []
  end.

%% Functions imported from Erlang module. MUST BE SORTED.
%% TODO: Inline operators here as well once bug in erl_eval is fixed.
in_erlang_functions() ->
  [
    { abs, 1 },
    { apply, 2 },
    { apply, 3 },
    { atom_to_list, 1 },
    { binary_part, 3 },
    { binary_to_float, 1 },
    { binary_to_float, 2 },
    { binary_to_integer, 1 },
    { binary_to_integer, 2 },
    { binary_to_term, 1 },
    { binary_to_term, 2 },
    { bit_size, 1 },
    { bitstring_to_list, 1 },
    { byte_size, 1 },
    % { date, 0 },
    { exit, 1 },
    % { float, 1 },
    { float_to_binary, 1 },
    % { float_to_binary, 2 },
    { float_to_list, 1 },
    % { float_to_list, 2 },
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
    { list_to_bitstring, 1 },
    { list_to_existing_atom, 1 },
    { list_to_float, 1 },
    { list_to_integer, 1 },
    { list_to_integer, 2 },
    { list_to_tuple, 1 },
    { make_ref, 0 },
    { max, 2 },
    { min, 2 },
    { node, 0 },
    { node, 1 },
    % { now, 0 },
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
