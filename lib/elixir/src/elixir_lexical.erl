%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

%% Module responsible for tracking lexical information.
-module(elixir_lexical).
-export([run/3, with_file/3, trace/2, format_error/1]).
-include("elixir.hrl").

run(#{tracers := Tracers} = E, ExecutionCallback, AfterExecutionCallback) ->
  case elixir_config:is_bootstrap() of
    false ->
      {ok, Pid} = ?tracker:start_link(),
      LexEnv = E#{lexical_tracker := Pid, tracers := [?MODULE | Tracers]},
      elixir_env:trace(start, LexEnv),

      try ExecutionCallback(LexEnv) of
        Res ->
          warn_unused_aliases(Pid, LexEnv),
          warn_unused_imports(Pid, LexEnv),
          warn_unused_requires(Pid, LexEnv),
          AfterExecutionCallback(LexEnv),
          Res
      after
        elixir_env:trace(stop, LexEnv),
        unlink(Pid),
        ?tracker:stop(Pid)
      end;

    true ->
      ExecutionCallback(E),
      AfterExecutionCallback(E)
  end.
trace({alias_expansion, _Meta, Lookup, _Result}, #{lexical_tracker := Pid}) ->
  ?tracker:alias_dispatch(Pid, Lookup),
  ok;
trace({require, Meta, Module, _Opts}, #{lexical_tracker := Pid}) ->
  case lists:keyfind(from_macro, 1, Meta) of
    {from_macro, true} -> ?tracker:remote_dispatch(Pid, Module, compile);
    _ -> ?tracker:add_export(Pid, Module)
  end,
  ok;
trace({struct_expansion, _Meta, Module, _Keys}, #{lexical_tracker := Pid}) ->
  ?tracker:add_export(Pid, Module),
  ok;
trace({alias_reference, _Meta, Module}, #{lexical_tracker := Pid} = E) ->
  case E of
    %% Alias references inside patterns and guards in functions are not
    %% compile time dependencies.
    #{function := nil} -> ?tracker:remote_dispatch(Pid, Module, compile);
    #{context := nil} -> ?tracker:remote_dispatch(Pid, Module, runtime);
    #{} -> ok
  end,
  ok;
trace({remote_function, _Meta, Module, _Function, _Arity}, #{lexical_tracker := Pid} = E) ->
  ?tracker:remote_dispatch(Pid, Module, mode(E)),
  ok;
trace({remote_macro, _Meta, Module, _Function, _Arity}, #{lexical_tracker := Pid}) ->
  ?tracker:remote_dispatch(Pid, Module, compile),
  ok;
trace({imported_function, _Meta, Module, Function, Arity}, #{lexical_tracker := Pid} = E) ->
  ?tracker:import_dispatch(Pid, Module, {Function, Arity}, mode(E)),
  ok;
trace({imported_macro, _Meta, Module, Function, Arity}, #{lexical_tracker := Pid}) ->
  ?tracker:import_dispatch(Pid, Module, {Function, Arity}, compile),
  ok;
trace({imported_quoted, _Meta, Module, Function, Arities}, #{lexical_tracker := Pid}) ->
  ?tracker:import_quoted(Pid, Module, Function, Arities),
  ok;
trace({compile_env, App, Path, Return}, #{lexical_tracker := Pid}) ->
  ?tracker:add_compile_env(Pid, App, Path, Return),
  ok;
trace(_, _) ->
  ok.

mode(#{function := nil}) -> compile;
mode(#{}) -> runtime.

%% EXTERNAL SOURCES

with_file(File, #{lexical_tracker := nil} = E, Callback) ->
  Callback(E#{file := File});
with_file(File, #{lexical_tracker := Pid} = E, Callback) ->
  try
    ?tracker:set_file(Pid, File),
    Callback(E#{file := File})
  after
    ?tracker:reset_file(Pid)
  end.

%% ERROR HANDLING

warn_unused_imports(Pid, E) ->
  [elixir_errors:file_warn(Meta, ?key(E, file), ?MODULE, {unused_import, ModOrMFA})
   || {Module, Imports} <- ?tracker:collect_unused_imports(Pid),
      {ModOrMFA, Meta} <- unused_imports_for_module(Module, Imports)],
  ok.

warn_unused_requires(Pid, E) ->
  [elixir_errors:file_warn(Meta, ?key(E, file), ?MODULE, {unused_require, Module, Alias, AliasUsed})
   || {Module, Meta, Alias, AliasUsed} <- ?tracker:collect_unused_requires(Pid)],
  ok.

unused_imports_for_module(Module, Imports) ->
  case Imports of
    #{Module := Meta} -> [{Module, Meta}];
    #{} -> [{{Module, Fun, Arity}, Meta} || {{Fun, Arity}, Meta} <- maps:to_list(Imports)]
  end.

warn_unused_aliases(Pid, E) ->
  [elixir_errors:file_warn(Meta, ?key(E, file), ?MODULE, {unused_alias, Module})
   || {Module, Meta} <- ?tracker:collect_unused_aliases(Pid)],
  ok.

format_error({unused_alias, Module}) ->
  io_lib:format("unused alias ~ts", [elixir_aliases:inspect(Module)]);
format_error({unused_import, {Module, Function, Arity}}) ->
  io_lib:format("unused import ~ts.~ts/~w", [elixir_aliases:inspect(Module), Function, Arity]);
format_error({unused_import, Module}) ->
  io_lib:format("unused import ~ts", [elixir_aliases:inspect(Module)]);
format_error({unused_require, Module, Alias, AliasUsed}) ->
  Message = if
    Alias == false -> "unused require ~ts";
    AliasUsed -> "unused require ~ts (convert it to an alias instead)";
    true -> "unused require ~ts (the alias is also unused)"
  end,
  io_lib:format(Message, [elixir_aliases:inspect(Module)]).
