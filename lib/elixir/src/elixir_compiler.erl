%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

%% Elixir compiler front-end to the Erlang backend.
-module(elixir_compiler).
-export([string/3, quoted/3, bootstrap/0, file/2, compile/4]).
-include("elixir.hrl").

string(Contents, File, Callback) ->
  Forms = elixir:'string_to_quoted!'(Contents, 1, 1, File, elixir_config:get(parser_options)),
  quoted(Forms, File, Callback).

quoted(Forms, File, Callback) ->
  Previous = get(elixir_module_binaries),

  try
    put(elixir_module_binaries, []),
    Env = (elixir_env:new())#{line := 1, file := File, tracers := elixir_config:get(tracers)},

    elixir_lexical:run(
      Env,
      fun (LexicalEnv) -> maybe_fast_compile(Forms, LexicalEnv) end,
      fun (#{lexical_tracker := Pid}) -> Callback(File, Pid) end
    ),

    lists:reverse(get(elixir_module_binaries))
  after
    put(elixir_module_binaries, Previous)
  end.

file(File, Callback) ->
  {ok, Bin} = file:read_file(File),
  string(elixir_utils:characters_to_list(Bin), File, Callback).

%% Evaluates the given code through the Erlang compiler.
%% It may end-up evaluating the code if it is deemed a
%% more efficient strategy depending on the code snippet.
maybe_fast_compile(Forms, E) ->
  case (?key(E, module) == nil) andalso allows_fast_compilation(Forms) andalso
        (not elixir_config:is_bootstrap()) of
    true  -> fast_compile(Forms, E);
    false -> compile(Forms, [], [], E)
  end,
  ok.

compile(Quoted, ArgsList, _CompilerOpts, #{line := Line, file := File} = E) ->
  Block = no_tail_optimize([{line, Line}], Quoted),
  {Expanded, SE, EE} = elixir_expand:expand(Block, elixir_env:env_to_ex(E), E),
  elixir_env:check_unused_vars(SE, EE),

  {Vars, TS} = elixir_erl_var:from_env(E),
  {ErlExprs, _} = elixir_erl_pass:translate(Expanded, erl_anno:new(Line), TS),
  Forms = code_eval(ErlExprs, Line, File, Vars),

  {value, Fun, _} = erl_eval:expr(Forms, #{}),
  Args = list_to_tuple(ArgsList),
  {Fun(Args), SE, EE}.

code_eval(Expr, Line, File, Vars) when is_binary(File), is_integer(Line) ->
  Ann = erl_anno:new(Line),
  Tuple = {tuple, Ann, [{var, Ann, Var} || {_, Var} <- Vars]},
  {'fun', Ann, {clauses, [{clause, Ann, [Tuple], [], [Expr]}]}}.

allows_fast_compilation({'__block__', _, Exprs}) ->
  lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule, _, [_, [{do, _}]]}) ->
  true;
allows_fast_compilation(_) ->
  false.

fast_compile({'__block__', _, Exprs}, E) ->
  lists:foldl(fun(Expr, _) -> fast_compile(Expr, E) end, nil, Exprs);
fast_compile({defmodule, Meta, [Mod, [{do, Block}]]}, NoLineE) ->
  E = NoLineE#{line := ?line(Meta)},

  Expanded = case Mod of
    {'__aliases__', AliasMeta, List} ->
      case elixir_aliases:expand_or_concat(AliasMeta, List, E, true) of
        Receiver when is_atom(Receiver) -> Receiver;
        _ -> 'Elixir.Macro':expand(Mod, E)
      end;

    _ ->
      'Elixir.Macro':expand(Mod, E)
  end,

  ContextModules = [Expanded | ?key(E, context_modules)],
  elixir_module:compile(Meta, Expanded, Block, [], false, E#{context_modules := ContextModules}).

no_tail_optimize(Meta, Block) ->
  {'__block__', Meta, [
    {'=', Meta, [{result, Meta, ?MODULE}, Block]},
    {{'.', Meta, [elixir_utils, noop]}, Meta, []},
    {result, Meta, ?MODULE}
  ]}.

%% Bootstrapper

bootstrap() ->
  {ok, _} = application:ensure_all_started(elixir),
  elixir_config:static(#{bootstrap => true}),
  elixir_config:put(docs, false),
  elixir_config:put(ignore_module_conflict, true),
  elixir_config:put(on_undefined_variable, raise),
  elixir_config:put(parser_options, []),
  elixir_config:put(relative_paths, false),
  elixir_config:put(tracers, []),
  elixir_config:put(infer_signatures, []),
  {Init, Main} = bootstrap_files(),
  {ok, Cwd} = file:get_cwd(),
  Lib = filename:join(Cwd, "lib/elixir/lib"),
  [bootstrap_file(Lib, File) || File <- [<<"kernel.ex">> | Init]],
  [bootstrap_file(Lib, File) || File <- [<<"kernel.ex">> | Main]].

bootstrap_file(Lib, Suffix) ->
  try
    File = filename:join(Lib, Suffix),
    Mods = file(File, fun(_, _) -> ok end),
    _ = [binary_to_path(X, "lib/elixir/ebin") || X <- Mods],
    io:format("Compiled ~ts~n", [Suffix])
  catch
    Kind:Reason:Stacktrace ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, Stacktrace]),
      erlang:halt(1)
  end.

bootstrap_files() ->
  {
    [
     <<"kernel/utils.ex">>,
     <<"macro/env.ex">>,
     <<"range.ex">>,
     <<"keyword.ex">>,
     <<"module.ex">>,
     <<"list.ex">>,
     <<"macro.ex">>,
     <<"kernel/typespec.ex">>,
     <<"code.ex">>,
     <<"code/identifier.ex">>,
     <<"protocol.ex">>,
     <<"stream/reducers.ex">>,
     <<"enum.ex">>,
     <<"regex.ex">>,
     <<"inspect/algebra.ex">>,
     <<"inspect.ex">>,
     <<"string.ex">>,
     <<"string/chars.ex">>
    ],
    [
     <<"list/chars.ex">>,
     <<"bitwise.ex">>,
     <<"map.ex">>,
     <<"module/parallel_checker.ex">>,
     <<"module/behaviour.ex">>,
     <<"module/types/helpers.ex">>,
     <<"module/types/descr.ex">>,
     <<"module/types/of.ex">>,
     <<"module/types/pattern.ex">>,
     <<"module/types/apply.ex">>,
     <<"module/types/expr.ex">>,
     <<"module/types/traverse.ex">>,
     <<"module/types.ex">>,
     <<"exception.ex">>,
     <<"path.ex">>,
     <<"file.ex">>,
     <<"access.ex">>,
     <<"io.ex">>,
     <<"system.ex">>,
     <<"code/formatter.ex">>,
     <<"code/normalizer.ex">>,
     <<"kernel/cli.ex">>,
     <<"kernel/error_handler.ex">>,
     <<"kernel/parallel_compiler.ex">>,
     <<"kernel/lexical_tracker.ex">>
    ]
  }.

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  case file:write_file(Path, Binary) of
    ok -> Path;
    {error, Reason} -> error('Elixir.File.Error':exception([{action, "write to"}, {path, Path}, {reason, Reason}]))
  end.
