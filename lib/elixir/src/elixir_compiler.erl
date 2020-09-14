%% Elixir compiler front-end to the Erlang backend.
-module(elixir_compiler).
-export([string/3, quoted/3, bootstrap/0,
         file/2, file_to_path/3, eval_forms/3]).
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
      fun (LexicalEnv) -> eval_forms(Forms, [], LexicalEnv) end,
      fun (#{lexical_tracker := Pid}) -> Callback(File, Pid) end
    ),

    lists:reverse(get(elixir_module_binaries))
  after
    put(elixir_module_binaries, Previous)
  end.

file(File, Callback) ->
  {ok, Bin} = file:read_file(File),
  string(elixir_utils:characters_to_list(Bin), File, Callback).

file_to_path(File, Dest, Callback) when is_binary(File), is_binary(Dest) ->
  file(File, fun(CallbackFile, CallbackLexical) ->
    _ = [binary_to_path(Mod, Dest) || Mod <- get(elixir_module_binaries)],
    Callback(CallbackFile, CallbackLexical)
  end).

%% Evaluates the given code through the Erlang compiler.
%% It may end-up evaluating the code if it is deemed a
%% more efficient strategy depending on the code snippet.

eval_forms(Forms, Args, E) ->
  case (?key(E, module) == nil) andalso allows_fast_compilation(Forms) of
    true  ->
      {Result, _Binding, EE} = elixir:eval_forms(Forms, [], E),
      {Result, EE};
    false ->
      compile(Forms, Args, E)
  end.

compile(Quoted, ArgsList, E) ->
  Args = list_to_tuple(ArgsList),
  {Expanded, EE} = elixir_expand:expand(Quoted, E),
  elixir_env:check_unused_vars(EE),

  {Module, Fun, Purgeable} =
    elixir_erl_compiler:spawn(fun spawned_compile/2, [Expanded, E]),

  {dispatch(Module, Fun, Args, Purgeable), EE}.

spawned_compile(ExExprs, #{line := Line, file := File} = E) ->
  {Vars, S} = elixir_env:env_to_scope(E),
  {ErlExprs, _} = elixir_erl_pass:translate(ExExprs, S),

  Module = retrieve_compiler_module(),
  Fun  = code_fun(?key(E, module)),
  Forms = code_mod(Fun, ErlExprs, Line, File, Module, Vars),

  {Module, Binary} = elixir_erl_compiler:noenv_forms(Forms, File, [nowarn_nomatch]),
  code:load_binary(Module, "", Binary),
  {Module, Fun, is_purgeable(Module, Binary)}.

dispatch(Module, Fun, Args, Purgeable) ->
  Res = Module:Fun(Args),
  code:delete(Module),
  Purgeable andalso code:purge(Module),
  return_compiler_module(Module, Purgeable),
  Res.

code_fun(nil) -> '__FILE__';
code_fun(_)   -> '__MODULE__'.

code_mod(Fun, Expr, Line, File, Module, Vars) when is_binary(File), is_integer(Line) ->
  Ann = erl_anno:new(Line),
  Tuple = {tuple, Ann, [{var, Ann, Var} || {_, Var} <- Vars]},
  Relative = elixir_utils:relative_to_cwd(File),

  [{attribute, Ann, file, {elixir_utils:characters_to_list(Relative), 1}},
   {attribute, Ann, module, Module},
   {attribute, Ann, compile, no_auto_import},
   {attribute, Ann, export, [{Fun, 1}, {'__RELATIVE__', 0}]},
   {function, Ann, Fun, 1, [
     {clause, Ann, [Tuple], [], [Expr]}
   ]},
   {function, Ann, '__RELATIVE__', 0, [
     {clause, Ann, [], [], [elixir_erl:elixir_to_erl(Relative)]}
   ]}].

retrieve_compiler_module() ->
  elixir_code_server:call(retrieve_compiler_module).

return_compiler_module(Module, Purgeable) ->
  elixir_code_server:cast({return_compiler_module, Module, Purgeable}).

is_purgeable(Module, Binary) ->
  beam_lib:chunks(Binary, [labeled_locals]) == {ok, {Module, [{labeled_locals, []}]}}.

allows_fast_compilation({'__block__', _, Exprs}) ->
  lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule, _, _}) ->
  true;
allows_fast_compilation(_) ->
  false.

%% Bootstrapper

bootstrap() ->
  {ok, _} = application:ensure_all_started(elixir),
  elixir_config:put(bootstrap, true),
  elixir_config:put(docs, false),
  elixir_config:put(relative_paths, false),
  elixir_config:put(ignore_module_conflict, true),
  elixir_config:put(tracers, []),
  elixir_config:put(parser_options, []),
  {Init, Main} = bootstrap_files(),
  [bootstrap_file(File) || File <- [<<"lib/elixir/lib/kernel.ex">> | Init]],
  elixir_config:put(bootstrap, true),
  elixir_config:put(docs, true),
  [bootstrap_file(File) || File <- [<<"lib/elixir/lib/kernel.ex">> | Main]].

bootstrap_file(File) ->
  try
    Lists = file(filename:absname(File), fun(_, _) -> ok end),
    _ = [binary_to_path(X, "lib/elixir/ebin") || X <- Lists],
    io:format("Compiled ~ts~n", [File])
  catch
    Kind:Reason:Stacktrace ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, Stacktrace]),
      erlang:halt(1)
  end.

bootstrap_files() ->
  {
    [
     <<"lib/elixir/lib/macro/env.ex">>,
     <<"lib/elixir/lib/keyword.ex">>,
     <<"lib/elixir/lib/module.ex">>,
     <<"lib/elixir/lib/list.ex">>,
     <<"lib/elixir/lib/macro.ex">>,
     <<"lib/elixir/lib/kernel/typespec.ex">>,
     <<"lib/elixir/lib/kernel/utils.ex">>,
     <<"lib/elixir/lib/code.ex">>,
     <<"lib/elixir/lib/code/identifier.ex">>,
     <<"lib/elixir/lib/protocol.ex">>,
     <<"lib/elixir/lib/stream/reducers.ex">>,
     <<"lib/elixir/lib/enum.ex">>,
     <<"lib/elixir/lib/regex.ex">>,
     <<"lib/elixir/lib/inspect/algebra.ex">>,
     <<"lib/elixir/lib/inspect.ex">>,
     <<"lib/elixir/lib/string.ex">>,
     <<"lib/elixir/lib/string/chars.ex">>
    ],
    [
     <<"lib/elixir/lib/list/chars.ex">>,
     <<"lib/elixir/lib/module/locals_tracker.ex">>,
     <<"lib/elixir/lib/module/parallel_checker.ex">>,
     <<"lib/elixir/lib/module/types/helpers.ex">>,
     <<"lib/elixir/lib/module/types/unify.ex">>,
     <<"lib/elixir/lib/module/types/of.ex">>,
     <<"lib/elixir/lib/module/types/pattern.ex">>,
     <<"lib/elixir/lib/module/types/expr.ex">>,
     <<"lib/elixir/lib/module/types.ex">>,
     <<"lib/elixir/lib/exception.ex">>,
     <<"lib/elixir/lib/path.ex">>,
     <<"lib/elixir/lib/file.ex">>,
     <<"lib/elixir/lib/map.ex">>,
     <<"lib/elixir/lib/range.ex">>,
     <<"lib/elixir/lib/access.ex">>,
     <<"lib/elixir/lib/io.ex">>,
     <<"lib/elixir/lib/system.ex">>,
     <<"lib/elixir/lib/kernel/cli.ex">>,
     <<"lib/elixir/lib/kernel/error_handler.ex">>,
     <<"lib/elixir/lib/kernel/parallel_compiler.ex">>,
     <<"lib/elixir/lib/kernel/lexical_tracker.ex">>
    ]
  }.

binary_to_path({ModuleName, _ModuleMap, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  case file:write_file(Path, Binary) of
    ok -> Path;
    {error, Reason} -> error('Elixir.File.Error':exception([{action, "write to"}, {path, Path}, {reason, Reason}]))
  end.
