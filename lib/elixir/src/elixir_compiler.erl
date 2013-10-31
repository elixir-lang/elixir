-module(elixir_compiler).
-export([get_opts/0, get_opt/1, get_opt/2, string/2, quoted/2, file/1, file_to_path/2]).
-export([core/0, module/4, eval_forms/4, format_error/1]).
-include("elixir.hrl").

%% Public API

%% Get compilation options.

get_opt(Key) -> get_opt(Key, get_opts()).

get_opt(Key, Dict) ->
  case lists:keyfind(Key, 1, Dict) of
    false -> false;
    { Key, Value } -> Value
  end.

get_opts() ->
  elixir_code_server:call(compiler_options).

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  Forms = elixir_translator:'forms!'(Contents, 1, File, []),
  quoted(Forms, File).

quoted(Forms, File) when is_binary(File) ->
  Previous = get(elixir_compiled),

  try
    put(elixir_compiled, []),
    elixir_lexical:run(File, fun
      () -> eval_forms(Forms, 1, [], elixir:scope_for_eval([{file,File}]))
    end),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end.

file(Relative) when is_binary(Relative) ->
  File = filename:absname(Relative),
  { ok, Bin } = file:read_file(File),
  string(elixir_utils:characters_to_list(Bin), File).

file_to_path(File, Path) when is_binary(File), is_binary(Path) ->
  Lists = file(File),
  [binary_to_path(X, Path) || X <- Lists],
  Lists.

%% Evaluation

eval_forms(Forms, Line, Vars, S) ->
  { Module, I } = retrieve_module_name(),
  { Exprs, FS } = elixir_translator:translate(Forms, S),

  Fun  = eval_fun(S#elixir_scope.module),
  Form = eval_mod(Fun, Exprs, Line, S#elixir_scope.file, Module, Vars),
  Args = [X || { _, _, _, X } <- Vars],

  %% Pass { native, false } to speed up bootstrap
  %% process when native is set to true
  { module(Form, S#elixir_scope.file, [{native,false}], true, fun(_, Binary) ->
    Res = Module:Fun(S#elixir_scope.module, Args),
    code:delete(Module),

    %% If we have labeled locals, anonymous functions
    %% were created and therefore we cannot ditch the
    %% module
    case beam_lib:chunks(Binary, [labeled_locals]) of
      { ok, { _, [{ labeled_locals, []}] } } ->
        code:purge(Module),
        return_module_name(I);
      _ ->
        ok
    end,

    Res
  end), FS }.

eval_fun(nil) -> '__FILE__';
eval_fun(_)   -> '__MODULE__'.

eval_mod(Fun, Exprs, Line, File, Module, Vars) when is_binary(File), is_integer(Line) ->
  Cons = lists:foldr(fun({ _, _, Var, _ }, Acc) ->
    { cons, Line, { var, Line, Var }, Acc }
  end, { nil, Line }, Vars),

  Args = [{ var, Line, '_@MODULE'}, Cons],
  Relative = elixir_utils:relative_to_cwd(File),

  [
    { attribute, Line, file, { elixir_utils:characters_to_list(File), 1 } },
    { attribute, Line, module, Module },
    { attribute, Line, export, [{ Fun, 2 }, { '__RELATIVE__', 0 }] },
    { function, Line, Fun, length(Args), [
      { clause, Line, Args, [], Exprs }
    ] },
    { function, Line, '__RELATIVE__', 0, [
      { clause, Line, [], [], [elixir_utils:elixir_to_erl(Relative)] }
    ] }
  ].

retrieve_module_name() ->
  elixir_code_server:call(retrieve_module_name).

return_module_name(I) ->
  elixir_code_server:cast({ return_module_name, I }).

%% INTERNAL API

%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module and elixir_module.
module(Forms, File, Opts, Callback) ->
  Final =
    case (get_opt(debug_info) == true) orelse
         lists:member(debug_info, Opts) of
      true  -> [debug_info];
      false -> []
    end,
  module(Forms, File, Final, false, Callback).

module(Forms, File, RawOptions, Bootstrap, Callback) when
    is_binary(File), is_list(Forms), is_list(RawOptions), is_boolean(Bootstrap), is_function(Callback) ->
  { Options, SkipNative } = compiler_options(Forms, RawOptions),
  Listname = elixir_utils:characters_to_list(File),

  case compile:noenv_forms([no_auto_import()|Forms], [return,{source,Listname}|Options]) of
    {ok, ModuleName, Binary, RawWarnings} ->
      Warnings = case SkipNative of
        true  -> [{?MODULE,[{0,?MODULE,{skip_native,ModuleName}}]}|RawWarnings];
        false -> RawWarnings
      end,
      format_warnings(Bootstrap, Warnings),
      code:load_binary(ModuleName, Listname, Binary),
      Callback(ModuleName, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Bootstrap, Warnings),
      format_errors(Errors)
  end.

compiler_options(Forms, Options) ->
  EnvOptions = elixir_code_server:call(erl_compiler_options),
  SkipNative = lists:member(native, EnvOptions) and contains_on_load(Forms),
  case SkipNative or lists:member([{native,false}], Options) of
    true  -> { Options ++ lists:delete(native, EnvOptions), SkipNative };
    false -> { Options ++ EnvOptions, SkipNative }
  end.

contains_on_load([{ attribute, _, on_load, _ }|_]) -> true;
contains_on_load([_|T]) -> contains_on_load(T);
contains_on_load([]) -> false.

no_auto_import() ->
  Bifs = [{ Name, Arity } || { Name, Arity } <- erlang:module_info(exports), erl_internal:bif(Name, Arity)],
  { attribute, 0, compile, { no_auto_import, Bifs } }.


%% CORE HANDLING

core() ->
  application:start(elixir),
  elixir_code_server:cast({ compiler_options, [{docs,false},{internal,true}] }),
  [core_file(File) || File <- core_main()].

core_file(File) ->
  try
    Lists = file(File),
    [binary_to_path(X, "lib/elixir/ebin") || X <- Lists],
    io:format("Compiled ~ts~n", [File])
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      exit(1)
  end.

core_main() ->
  [
    <<"lib/elixir/lib/kernel.ex">>,
    <<"lib/elixir/lib/keyword.ex">>,
    <<"lib/elixir/lib/module.ex">>,
    <<"lib/elixir/lib/list.ex">>,
    <<"lib/elixir/lib/kernel/typespec.ex">>,
    <<"lib/elixir/lib/record.ex">>,
    <<"lib/elixir/lib/macro.ex">>,
    <<"lib/elixir/lib/macro/env.ex">>,
    <<"lib/elixir/lib/exception.ex">>,
    <<"lib/elixir/lib/code.ex">>,
    <<"lib/elixir/lib/protocol.ex">>,
    <<"lib/elixir/lib/enum.ex">>,
    <<"lib/elixir/lib/inspect/algebra.ex">>,
    <<"lib/elixir/lib/inspect.ex">>,
    <<"lib/elixir/lib/range.ex">>,
    <<"lib/elixir/lib/regex.ex">>,
    <<"lib/elixir/lib/string.ex">>,
    <<"lib/elixir/lib/string/chars.ex">>,
    <<"lib/elixir/lib/io.ex">>,
    <<"lib/elixir/lib/path.ex">>,
    <<"lib/elixir/lib/system.ex">>,
    <<"lib/elixir/lib/kernel/cli.ex">>,
    <<"lib/elixir/lib/kernel/error_handler.ex">>,
    <<"lib/elixir/lib/kernel/parallel_compiler.ex">>,
    <<"lib/elixir/lib/kernel/record_rewriter.ex">>,
    <<"lib/elixir/lib/kernel/lexical_tracker.ex">>,
    <<"lib/elixir/lib/module/dispatch_tracker.ex">>
  ].

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary),
  Path.

%% ERROR HANDLING

format_error({ skip_native, Module }) ->
  io_lib:format("skipping native compilation for ~ts because it contains on_load attribute",
    [elixir_errors:inspect(Module)]).

format_errors([]) ->
  exit({ nocompile, "compilation failed but no error was raised" });

format_errors(Errors) ->
  lists:foreach(fun ({File, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(File, Error) end, Each)
  end, Errors).

format_warnings(Bootstrap, Warnings) ->
  lists:foreach(fun ({File, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Bootstrap, File, Warning) end, Each)
  end, Warnings).
