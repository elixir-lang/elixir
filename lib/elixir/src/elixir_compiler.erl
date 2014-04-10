-module(elixir_compiler).
-export([get_opt/1, string/2, quoted/2, file/1, file_to_path/2]).
-export([core/0, module/4, eval_forms/3]).
-include("elixir.hrl").

%% Public API

get_opt(Key) ->
  Dict = elixir_code_server:call(compiler_options),
  case lists:keyfind(Key, 1, Dict) of
    false -> false;
    { Key, Value } -> Value
  end.

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  Forms = elixir:'string_to_quoted!'(Contents, 1, File, []),
  quoted(Forms, File).

quoted(Forms, File) when is_binary(File) ->
  Previous = get(elixir_compiled),

  try
    put(elixir_compiled, []),
    elixir_lexical:run(File, fun
      (Pid) ->
        Env = elixir:env_for_eval([{line,1},{file,File}]),
        eval_forms(Forms, [], Env#elixir_env{lexical_tracker=Pid})
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

eval_forms(Forms, Vars, E) ->
  case (E#elixir_env.module == nil) andalso allows_fast_compilation(Forms) of
    true  -> eval_compilation(Forms, Vars, E);
    false -> code_loading_compilation(Forms, Vars, E)
  end.

eval_compilation(Forms, Vars, E) ->
  Binding = [{ Key, Value } || { _Name, _Kind, Key, Value } <- Vars],
  { Result, _Binding, EE, _S } = elixir:eval_forms(Forms, Binding, E),
  { Result, EE }.

code_loading_compilation(Forms, Vars, #elixir_env{line=Line} = E) ->
  Dict = [{ { Name, Kind }, { Value, 0 } } || { Name, Kind, Value, _ } <- Vars],
  S = elixir_env:env_to_scope_with_vars(E, Dict),
  { Expr, EE, _S } = elixir:quoted_to_erl(Forms, E, S),

  { Module, I } = retrieve_module_name(),
  Fun  = code_fun(E#elixir_env.module),
  Form = code_mod(Fun, Expr, Line, E#elixir_env.file, Module, Vars),
  Args = list_to_tuple([V || { _, _, _, V } <- Vars]),

  %% Pass { native, false } to speed up bootstrap
  %% process when native is set to true
  module(Form, E#elixir_env.file, [{native,false}], true, fun(_, Binary) ->
    %% If we have labeled locals, anonymous functions
    %% were created and therefore we cannot ditch the
    %% module
    Purgeable =
      case beam_lib:chunks(Binary, [labeled_locals]) of
        { ok, { _, [{ labeled_locals, []}] } } -> true;
        _ -> false
      end,
    dispatch_loaded(Module, Fun, Args, Purgeable, I, EE)
  end).

dispatch_loaded(Module, Fun, Args, Purgeable, I, E) ->
  Res = Module:Fun(Args),
  code:delete(Module),
  if Purgeable ->
      code:purge(Module),
      return_module_name(I);
     true ->
       ok
  end,
  { Res, E }.

code_fun(nil) -> '__FILE__';
code_fun(_)   -> '__MODULE__'.

code_mod(Fun, Expr, Line, File, Module, Vars) when is_binary(File), is_integer(Line) ->
  Tuple    = { tuple, Line, [{ var, Line, K } || { _, _, K, _ } <- Vars] },
  Relative = elixir_utils:relative_to_cwd(File),

  [
    { attribute, Line, file, { elixir_utils:characters_to_list(Relative), 1 } },
    { attribute, Line, module, Module },
    { attribute, Line, export, [{ Fun, 1 }, { '__RELATIVE__', 0 }] },
    { function, Line, Fun, 1, [
      { clause, Line, [Tuple], [], [Expr] }
    ] },
    { function, Line, '__RELATIVE__', 0, [
      { clause, Line, [], [], [elixir_utils:elixir_to_erl(Relative)] }
    ] }
  ].

retrieve_module_name() ->
  elixir_code_server:call(retrieve_module_name).

return_module_name(I) ->
  elixir_code_server:cast({ return_module_name, I }).

allows_fast_compilation({ '__block__', _, Exprs }) ->
  lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule,_,_}) -> true;
allows_fast_compilation(_) -> false.

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
  Options = RawOptions ++ elixir_code_server:call(erl_compiler_options),
  Listname = elixir_utils:characters_to_list(File),

  case compile:noenv_forms(Forms, [no_auto_import,return,{source,Listname}|Options]) of
    {ok, ModuleName, Binary, Warnings} ->
      format_warnings(Bootstrap, Warnings),
      code:load_binary(ModuleName, Listname, Binary),
      Callback(ModuleName, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Bootstrap, Warnings),
      format_errors(Errors)
  end.

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
      erlang:halt(1)
  end.

core_main() ->
  [
    <<"lib/elixir/lib/kernel.ex">>,
    <<"lib/elixir/lib/keyword.ex">>,
    <<"lib/elixir/lib/module.ex">>,
    <<"lib/elixir/lib/list.ex">>,
    <<"lib/elixir/lib/record.ex">>,
    <<"lib/elixir/lib/record/backend.ex">>,
    <<"lib/elixir/lib/record/deprecated.ex">>,
    <<"lib/elixir/lib/macro.ex">>,
    <<"lib/elixir/lib/macro/env.ex">>,
    <<"lib/elixir/lib/exception.ex">>,
    <<"lib/elixir/lib/code.ex">>,
    <<"lib/elixir/lib/module/locals_tracker.ex">>,
    <<"lib/elixir/lib/protocol.ex">>,
    <<"lib/elixir/lib/stream/reducers.ex">>,
    <<"lib/elixir/lib/enum.ex">>,
    <<"lib/elixir/lib/inspect/algebra.ex">>,
    <<"lib/elixir/lib/inspect.ex">>,
    <<"lib/elixir/lib/range.ex">>,
    <<"lib/elixir/lib/regex.ex">>,
    <<"lib/elixir/lib/string.ex">>,
    <<"lib/elixir/lib/string/chars.ex">>,
    <<"lib/elixir/lib/collectable.ex">>,
    <<"lib/elixir/lib/io.ex">>,
    <<"lib/elixir/lib/path.ex">>,
    <<"lib/elixir/lib/system.ex">>,
    <<"lib/elixir/lib/kernel/typespec.ex">>,
    <<"lib/elixir/lib/kernel/cli.ex">>,
    <<"lib/elixir/lib/kernel/error_handler.ex">>,
    <<"lib/elixir/lib/kernel/parallel_compiler.ex">>,
    <<"lib/elixir/lib/kernel/lexical_tracker.ex">>
  ].

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary),
  Path.

%% ERROR HANDLING

format_errors([]) ->
  exit({ nocompile, "compilation failed but no error was raised" });

format_errors(Errors) ->
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(BinFile, Error) end, Each)
  end, Errors).

format_warnings(Bootstrap, Warnings) ->
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Bootstrap, BinFile, Warning) end, Each)
  end, Warnings).
