-module(elixir_compiler).
-export([get_opt/1, string/2, quoted/2, file/1, file/2, file_to_path/2]).
-export([core/0, module/4, eval_forms/3]).
-include("elixir.hrl").

%% Public API

get_opt(Key) ->
  Map = elixir_config:get(compiler_options),
  case maps:find(Key, Map) of
    {ok, Value} -> Value;
    error -> false
  end.

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  string(Contents, File, nil).
string(Contents, File, Dest) ->
  Forms = elixir:'string_to_quoted!'(Contents, 1, File, []),
  quoted(Forms, File, Dest).

quoted(Forms, File) when is_binary(File) ->
  quoted(Forms, File, nil).
quoted(Forms, File, Dest) ->
  Previous = get(elixir_module_binaries),

  try
    put(elixir_module_binaries, []),
    elixir_lexical:run(File, Dest, fun
      (Pid) ->
        Env = elixir:env_for_eval([{line, 1}, {file, File}]),
        eval_forms(Forms, [], Env#{lexical_tracker := Pid})
    end),
    lists:reverse(get(elixir_module_binaries))
  after
    put(elixir_module_binaries, Previous)
  end.

file(Relative) when is_binary(Relative) ->
  file(Relative, nil).
file(Relative, Dest) ->
  File = filename:absname(Relative),
  {ok, Bin} = file:read_file(File),
  string(elixir_utils:characters_to_list(Bin), File, case Dest of
    nil -> Dest;
    _   -> filename:absname(Dest)
  end).

file_to_path(File, Dest) when is_binary(File), is_binary(Dest) ->
  Comp = file(File, Dest),
  Abs  = filename:absname(Dest),
  _ = [binary_to_path(X, Abs) || X <- Comp],
  Comp.

%% Evaluation

eval_forms(Forms, Vars, E) ->
  case (?m(E, module) == nil) andalso allows_fast_compilation(Forms) of
    true  -> eval_compilation(Forms, Vars, E);
    false -> code_loading_compilation(Forms, Vars, E)
  end.

eval_compilation(Forms, Vars, E) ->
  Binding = [{Key, Value} || {_Name, _Kind, Key, Value} <- Vars],
  {Result, _Binding, EE, _S} = elixir:eval_forms(Forms, Binding, E),
  {Result, EE}.

code_loading_compilation(Forms, Vars, #{line := Line} = E) ->
  Dict = [{{Name, Kind}, {Value, 0, true}} || {Name, Kind, Value, _} <- Vars],
  S = elixir_env:env_to_scope_with_vars(E, Dict),
  {Expr, EE, _S} = elixir:quoted_to_erl(Forms, E, S),

  {Module, I} = retrieve_compiler_module(),
  Fun  = code_fun(?m(E, module)),
  Form = code_mod(Fun, Expr, Line, ?m(E, file), Module, Vars),
  Args = list_to_tuple([V || {_, _, _, V} <- Vars]),

  %% Pass {native, false} to speed up bootstrap
  %% process when native is set to true
  ErlOpts = options() -- [native, warn_missing_spec],
  inner_module(Form, ErlOpts, [{bootstrap, true}], E, fun(_, Binary) ->
    %% If we have labeled locals, anonymous functions
    %% were created and therefore we cannot ditch the
    %% module
    Purgeable =
      case beam_lib:chunks(Binary, [labeled_locals]) of
        {ok, {_, [{labeled_locals, []}]}} -> true;
        _ -> false
      end,
    dispatch_loaded(Module, Fun, Args, Purgeable, I, EE)
  end).

options() ->
  case elixir_config:get(erl_compiler_options) of
    nil ->
      elixir_config:update(erl_compiler_options, fun options/1);
    Opts ->
      Opts
  end.

options(nil) ->
  Key = "ERL_COMPILER_OPTIONS",
  case os:getenv(Key) of
    false -> [];
    Str when is_list(Str) ->
      case erl_scan:string(Str) of
        {ok, Tokens, _} ->
          case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
            {ok, List} when is_list(List) -> List;
            {ok, Term} -> [Term];
            {error, _Reason} ->
              io:format("Ignoring bad term in ~ts\n", [Key]),
              []
          end;
        {error, {_, _, _Reason}, _} ->
          io:format("Ignoring bad term in ~ts\n", [Key]),
          []
      end
  end;
options(Opts) ->
    Opts.

dispatch_loaded(Module, Fun, Args, Purgeable, I, E) ->
  Res = Module:Fun(Args),
  code:delete(Module),
  if Purgeable ->
      code:purge(Module),
      return_compiler_module(I);
     true ->
       ok
  end,
  {Res, E}.

code_fun(nil) -> '__FILE__';
code_fun(_)   -> '__MODULE__'.

code_mod(Fun, Expr, Line, File, Module, Vars) when is_binary(File), is_integer(Line) ->
  Tuple    = {tuple, Line, [{var, Line, K} || {_, _, K, _} <- Vars]},
  Relative = elixir_utils:relative_to_cwd(File),

  [
    {attribute, Line, file, {elixir_utils:characters_to_list(Relative), 1}},
    {attribute, Line, module, Module},
    {attribute, Line, export, [{Fun, 1}, {'__RELATIVE__', 0}]},
    {function, Line, Fun, 1, [
      {clause, Line, [Tuple], [], [Expr]}
    ]},
    {function, Line, '__RELATIVE__', 0, [
      {clause, Line, [], [], [elixir_utils:elixir_to_erl(Relative)]}
    ]}
  ].

retrieve_compiler_module() ->
  elixir_code_server:call(retrieve_compiler_module).

return_compiler_module(I) ->
  elixir_code_server:cast({return_compiler_module, I}).

allows_fast_compilation({'__block__', _, Exprs}) ->
  lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule, _, _}) -> true;
allows_fast_compilation(_) -> false.

%% INTERNAL API

%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module and elixir_module.
module(Forms, Opts, E, Callback) ->
  ErlOpts =
    case proplists:get_value(debug_info, Opts) of
      true -> [debug_info];
      false -> [];
      undefined ->
        case get_opt(debug_info) of
          true  -> [debug_info];
          false -> []
        end
    end,
  inner_module(Forms, ErlOpts ++ options(), Opts, E, Callback).

inner_module(Forms, ErlOpts, ExOpts, #{file := File} = E, Callback) when
    is_list(Forms), is_list(ErlOpts), is_list(ExOpts), is_function(Callback) ->
  Source = elixir_utils:characters_to_list(File),
  Autoload = proplists:get_value(autoload, ExOpts, true),
  Bootstrap = proplists:get_value(bootstrap, ExOpts, false),

  case compile:noenv_forms([no_auto_import() | Forms], [return, {source, Source} | ErlOpts]) of
    {ok, Module, Binary, Warnings} ->
      format_warnings(Bootstrap, Warnings),
      {module, Module} =
        case Autoload of
          true  -> code:load_binary(Module, beam_location(E), Binary);
          false -> {module, Module}
        end,
      Callback(Module, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Bootstrap, Warnings),
      format_errors(Errors)
  end.

beam_location(#{module := nil}) -> in_memory;
beam_location(#{lexical_tracker := Pid, module := Module}) ->
  case elixir_lexical:dest(Pid) of
    nil  -> in_memory;
    Dest ->
      filename:join(elixir_utils:characters_to_list(Dest),
                    atom_to_list(Module) ++ ".beam")
  end.

no_auto_import() ->
  {attribute, 0, compile, no_auto_import}.

%% CORE HANDLING

core() ->
  {ok, _} = application:ensure_all_started(elixir),
  Update = fun(Old) -> maps:merge(Old, #{docs => false, internal => true,
                                         relative_paths => false}) end,
  _ = elixir_config:update(compiler_options, Update),
  [core_file(File) || File <- core_main()].

core_file(File) ->
  try
    Lists = file(File),
    _ = [binary_to_path(X, "lib/elixir/ebin") || X <- Lists],
    io:format("Compiled ~ts~n", [File])
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      erlang:halt(1)
  end.

core_main() ->
  [<<"lib/elixir/lib/kernel.ex">>,
   <<"lib/elixir/lib/macro/env.ex">>,
   <<"lib/elixir/lib/keyword.ex">>,
   <<"lib/elixir/lib/module.ex">>,
   <<"lib/elixir/lib/list.ex">>,
   <<"lib/elixir/lib/macro.ex">>,
   <<"lib/elixir/lib/code.ex">>,
   <<"lib/elixir/lib/module/locals_tracker.ex">>,
   <<"lib/elixir/lib/kernel/typespec.ex">>,
   <<"lib/elixir/lib/kernel/utils.ex">>,
   <<"lib/elixir/lib/behaviour.ex">>,
   <<"lib/elixir/lib/exception.ex">>,
   <<"lib/elixir/lib/protocol.ex">>,
   <<"lib/elixir/lib/stream/reducers.ex">>,
   <<"lib/elixir/lib/enum.ex">>,
   <<"lib/elixir/lib/inspect/algebra.ex">>,
   <<"lib/elixir/lib/inspect.ex">>,
   <<"lib/elixir/lib/range.ex">>,
   <<"lib/elixir/lib/regex.ex">>,
   <<"lib/elixir/lib/string.ex">>,
   <<"lib/elixir/lib/string/chars.ex">>,
   <<"lib/elixir/lib/io.ex">>,
   <<"lib/elixir/lib/path.ex">>,
   <<"lib/elixir/lib/file.ex">>,
   <<"lib/elixir/lib/system.ex">>,
   <<"lib/elixir/lib/kernel/cli.ex">>,
   <<"lib/elixir/lib/kernel/error_handler.ex">>,
   <<"lib/elixir/lib/kernel/parallel_compiler.ex">>,
   <<"lib/elixir/lib/kernel/lexical_tracker.ex">>].

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  case file:write_file(Path, Binary) of
    ok -> Path;
    {error, Reason} -> error('Elixir.File.Error':exception([{action, "write to"}, {path, Path}, {reason, Reason}]))
  end.

%% ERROR HANDLING

format_errors([]) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(Errors) ->
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun(Error) -> elixir_errors:handle_file_error(BinFile, Error) end, Each)
  end, Errors).

format_warnings(Bootstrap, Warnings) ->
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun(Warning) -> elixir_errors:handle_file_warning(Bootstrap, BinFile, Warning) end, Each)
  end, Warnings).
