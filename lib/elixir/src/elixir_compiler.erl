-module(elixir_compiler).
-export([get_opts/0, get_opt/1, get_opt/2, string/2, quoted/2, file/1, file_to_path/2]).
-export([core/0, module/3, eval_forms/4, format_error/1]).
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

%% Compiles the given string.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  Forms = elixir_translator:'forms!'(Contents, 1, File, []),
  quoted(Forms, File).

quoted(Forms, File) when is_binary(File) ->
  Previous = get(elixir_compiled),

  try
    put(elixir_compiled, []),
    eval_forms(Forms, 1, [], elixir:scope_for_eval([{file,File}])),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end.

%% Compile a file, return a tuple of module names and binaries.

file(Relative) when is_binary(Relative) ->
  File = filename:absname(Relative),
  { ok, Bin } = file:read_file(File),
  string(unicode:characters_to_list(Bin), File).

%% Compiles a file to the given path (directory).

file_to_path(File, Path) when is_binary(File), is_binary(Path) ->
  Lists = file(File),
  [binary_to_path(X, Path) || X <- Lists],
  Lists.

%% Evaluates the contents/forms by compiling them to an Erlang module.

eval_forms(Forms, Line, Vars, S) ->
  { Module, I } = retrieve_module_name(),
  { Exprs, FS } = elixir_translator:translate(Forms, S),

  Fun  = module_form_fun(S#elixir_scope.module),
  Form = module_form(Fun, Exprs, Line, S#elixir_scope.file, Module, Vars),
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

%% Internal API

%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module and elixir_module.
module(Forms, File, Callback) ->
  Options = case get_opt(debug_info) of
    true -> [debug_info];
    _ -> []
  end,
  module(Forms, File, Options, false, Callback).

module(Forms, File, RawOptions, Bootstrap, Callback) when
    is_binary(File), is_list(Forms), is_list(RawOptions), is_boolean(Bootstrap), is_function(Callback) ->
  { Options, SkipNative } = compile_opts(Forms, RawOptions),
  Listname = unicode:characters_to_list(File),

  case compile:noenv_forms([no_auto_import()|Forms], [return,{source,Listname}|Options]) of
    {ok, ModuleName, Binary, RawWarnings} ->
      Warnings = case SkipNative of
        true  -> [{?MODULE,[{0,?MODULE,{skip_native,ModuleName}}]}|RawWarnings];
        false -> RawWarnings
      end,
      format_warnings(Bootstrap, File, Warnings),
      code:load_binary(ModuleName, Listname, Binary),
      Callback(ModuleName, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Bootstrap, File, Warnings),
      format_errors(File, Errors)
  end.

%% Compile core files for bootstrap.
%% Invoked from the Makefile.

core() ->
  application:start(elixir),
  elixir_code_server:cast({ compiler_options, [{docs,false},{internal,true}] }),
  [core_file(File) || File <- core_main()].

%% HELPERS

compile_opts(Forms, Options) ->
  EnvOptions = env_default_opts(),
  SkipNative = lists:member(native, EnvOptions) and contains_on_load(Forms),
  case SkipNative or lists:member([{native,false}], Options) of
    true  -> { Options ++ lists:delete(native, EnvOptions), SkipNative };
    false -> { Options ++ EnvOptions, SkipNative }
  end.

env_default_opts() ->
  Key = "ERL_COMPILER_OPTIONS",
  case os:getenv(Key) of
    false -> [];
    Str when is_list(Str) ->
      case erl_scan:string(Str) of
        {ok,Tokens,_} ->
          case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
            {ok,List} when is_list(List) -> List;
            {ok,Term} -> [Term];
            {error,_Reason} ->
              io:format("Ignoring bad term in ~ts\n", [Key]),
              []
          end;
        {error, {_,_,_Reason}, _} ->
          io:format("Ignoring bad term in ~ts\n", [Key]),
          []
      end
  end.

contains_on_load([{ attribute, _, on_load, _ }|_]) -> true;
contains_on_load([_|T]) -> contains_on_load(T);
contains_on_load([]) -> false.

no_auto_import() ->
  Bifs = [{ Name, Arity } || { Name, Arity } <- erlang:module_info(exports), erl_internal:bif(Name, Arity)],
  { attribute, 0, compile, { no_auto_import, Bifs } }.

module_form_fun(nil) -> '__FILE__';
module_form_fun(_)   -> '__MODULE__'.

module_form(Fun, Exprs, Line, File, Module, Vars) when
    is_binary(File), is_integer(Line), is_list(Exprs), is_atom(Module) ->
  Cons = lists:foldr(fun({ _, _, Var, _ }, Acc) ->
    { cons, Line, { var, Line, Var }, Acc }
  end, { nil, Line }, Vars),

  Args = [{ var, Line, '_@MODULE'}, Cons],

  Relative =
    case get_opt(internal) of
      true  -> File;
      false -> 'Elixir.Path':relative_to(File, 'Elixir.System':'cwd!'())
    end,

  [
    { attribute, Line, file, { unicode:characters_to_list(File), 1 } },
    { attribute, Line, module, Module },
    { attribute, Line, export, [{ Fun, 2 }, { '__RELATIVE__', 0 }] },
    { function, Line, Fun, length(Args), [
      { clause, Line, Args, [], Exprs }
    ] },
    { function, Line, '__RELATIVE__', 0, [
      { clause, Line, [], [], [elixir_tree_helpers:elixir_to_erl(Relative)] }
    ] }
  ].

%% Generate module names from code server.

retrieve_module_name() ->
  elixir_code_server:call(retrieve_module_name).

return_module_name(I) ->
  elixir_code_server:cast({ return_module_name, I }).

%% Receives a module Binary and outputs it in the given path.

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary),
  Path.

%% CORE FILES COMPILATION

core_file(File) ->
  try
    Lists = file(list_to_binary(File)),
    [binary_to_path(X, "lib/elixir/ebin") || X <- Lists],
    io:format("Compiled ~ts~n", [File])
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      exit(1)
  end.

core_main() ->
  [
    "lib/elixir/lib/kernel.ex",
    "lib/elixir/lib/keyword.ex",
    "lib/elixir/lib/module.ex",
    "lib/elixir/lib/list.ex",
    "lib/elixir/lib/kernel/typespec.ex",
    "lib/elixir/lib/record.ex",
    "lib/elixir/lib/macro.ex",
    "lib/elixir/lib/macro/env.ex",
    "lib/elixir/lib/exception.ex",
    "lib/elixir/lib/code.ex",
    "lib/elixir/lib/protocol.ex",
    "lib/elixir/lib/enum.ex",
    "lib/elixir/lib/inspect/algebra.ex",
    "lib/elixir/lib/inspect.ex",
    "lib/elixir/lib/binary/chars.ex",
    "lib/elixir/lib/io.ex",
    "lib/elixir/lib/path.ex",
    "lib/elixir/lib/system.ex",
    "lib/elixir/lib/kernel/cli.ex",
    "lib/elixir/lib/kernel/error_handler.ex",
    "lib/elixir/lib/kernel/parallel_compiler.ex",
    "lib/elixir/lib/kernel/record_rewriter.ex",
    "lib/elixir/lib/module/dispatch_tracker.ex"
  ].

%% ERROR HANDLING

format_error({ skip_native, Module }) ->
  io_lib:format("skipping native compilation for ~ts because it contains on_load attribute",
    [elixir_errors:inspect(Module)]).

format_errors(_File, []) ->
  exit({ nocompile, "compilation failed but no error was raised" });

format_errors(File, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(File, Error) end, Each)
  end, Errors).

format_warnings(Bootstrap, File, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Bootstrap, File, Warning) end, Each)
  end, Warnings).
