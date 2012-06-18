-module(elixir_compiler).
-export([get_opts/0, get_opt/1, get_opt/2, string/2, file/1, file_to_path/2]).
-export([core/0, module/3, eval_forms/4]).
-include("elixir.hrl").

%% Public API

%% Get compilation options.

get_opt(Key) -> get_opt(Key, get_opts()).

get_opt(Key, Dict) ->
  case orddict:find(Key, Dict) of
    { ok, Value } -> Value;
    error -> false
  end.

get_opts() ->
  gen_server:call(elixir_code_server, compiler_options).

%% Compiles the given string.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  Previous = get(elixir_compiled),

  try
    put(elixir_compiled, []),
    Forms = elixir_translator:forms(Contents, 1, File),
    eval_forms(Forms, 1, File, #elixir_scope{file=File}),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end.

%% Compile a file, return a tuple of module names and binaries.

file(Relative) when is_binary(Relative) ->
  File = filename:absname(Relative),
  case file:read_file(File) of
    {ok, Bin} ->
      string(unicode:characters_to_list(Bin), File);
    Error ->
      erlang:error(Error)
  end.

%% Compiles a file to the given path (directory).

file_to_path(File, Path) when is_binary(File), is_binary(Path) ->
  Lists = file(File),
  [binary_to_path(X, Path) || X <- Lists],
  Lists.

%% Evaluates the contents/forms by compiling them to an Erlang module.

eval_forms(Forms, Line, Module, #elixir_scope{module=nil} = S) ->
  eval_forms(Forms, Line, Module, nil, S);

eval_forms(Forms, Line, Module, #elixir_scope{module=Value} = S) ->
  eval_forms(Forms, Line, Module, Value, S).

eval_forms(Forms, Line, RawModule, Value, S) ->
  case (Value == nil) and allows_fast_compilation(Forms) of
    true  -> eval_compilation(Forms, Line, S);
    false -> code_loading_compilation(Forms, Line, RawModule, Value, S)
  end.

eval_compilation(Forms, _Line, S) ->
  { Result, _Binding, FS } = elixir:eval_forms(Forms, [{'_@MODULE',nil}], S),
  { Result, FS }.

code_loading_compilation(Forms, Line, RawModule, Value, S) ->
  Module = escape_module(RawModule),
  { Exprs, FS } = elixir_translator:translate(Forms, S),
  ModuleForm = module_form(Exprs, Line, S#elixir_scope.file, Module),
  { module(ModuleForm, S, fun(Mod, _) ->
    Res = Mod:'BOOTSTRAP'(Value),
    code:purge(Module),
    code:delete(Module),
    Res
  end), FS }.

%% Internal API

%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module and elixir_module.
module(Forms, S, Callback) ->
  Options = case get_opt(debug_info) of
    true -> [debug_info];
    _ -> []
  end,
  module(Forms, S#elixir_scope.file, Options, Callback).

module(Forms, File, Options, Callback) when
    is_binary(File), is_list(Forms), is_list(Options), is_function(Callback) ->
  Listname = binary_to_list(File),
  case compile:forms([no_auto_import()|Forms], [return,{source,Listname}|Options]) of
    {ok, ModuleName, Binary, Warnings} ->
      format_warnings(File, Warnings),
      code:load_binary(ModuleName, Listname, Binary),
      Callback(ModuleName, Binary);
    {error, Errors, Warnings} ->
      format_warnings(File, Warnings),
      format_errors(File, Errors)
  end.

%% Compile core files for bootstrap.
%% Invoked from the Makefile.

core() ->
  elixir:start_app(),
  gen_server:call(elixir_code_server, { compiler_options, [{internal,true}] }),
  [core_file(File) || File <- core_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- core_list()],
  Files = lists:append(AllLists) -- core_main(),
  [core_file(File) || File <- '__MAIN__-List':uniq(Files)].

%% HELPERS

no_auto_import() ->
  { attribute, 0, compile, {
    no_auto_import, erlang:module_info(exports) } }.

module_form(Exprs, Line, File, Module) when
    is_binary(File), is_list(Exprs), is_integer(Line), is_atom(Module) ->
  Args = [{ var, Line, '_@MODULE'}],

  [
    { attribute, Line, file, { binary_to_list(File), 1 } },
    { attribute, Line, module, Module },
    { attribute, Line, export, [{ 'BOOTSTRAP',1 }] },
    { function, Line, 'BOOTSTRAP', length(Args), [
      { clause, Line, Args, [], Exprs }
    ] }
  ].

%% Fast compilation is available?

allows_fast_compilation([{defmodule,_,_}|T]) -> allows_fast_compilation(T);
allows_fast_compilation([]) -> true;
allows_fast_compilation(_) -> false.

%% Escape the module name, removing slashes, dots,
%% so it can be loaded by Erlang.

escape_module(Module) when is_atom(Module) ->
  escape_module(atom_to_list(Module));

escape_module(Module) when is_binary(Module) ->
  escape_module(binary_to_list(Module));

escape_module(Module) when is_list(Module) ->
  list_to_atom(escape_each(Module)).

escape_each([H|T]) when H >= $A, H =< $Z; H >= $a, H =< $z; H >= $0, H =< $9; H == $- ->
  [H|escape_each(T)];

escape_each([_|T]) ->
  [$_|escape_each(T)];

escape_each([]) -> [].

%% Receives a module Binary and outputs it in the given path.

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary),
  Path.

%% CORE FILES COMPILATION

core_file(File) ->
  try
    Lists = file(list_to_binary(File)),
    [binary_to_path(X, "ebin") || X <- Lists],
    io:format("Compiled ~s~n", [File])
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      exit(1)
  end.

core_list() ->
  [
    "lib/uri/parser.ex",
    "lib/elixir/formatter.ex",
    "lib/dict.ex",
    "lib/*/*.ex",
    "lib/*.ex"
  ].

core_main() ->
  [
    "lib/elixir/builtin.ex",
    "lib/macro/env.ex",
    "lib/macro.ex",
    "lib/keyword.ex",
    "lib/module.ex",
    "lib/list.ex",
    "lib/protocol.ex",
    "lib/enum.ex",
    "lib/record.ex",
    "lib/exception.ex",
    "lib/binary/inspect.ex",
    "lib/binary/chars.ex",
    "lib/list/chars.ex",
    "lib/gen_server/behavior.ex"
  ].

%% ERROR HANDLING

format_errors(_File, []) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(File, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(File, Error) end, Each)
  end, Errors).

format_warnings(File, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(File, Warning) end, Each)
  end, Warnings).
