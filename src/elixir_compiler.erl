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

string(Contents, Filename) ->
  Previous = get(elixir_compiled),

  try
    put(elixir_compiled, []),
    Forms = elixir_translator:forms(Contents, 1, Filename),
    eval_forms(Forms, 1, Filename, #elixir_scope{filename=Filename}),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end.

%% Compile a file, return a tuple of module names and binaries.

file(Relative) ->
  Filename = filename:absname(Relative),
  case file:read_file(Filename) of
    {ok, Bin} ->
      string(unicode:characters_to_list(Bin), Filename);
    Error ->
      erlang:error(Error)
  end.

%% Compiles a file to the given path (directory).

file_to_path(File, Path) ->
  Lists = file(File),
  [binary_to_path(X, Path) || X <- Lists],
  Lists.

%% Evaluates the contents/forms by compiling them to an Erlang module.

eval_forms(Forms, Line, Module, #elixir_scope{module=[]} = S) ->
  eval_forms(Forms, Line, Module, nil, S);

eval_forms(Forms, Line, Module, #elixir_scope{module=Value} = S) ->
  eval_forms(Forms, Line, Module, Value, S).

eval_forms(Forms, Line, RawModule, Value, S) ->
  case (Value == nil) and allows_fast_compilation(Forms) of
    true  -> eval_compilation(Forms, Line, S);
    false -> code_loading_compilation(Forms, Line, RawModule, Value, S)
  end.

eval_compilation(Forms, Line, S) ->
  { Result, _Binding, FS } = elixir:eval_quoted(Forms, [{'_EXMODULE',nil}], Line, S),
  { Result, FS }.

code_loading_compilation(Forms, Line, RawModule, Value, S) ->
  Module = escape_module(RawModule),
  { Exprs, FS } = elixir_translator:translate(Forms, S),
  ModuleForm = module_form(Exprs, Line, S#elixir_scope.filename, Module),
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
  module(Forms, S#elixir_scope.filename, Options, Callback).

module(Forms, Filename, Options, Callback) ->
  case compile:forms([no_auto_import()|Forms], [return,{source,Filename}|Options]) of
    {ok, ModuleName, Binary, Warnings} ->
      format_warnings(Filename, Warnings),
      code:load_binary(ModuleName, Filename, Binary),
      Callback(ModuleName, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Filename, Warnings),
      format_errors(Filename, Errors)
  end.

%% Compile core files for bootstrap.
%% Invoked from the Makefile.

core() ->
  elixir:start_app(),
  gen_server:call(elixir_code_server, { compiler_options, [{internal,true}] }),
  [core_file(File) || File <- core_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- core_list()],
  Files = lists:append(AllLists) -- core_main(),
  [core_file(File) || File <- '__MAIN__.List':uniq(Files)].

%% HELPERS

no_auto_import() ->
  { attribute, 0, compile, {
    no_auto_import, erlang:module_info(exports) } }.

module_form(Exprs, Line, Filename, Module) ->
  Args = [{ var, Line, '_EXMODULE'}],

  [
    { attribute, Line, module, Module },
    { attribute, Line, file, { Filename, 1 } },
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

escape_module(Module) when is_list(Module) ->
  list_to_atom(escape_each(Module)).

escape_each([H|T]) when H >= $A, H =< $Z; H >= $a, H =< $z; H >= $0, H =< $9 ->
  [H|escape_each(T)];

escape_each([_|T]) ->
  [$_|escape_each(T)];

escape_each([]) -> [].

%% Receives a module Binary and outputs it in the given path.

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = make_dir(CompilePath, atom_to_list(ModuleName), []),
  ok = file:write_file(Path, Binary),
  Path.

%% Loops through a module name creating the directories
%% in the destination. Returns the final filename with .beam.

make_dir(Current, [$.|T], Buffer) ->
  NewCurrent = filename:join(Current, lists:reverse(Buffer)),
  case file:make_dir(NewCurrent) of
    { error, eexist } -> [];
    ok -> []
  end,
  make_dir(NewCurrent, T, []);

make_dir(Current, [H|T], Buffer) ->
  make_dir(Current, T, [H|Buffer]);

make_dir(Current, [], Buffer) ->
  filename:join(Current, lists:reverse(Buffer) ++ ".beam").

%% CORE FILES COMPILATION

core_file(File) ->
  try
    Lists = file(File),
    [binary_to_path(X, "exbin") || X <- Lists],
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
    "lib/module.ex",
    "lib/keyword.ex",
    "lib/list.ex",
    "lib/protocol.ex",
    "lib/enum.ex",
    "lib/macro.ex",
    "lib/record.ex",
    "lib/exception.ex",
    "lib/binary/inspect.ex",
    "lib/binary/chars.ex",
    "lib/list/chars.ex",
    "lib/gen_server/behavior.ex"
  ].

%% ERROR HANDLING

format_errors(_Filename, []) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(Filename, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(Filename, Error) end, Each)
  end, Errors).

format_warnings(Filename, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Filename, Warning) end, Each)
  end, Warnings).
