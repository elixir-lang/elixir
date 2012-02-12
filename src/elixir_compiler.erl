-module(elixir_compiler).
-export([file/1, file_to_path/2, core/0, module/3, eval/4, eval_forms/4]).
-include("elixir.hrl").

%% Public API

%% Compiles the given file. Returns a list of tuples
%% with module names and binaries.

file(Filename) ->
  Previous = get(elixir_compiled),
  try
    put(elixir_compiled, []),
    Contents = case file:read_file(Filename) of
      {ok, Bin} -> binary_to_list(Bin);
      Error -> erlang:error(Error)
    end,

    % elixir:eval(Contents, [], Filename),
    eval(Contents, 1, Filename, list_to_atom(filename_to_module(Filename))),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end.

%% Compiles a file to the given path (directory).

file_to_path(File, Path) ->
  Lists = file(File),
  [binary_to_path(X, Path) || X <- Lists].

%% Evaluates the contents/forms by compiling them to an Erlang module.

eval(String, Line, Filename, Module) ->
  Forms = elixir_translator:forms(String, Line, Filename),
  eval_forms(Forms, Line, Module, #elixir_scope{filename=Filename}).

eval_forms(Forms, Line, Module, #elixir_scope{module=[]} = S) ->
  eval_forms(Forms, Line, Module, nil, S);

eval_forms(Forms, Line, Module, #elixir_scope{module=Value} = S) ->
  eval_forms(Forms, Line, Module, Value, S).

eval_forms(Forms, Line, Module, Value, S) ->
  Filename = S#elixir_scope.filename,
  { Exprs, FS } = elixir_translator:translate(Forms, S),
  ModuleForm = module_form(Exprs, Line, Filename, Module),
  { module(ModuleForm, Filename, fun(Mod, _) ->
    Res = Mod:'BOOTSTRAP'(Value),
    code:purge(Module),
    code:delete(Module),
    Res
  end), FS }.

%% Internal API

%% Compile the module by forms at the filename and
%% executes the callback in case of success. This
%% automatically handles errors and warnings.
%% Used by this module and elixir_module.
module(Forms, Filename, Callback) ->
  case compile:forms([no_auto_import()|Forms], [return]) of
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
  [core_file(File) || File <- core_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- core_list()],
  Files = lists:append(AllLists) -- core_main(),
  [core_file(File) || File <- '::List':uniq(Files)].

%% HELPERS

no_auto_import() ->
  { attribute, 0, compile, {
    no_auto_import, [
      {get,0}, {get,1}, {get_keys,1}, {is_process_alive,1},
      {erase,0}, {erase,1}, {processes,0}, {put,1},
      {process_info,1}, {process_info,2}, {process_flag,2},
      {process_flag,3}, {link,1}, {unlink,1}, {register,2},
      {unregister,1}, {whereis,1}, {registered,0}
    ]
  } }.

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

filename_to_module([H|T]) when H >= $A, H =< $Z; H >= $a, H =< $z; H >= $0, H =< $9 ->
  [H|filename_to_module(T)];

filename_to_module([_|T]) ->
  [$_|filename_to_module(T)];

filename_to_module([]) -> [].

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary),
  CompilePath.

core_file(File) ->
  io:format("Compiling ~s~n", [File]),
  try
    file_to_path(File, "exbin")
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      exit(1)
  end.

core_list() ->
  [
    "lib/elixir/formatter.ex",
    "lib/*/*.ex",
    "lib/*.ex"
  ].

core_main() ->
  [
    "lib/elixir/builtin.ex",
    "lib/module.ex",
    "lib/orddict.ex",
    "lib/list.ex",
    "lib/protocol.ex",
    "lib/enum.ex",
    "lib/record.ex",
    "lib/exception.ex",
    "lib/string/inspect.ex",
    "lib/string/chars.ex",
    "lib/list/chars.ex"
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