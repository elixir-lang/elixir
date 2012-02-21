-module(elixir_compiler).
-export([file/2, file_to_path/3, core/0, module/3, eval_forms/4]).
-include("elixir.hrl").

%% Public API

%% Compiles the given file. Returns a list of tuples
%% with module names and binaries.

file(Filename, #elixir_compile{} = C) ->
  Previous = get(elixir_compiled),
  try
    put(elixir_compiled, []),
    Contents = case file:read_file(Filename) of
      {ok, Bin} -> binary_to_list(Bin);
      Error -> erlang:error(Error)
    end,

    eval(Contents, 1, Filename, list_to_atom(filename_to_module(Filename)), C),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end;

file(Filename, Opts) ->
  file(Filename, #elixir_compile {
    docs=get_value(Opts, docs, false),
    debug_info=get_value(Opts, debug_info, false),
    ignore_module_conflict=get_value(Opts, ignore_module_conflict, false)
  }).

%% Compiles a file to the given path (directory).

file_to_path(File, Path, Opts) ->
  Lists = file(File, Opts),
  [binary_to_path(X, Path) || X <- Lists].

%% Evaluates the contents/forms by compiling them to an Erlang module.

eval(String, Line, Filename, Module, C) ->
  Forms = elixir_translator:forms(String, Line, Filename),
  eval_forms(Forms, Line, Module, #elixir_scope{filename=Filename,compile=C}).

eval_forms(Forms, Line, Module, #elixir_scope{module=[]} = S) ->
  eval_forms(Forms, Line, Module, nil, S);

eval_forms(Forms, Line, Module, #elixir_scope{module=Value} = S) ->
  eval_forms(Forms, Line, Module, Value, S).

eval_forms(Forms, Line, Module, Value, S) ->
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
module(Forms, #elixir_scope{compile=C} = S, Callback) ->
  Options = case C#elixir_compile.debug_info of
    true -> [debug_info];
    _ -> []
  end,
  module(Forms, S#elixir_scope.filename, Options, Callback).

module(Forms, Filename, Options, Callback) ->
  case compile:forms([no_auto_import()|Forms], [return|Options]) of
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

get_value(Orddict, Key, Default) ->
  case orddict:find(Key, Orddict) of
    { ok, Value } -> Value;
    error -> Default
  end.

no_auto_import() ->
  { attribute, 0, compile, {
    no_auto_import, [
      { abs, 1 },
      { apply, 2 },
      { apply, 3 },
      { atom_to_binary, 2 },
      { atom_to_list, 1 },
      { binary_part, 2 },
      { binary_part, 3 },
      { binary_to_atom, 2 },
      { binary_to_existing_atom, 2 },
      { binary_to_list, 1 },
      { binary_to_list, 3 },
      { binary_to_term, 1 },
      { binary_to_term, 2 },
      { bit_size, 1 },
      { bitstring_to_list, 1 },
      { byte_size, 1 },
      { check_old_code, 1 },
      { check_process_code, 2 },
      { date, 0 },
      { delete_module, 1 },
      { demonitor, 1 },
      { demonitor, 2 },
      { disconnect_node, 1 },
      { element, 2 },
      { erase, 0 },
      { erase, 1 },
      { error, 1 },
      { error, 2 },
      { exit, 1 },
      { exit, 2 },
      { float, 1 },
      { float_to_list, 1 },
      { garbage_collect, 0 },
      { garbage_collect, 1 },
      { get, 0 },
      { get, 1 },
      { get_keys, 1 },
      { group_leader, 0 },
      { group_leader, 2 },
      { halt, 0 },
      { halt, 1 },
      { hd, 1 },
      { integer_to_list, 1 },
      { integer_to_list, 2 },
      { iolist_size, 1 },
      { iolist_to_binary, 1 },
      { is_alive, 0 },
      { is_atom, 1 },
      { is_binary, 1 },
      { is_bitstring, 1 },
      { is_boolean, 1 },
      { is_float, 1 },
      { is_function, 1 },
      { is_function, 2 },
      { is_integer, 1 },
      { is_list, 1 },
      { is_number, 1 },
      { is_pid, 1 },
      { is_port, 1 },
      { is_process_alive, 1 },
      { is_record, 2 },
      { is_record, 3 },
      { is_reference, 1 },
      { is_tuple, 1 },
      { length, 1 },
      { link, 1 },
      { list_to_atom, 1 },
      { list_to_binary, 1 },
      { list_to_bitstring, 1 },
      { list_to_existing_atom, 1 },
      { list_to_float, 1 },
      { list_to_integer, 1 },
      { list_to_integer, 2 },
      { list_to_pid, 1 },
      { list_to_tuple, 1 },
      { load_module, 2 },
      { make_ref, 0 },
      { max, 2 },
      { min, 2 },
      { module_loaded, 1 },
      { monitor, 2 },
      { monitor_node, 2 },
      { node, 0 },
      { node, 1 },
      { nodes, 0 },
      { nodes, 1 },
      { now, 0 },
      { open_port, 2 },
      { pid_to_list, 1 },
      { port_close, 1 },
      { port_command, 2 },
      { port_command, 3 },
      { port_connect, 2 },
      { port_control, 3 },
      { pre_loaded, 0 },
      { processes, 0 },
      { process_flag, 2 },
      { process_flag, 3 },
      { process_info, 1 },
      { process_info, 2 },
      { purge_module, 1 },
      { put, 2 },
      { register, 2 },
      { registered, 0 },
      { round, 1 },
      { self, 0 },
      { setelement, 3 },
      { size, 1 },
      { spawn, 1 },
      { spawn, 2 },
      { spawn, 3 },
      { spawn, 4 },
      { spawn_link, 1 },
      { spawn_link, 2 },
      { spawn_link, 3 },
      { spawn_link, 4 },
      { spawn_monitor, 1 },
      { spawn_monitor, 3 },
      { spawn_opt, 2 },
      { spawn_opt, 3 },
      { spawn_opt, 4 },
      { spawn_opt, 5 },
      { split_binary, 2 },
      { statistics, 1 },
      { term_to_binary, 1 },
      { term_to_binary, 2 },
      { throw, 1 },
      { time, 0 },
      { tl, 1 },
      { trunc, 1 },
      { tuple_size, 1 },
      { tuple_to_list, 1 },
      { unlink, 1 },
      { unregister, 1 },
      { whereis, 1 }
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
    Lists = file(File, #elixir_compile{internal=true}),
    [binary_to_path(X, "exbin") || X <- Lists]
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