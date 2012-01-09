-module(elixir_compiler).
-export([file/2, file_to_path/2, core/0]).
-include("elixir.hrl").

file(File, Binding) ->
  try
    put(elixir_compiled, []),
    elixir:file(File, Binding),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, undefined)
  end.

file_to_path(File, Path) -> file_to_path(File, Path, []).

file_to_path(File, Path, Binding) ->
  Lists = file(File, Binding),
  lists:foreach(fun (X) -> binary_to_path(X, Path) end, Lists).

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary).

internal_file(File) ->
  io:format("Compiling ~s~n", [File]),
  try
    file_to_path(File, "exbin")
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      exit(1)
  end.

core() ->
  [internal_file(File) || File <- compile_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- compile_list()],
  Files = lists:append(AllLists) -- compile_main(),
  [internal_file(File) || File <- Files].

compile_list() ->
  [
    "lib/*.ex",
    "lib/*/*.ex"
  ].

compile_main() ->
  [
    "lib/elixir/macros.ex",
    "lib/module.ex",
    "lib/orddict.ex",
    "lib/list.ex",
    "lib/protocol.ex",
    "lib/enum.ex",
    "lib/record.ex",
    "lib/string/inspect.ex",
    "lib/list/inspect.ex"
  ].