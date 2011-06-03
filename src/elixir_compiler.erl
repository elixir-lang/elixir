-module(elixir_compiler).
-export([file/1, file_to_path/2, core/0]).
-include("elixir.hrl").

file(File) -> file(File, []).

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
  file_to_path(File, "exbin", [{self,nil}]).

core() ->
  code:ensure_loaded(elixir_object_methods),
  [internal_file(File) || File <- compile_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- compile_list()],
  Files = lists:append(AllLists) -- compile_main(),
  [file_to_path(File, "exbin") || File <- Files].

compile_list() ->
  [
    "lib/*.ex",
    "lib/*/*.ex"
  ].

compile_main() ->
  [
    "lib/module.ex",
    "lib/io.ex",
    "lib/atom.ex",
    "lib/list.ex",
    "lib/numeric.ex",
    "lib/integer.ex",
    "lib/float.ex",
    "lib/tuple.ex",
    "lib/regexp.ex",
    "lib/string.ex",
    "lib/ordered_dict.ex",
    "lib/bit_string.ex",
    "lib/process.ex",
    "lib/port.ex",
    "lib/reference.ex",
    "lib/function.ex",
    "lib/gen_server.ex",
    "lib/record.ex",
    "lib/file.ex",
    "lib/unbound_method.ex",
    "lib/method.ex",
    "lib/code.ex",
    "lib/code/formatter.ex",
    "lib/code/init.ex",
    "lib/code/server.ex"
  ].