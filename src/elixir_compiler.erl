-module(elixir_compiler).
-export([file/2, core/0]).
-include("elixir.hrl").

file(File, Path) ->
  elixir:file(File, [], Path).

internal_file(File) ->
  elixir:file(File, [{self,nil}], "exbin").

core() ->
  code:ensure_loaded(elixir_object_methods),
  [internal_file(File) || File <- compile_main()],
  AllLists = [filelib:wildcard(Wildcard) || Wildcard <- compile_list()],
  Files = lists:append(AllLists) -- compile_main(),
  [file(File, "exbin") || File <- Files].

compile_list() ->
  [
    "lib/*.ex",
    "lib/*/*.ex"
  ].

compile_main() ->
  [
    "lib/object.ex",
    "lib/module.ex",
    "lib/io.ex",
    "lib/atom.ex",
    "lib/list.ex",
    "lib/numeric.ex",
    "lib/integer.ex",
    "lib/float.ex",
    "lib/tuple.ex",
    "lib/string.ex",
    "lib/ordered_dict.ex",
    "lib/regexp.ex",
    "lib/bit_string.ex",
    "lib/process.ex",
    "lib/port.ex",
    "lib/reference.ex",
    "lib/function.ex",
    "lib/gen_server.ex",
    "lib/record.ex",
    "lib/file.ex",
    "lib/code.ex",
    "lib/code/formatter.ex",
    "lib/code/init.ex",
    "lib/code/server.ex"
  ].