-module(elixir_file_methods).
-export([is_file/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

is_file(Bin) ->
  case file:read_file_info(Bin) of
    { ok, FileInfo } -> FileInfo#file_info.type == regular;
    _ -> false
  end.
