#!/usr/bin/env escript
%%! -noinput

%% This file is used by the Elixir installer and uninstaller.
main(["add", ";" ++ PathsToAdd]) ->
  {ok, Reg} = win32reg:open([read, write]),
  ok = win32reg:change_key(Reg, "\\hkey_current_user\\environment"),
  {ok, SystemPath} = win32reg:value(Reg, "path"),

  NewSystemPath =
    lists:foldl(
      fun(Elem, Acc) ->
        Elem ++ ";" ++
          binary_to_list(
            iolist_to_binary(
              string:replace(Acc, Elem ++ ";", "", all)))
      end,
      SystemPath,
      string:split(PathsToAdd, ";", all)
    ),

  ok = win32reg:set_value(Reg, "Path", NewSystemPath),
  ok;

main(["remove", ";" ++ PathsToRemove]) ->
  {ok, Reg} = win32reg:open([read, write]),
  ok = win32reg:change_key(Reg, "\\hkey_current_user\\environment"),
  {ok, SystemPath} = win32reg:value(Reg, "path"),

  NewSystemPath =
    lists:foldl(
      fun(Elem, Acc) ->
        binary_to_list(
          iolist_to_binary(
            string:replace(Acc, Elem ++ ";", "", all)))
      end,
      SystemPath,
      string:split(PathsToRemove, ";", all)
    ),

  ok = win32reg:set_value(Reg, "Path", NewSystemPath),
  ok.
