%% Compiler errors default handler
%%
-module(elixir_errors_default).

-behaviour(elixir_errors).

%% elixir_errors_handler callbacks
-export([handle_warn/4,
         handle_error/4]).

-include("elixir.hrl").

handle_warn(File, Line, _Type, Msg) ->
  CompilerPid = get(elixir_compiler_pid),
  if
    CompilerPid =/= undefined -> elixir_code_server:cast({register_warning, CompilerPid});
    true -> ok
  end,
  FmtMsg = [Msg, "\n ", file_format(Line, File), $\n],
  io:put_chars(standard_error, [warning_prefix(), FmtMsg, $\n]).


handle_error(File, Line, Type, Msg) ->
  try
    throw(ok)
  catch
    ok -> ok
  end,
  StackTrace = erlang:get_stacktrace(),
  Exception = Type:exception([{description, Msg}, {file, File}, {line, Line}]),
  erlang:raise(error, Exception, tl(StackTrace)).

%%
%% Helpers
%%
warning_prefix() ->
  case application:get_env(elixir, ansi_enabled) of
    {ok, true} -> <<"\e[33mwarning: \e[0m">>;
    _ -> <<"warning: ">>
  end.

file_format(0, File) ->
  io_lib:format("~ts", [elixir_utils:relative_to_cwd(File)]);

file_format(Line, File) ->
  io_lib:format("~ts:~w", [elixir_utils:relative_to_cwd(File), Line]).
