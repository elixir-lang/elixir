@echo off
%~dp0\erl -env ERL_LIBS %~dp0\..\lib -noshell %ELIXIR_ERL_OPTS% -s elixir start_cli -extra --no-halt -e "Elixir.IEx.start" %*