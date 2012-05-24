@echo off
erl -pa %~dp0\..\ebin -noshell %ELIXIR_ERL_OPTS% -s elixir start_cli -extra --no-halt -e "Elixir.IEx.start" %*