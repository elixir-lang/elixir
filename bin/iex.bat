@echo off
erl -env ERL_LIBS %ERL_LIBS%;%~dp0\..\lib -noshell %ELIXIR_ERL_OPTS% -s elixir start_cli -extra --no-halt -e "Elixir.IEx.start" %*