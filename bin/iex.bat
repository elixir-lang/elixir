@echo off
erl -pa %~dp0\..\ebin -noshell %ELIXIR_ERL_OPTS% -s "__MAIN__.Elixir.IEx" simple_start -extra %*