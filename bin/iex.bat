@echo off
SET useWerl=1
call "%~dp0\elixir.bat" +iex --no-halt -e "IEx.start" %*