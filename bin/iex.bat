@echo off
call "%~dp0\elixir.bat" +iex --erl "-user Elixir.IEx.CLI" --no-halt %*
