IF DEFINED EX_DEBUG (@echo on) ELSE (@echo off)
call "%~dp0\elixir.bat" +iex --erl "-user Elixir.IEx.CLI" --no-halt %*
