if defined Elixir_CLI_Echo (@echo on) else  (@echo off)
call "%~dp0\elixir.bat" +iex --erl "-user Elixir.IEx.CLI" --no-halt %*
