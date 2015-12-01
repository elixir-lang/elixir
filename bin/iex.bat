if defined Ex_Debug (@echo on) else  (@echo off)
call "%~dp0\elixir.bat" +iex --erl "-user Elixir.IEx.CLI" --no-halt %*
