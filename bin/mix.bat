@if defined Elixir_CLI_Echo (@echo on) else  (@echo off)
call "%~dp0\elixir.bat" -e Mix.start -e Mix.CLI.main %*
