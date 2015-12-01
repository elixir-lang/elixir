if defined Ex_Debug (@echo on) else  (@echo off)
call "%~dp0\elixir.bat" -e Mix.start -e Mix.CLI.main %*
