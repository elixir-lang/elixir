@echo off
call "%~dp0\elixir.bat" --no-halt -e "IEx.CLI.start" %*