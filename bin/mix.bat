@echo off
if "%1" == "iex" (
  goto iex
) else (
  goto elixir
)
:iex
call "%~dp0\iex.bat" -e "Mix.start" -e "Mix.CLI.run" -- %*
:elixir
call "%~dp0\elixir.bat" -e "Mix.start" -e "Mix.CLI.run" -- %*