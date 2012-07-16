@echo off
call %~dp0\elixir.bat -e "Mix.start; Mix.load /> Mix.run" %*