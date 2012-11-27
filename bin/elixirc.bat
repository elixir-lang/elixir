@echo off
if "%*" == "" (
	goto documentation
) else (
	goto run 
) 
:documentation
echo Usage: %~nx0 [elixir switches] [compiler switches] [.ex files]
echo.
echo  -o               The directory to output compiled files
echo  --no-docs        Do not attach documentation to compiled modules
echo  --no-debug-info  Do not attach debug info to compiled modules
echo  --ignore-module-conflict
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after -- are passed down to the executed code
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTS" >&2
:run
call "%~dp0\elixir.bat" --compile %*