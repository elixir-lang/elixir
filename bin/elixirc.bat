@echo off
if "%*" == "" (
	goto documentation
) else (
	goto run 
) 
:documentation
echo Usage: %~nx0 [switches] [.ex files]
echo.
echo  -v              Prints version and exit
echo  -o              The directory to output compiled files
echo  -pa path      Prepend the given path to Erlang code path (*)
echo  -pz path      Append the given path to Erlang code path (*)
echo  --no-docs       Do not attach documentation with compiled code
echo  --debug-info    Attach debug info to compiled modules
echo  --ignore-module-conflict
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after -- are passed down to the executed code
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTS" >&2
:run
call "%~dp0\elixir.bat" --compile %*