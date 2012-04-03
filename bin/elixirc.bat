@echo off
if "%*" == "" (
	goto documentation
) else (
	goto run 
) 
:documentation
echo Usage: %~nx0 [switches] [.ex files]
echo.
echo  -v                    Prints version and exit
echo  -o                    The directory to output compiled files
echo  -pa dir1 dir2 ...   Prepend the given path to Erlang code path (*)
echo  -pz dir1 dir2 ...   Append the given path to Erlang code path (*)
echo  --docs                Attach documentation to compiled modules
echo  --debug-info       Attach debug info to compiled modules
echo  --ignore-module-conflict
echo.
echo ** Options marked with (*) can be given more than once;
echo.
echo ** Options given after -- are passed down to the executed code;
echo.
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTS." >&2
:run
call %~dp0\elixir.bat +compile %*