@echo off
if "%1"==""       goto documentation
if "%1"=="--help" goto documentation
if "%1"=="-h"     goto documentation
if "%1"=="/h"     goto documentation
goto parseopts

:documentation
echo Usage: %~nx0 [options] [.exs file] [data]
echo.
echo   -v                Prints version and exit
echo   -e command        Evaluates the given command (*)
echo   -r file           Requires the given files/patterns (*)
echo   -S script         Finds and executes the given script
echo   -pr file          Requires the given files/patterns in parallel (*)
echo   -pa path          Prepends the given path to Erlang code path (*)
echo   -pz path          Appends the given path to Erlang code path (*)
echo   --app app         Start the given app and its dependencies (*)
echo   --erl switches    Switches to be passed down to erlang (*)
echo   --name name       Makes and assigns a name to the distributed node
echo   --sname name      Makes and assigns a short name to the distributed node
echo   --cookie cookie   Sets a cookie for this distributed node
echo   --hidden          Makes a hidden node
echo   --detached        Starts the Erlang VM detached from console
echo   --no-halt         Does not halt the Erlang VM after execution
echo   --gen-debug       Turns on default debugging for all GenServers
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after the .exs file or -- are passed down to the executed code
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTIONS or --erl
goto :EOF

:parseopts

rem Parameters for Erlang
set parsErlang=

rem Make sure we keep a copy of all parameters
set allPars=%*

rem Get the original path name from the batch file
set originPath=%~dp0

rem Derive library path from originPath
set libPath=%originPath:\bin\=\lib%

rem Optional parameters before the "-extra" parameter
set beforeExtra=

rem Flag which determines whether or not to use werl vs erl
set useWerl=0

rem Recursive loop called for each parameter that parses the cmd line parameters
:startloop
set par="%1"
shift
if "%par%"=="" (
  rem if no parameters defined
  goto :run
)
if "%par%"=="""" (
  rem if no parameters defined - special case for parameter that is already quoted
  goto :run
)
rem ******* EXECUTION OPTIONS **********************
IF "%par%"==""+iex"" (Set useWerl=1)
rem ******* ERLANG PARAMETERS **********************
IF NOT "%par%"=="%par:--detached=%" (Set parsErlang=%parsErlang% -detached) 
IF NOT "%par%"=="%par:--hidden=%"   (Set parsErlang=%parsErlang% -hidden)
IF NOT "%par%"=="%par:--cookie=%"   (Set parsErlang=%parsErlang% -setcookie %1 && shift)
IF NOT "%par%"=="%par:--sname=%"    (Set parsErlang=%parsErlang% -sname %1 && shift) 
IF NOT "%par%"=="%par:--name=%"     (Set parsErlang=%parsErlang% -name %1 && shift) 
IF NOT "%par%"=="%par:--erl=%"      (Set beforeExtra=%beforeExtra% %~1 && shift) 
IF NOT "%par%"=="%par:--gen-debug=%" (Set parsErlang=%parsErlang% -generic_debug)
rem ******* elixir parameters **********************
rem Note: we don't have to do anything with options that don't take an argument
IF NOT "%par%"=="%par:-e=%"      (shift) 
IF NOT "%par%"=="%par:-r=%"      (shift) 
IF NOT "%par%"=="%par:-pr=%"     (shift) 
IF NOT "%par%"=="%par:-pa=%"     (shift) 
IF NOT "%par%"=="%par:-pz=%"     (shift) 
IF NOT "%par%"=="%par:--app=%"   (shift) 
IF NOT "%par%"=="%par:--remsh=%" (shift) 
goto:startloop

rem ******* assume all pre-params are parsed ********************
:run
setlocal
IF DEFINED ERL_LIBS (
	set ERL_LIBS=%libPath%;%ERL_LIBS%
) ELSE (
	set ERL_LIBS=%libPath%
)

IF %useWerl% EQU 1 (
    werl.exe %ELIXIR_ERL_OPTIONS% %parsErlang% -s elixir start_cli %beforeExtra% -extra %*
) ELSE (
    erl.exe -noshell %ELIXIR_ERL_OPTIONS% %parsErlang% -s elixir start_cli %beforeExtra% -extra %*
)
