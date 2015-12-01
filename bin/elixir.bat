if defined Ex_Debug (@echo on) else  (@echo off)
setlocal
if ""%1""==""""       goto :documentation
if ""%1""==""--help"" goto :documentation
if ""%1""==""-h""     goto :documentation
if ""%1""==""/h""     goto :documentation
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
echo   --werl            Uses Erlang's Windows shell GUI
echo   --no-halt         Does not halt the Erlang VM after execution
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after the .exs file or -- are passed down to the executed code
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTIONS or --erl
goto end

:parseopts

rem Parameters for Erlang
set parsErlang=

rem Make sure we keep a copy of all parameters
set allPars=%*

rem Get the original path name from the batch file
set originPath=%~dp0

rem Optional parameters before the "-extra" parameter
set beforeExtra=

rem Flag which determines whether or not to use werl vs erl
set useWerl=0

rem Designates which mode / Elixir component to run as
set runMode="elixir"

rem Recursive loop called for each parameter that parses the cmd line parameters
:startloop
set par="%1"
shift
if "%par%"=="" (
  rem if no parameters defined
  goto :expand_erl_libs
)
if "%par%"=="""" (
  rem if no parameters defined - special case for parameter that is already quoted
  goto :expand_erl_libs
)
rem ******* EXECUTION OPTIONS **********************
IF "%par%"==""--werl"" (Set useWerl=1)
IF "%par%"==""+iex"" (Set runMode="iex")
rem ******* elixir parameters **********************
rem Note: we don't have to do anything with options that don't take an argument
IF """"=="%par:-e=%"      (shift) 
IF """"=="%par:-r=%"      (shift) 
IF """"=="%par:-pr=%"     (shift) 
IF """"=="%par:-pa=%"     (shift) 
IF """"=="%par:-pz=%"     (shift) 
IF """"=="%par:--app=%"   (shift) 
IF """"=="%par:--remsh=%" (shift) 
rem ******* ERLANG PARAMETERS **********************
IF """"=="%par:--detached=%" (Set parsErlang=%parsErlang% -detached) 
IF """"=="%par:--hidden=%"   (Set parsErlang=%parsErlang% -hidden)
IF """"=="%par:--cookie=%"   (Set parsErlang=%parsErlang% -setcookie %1 && shift)
IF """"=="%par:--sname=%"    (Set parsErlang=%parsErlang% -sname %1 && shift) 
IF """"=="%par:--name=%"     (Set parsErlang=%parsErlang% -name %1 && shift) 
IF """"=="%par:--erl=%"      (Set beforeExtra=%beforeExtra% %~1 && shift) 
goto:startloop

rem ******* assume all pre-params are parsed ********************
:expand_erl_libs
rem ******* expand all ebin paths as Windows does not support the ..\*\ebin wildcard ********************
SETLOCAL enabledelayedexpansion
set ext_libs=
for  /d %%d in ("%originPath%..\lib\*.") do (
  set ext_libs=!ext_libs! -pa "%%~fd\ebin"
)
SETLOCAL disabledelayedexpansion
:run
IF NOT %runMode% == "iex" (
  set beforeExtra=-noshell -s elixir start_cli %beforeExtra%
)
IF %useWerl% EQU 1 (
  start werl.exe %ext_libs% %ELIXIR_ERL_OPTIONS% %parsErlang% %beforeExtra% -extra %*
) ELSE (
  erl.exe %ext_libs% %ELIXIR_ERL_OPTIONS% %parsErlang% %beforeExtra% -extra %*
)
:end
endlocal
