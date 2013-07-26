@echo off
set argc=0
for %%x in (%*) do set /A argc+=1
if  %argc%== 0 (
  goto documentation
) else (
  goto parseopts
)
:documentation
echo Usage: %~nx0 [options] [.exs file] [data]
echo.
echo   -v                Prints version and exit
echo   -e command        Evaluates the given command (*)
echo   -r file           Requires the given files/patterns (*)
echo   -S script         Finds and executes the given script (*)
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
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after the .exs file or -- are passed down to the executed code
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTS or --erl
goto :EOF

:parseopts

rem Parameters for Erlang
set parsErlang=

rem Make sure we keep a copy of all parameters
set allPars=%*

rem Get the original path name from the batch file
set originPath=%~dp0

rem Optional parameters before the "-extra" parameter
set beforeExtra=

rem Recursive loop called for each parameter that parses the cmd line parameters
:startloop
set par="%1"
shift
if "%par%"=="" (
  rem if no parameters defined
  goto :expand_erl_libs
)
rem ******* ERLANG PARAMETERS **********************
for /f "usebackq" %%m in (`echo %par%^|findstr \--detached`) do (
  set parsErlang=%parsErlang% -detached
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--hidden`) do (
  set parsErlang=%parsErlang% -hidden
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--cookie`) do (
  set parsErlang=%parsErlang% -setcookie %1
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--sname`) do (
  set parsErlang=%parsErlang% -sname %1
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--name`) do (
  set parsErlang=%parsErlang% -name %1
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--erl`) do (
  set beforeExtra=%beforeExtra% %~1
  shift
  goto:startloop
)
rem ******* elixir parameters **********************
for /f "usebackq" %%m in (`echo %par%^|findstr \--v`) do (
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--compile`) do (
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--no-halt`) do (
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \+iex`) do (
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \+compile`) do (
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \-[er]`) do (
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \-p[raz]`) do (
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--app`) do (
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--remsh`) do (
  shift
  goto:startloop
)
rem ******* assume all pre-params are parsed ********************
:expand_erl_libs
rem ******* expand all ebin paths as Windows does not support the ..\*\ebin wildcard ********************
set ext_libs=
SETLOCAL enabledelayedexpansion
for  /d %%d in ("%originPath%..\lib\*.") do (
  set ext_libs=!ext_libs! -pa "%%~fd\ebin"
)
SETLOCAL disabledelayedexpansion
:run
set to_run=erl %ext_libs% -noshell %ELIXIR_ERL_OPTS% %parsErlang% -s elixir start_cli %beforeExtra% -extra %* 
:pipe_through_wac_if_availble
REM ******* this gives us ANSI color support on windows ********************
if exist %originPath%wac.exe (
   %to_run% | %originPath%wac.exe
) else (
   %to_run%
)
