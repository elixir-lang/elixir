@echo off
SETLOCAL enabledelayedexpansion
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
echo  -v            Prints version and exit
echo  -e command    Evaluates the given command (*)
echo  -r command    Requires the given file/pattern (*)
echo  -pr command   Requires the given file/pattern in parallel (*)
echo  -pa path      Prepend the given path to Erlang code path (*)
echo  -pz path      Append the given path to Erlang code path (*)
echo  --app app     Start the given app and its dependencies (*)
echo  --no-halt     Do not halt the Erlang VM after execution
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after the .exs file or -- are passed down to the executed code
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTS
goto :EOF

:parseopts

rem Parameters for Erlang
set parsErlang=

rem Make sure we keep a copy of all parameters
set allPars=%*

rem Optional parameters before the "-extra" parameter
set beforeExtra=

rem Recursive loop called for each parameter that parses the cmd line parameters
:startloop
set par=%1
shift
if "%par%"=="" (
  rem if no parameters defined
  goto :run
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
for /f "usebackq" %%m in (`echo %par%^|findstr \-[erS]`) do (
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \-p[raz]`) do (
  shift
  goto:startloop
)
for /f "usebackq" %%m in (`echo %par%^|findstr \--remsh`) do (
  shift
  goto:startloop
)
rem ******* elixir file **********************
for /f "usebackq" %%m in (`echo %par%^|findstr \.ex`) do (
  goto:run
)
REM Others should give a problem
echo ERROR: Parameter %par% is not allowed before the .ex file
exit /B -1
:run
echo erl -env ERL_LIBS %ERL_LIBS%;"%~dp0\..\lib" -noshell %ELIXIR_ERL_OPTS% %parsErlang% -s elixir start_cli %beforeExtra% -extra %*
