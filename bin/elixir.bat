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

rem parameters for Erlang
set parsErlang=

rem make sure we keep a copy of all parameters
set allPars=%*

rem Recursive loop called for each parameter
:startloop
set par=%1
shift
if "%par%"=="" (
  rem if no parameters defined
  goto :run
)
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
goto:startloop
:run
erl -env ERL_LIBS %ERL_LIBS%;"%~dp0\..\lib" -noshell %ELIXIR_ERL_OPTS% %parsErlang% -s elixir start_cli -extra %allPars%
