@if defined ELIXIR_CLI_ECHO (@echo on) else (@echo off)
setlocal
if    ""%1""==""""       goto documentation
if /I ""%1""==""--help"" goto documentation
if /I ""%1""==""-h""     goto documentation
if /I ""%1""==""/h""     goto documentation
if    ""%1""==""/?""     goto documentation
goto parseopts

:documentation
echo Usage: %~nx0 [options] [.exs file] [data]
echo.
echo ## General options
echo.
echo   -e "COMMAND"                 Evaluates the given command (*)
echo   -h, --help                   Prints this message and exits
echo   -r "FILE"                    Requires the given files/patterns (*)
echo   -S SCRIPT                    Finds and executes the given script in $PATH
echo   -pr "FILE"                   Requires the given files/patterns in parallel (*)
echo   -pa "PATH"                   Prepends the given path to Erlang code path (*)
echo   -pz "PATH"                   Appends the given path to Erlang code path (*)
echo   -v, --version                Prints Elixir version and exits
echo.
echo   --app APP                    Starts the given app and its dependencies (*)
echo   --erl "SWITCHES"             Switches to be passed down to Erlang (*)
echo   --eval "COMMAND"             Evaluates the given command, same as -e (*)
echo   --logger-otp-reports BOOL    Enables or disables OTP reporting
echo   --logger-sasl-reports BOOL   Enables or disables SASL reporting
echo   --no-halt                    Does not halt the Erlang VM after execution
echo   --werl                       Uses Erlang's Windows shell GUI (Windows only)
echo.
echo Options given after the .exs file or -- are passed down to the executed code.
echo Options can be passed to the Erlang runtime using $ELIXIR_ERL_OPTIONS or --erl.
echo.
echo ## Distribution options
echo.
echo The following options are related to node distribution.
echo.
echo   --cookie COOKIE              Sets a cookie for this distributed node
echo   --hidden                     Makes a hidden node
echo   --name NAME                  Makes and assigns a name to the distributed node
echo   --rpc-eval NODE "COMMAND"    Evaluates the given command on the given remote node (*)
echo   --sname NAME                 Makes and assigns a short name to the distributed node
echo.
echo ## Release options
echo.
echo The following options are generally used under releases.
echo.
echo   --boot "FILE"                Uses the given FILE.boot to start the system
echo   --boot-var VAR "VALUE"       Makes $VAR available as VALUE to FILE.boot (*)
echo   --pipe-to "PIPEDIR" "LOGDIR" Starts the Erlang VM as a named PIPEDIR and LOGDIR
echo   --vm-args "FILE"             Passes the contents in file as arguments to the VM
echo.
echo --pipe-to is not supported on Windows. If set, Elixir won't boot.
echo.
echo ** Options marked with (*) can be given more than once.
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

rem Option which determines whether to use werl or erl
set useWerl=0

rem Designates which mode / Elixir component to run as
set runMode="elixir"

rem Recursive loop called for each parameter that parses the cmd line parameters
:startloop
set par="%1"
shift
if "%par%"=="" (
  rem if no parameters defined
  goto expand_erl_libs
)
if "%par%"=="""" (
  rem if no parameters defined - special case for parameter that is already quoted
  goto expand_erl_libs
)
rem ******* EXECUTION OPTIONS **********************
if "%par%"==""--werl"" (set useWerl=1)
if "%par%"==""+iex"" (set runMode="iex")
rem ******* ELIXIR PARAMETERS **********************
rem Note: we don't have to do anything with options that don't take an argument
if """"=="%par:-e=%"          (shift)
if """"=="%par:-r=%"          (shift)
if """"=="%par:-pr=%"         (shift)
if """"=="%par:-pa=%"         (shift)
if """"=="%par:-pz=%"         (shift)
if """"=="%par:--app=%"       (shift)
if """"=="%par:--eval=%"      (shift)
if """"=="%par:--remsh=%"     (shift)
if """"=="%par:--rpc-eval=%"  (shift && shift)
rem ******* ERLANG PARAMETERS **********************
if """"=="%par:--boot=%"                (set parsErlang=%parsErlang% -boot %1 && shift)
if """"=="%par:--boot-var=%"            (set parsErlang=%parsErlang% -boot_var %1 %2 && shift && shift)
if """"=="%par:--cookie=%"              (set parsErlang=%parsErlang% -setcookie %1 && shift)
if """"=="%par:--hidden=%"              (set parsErlang=%parsErlang% -hidden)
if """"=="%par:--logger-otp-reports=%"  (set parsErlang=%parsErlang% -logger handle_otp_reports %1 && shift)
if """"=="%par:--logger-sasl-reports=%" (set parsErlang=%parsErlang% -logger handle_sasl_reports %1 && shift)
if """"=="%par:--name=%"                (set parsErlang=%parsErlang% -name %1 && shift)
if """"=="%par:--sname=%"               (set parsErlang=%parsErlang% -sname %1 && shift)
if """"=="%par:--vm-args=%"             (set parsErlang=%parsErlang% -args_file %1 && shift)
if """"=="%par:--erl=%"                 (set "beforeExtra=%beforeExtra% %~1" && shift)
if """"=="%par:--pipe-to=%"             (echo --pipe-to : Option is not supported on Windows && goto end)
goto startloop

rem ******* assume all pre-params are parsed ********************
:expand_erl_libs
rem ******* expand all ebin paths as Windows does not support the ..\*\ebin wildcard ********************
setlocal enabledelayedexpansion
set ext_libs=
for  /d %%d in ("%originPath%..\lib\*.") do (
  set ext_libs=!ext_libs! -pa "%%~fd\ebin"
)
setlocal disabledelayedexpansion

:run
if not %runMode% == "iex" (
  set beforeExtra=-noshell -s elixir start_cli %beforeExtra%
)
if %useWerl% equ 1 (
  start werl.exe %ext_libs% %ELIXIR_ERL_OPTIONS% %parsErlang% %beforeExtra% -extra %*
) else (
  erl.exe %ext_libs% %ELIXIR_ERL_OPTIONS% %parsErlang% %beforeExtra% -extra %*
)
:end
endlocal
