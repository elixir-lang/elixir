@if defined ELIXIR_CLI_ECHO (@echo on) else (@echo off)

set ELIXIR_VERSION=1.16.0

setlocal enabledelayedexpansion
if    ""%1""==""""                if ""%2""=="""" goto documentation
if /I ""%1""==""--help""          if ""%2""=="""" goto documentation
if /I ""%1""==""-h""              if ""%2""=="""" goto documentation
if /I ""%1""==""/h""              if ""%2""=="""" goto documentation
if    ""%1""==""/?""              if ""%2""=="""" goto documentation
if /I ""%1""==""--short-version"" if ""%2""=="""" goto shortversion
goto parseopts

:documentation
echo Usage: %~nx0 [options] [.exs file] [data]
echo.
echo ## General options
echo.
echo   -e "COMMAND"                 Evaluates the given command (*)
echo   -h, --help                   Prints this message (standalone)
echo   -r "FILE"                    Requires the given files/patterns (*)
echo   -S SCRIPT                    Finds and executes the given script in $PATH
echo   -pr "FILE"                   Requires the given files/patterns in parallel (*)
echo   -pa "PATH"                   Prepends the given path to Erlang code path (*)
echo   -pz "PATH"                   Appends the given path to Erlang code path (*)
echo   -v, --version                Prints Erlang/OTP and Elixir versions (standalone)
echo.
echo   --erl "SWITCHES"             Switches to be passed down to Erlang (*)
echo   --eval "COMMAND"             Evaluates the given command, same as -e (*)
echo   --logger-otp-reports BOOL    Enables or disables OTP reporting
echo   --logger-sasl-reports BOOL   Enables or disables SASL reporting
echo   --no-halt                    Does not halt the Erlang VM after execution
echo   --short-version              Prints Elixir version (standalone)
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
echo --name and --sname may be set to undefined so one is automatically generated.
echo.
echo ## Release options
echo.
echo The following options are generally used under releases.
echo.
echo   --boot "FILE"                Uses the given FILE.boot to start the system
echo   --boot-var VAR "VALUE"       Makes $VAR available as VALUE to FILE.boot (*)
echo   --erl-config "FILE"          Loads configuration in FILE.config written in Erlang (*)
echo   --vm-args "FILE"             Passes the contents in file as arguments to the VM
echo.
echo --pipe-to is not supported on Windows. If set, Elixir won't boot.
echo.
echo ** Options marked with (*) can be given more than once.
echo ** Standalone options can't be combined with other options.
goto end

:shortversion
echo !ELIXIR_VERSION!
goto end

:parseopts

rem Parameters for Elixir
set parsElixir=

rem Parameters for Erlang
set parsErlang=

rem Optional parameters before the "-extra" parameter
set beforeExtra=

rem Option which determines whether the loop is over
set endLoop=0

rem Designates the path to the current script
set SCRIPT_PATH=%~dp0

rem Designates the path to the ERTS system
set ERTS_BIN=
set ERTS_BIN=!ERTS_BIN!

rem Recursive loop called for each parameter that parses the cmd line parameters
:startloop
set "par=%~1"
if "!par!"=="" (
  rem skip if no parameter
  goto run
)
shift
set par="!par:"=\"!"
if !endLoop! == 1 (
  set parsElixir=!parsElixir! !par!
  goto startloop
)
rem ******* EXECUTION OPTIONS **********************
if !par!=="--werl"   (set useWerl=1 && goto startloop)
if !par!=="+iex"     (set parsElixir=!parsElixir! +iex && set useIEx=1 && goto startloop)
if !par!=="+elixirc" (set parsElixir=!parsElixir! +elixirc && goto startloop)
rem ******* EVAL PARAMETERS ************************
if ""==!par:-e=! (
  set "VAR=%~1"
  if not defined VAR (set VAR= )
  set parsElixir=!parsElixir! -e "!VAR:"=\"!"
  shift
  goto startloop
)
if ""==!par:--eval=! (
  set "VAR=%~1"
  if not defined VAR (set VAR= )
  set parsElixir=!parsElixir! --eval "!VAR:"=\"!"
  shift
  goto startloop
)
if ""==!par:--rpc-eval=! (
  set "VAR=%~2"
  if not defined VAR (set VAR= )
  set parsElixir=!parsElixir! --rpc-eval %1 "!VAR:"=\"!"
  shift
  shift
  goto startloop
)
rem ******* ELIXIR PARAMETERS **********************
if ""==!par:-r=!          (set "parsElixir=!parsElixir! -r %~1" && shift && goto startloop)
if ""==!par:-pr=!         (set "parsElixir=!parsElixir! -pr %~1" && shift && goto startloop)
if ""==!par:-pa=!         (set "parsElixir=!parsElixir! -pa %~1" && shift && goto startloop)
if ""==!par:-pz=!         (set "parsElixir=!parsElixir! -pz %~1" && shift && goto startloop)
if ""==!par:-v=!          (set "parsElixir=!parsElixir! -v" && goto startloop)
if ""==!par:--version=!   (set "parsElixir=!parsElixir! --version" && goto startloop)
if ""==!par:--no-halt=!   (set "parsElixir=!parsElixir! --no-halt" && goto startloop)
if ""==!par:--remsh=!     (set "parsElixir=!parsElixir! --remsh %~1" && shift && goto startloop)
if ""==!par:--dot-iex=!   (set "parsElixir=!parsElixir! --dot-iex %~1" && shift && goto startloop)
if ""==!par:--dbg=!       (set "parsElixir=!parsElixir! --dbg %~1" && shift && goto startloop)
rem ******* ERLANG PARAMETERS **********************
if ""==!par:--boot=!                (set "parsErlang=!parsErlang! -boot "%~1"" && shift && goto startloop)
if ""==!par:--boot-var=!            (set "parsErlang=!parsErlang! -boot_var "%~1" "%~2"" && shift && shift && goto startloop)
if ""==!par:--cookie=!              (set "parsErlang=!parsErlang! -setcookie "%~1"" && shift && goto startloop)
if ""==!par:--hidden=!              (set "parsErlang=!parsErlang! -hidden" && goto startloop)
if ""==!par:--erl-config=!          (set "parsErlang=!parsErlang! -config "%~1"" && shift && goto startloop)
if ""==!par:--logger-otp-reports=!  (set "parsErlang=!parsErlang! -logger handle_otp_reports %1" && shift && goto startloop)
if ""==!par:--logger-sasl-reports=! (set "parsErlang=!parsErlang! -logger handle_sasl_reports %1" && shift && goto startloop)
if ""==!par:--name=!                (set "parsErlang=!parsErlang! -name "%~1"" && shift && goto startloop)
if ""==!par:--sname=!               (set "parsErlang=!parsErlang! -sname "%~1"" && shift && goto startloop)
if ""==!par:--vm-args=!             (set "parsErlang=!parsErlang! -args_file "%~1"" && shift && goto startloop)
if ""==!par:--erl=!                 (set "beforeExtra=!beforeExtra! %~1" && shift && goto startloop)
if ""==!par:--pipe-to=!             (echo --pipe-to : Option is not supported on Windows && goto end)
set endLoop=1
set parsElixir=!parsElixir! !par!
goto startloop

:run
reg query HKCU\Console /v VirtualTerminalLevel 2>nul | findstr /e "0x1" >nul 2>nul
if %errorlevel% == 0 (
  set beforeExtra=-elixir ansi_enabled true !beforeExtra!
)
if defined useIEx (
  set beforeExtra=-s elixir start_iex !beforeExtra!
) else (
  set beforeExtra=-s elixir start_cli !beforeExtra!
)

set beforeExtra=-noshell -elixir_root "!SCRIPT_PATH!..\lib" -pa "!SCRIPT_PATH!..\lib\elixir\ebin" !beforeExtra!

if defined ELIXIR_CLI_DRY_RUN (
   if defined useWerl (
     echo start "" "!ERTS_BIN!werl.exe" !ext_libs! !ELIXIR_ERL_OPTIONS! !parsErlang! !beforeExtra! -extra !parsElixir!
   ) else (
     echo "!ERTS_BIN!erl.exe" !ext_libs! !ELIXIR_ERL_OPTIONS! !parsErlang! !beforeExtra! -extra !parsElixir!
   )
) else (
  if defined useWerl (
    start "" "!ERTS_BIN!werl.exe" !ext_libs! !ELIXIR_ERL_OPTIONS! !parsErlang! !beforeExtra! -extra !parsElixir!
  ) else (
    "!ERTS_BIN!erl.exe" !ext_libs! !ELIXIR_ERL_OPTIONS! !parsErlang! !beforeExtra! -extra !parsElixir!
  )
)
exit /B %ERRORLEVEL%
:end
endlocal
