@if defined ELIXIR_CLI_ECHO (@echo on) else (@echo off)
setlocal
if /I ""%1""==""--help"" goto documentation
if /I ""%1""==""-h""     goto documentation
if /I ""%1""==""/h""     goto documentation
if    ""%1""==""/?""     goto documentation
goto run

:documentation
echo Usage: %~nx0 [options] [.exs file] [data]
echo.
echo The following options are exclusive to IEx:
echo.
echo   --dbg pry           Sets the backend for Kernel.dbg/2 to IEx.pry/0
echo   --dot-iex "FILE"    Evaluates FILE, line by line, to set up IEx' environment.
echo                       Defaults to evaluating .iex.exs or ~/.iex.exs, if any exists.
echo                       If FILE is empty, then no file will be loaded.
echo   --remsh NAME        Connects to a node using a remote shell
echo   --werl              Uses Erlang's Windows shell GUI (Windows only)
echo.
echo Set the IEX_WITH_WERL environment variable to always use werl.
echo It accepts all other options listed by "elixir --help".
goto end

:run
if defined IEX_WITH_WERL (set __ELIXIR_IEX_FLAGS=--werl) else (set __ELIXIR_IEX_FLAGS=)
call "%~dp0\elixir.bat" --no-halt --erl "-user elixir" +iex %__ELIXIR_IEX_FLAGS% %*
:end
endlocal
