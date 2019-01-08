@if defined ELIXIR_CLI_ECHO (@echo on) else  (@echo off)
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
echo   --dot-iex "PATH"    Overrides default .iex.exs file and uses path instead;
echo                       path can be empty, then no file will be loaded
echo   --remsh NAME        Connects to a node using a remote shell
echo.
echo The remaining options are the same as in the "elixir" executable. Run "elixir --help" to see them
goto end

:run
@if defined IEX_WITH_WERL (@set __ELIXIR_IEX_FLAGS=--werl) else (set __ELIXIR_IEX_FLAGS=)
call "%~dp0\elixir.bat" --no-halt --erl "-noshell -user Elixir.IEx.CLI" +iex %__ELIXIR_IEX_FLAGS% %*
:end
endlocal
