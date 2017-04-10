@if defined ELIXIR_CLI_ECHO (@echo on) else (@echo off)
setlocal
set argc=0
for %%A in (%*) do (
  if /I "%%A"=="--help" goto documentation
  if /I "%%A"=="-h"     goto documentation
  if /I "%%A"=="/h"     goto documentation
  if    "%%A"=="/?"     goto documentation
  set /A argc+=1
)
if %argc%==0 goto documentation
goto run

:documentation
echo Usage: %~nx0 [elixir switches] [compiler switches] [.ex files]
echo.
echo   -o                          The directory to output compiled files
echo   --no-docs                   Does not attach documentation to compiled modules
echo   --no-debug-info             Does not attach debug info to compiled modules
echo   --verbose                   Prints compilation status
echo   --warnings-as-errors        Treats warnings as errors and returns non-zero exit code
echo   --ignore-module-conflict    Does not emit warnings if a module was previously defined
echo.
echo ** Options given after -- are passed down to the executed code
echo ** Options can be passed to the Erlang runtime using ELIXIR_ERL_OPTIONS
echo ** Options can be passed to the Erlang compiler using ERL_COMPILER_OPTIONS
goto end

:run
call "%~dp0\elixir.bat" +elixirc %*

:end
endlocal
