@if defined ELIXIR_CLI_ECHO (@echo on) else  (@echo off)
set argc=0
for %%A in (%*) do (
    if "%%A"=="--help" goto documentation
    if "%%A"=="-h"     goto documentation
    if "%%A"=="/h"     goto documentation
    set /A argc+=1
)
if %argc%==0 goto documentation
goto run

:documentation
echo Usage: %~nx0 [elixir switches] [compiler switches] [.ex files]
echo.
echo  -o               The directory to output compiled files
echo  --no-docs        Do not attach documentation to compiled modules
echo  --no-debug-info  Do not attach debug info to compiled modules
echo  --ignore-module-conflict
echo  --warnings-as-errors Treat warnings as errors and return non-zero exit code
echo  --verbose        Print informational messages.
echo.
echo ** Options given after -- are passed down to the executed code
echo ** Options can be passed to the Erlang runtime using ELIXIR_ERL_OPTIONS
echo ** Options can be passed to the Erlang compiler using ERL_COMPILER_OPTIONS >&2
:run
call "%~dp0\elixir.bat" +elixirc %*
