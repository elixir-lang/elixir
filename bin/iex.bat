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
echo   -e "COMMAND"                Evaluates the given command (*)
echo   -r "FILE"                   Requires the given files/patterns (*)
echo   -S SCRIPT                   Finds and executes the given script in PATH
echo   -pr "FILE"                  Requires the given files/patterns in parallel (*)
echo   -pa "PATH"                  Prepends the given path to Erlang code path (*)
echo   -pz "PATH"                  Appends the given path to Erlang code path (*)
echo.
echo   --app APP                   Starts the given app and its dependencies (*)
echo   --cookie COOKIE             Sets a cookie for this distributed node
echo   --detached                  Starts the Erlang VM detached from console
echo   --erl "SWITCHES"            Switches to be passed down to Erlang (*)
echo   --help, -h                  Prints this message and exits
echo   --hidden                    Makes a hidden node
echo   --logger-otp-reports BOOL   Enables or disables OTP reporting
echo   --logger-sasl-reports BOOL  Enables or disables SASL reporting
echo   --name NAME                 Makes and assigns a name to the distributed node
echo   --no-halt                   Does not halt the Erlang VM after execution
echo   --sname NAME                Makes and assigns a short name to the distributed node
echo   --version, -v               Prints IEx version and exits
echo   --werl                      Uses Erlang's Windows shell GUI (Windows only)
echo.
echo   --dot-iex PATH              Overrides default .iex.exs file and uses path instead;
echo                               path can be empty, then no file will be loaded
echo   --remsh NAME                Connects to a node using a remote shell
echo.
echo ** Options marked with (*) can be given more than once
echo ** Options given after the .exs file or -- are passed down to the executed code
echo ** Options can be passed to the Erlang VM using ELIXIR_ERL_OPTIONS or --erl
goto end

:run
@if defined IEX_WITH_WERL (@set __ELIXIR_IEX_FLAGS=--werl) else (set __ELIXIR_IEX_FLAGS=)
call "%~dp0\elixir.bat" --no-halt --erl "-noshell -user Elixir.IEx.CLI" +iex %__ELIXIR_IEX_FLAGS% %*
:end
endlocal
