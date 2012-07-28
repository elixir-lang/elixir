@echo off
if "%*" == "" (
	goto documentation
) else (
	goto run 
) 
:documentation
echo Usage: %~nx0 [options] [.exs file] [data]
echo.
echo  -v              Prints version and exit
echo  -e command    Evaluates the given command (*)
echo  -r command    Requires the given file/pattern (*)
echo  -pr command   Requires the given file/pattern in parallel (*)
echo  -pa path     Prepend the given path to Erlang code path (*)
echo  -pz path      Append the given path to Erlang code path (*)
echo  --no-halt       Do not halt the Erlang VM after execution
echo.
echo ** Options marked with (*) can be given more than once;
echo.
echo ** Options given after the .exs file or -- are passed down to the executed code;
echo.
echo ** Options can be passed to the erlang runtime using ELIXIR_ERL_OPTS.
:run
erl -env ERL_LIBS %ERL_LIBS%;"%~dp0\..\lib" -noshell %ELIXIR_ERL_OPTS% -s elixir start_cli -extra %*