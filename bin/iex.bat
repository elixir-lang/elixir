@if defined ELIXIR_CLI_ECHO (@echo on) else  (@echo off)
@if defined IEX_WITH_WERL (@set __ELIXIR_IEX_FLAGS=--werl) else (set __ELIXIR_IEX_FLAGS=)
call "%~dp0\elixir.bat" +iex --erl "-user Elixir.IEx.CLI" --no-halt %__ELIXIR_IEX_FLAGS% %*
@set __ELIXIR_IEX_FLAGS=
