-module(iex).
-export([cli/0]).

%% Manual tests for changing the CLI boot.
%%
%% 1. In some situations, we cannot read inputs as IEx boots:
%%
%%      $ iex -e ":io.get_line(:foo)"
%%
%% 2. In some situations, connecting to a remote node via --remsh
%%    is not possible. This can be tested by starting two IEx nodes:
%%
%%      $ iex --sname foo
%%      $ iex --sname bar --remsh foo
%%
%% 3. When still using --remsh, we need to guarantee the arguments
%%    are processed on the local node and not the remote one. For such,
%%    one can replace the last line above by:
%%
%%      $ iex --sname bar --remsh foo -e 'IO.inspect node()'
%%
%%    And verify that the local node name is printed.
%%
%% 4. Finally, in some other circumstances, printing messages may become
%%    borked. This can be verified with:
%%
%%      $ iex -e ":logger.info('foo~nbar', [])"
%%
%% By the time those instructions have been written, all tests above pass.
cli() ->
  spawn(fun() ->
    elixir_config:wait_until_booted(),
    (shell:whereis() =:= undefined) andalso start_shell()
  end).

start_shell() ->
  Args = init:get_plain_arguments(),
  Opts = [{remote, get_remsh(Args)}, {dot_iex_path, get_dot_iex(Args)}, {on_eof, halt}],

  case 'Elixir.IEx':shell(Opts) of
    {ok, _Shell} ->
      ok;

    {error, Reason} ->
      io:format(standard_error, "Could not start IEx CLI due to reason: ~tp", [Reason]),
      erlang:halt(1)
  end.

get_dot_iex(["--dot-iex", H | _]) -> elixir_utils:characters_to_binary(H);
get_dot_iex([_ | T]) -> get_dot_iex(T);
get_dot_iex([]) -> nil.

get_remsh(["--remsh", H | _]) -> H;
get_remsh([_ | T]) -> get_remsh(T);
get_remsh([]) -> nil.
