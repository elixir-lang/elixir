-module(iex).
-export([start/0, start/2, shell/0, sync_remote/2]).

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
%%      $ iex -e ":logger.info(~c'foo~nbar', [])"
%%
%% By the time those instructions have been written, all tests above pass.

start() ->
  start([], {elixir_utils, noop, []}).

start(Opts, MFA) ->
  {ok, _} = application:ensure_all_started(elixir),
  {ok, _} = application:ensure_all_started(iex),

  spawn(fun() ->
    case init:notify_when_started(self()) of
      started -> ok;
      _ -> init:wait_until_started()
    end,

    ok = io:setopts([{binary, true}, {encoding, unicode}]),
    'Elixir.IEx.Server':run_from_shell(Opts, MFA)
  end).

shell() ->
  Args = init:get_plain_arguments(),

  case get_remsh(Args) of
    nil ->
      start_mfa(Args, {elixir, start_cli, []});

    Remote ->
      Ref = make_ref(),

      Parent =
        spawn_link(fun() ->
          receive
            {'begin', Ref, Other} ->
              elixir:start_cli(),
              Other ! {done, Ref}
          end
        end),

      {remote, Remote, start_mfa(Args, {?MODULE, sync_remote, [Parent, Ref]})}
  end.

sync_remote(Parent, Ref) ->
  Parent ! {'begin', Ref, self()},
  receive {done, Ref} -> ok end.

start_mfa(Args, MFA) ->
  Opts = [{dot_iex, get_dot_iex(Args)}, {on_eof, halt}],
  {?MODULE, start, [Opts, MFA]}.

get_dot_iex(["--dot-iex", H | _]) -> elixir_utils:characters_to_binary(H);
get_dot_iex([_ | T]) -> get_dot_iex(T);
get_dot_iex([]) -> nil.

get_remsh(["--remsh", H | _]) -> H;
get_remsh([_ | T]) -> get_remsh(T);
get_remsh([]) -> nil.
