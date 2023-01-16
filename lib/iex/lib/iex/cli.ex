# IEx sets the Erlang user to be IEx.CLI via the command line.
# While this works most of the time, there are some problems
# that may happen depending on how the booting process goes.
# Those problems need to be manually tested in case changes are
# done to this file.
#
# 1. In some situations, printing something before the shell
#    starts becomes very slow. To verify this feature, just
#    get an existing project and run:
#
#      $ mix clean
#      $ iex -S mix
#
#    If the printing of data is slower than usual. This particular
#    bug has arisen;
#
# 2. In some situations, connecting to a remote node via --remsh
#    is not possible. This can be tested by starting two IEx nodes:
#
#      $ iex --sname foo
#      $ iex --sname bar --remsh foo@localhost
#
# 3. When still using --remsh, we need to guarantee the arguments
#    are processed on the local node and not the remote one. For such,
#    one can replace the last line above by:
#
#      $ iex --sname bar --remsh foo@localhost -e 'IO.inspect node()'
#
#    And verify that the local node name is printed.
#
# 4. Finally, in some other circumstances, printing messages may become
#    borked. This can be verified with:
#
#      $ iex -e ":logger.info('foo~nbar', [])"
#
# By the time those instructions have been written, all tests above pass.
defmodule IEx.CLI do
  @moduledoc false

  @compile {:no_warn_undefined, {:user, :start, 0}}

  @doc """
  In order to work properly, IEx needs to be set as the
  proper `-user` when starting the Erlang VM and we do so
  by pointing exactly to this function.

  If possible, Elixir will start a tty (smart terminal)
  which makes all control commands available in tty
  available to the developer.

  In case `tty` is not available (for example, Windows),
  a dumb terminal version is started instead.
  """
  def start do
    # TODO: We can remove the -user callback on Erlang/OTP 26+
    # in favor of using -eval and :shell.start_interactive()
    cond do
      Code.ensure_loaded?(:prim_tty) ->
        :user_drv.start(%{initial_shell: new_tty_args()})

      tty_works?() ->
        :user_drv.start([:"tty_sl -c -e", old_tty_args()])

      true ->
        if get_remsh(:init.get_plain_arguments()) do
          IO.puts(
            :stderr,
            "warning: the --remsh option will be ignored because IEx is running on limited shell"
          )
        end

        :user.start()

        # IEx.Broker is capable of considering all groups under user_drv but
        # when we use :user.start(), we need to explicitly register it instead.
        # If we don't register, pry doesn't work.
        IEx.start([register: true] ++ options(), {:elixir, :start_cli, []})
    end
  end

  def prompt(_n) do
    []
  end

  # Check if tty works. If it does not, we fall back to the
  # simple/dumb terminal. This is starting the linked in
  # driver twice, it would be nice and appropriate if we had
  # to do it just once.
  defp tty_works? do
    try do
      port = Port.open({:spawn, ~c"tty_sl -c -e"}, [:eof])
      Port.close(port)
    catch
      _, _ -> false
    end
  end

  defp new_tty_args do
    if remote = get_remsh(:init.get_plain_arguments()) do
      {:remote, remote, remote_start_mfa()}
    else
      local_start_mfa()
    end
  end

  defp old_tty_args do
    if remote = get_remsh(:init.get_plain_arguments()) do
      remote = List.to_atom(append_hostname(remote))

      # Explicitly connect the node in case the rpc node was started with --sname/--name undefined.
      _ = :net_kernel.connect_node(remote)

      case :rpc.call(remote, :code, :ensure_loaded, [IEx]) do
        {:badrpc, reason} ->
          message =
            if reason == :nodedown and :net_kernel.nodename() == :ignored do
              "In order to use --remsh, you need to name the current node using --name or --sname. Aborting..."
            else
              "Could not contact remote node #{remote}, reason: #{inspect(reason)}. Aborting..."
            end

          abort(message)

        {:module, IEx} ->
          case :rpc.call(remote, :net_kernel, :get_net_ticktime, []) do
            seconds when is_integer(seconds) -> :net_kernel.set_net_ticktime(seconds)
            _ -> :ok
          end

          {mod, fun, args} = remote_start_mfa()
          {remote, mod, fun, args}

        _ ->
          abort("Could not find IEx on remote node #{remote}. Aborting...")
      end
    else
      local_start_mfa()
    end
  end

  def remote_start(parent, ref) do
    send(parent, {:begin, ref, self()})
    receive do: ({:done, ^ref} -> :ok)
  end

  defp local_start_mfa do
    {IEx, :start, [options(), {:elixir, :start_cli, []}]}
  end

  defp remote_start_mfa do
    ref = make_ref()
    opts = options()

    parent =
      spawn_link(fn ->
        receive do
          {:begin, ^ref, other} ->
            :elixir.start_cli()
            send(other, {:done, ref})
        end
      end)

    {IEx, :start, [opts, {__MODULE__, :remote_start, [parent, ref]}]}
  end

  defp options do
    [dot_iex_path: find_dot_iex(:init.get_plain_arguments()), on_eof: :halt]
  end

  defp abort(msg) do
    function = fn ->
      IO.puts(:stderr, msg)
      System.halt(1)
    end

    {:erlang, :apply, [function, []]}
  end

  defp find_dot_iex([~c"--dot-iex", h | _]), do: List.to_string(h)
  defp find_dot_iex([_ | t]), do: find_dot_iex(t)
  defp find_dot_iex([]), do: nil

  defp get_remsh([~c"--remsh", h | _]), do: h
  defp get_remsh([_ | t]), do: get_remsh(t)
  defp get_remsh([]), do: nil

  defp append_hostname(node) do
    with :nomatch <- :string.find(node, "@"),
         [_ | _] = suffix <- :string.find(Atom.to_charlist(:net_kernel.nodename()), "@") do
      node ++ suffix
    else
      _ -> node
    end
  end
end
