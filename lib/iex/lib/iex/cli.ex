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
#      $ iex -e ':error_logger.info_msg("foo~nbar", [])'
#
# By the time those instructions have been written, all tests above pass.
defmodule IEx.CLI do
  @moduledoc false

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
    if tty_works?() do
      :user_drv.start([:"tty_sl -c -e", tty_args()])
    else
      if get_remsh(:init.get_plain_arguments()) do
        IO.puts(
          :stderr,
          "warning: Connecting to a remote node via --remsh is not possible using the 'dumb' terminal"
        )
      end

      :application.set_env(:stdlib, :shell_prompt_func, {__MODULE__, :prompt})
      :user.start()
      local_start()
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
      port = Port.open({:spawn, 'tty_sl -c -e'}, [:eof])
      Port.close(port)
    catch
      _, _ -> false
    end
  end

  defp tty_args do
    if remote = get_remsh(:init.get_plain_arguments()) do
      if Node.alive?() do
        case :rpc.call(remote, :code, :ensure_loaded, [IEx]) do
          {:badrpc, reason} ->
            suggestion =
              if Atom.to_string(remote) =~ "@" do
                ""
              else
                "Make sure the node given to --remsh is in the node@host format. "
              end

            message =
              "Could not contact remote node #{remote}, reason: #{inspect(reason)}. " <>
                suggestion <> "Aborting..."

            abort(message)

          {:module, IEx} ->
            {mod, fun, args} = remote_start_mfa()
            {remote, mod, fun, args}

          _ ->
            abort("Could not find IEx on remote node #{remote}. Aborting...")
        end
      else
        abort(
          "In order to use --remsh, you need to name the current node using --name or --sname. Aborting..."
        )
      end
    else
      {:erlang, :apply, [local_start_function(), []]}
    end
  end

  def local_start do
    IEx.start(options(), {:elixir, :start_cli, []})
  end

  def remote_start(parent, ref) do
    send(parent, {:begin, ref, self()})
    receive do: ({:done, ^ref} -> :ok)
  end

  defp local_start_function do
    &local_start/0
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

  defp find_dot_iex(['--dot-iex', h | _]), do: List.to_string(h)
  defp find_dot_iex([_ | t]), do: find_dot_iex(t)
  defp find_dot_iex([]), do: nil

  defp get_remsh(['--remsh', h | _]), do: List.to_atom(h)
  defp get_remsh([_ | t]), do: get_remsh(t)
  defp get_remsh([]), do: nil
end
