# TODO: Remove this whole module on Erlang/OTP 26+.
defmodule IEx.CLI do
  @moduledoc false

  @compile {:no_warn_undefined, {:user, :start, 0}}

  def deprecated do
    if tty_works?() do
      :user_drv.start([:"tty_sl -c -e", tty_args()])
    else
      if get_remsh(:init.get_plain_arguments()) do
        IO.puts(
          :stderr,
          "warning: the --remsh option will be ignored because IEx is running on limited shell"
        )
      end

      :user.start()

      spawn(fn ->
        :application.ensure_all_started(:iex)

        case :init.notify_when_started(self()) do
          :started -> :ok
          _ -> :init.wait_until_started()
        end

        :ok = :io.setopts(binary: true, encoding: :unicode)
        IEx.Server.run_from_shell([register: true] ++ options(), {:elixir, :start_cli, []})
      end)
    end
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

  defp tty_args do
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

  defp local_start_mfa do
    {:iex, :start, [options(), {:elixir, :start_cli, []}]}
  end

  def remote_start(parent, ref) do
    send(parent, {:begin, ref, self()})
    receive do: ({:done, ^ref} -> :ok)
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

    {:iex, :start, [opts, {__MODULE__, :remote_start, [parent, ref]}]}
  end

  defp options do
    [dot_iex: find_dot_iex(:init.get_plain_arguments()), on_eof: :halt]
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
