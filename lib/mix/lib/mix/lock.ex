defmodule Mix.Lock do
  @moduledoc false

  # Lock implementation working across multiple OS processes.
  #
  # The lock is implemented using TCP sockets and hard links.
  #
  # A process holds the lock if it owns a TCP socket, whose port is
  # written in the lock_0 file. We need to create such lock files
  # atomically, so the process first writes its port to a port_P
  # file and then attempts to create a hard link to it at lock_0.
  #
  # An inherent problem with lock files is that the lock owner may
  # terminate abruptly, leaving a "stale" file. Other processes can
  # detect a stale file by reading the port written in that file,
  # trying to connect to that port and failing. In order for another
  # process to link to the same path, the file needs to be replaced.
  # However, we need to guarantee that only a single process can
  # remove or replace the file, otherwise a concurrent process may
  # end up removing a newly linked file.
  #
  # To address this problem we employ a chained locking procedure.
  # Specifically, we attempt to link our port to lock_0, if that
  # fails, we try to connect to the lock_0 port. If we manage to
  # connect, it means the lock is taken, so we wait for it to close
  # and start over. If we fail to connect, it means the lock is stale,
  # so we want to replace it. In order to do that, we try to obtain
  # lock_1. Again, we try to link and connect. Eventually, we should
  # successfully link to lock_N. At that point we can clean up all
  # the files, so we perform these steps:
  #
  #   * move our port_P to lock_0
  #   * remove all the other port_P files
  #   * remove all lock_1+ files
  #
  # It is important to perform these steps in this order, to avoid
  # race conditions. By moving to lock_0, we make sure that all new
  # processes trying to lock will connect to our port. By removing
  # all port_P files we make sure that currently paused processes
  # that are about to link port_P at lock_N will fail to link, since
  # the port_P file will no longer exist (once lock_N is removed).
  #
  # Finally, note that we do not remove the lock file in `unlock/1`.
  # If we did that, another process could try to connect and fail
  # because the file would not exist, in such case the process would
  # assume the file is stale and needs to be replaced, therefore
  # possibly replacing another process who successfully links at the
  # empty spot. This means we effectively always leave a stale file,
  # however, in order to shortcut the port check for future processes,
  # we atomically replace the file content with port 0, to indicate
  # the file is stale.
  #
  # The main caveat of using ephemeral TCP ports is that they are not
  # unique. This creates a theoretical scenario where the lock holder
  # terminates abruptly and leaves its port in lock_0, then the port
  # is assigned to a unrelated process (unaware of the locking). To
  # handle this scenario, when we connect to a lock_N port, we expect
  # it to immediately send us `@probe_data`. If this does not happen
  # within `@probe_timeout_ms`, we assume the port is taken by an
  # unrelated process and the lock file is stale. Note that it is ok
  # to use a long timeout, because this scenario is very unlikely.
  # Theoretically, if an actual lock owner is not able to send the
  # probe data within the timeout, the lock will fail, however with
  # a high enough timeout, this should not be a problem in practice.

  @loopback {127, 0, 0, 1}
  @listen_opts [:binary, ip: @loopback, packet: :raw, nodelay: true, backlog: 128, active: false]
  @connect_opts [:binary, packet: :raw, nodelay: true, active: false]
  @probe_data "elixirlock"
  @probe_timeout_ms 5_000

  @doc """
  Acquires a lock identified by the given key.

  This function blocks until the lock is acquired by this process,
  and then executes `fun`, returning its return value.

  This function can also be called if this process already has the
  lock. In such case the function is executed immediately.
  """
  @spec with_lock(iodata(), (-> term())) :: :ok
  def with_lock(key, fun) do
    key = key |> :erlang.md5() |> Base.url_encode64(padding: false)
    path = Path.join([System.tmp_dir!(), "mix_lock", key])

    pdict_key = {__MODULE__, path}
    has_lock? = Process.get(pdict_key)

    if has_lock? do
      fun.()
    else
      lock = lock(path)
      Process.put(pdict_key, true)

      try do
        fun.()
      after
        # Unlocking will always close the socket, but it may raise,
        # so we remove key from the dictionary first
        Process.delete(pdict_key)
        unlock(lock)
      end
    end
  end

  defp lock(path) do
    File.mkdir_p!(path)

    case listen() do
      {:ok, socket, port} ->
        spawn_link(fn -> accept_loop(socket) end)

        try do
          try_lock(path, socket, port)
        rescue
          exception ->
            # Close the socket to make sure we don't block the lock
            :gen_tcp.close(socket)
            reraise exception, __STACKTRACE__
        end

      {:error, reason} ->
        raise Mix.Error,
              "failed to open a TCP socket while acquiring a lock, reason: #{inspect(reason)}"
    end
  end

  defp listen() do
    with {:ok, socket} <- :gen_tcp.listen(0, @listen_opts) do
      case :inet.port(socket) do
        {:ok, port} ->
          {:ok, socket, port}

        {:error, reason} ->
          :socket.close(socket)
          {:error, reason}
      end
    end
  end

  defp try_lock(path, socket, port) do
    port_path = Path.join(path, "port_#{port}")

    File.write!(port_path, <<port::unsigned-integer-32>>, [:raw])

    case grab_lock(path, port_path, 0) do
      {:ok, 0} ->
        # We grabbed lock_0, so all good
        %{socket: socket, path: path}

      {:ok, _n} ->
        # We grabbed lock_1+, so we need to replace lock_0 and clean
        # up. This must happen in a precise order, so if anything
        # fails, we keep the files as is and the next process that
        # grabs the lock will do the cleanup
        take_over(path, port_path)
        %{socket: socket, path: path}

      {:taken, probe_socket} ->
        # Another process has the lock, wait for close and start over
        await_close(probe_socket)
        try_lock(path, socket, port)

      :invalidated ->
        try_lock(path, socket, port)
    end
  end

  defp grab_lock(path, port_path, n) do
    lock_path = Path.join(path, "lock_#{n}")

    case File.ln(port_path, lock_path) do
      :ok ->
        {:ok, n}

      {:error, :eexist} ->
        case probe(lock_path) do
          {:ok, probe_socket} ->
            {:taken, probe_socket}

          {:error, _reason} ->
            grab_lock(path, port_path, n + 1)
        end

      {:error, :enoent} ->
        :invalidated

      {:error, reason} ->
        raise File.LinkError,
          reason: reason,
          action: "create hard link",
          existing: port_path,
          new: lock_path
    end
  end

  defp accept_loop(listen_socket) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, socket} ->
        _ = :gen_tcp.send(socket, @probe_data)
        accept_loop(listen_socket)

      {:error, :eintr} ->
        accept_loop(listen_socket)

      {:error, reason} when reason in [:closed, :einval] ->
        :ok
    end
  end

  defp probe(port_path) do
    with {:ok, port} <- fetch_probe_port(port_path),
         {:ok, socket} <- connect(port) do
      await_probe_data(socket)
    end
  end

  defp fetch_probe_port(port_path) do
    case File.read(port_path) do
      {:ok, <<0::unsigned-integer-32>>} -> {:error, :ignore}
      {:ok, <<port::unsigned-integer-32>>} -> {:ok, port}
      {:error, reason} -> {:error, reason}
    end
  end

  defp connect(port) do
    # On Windows connecting to an unbound port takes a few seconds to
    # fail, so instead we shortcut the check by attempting a listen,
    # which succeeds or fails immediately
    case :gen_tcp.listen(port, [reuseaddr: true] ++ @listen_opts) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        # The port is free, so connecting would fail
        {:error, :econnrefused}

      {:error, _reason} ->
        :gen_tcp.connect(@loopback, port, @connect_opts)
    end
  end

  defp await_probe_data(socket) do
    case :gen_tcp.recv(socket, 0, @probe_timeout_ms) do
      {:ok, @probe_data} ->
        {:ok, socket}

      {:ok, _data} ->
        :gen_tcp.close(socket)
        {:error, :unexpected_port_owner}

      {:error, :eintr} ->
        await_probe_data(socket)

      {:error, reason} ->
        :gen_tcp.close(socket)
        {:error, reason}
    end
  end

  defp take_over(path, port_path) do
    lock_path = Path.join(path, "lock_0")

    # We linked to lock_N successfully, so port_path should exist
    File.rename!(port_path, lock_path)

    names = File.ls!(path)

    for "port_" <> _ = name <- names do
      File.rm!(Path.join(path, name))
    end

    for "lock_" <> _ = name <- names, name != "lock_0" do
      File.rm!(Path.join(path, name))
    end
  end

  defp await_close(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:error, :closed} ->
        :ok

      {:error, :eintr} ->
        await_close(socket)

      {:error, _other} ->
        # In case of an unexpected error, we close the socket ourselves
        # to retry
        :gen_tcp.close(socket)
    end
  end

  defp unlock(lock) do
    port_path = Path.join(lock.path, "port_0")
    lock_path = Path.join(lock.path, "lock_0")

    File.write!(port_path, <<0::unsigned-integer-32>>, [:raw])
    File.rename!(port_path, lock_path)
  after
    # Closing the socket will cause the accepting process to finish
    # and all accepted sockets (tied to that process) will get closed
    :gen_tcp.close(lock.socket)
  end
end
