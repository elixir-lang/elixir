defmodule Mix.Sync.PubSub do
  @moduledoc false

  # Pub/sub implementation working across multiple OS processes.
  #
  # The pub/sub is implemented using TCP sockets.
  #
  # Every subscriber opens a TCP socket and creates a port_P file.
  # A publisher lists the directory, connects to each port and sends
  # the message payload. When subscriber processes terminate, the
  # files are left behind, so whenever a publisher fails to connect
  # to one of the ports, it removes the corresponding file.
  #
  # We use a single socket for all the subscriptions. We create the
  # socket lazily on the first subscription. Since the socket is
  # shared, we include the hashed key in the broadcast payload, so
  # that we can determine which subscribers to notify.

  use GenServer

  @opaque subscription :: %{socket: :gen_tcp.socket()}

  @loopback {127, 0, 0, 1}
  @listen_opts [:binary, ip: @loopback, packet: :raw, nodelay: true, backlog: 128, active: false]
  @connect_opts [:binary, packet: :raw, nodelay: true, active: false]

  @tag_data "mixpubsub"
  @tag_data_size byte_size(@tag_data)

  @name __MODULE__

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {}, name: @name)
  end

  @doc """
  Subscribes the caller to messages for `key`.

  The messages are delivered to the caller's message queue.
  """
  @spec subscribe(String.t()) :: :ok
  def subscribe(key) do
    case GenServer.call(@name, {:subscribe, self(), key}, :infinity) do
      :ok -> :ok
      {:error, message} -> Mix.raise(message)
    end
  end

  @doc """
  Sends `message` to all processes subscribed to `key`.
  """
  @spec broadcast(String.t(), term() | (-> term())) :: :ok
  def broadcast(key, message) do
    hash = hash(key)
    path = path(hash)

    case File.ls(path) do
      {:ok, []} ->
        :ok

      {:ok, names} ->
        message =
          case message do
            lazy_message when is_function(lazy_message, 0) -> lazy_message.()
            message -> message
          end

        binary = :erlang.term_to_binary(message)

        for "port_" <> port = name <- names do
          port = String.to_integer(port)
          port_path = Path.join(path, name)
          _ = send_message(port_path, port, hash, binary)
        end

        :ok

      {:error, :enoent} ->
        :ok

      {:error, reason} ->
        raise File.Error, reason: reason, action: "list directory", path: path
    end
  end

  defp send_message(port_path, port, hash, binary) do
    # On Windows connecting to an unbound port takes a few seconds to
    # fail, so instead we shortcut the check by attempting a listen,
    # which succeeds or fails immediately. Note that `reuseaddr` here
    # ensures that if the listening socket closed recently, we can
    # immediately reclaim the same port.
    #
    # Also, if we manage to bind, it means the port is free and the
    # subscription is no longer active, so we remove the file. It is
    # important we remove the file while holding onto the port, so
    # it is not claimed by a new subscriber as we remove the file.
    #
    # Finally, if we fail to connect, we could attempt to bind again
    # and remove the file, but we just leave it up to the next send
    # attempt.

    case :gen_tcp.listen(port, [reuseaddr: true] ++ @listen_opts) do
      {:ok, socket} ->
        _ = File.rm(port_path)
        :gen_tcp.close(socket)
        {:error, :econnrefused}

      {:error, _reason} ->
        with {:ok, socket} <- :gen_tcp.connect(@loopback, port, @connect_opts) do
          size = byte_size(binary)

          result =
            :gen_tcp.send(
              socket,
              <<@tag_data, hash::binary, size::unsigned-integer-64, binary::binary>>
            )

          :gen_tcp.shutdown(socket, :read_write)
          :gen_tcp.close(socket)

          result
        end
    end
  end

  @impl true
  def init({}) do
    state = %{port: nil, hash_to_pids: %{}}
    {:ok, state}
  end

  @impl true
  def handle_call({:subscribe, pid, key}, _from, state) do
    Process.monitor(pid)

    case ensure_socket(state) do
      {:ok, state} ->
        hash = hash(key)
        path = path(hash)

        create_subscription_file(path, state.port)

        state =
          update_in(state.hash_to_pids, fn
            %{^hash => pids} = hash_to_pids ->
              %{hash_to_pids | hash => MapSet.put(pids, pid)}

            %{} = hash_to_pids ->
              Map.put(hash_to_pids, hash, MapSet.new([pid]))
          end)

        {:reply, :ok, state}

      {:error, error} ->
        {:reply, {:error, error}, state}
    end
  end

  @impl true
  def handle_info({:message, hash, message}, state) do
    if pids = state.hash_to_pids[hash] do
      for pid <- pids do
        send(pid, message)
      end
    end

    {:noreply, state}
  end

  def handle_info({:DOWN, _, :process, pid, _}, state) do
    hash_to_pids =
      for {hash, pids} <- state.hash_to_pids,
          pids = remove_pid(pids, pid, hash, state.port),
          into: %{},
          do: {hash, pids}

    {:noreply, %{state | hash_to_pids: hash_to_pids}}
  end

  defp remove_pid(pids, pid, hash, port) do
    pids = MapSet.delete(pids, pid)

    if Enum.empty?(pids) do
      path = path(hash)
      remove_subscription_file(path, port)
      nil
    else
      pids
    end
  end

  defp ensure_socket(%{port: nil} = state) do
    case listen() do
      {:ok, socket, port} ->
        parent = self()
        spawn_link(fn -> receive_message_loop(socket, parent) end)
        {:ok, %{state | port: port}}

      {:error, reason} ->
        {:error,
         "failed to open a TCP socket in #{inspect(__MODULE__)}.subscribe/1, reason: #{inspect(reason)}"}
    end
  end

  defp ensure_socket(state), do: {:ok, state}

  defp listen do
    with {:ok, socket} <- :gen_tcp.listen(0, @listen_opts) do
      case :inet.port(socket) do
        {:ok, port} ->
          {:ok, socket, port}

        {:error, reason} ->
          :gen_tcp.close(socket)
          {:error, reason}
      end
    end
  end

  defp create_subscription_file(path, port) do
    File.mkdir_p!(path)
    port_path = Path.join(path, "port_#{port}")
    File.touch!(port_path)
  end

  defp remove_subscription_file(path, port) do
    port_path = Path.join(path, "port_#{port}")
    _ = File.rm(port_path)
  end

  defp receive_message_loop(socket, parent) do
    {hash, message} = receive_message(socket)
    send(parent, {:message, hash, message})
    receive_message_loop(socket, parent)
  end

  defp receive_message(listen_socket) do
    # Note that receive failures may happen if the sender terminates
    # abruptly. We ignore such attempts and wait for the next message.
    #
    # Also, the first recv has a timeout in case an unrelated process
    # connects to the port and doesn't send any data.

    case accept(listen_socket) do
      {:ok, socket} ->
        with {:ok, <<@tag_data, hash::binary-size(16), size::unsigned-integer-64>>} <-
               recv(socket, @tag_data_size + 16 + 8, 1_000),
             {:ok, binary} <- recv(socket, size),
             {:ok, data} <- decode_data(binary) do
          :gen_tcp.close(socket)
          {hash, data}
        else
          _other ->
            :gen_tcp.close(socket)
            receive_message(listen_socket)
        end

      {:error, reason} ->
        raise RuntimeError,
              "failed to accept connection in #{inspect(__MODULE__)}.receive_message/1, reason: #{inspect(reason)}"
    end
  end

  defp decode_data(binary) do
    try do
      {:ok, :erlang.binary_to_term(binary)}
    rescue
      _error -> :error
    end
  end

  defp hash(key), do: :erlang.md5(key)

  defp path(hash) do
    hash = Base.url_encode64(hash, padding: false)
    Path.join([System.tmp_dir!(), "mix_pubsub", hash])
  end

  defp recv(socket, size, timeout \\ :infinity) do
    # eintr is "Interrupted system call".
    with {:error, :eintr} <- :gen_tcp.recv(socket, size, timeout) do
      recv(socket, size)
    end
  end

  defp accept(socket) do
    with {:error, :eintr} <- :gen_tcp.accept(socket) do
      accept(socket)
    end
  end
end
