defmodule Mix.Tasks.Deps.Parallel do
  @moduledoc false
  use Mix.Task

  ## Server

  def server(deps, count, force?) do
    elixir =
      System.find_executable("elixir") ||
        raise "cannot find elixir executable for parallel compilation"

    {:ok, socket} = :gen_tcp.listen(0, [:binary, packet: :line, active: true, reuseaddr: true])
    {:ok, {ip, port}} = :inet.sockname(socket)
    ansi_flag = if IO.ANSI.enabled?(), do: ~c"--color", else: ~c"--no-color"
    force_flag = if force?, do: ~c"--force", else: ~c"--no-force"

    args = [
      ansi_flag,
      ~c"-e",
      ~c"Mix.CLI.main()",
      ~c"deps.parallel",
      force_flag,
      ~c"--port",
      Integer.to_charlist(port),
      ~c"--host",
      :inet.ntoa(ip)
    ]

    options = [
      :binary,
      :hide,
      :use_stdio,
      :stderr_to_stdout,
      line: 1_000_000,
      args: args,
      env: [{~c"MIX_OS_CONCURRENCY_LOCK", ~c"false"}]
    ]

    clients =
      Enum.map(1..count//1, fn index ->
        if Mix.debug?() do
          IO.puts("-> Starting mix deps.parallel ##{index}")
        end

        port = Port.open({:spawn_executable, String.to_charlist(elixir)}, options)

        case :gen_tcp.accept(socket, 15000) do
          {:ok, client} ->
            %{port: port, index: index, socket: client}

          error ->
            raise """
            could not start parallel dependency compiler, no connection made to TCP port: #{inspect(error)}

            The spawned operating system process wrote the following output:
            #{collect_data(port, "")}
            """
        end
      end)

    send_deps_and_server_loop(clients, [], deps, [])
  end

  defp send_deps_and_server_loop(available, busy, deps, completed) do
    {available, busy, deps} = send_deps(available, busy, deps, completed)
    server_loop(available, busy, deps, completed)
  end

  defp send_deps([client | available], busy, deps, completed) do
    case pop_with(deps, fn dep -> Enum.all?(dep.deps, &Keyword.has_key?(completed, &1.app)) end) do
      :error ->
        {[client | available], busy, deps}

      {dep, deps} ->
        if Mix.debug?() do
          Mix.shell().info("-- Sending #{dep.app} to mix deps.parallel #{client.index}")
        end

        :gen_tcp.send(client.socket, "#{dep.app}\n")
        send_deps(available, [client | busy], deps, completed)
    end
  end

  defp send_deps([], busy, deps, _completed) do
    {[], busy, deps}
  end

  defp server_loop(available, _busy = [], _deps = [], completed) do
    shutdown_clients(available)
    Enum.any?(completed, &(elem(&1, 1) == true))
  end

  defp server_loop(available, busy, deps, completed) do
    receive do
      {:tcp, socket, data} ->
        [app, status] = data |> String.trim() |> String.split(":") |> Enum.map(&String.to_atom/1)
        deps = Enum.reject(deps, &(&1.app == app))
        {client, busy} = pop_with(busy, &(&1.socket == socket))

        if Mix.debug?() do
          Mix.shell().info("-- mix deps.parallel #{client.index} compiled #{app}")
        end

        send_deps_and_server_loop([client | available], busy, deps, [{app, status} | completed])

      {:tcp_closed, socket} ->
        shutdown_clients(available ++ busy)
        raise "socket #{inspect(socket)} closed unexpectedly"

      {:tcp_error, socket, error} ->
        shutdown_clients(available ++ busy)
        raise "socket #{inspect(socket)} errored: #{inspect(error)}"

      {port, {:data, {eol, data}}} ->
        with %{index: index} <-
               Enum.find(busy, &(&1.port == port)) || Enum.find(available, &(&1.port == port)) do
          terminator = if eol == :eol, do: "\n", else: ""
          IO.write([Integer.to_string(index), "> ", data, terminator])
        end

        server_loop(available, busy, deps, completed)
    end
  end

  defp pop_with(list, fun) do
    case Enum.split_while(list, &(not fun.(&1))) do
      {_, []} -> :error
      {pre, [result | post]} -> {result, pre ++ post}
    end
  end

  defp shutdown_clients(clients) do
    Enum.each(clients, fn %{socket: socket, port: port, index: index} ->
      if Mix.debug?() do
        IO.puts("-> Closing mix deps.parallel ##{index}")
      end

      _ = :gen_tcp.close(socket)
      IO.write(collect_data(port, "#{index}> "))
    end)
  end

  defp collect_data(port, prefix) do
    receive do
      {^port, {:data, {:eol, data}}} -> [prefix, data, ?\n | collect_data(port, prefix)]
      {^port, {:data, {:noeol, data}}} -> [data | collect_data(port, prefix)]
    after
      0 -> []
    end
  end

  ## Client

  @switches [port: :integer, host: :string, force: :boolean]

  @impl true
  def run(args) do
    # If stdin closes, we shutdown the VM
    spawn(fn ->
      _ = IO.gets("")
      System.halt(0)
    end)

    {opts, []} = OptionParser.parse!(args, strict: @switches)
    host = Keyword.fetch!(opts, :host)
    port = Keyword.fetch!(opts, :port)
    force? = Keyword.get(opts, :force, false)

    {:ok, socket} =
      :gen_tcp.connect(String.to_charlist(host), port, [:binary, packet: :line, active: false])

    deps = Mix.Dep.load_and_cache()
    client_loop(socket, deps, force?, Mix.Project.deps_config())
  end

  def client_loop(socket, deps, force?, config) do
    case :gen_tcp.recv(socket, 0, :infinity) do
      {:ok, app} ->
        app = app |> String.trim() |> String.to_atom()

        dep =
          Enum.find(deps, &(&1.app == app)) || raise "could not find dependency #{inspect(app)}"

        compiled? = Mix.Tasks.Deps.Compile.compile_single(dep, force?, config)
        :ok = :gen_tcp.send(socket, "#{app}:#{compiled?}\n")
        client_loop(socket, deps, force?, config)

      {:error, :closed} ->
        :ok
    end
  end
end
