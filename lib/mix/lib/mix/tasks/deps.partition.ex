# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Mix.Tasks.Deps.Partition do
  @moduledoc false
  use Mix.Task

  ## Server

  def server(deps, count, force?) do
    {:ok, socket} = :gen_tcp.listen(0, [:binary, packet: :line, active: false])

    try do
      server(socket, deps, count, force?)
    after
      :gen_tcp.close(socket)
    end
  end

  defp server(socket, deps, count, force?) do
    elixir =
      System.get_env("MIX_OS_DEPS_COMPILE_PARTITION_ELIXIR_EXECUTABLE") ||
        System.find_executable("elixir") ||
        raise "cannot find elixir executable for partition compilation"

    {:ok, {_ip, port}} = :inet.sockname(socket)
    ansi_flag = if IO.ANSI.enabled?(), do: ~c"--color", else: ~c"--no-color"
    force_flag = if force?, do: ~c"--force", else: ~c"--no-force"

    args = [
      ansi_flag,
      ~c"-e",
      ~c"Mix.Tasks.Deps.Partition.client",
      ~c"--",
      force_flag,
      ~c"--port",
      Integer.to_charlist(port),
      ~c"--host",
      ~c"127.0.0.1",
      ~c"--config",
      Mix.ProjectStack.peek() |> :erlang.term_to_binary() |> Base.url_encode64()
    ]

    options = [
      :exit_status,
      :binary,
      :hide,
      :use_stdio,
      :stderr_to_stdout,
      line: 1_000_000,
      env: [
        {~c"MIX_OS_CONCURRENCY_LOCK", ~c"false"},
        {~c"MIX_ENV", Atom.to_charlist(Mix.env())},
        {~c"MIX_TARGET", Atom.to_charlist(Mix.target())}
      ]
    ]

    ports =
      Map.new(1..count//1, fn index ->
        if Mix.debug?() do
          IO.puts("-> Starting mix deps.partition ##{index}")
        end

        args = args ++ [~c"--index", Integer.to_charlist(index)]
        port = Port.open({:spawn_executable, String.to_charlist(elixir)}, [args: args] ++ options)

        {index, port}
      end)

    clients =
      Enum.map(1..count//1, fn _ ->
        with {:ok, client} <- :gen_tcp.accept(socket, 15_000),
             {:ok, message} <- :gen_tcp.recv(client, 0, 15_000) do
          :inet.setopts(client, active: true)
          index = message |> String.trim() |> String.to_integer()
          %{port: Map.fetch!(ports, index), index: index, socket: client}
        else
          error ->
            logs =
              Enum.map_join(ports, "\n", fn {index, port} -> close_port(port, "#{index} >") end)

            Mix.raise("""
            could not start partition dependency compiler, no connection made to TCP port: #{inspect(error)}

            #{logs}
            """)
        end
      end)

    status = Map.new(deps, &{&1.app, :pending})
    send_deps_and_server_loop(clients, [], deps, status)
  end

  defp send_deps_and_server_loop(available, busy, deps, status) do
    {available, busy, deps} = send_deps(available, busy, deps, status)
    server_loop(available, busy, deps, status)
  end

  defp send_deps([client | available], busy, deps, status) do
    case pop_with(deps, fn dep ->
           Enum.all?(dep.deps, &(Map.get(status, &1.app, :unknown) != :pending))
         end) do
      :error ->
        {[client | available], busy, deps}

      {dep, deps} ->
        if Mix.debug?() do
          Mix.shell().info("-- Sending #{dep.app} to mix deps.partition #{client.index}")
        end

        :gen_tcp.send(client.socket, "#{dep.app}\n")
        send_deps(available, [client | busy], deps, status)
    end
  end

  defp send_deps([], busy, deps, _status) do
    {[], busy, deps}
  end

  defp server_loop(available, _busy = [], _deps = [], status) do
    shutdown_clients(available)
    Enum.any?(status, &(elem(&1, 1) == true))
  end

  defp server_loop(available, busy, deps, status) do
    receive do
      {:tcp, socket, data} ->
        [app, compiled?] =
          data |> String.trim() |> String.split(":") |> Enum.map(&String.to_atom/1)

        deps = Enum.reject(deps, &(&1.app == app))
        status = Map.replace!(status, app, compiled?)
        {client, busy} = pop_with(busy, &(&1.socket == socket))

        if Mix.debug?() do
          Mix.shell().info("-- mix deps.partition #{client.index} compiled #{app}")
        end

        send_deps_and_server_loop([client | available], busy, deps, status)

      {:tcp_closed, socket} ->
        tcp_failed!("closed unexpectedly", socket, available, busy)

      {:tcp_error, socket, error} ->
        tcp_failed!("errored: #{inspect(error)}", socket, available, busy)

      {port, {:data, {eol, data}}} ->
        with %{index: index} <-
               Enum.find(busy, &(&1.port == port)) || Enum.find(available, &(&1.port == port)) do
          terminator = if eol == :eol, do: "\n", else: ""
          IO.write([Integer.to_string(index), "> ", data, terminator])
        end

        server_loop(available, busy, deps, status)
    end
  end

  defp pop_with(list, fun) do
    case Enum.split_while(list, &(not fun.(&1))) do
      {_, []} -> :error
      {pre, [result | post]} -> {result, pre ++ post}
    end
  end

  defp tcp_failed!(message, socket, available, busy) do
    {%{port: port} = client, busy} = pop_with(busy, &(&1.socket == socket))

    # Let's make sure it has all been written out
    # but don't wait for more than 5 seconds if it
    # gets stuck for some unknown reason
    receive do
      {^port, {:exit_status, _}} -> :ok
    after
      5_000 -> Mix.shell().error("Timed out waiting for port exit #{inspect(port)}")
    end

    shutdown_clients(available ++ busy ++ [client])

    Mix.raise(
      "mix deps.partition #{inspect(socket)} #{message} " <>
        "(set MIX_OS_DEPS_COMPILE_PARTITION_COUNT=1 to run in serial)"
    )
  end

  defp shutdown_clients(clients) do
    Enum.each(clients, fn %{socket: socket, port: port, index: index} ->
      if Mix.debug?() do
        IO.puts("-> Closing mix deps.partition ##{index}")
      end

      _ = :gen_tcp.close(socket)
      IO.write(close_port(port, "#{index}> "))
    end)
  end

  defp close_port(port, prefix) do
    try do
      Port.close(port)
    catch
      _, _ -> :ok
    end

    loop_close_port(port, prefix)
  end

  defp loop_close_port(port, prefix) do
    receive do
      {^port, {:data, {:eol, data}}} -> [prefix, data, ?\n | loop_close_port(port, prefix)]
      {^port, {:data, {:noeol, data}}} -> [data | loop_close_port(port, prefix)]
      {^port, {:exit_status, _}} -> loop_close_port(port, prefix)
    after
      0 -> []
    end
  end

  ## Client

  @switches [port: :integer, host: :string, force: :boolean, index: :string, config: :string]

  def client do
    # If stdin closes, we shutdown the VM
    spawn(fn ->
      _ = IO.gets("")
      System.halt(0)
    end)

    args = System.argv()
    {opts, []} = OptionParser.parse!(args, strict: @switches)
    peek = Keyword.fetch!(opts, :config) |> Base.url_decode64!() |> :erlang.binary_to_term()

    # This is specific to Mix.install/2 and how it handles compile-time config
    if compile_config = peek.config[:compile_config] do
      Application.put_all_env(compile_config, persistent: true)
    end

    partition_args =
      opts
      |> Keyword.take([:host, :port, :index, :force])
      |> OptionParser.to_argv()

    Mix.start()
    Mix.ProjectStack.push(peek.name, peek.config, peek.file)
    Mix.CLI.main(["deps.partition" | partition_args], nil)
  end

  @impl true
  def run(args) do
    {opts, []} = OptionParser.parse!(args, strict: @switches)
    host = Keyword.fetch!(opts, :host)
    port = Keyword.fetch!(opts, :port)
    index = Keyword.fetch!(opts, :index)
    force? = Keyword.get(opts, :force, false)

    {:ok, socket} =
      :gen_tcp.connect(String.to_charlist(host), port, [:binary, packet: :line, active: false])

    :gen_tcp.send(socket, "#{index}\n")

    try do
      deps = Mix.Dep.load_and_cache()
      client_loop(socket, deps, force?, Mix.Project.deps_config())
    after
      :gen_tcp.close(socket)
    end
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
