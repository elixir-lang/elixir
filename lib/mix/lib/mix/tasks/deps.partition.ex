# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Mix.Tasks.Deps.Partition do
  @moduledoc false
  use Mix.Task

  ## Server

  @deps_partition_install_mix_exs ~c"deps.partition.mix.exs"

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

    env_vars =
      if Mix.install?() do
        blob =
          Mix.Project.config()
          |> :erlang.term_to_binary()
          |> :binary.bin_to_list()
          |> Enum.join(",")

        # We replicate the initialization logic from Mix.install/2 as part of mix.exs
        File.write!(@deps_partition_install_mix_exs, """
        config = <<#{blob}>>
        project = :erlang.binary_to_term(config)

        if compile_config = project[:compile_config] do
          Application.put_all_env(compile_config, persistent: true)
        end

        Mix.ProjectStack.push(Mix.InstallProject, project, "nofile")
        """)

        [{~c"MIX_EXS", @deps_partition_install_mix_exs}]
      else
        []
      end

    args = [
      ansi_flag,
      ~c"-e",
      ~c"Mix.CLI.main",
      ~c"deps.partition",
      force_flag,
      ~c"--port",
      Integer.to_charlist(port),
      ~c"--host",
      ~c"127.0.0.1"
    ]

    options = [
      :binary,
      :hide,
      :use_stdio,
      :stderr_to_stdout,
      line: 1_000_000,
      env: [
        {~c"MIX_OS_CONCURRENCY_LOCK", ~c"false"},
        {~c"MIX_ENV", Atom.to_charlist(Mix.env())},
        {~c"MIX_TARGET", Atom.to_charlist(Mix.target())}
        | env_vars
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
        with {:ok, client} <- :gen_tcp.accept(socket, 15000),
             {:ok, message} <- :gen_tcp.recv(client, 0, 15000) do
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
        shutdown_clients(available ++ busy)
        Mix.raise("mix deps.partition #{inspect(socket)} closed unexpectedly")

      {:tcp_error, socket, error} ->
        shutdown_clients(available ++ busy)
        Mix.raise("mix deps.partition #{inspect(socket)} errored: #{inspect(error)}")

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
    receive do
      {^port, {:data, {:eol, data}}} -> [prefix, data, ?\n | close_port(port, prefix)]
      {^port, {:data, {:noeol, data}}} -> [data | close_port(port, prefix)]
    after
      0 ->
        try do
          Port.close(port)
        catch
          _, _ -> :ok
        end

        []
    end
  end

  ## Client

  @switches [port: :integer, host: :string, force: :boolean, index: :string]

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
