<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Task and gen_tcp

In this chapter, we are going to learn how to use Erlang's [`:gen_tcp` module](`:gen_tcp`) to serve requests. This provides a great opportunity to explore Elixir's `Task` module. In future chapters, we will expand our server so that it can actually interact with buckets.

## Echo server

We will start our TCP server by first implementing an echo server. It will send a response with the text it received in the request. We will slowly improve our server until it is supervised and ready to handle multiple connections.

A TCP server, in broad strokes, performs the following steps:

  1. Listens to a port until the port is available and it gets hold of the socket
  2. Waits for a client connection on that port and accepts it
  3. Reads the client request and writes a response back

Let's implement those steps. Create a new `lib/kv/server.ex` and add the following functions:

```elixir
defmodule KV.Server do
  require Logger

  def accept(port) do
    # The options below mean:
    #
    # 1. `:binary` - receives data as binaries (instead of lists)
    # 2. `packet: :line` - receives data line by line
    # 3. `active: false` - blocks on `:gen_tcp.recv/2` until data is available
    # 4. `reuseaddr: true` - allows us to reuse the address if the listener crashes
    #
    {:ok, socket} =
      :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])
    Logger.info("Accepting connections on port #{port}")
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    serve(client)
    loop_acceptor(socket)
  end

  defp serve(socket) do
    socket
    |> read_line()
    |> write_line(socket)

    serve(socket)
  end

  defp read_line(socket) do
    {:ok, data} = :gen_tcp.recv(socket, 0)
    data
  end

  defp write_line(line, socket) do
    :gen_tcp.send(socket, line)
  end
end
```

We are going to start our server by calling `KV.Server.accept(4040)`, where 4040 is the port. The first step in `accept/1` is to listen to the port until the socket becomes available and then call `loop_acceptor/1`. `loop_acceptor/1` is a loop accepting client connections. For each accepted connection, we call `serve/1`.

`serve/1` is another loop that reads a line from the socket and writes those lines back to the socket. Note that the `serve/1` function uses the pipe operator `|>/2` to express this flow of operations. The pipe operator evaluates the left side and passes its result as the first argument to the function on the right side. The example above:

```elixir
socket |> read_line() |> write_line(socket)
```

is equivalent to:

```elixir
write_line(read_line(socket), socket)
```

The `read_line/1` implementation receives data from the socket using `:gen_tcp.recv/2` and `write_line/2` writes to the socket using `:gen_tcp.send/2`.

Note that `serve/1` is an infinite loop called sequentially inside `loop_acceptor/1`, so the tail call to `loop_acceptor/1` is never reached and could be avoided. However, as we shall see, we will need to execute `serve/1` in a separate process, so we will need that tail call soon.

This is pretty much all we need to implement our echo server. Let's give it a try!

Start an IEx session inside the `kv_server` application with `iex -S mix`. Inside IEx, run:

```elixir
iex> KV.Server.accept(4040)
```

The server is now running, and you will even notice the console is blocked. Let's use [a `telnet` client](https://en.wikipedia.org/wiki/Telnet) to access our server. There are clients available on most operating systems, and their command lines are generally similar:

```console
$ telnet 127.0.0.1 4040
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
hello
is it me
is it me
you are looking for?
you are looking for?
```

Type "hello", press enter, and you will get "hello" back. Excellent!

My particular telnet client can be exited by typing `ctrl + ]`, typing `quit`, and pressing `<Enter>`, but your client may require different steps.

Once you exit the telnet client, you will likely see an error in the IEx session:

```text
** (MatchError) no match of right hand side value: {:error, :closed}
    (kv) lib/kv/server.ex:45: KV.Server.read_line/1
    (kv) lib/kv/server.ex:37: KV.Server.serve/1
    (kv) lib/kv/server.ex:30: KV.Server.loop_acceptor/1
```

That's because we were expecting data from `:gen_tcp.recv/2` but the client closed the connection. We need to handle such cases better in future revisions of our server.

For now, there is a more important bug we need to fix: what happens if our TCP acceptor crashes? Since there is no supervision, the server dies and we won't be able to serve more requests, because it won't be restarted. That's why we must move our server to a supervision tree.

## Tasks

Whenever you have an existing function and you simply want to execute it when your application starts, the `Task` module is exactly you need. For example, it has a `Task.start_link/1` function that receives an anonymous function and executes it inside a new process that will be part of a supervision tree.

Let's give it a try. Open up `lib/kv.ex` and let's add a new child:

```elixir
  def start(_type, _args) do
    children = [
      {Registry, name: KV, keys: :unique},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one},
      {Task, fn -> KV.Server.accept(4040) end}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
```

With this change, we are saying that we want to run `KV.Server.accept(4040)` as a task. We are hardcoding the port for now but we will make this a configuration in later chapters. As usual, we've passed a two-element tuple as a child specification, which in turn will invoke `Task.start_link/1`.

Now that the server is part of the supervision tree, it should start automatically when we run the application. Run `iex -S mix` to boot the app and use the `telnet` client to make sure that everything still works:

```console
$ telnet 127.0.0.1 4321
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
say you
say you
say me
say me
```

Yes, it works! However, can it handle more than one client?

Try to connect two telnet clients at the same time. When you do so, you will notice that the second client doesn't echo:

```console
$ telnet 127.0.0.1 4321
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
hello?
HELLOOOOOO?
```

It doesn't seem to work at all. That's because we are serving requests in the same process that are accepting connections. When one client is connected, we can't accept another client.

## Adding (flawed) concurrency

In order to make our server handle simultaneous connections, we need to have one process working as an acceptor that spawns other processes to serve requests. One solution would be to change:

```elixir
defp loop_acceptor(socket) do
  {:ok, client} = :gen_tcp.accept(socket)
  serve(client)
  loop_acceptor(socket)
end
```

to also use `Task.start_link/1`:

```elixir
defp loop_acceptor(socket) do
  {:ok, client} = :gen_tcp.accept(socket)
  {:ok, pid} = Task.start_link(fn -> serve(client) end)
  :ok = :gen_tcp.controlling_process(client, pid)
  loop_acceptor(socket)
end
```

In the new acceptor loop, we are starting a new task every time there is a new client. Now, if you attempt to connect two clients at the same time, it should work!

Or does it? For example, what happens when you exit one telnet session? The other session should crash! The reason of this crash is two fold:

1. We have a bug in our server where we don't expect `:gen_tcp.recv/2` to return an `{:error, :closed}` tuple

2. Because each server task is linked to the acceptor process, if one task crashes, the acceptor process will also crash, taking down all other tasks and clients

An important rule thumb throughout this guide is to always start processes as children of supervisors. The code above is an excellent example of what happens when we don't. If we don't isolate the different parts of our systems, failures can now cascade through our system, as it would happen in other languages.

To fix this, we could use a `DynamicSupervisor`, but tasks also provide a specialized `Task.Supervisor` which has better ergonomics and is optimized for supervising tasks themselves. Let's give it a try.

## Adding a task supervisor

Let's change `start/2` in `lib/kv.ex` once more, to add the task supervisor to our tree:

```elixir
  def start(_type, _args) do
    children = [
      {Registry, name: KV, keys: :unique},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one},
      {Task.Supervisor, name: KV.ServerSupervisor},
      {Task, fn -> KV.Server.accept(4040) end}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
```

We'll now start a `Task.Supervisor` process with name `KV.TaskSupervisor`. Keep in mind that the order children are started matters. For example, the acceptor must come last because, if it comes first, it means our application can start accepting requests before the `Task.Supervisor` is running or before we can locate buckets. Shutting down an application will also stop the children in reverse order, guaranteeing a clean termination.

Now we need to change `loop_acceptor/1` to use `Task.Supervisor` to serve each request:

```elixir
defp loop_acceptor(socket) do
  {:ok, client} = :gen_tcp.accept(socket)
  {:ok, pid} = Task.Supervisor.start_child(KV.ServerSupervisor, fn -> serve(client) end)
  :ok = :gen_tcp.controlling_process(client, pid)
  loop_acceptor(socket)
end
```

You might notice that we added a line, `:ok = :gen_tcp.controlling_process(client, pid)`. This makes the child process the "controlling process" of the `client` socket. If we didn't do this, the acceptor would bring down all the clients if it crashed because sockets would be tied to the process that accepted them (which is the default behavior).

Now start a new server with `iex -S mix` and try to open up many concurrent telnet clients. You will notice that quitting a client does not bring the acceptor down, even though we haven't fixed the bug in `:gen_tcp.recv/2` yet (which we will address in the next chapter). Excellent!

## Restart strategies

There is one important topic we haven't explored yet with the necessary depth. What happens when a supervised process crashes?

In the previous chapter, when we started a bucket and killed it, the supervisor automatically started one in its place:

```elixir
iex> children = [{KV.Bucket, name: :shopping}]
iex> Supervisor.start_link(children, strategy: :one_for_one)
iex> KV.Bucket.put(:shopping, "milk", 1)
iex> pid = Process.whereis(:shopping)
#PID<0.48.0>
iex> Process.exit(pid, :kill)
true
iex> Process.whereis(:shopping)
#PID<0.50.0>
```

What exactly happens when a process terminates is part of its child specification. For `KV.Bucket`, we have this:

```elixir
iex> KV.Bucket.child_spec([])
%{id: KV.Bucket, start: {KV.Bucket, :start_link, [[]]}}
```

However, for tasks, we have this:

```elixir
iex> Task.child_spec(fn -> :ok end)
%{
  id: Task,
  restart: :temporary,
  start: {Task, :start_link, [#Function<43.39164016/0 in :erl_eval.expr/6>]}
}
```

Notice that a task says `:restart` is `:temporary`. `KV.Bucket` says nothing, which means it defaults to `:permanent`. `:temporary` means that a process is never restarted, regardless of why it crashed. `:permanent` means a process is always restarted, regardless of the exit reason. There is also `:transient`, which means it won't be restarted as long as it terminates successfully.

Now we must ask ourselves, are those the correct settings?

For `KV.Bucket`, using `:permanent` seem logical, as should not request the user to recreate a bucket they have previous created. Although currently we would lose the bucket data, in actual system we would add mechanisms to recover it on initialization. However, for tasks, we have used them in two opposing ways in this chapter, which means at least one of them is wrong.

We use a task to start the acceptor. The acceptor is a critical component of our infrastructure. If it crashes, it means we won't accept further requests, and our server would then be useless as no one can connect to it. On the other hand, we also use `Task.Supervisor` to start tasks that deal with each connection. In this case, restarting may not be useful at all, given the reason we crashed could just as well be a connection issue, and attempting to restart over the same connection would lead to further failures.

Therefore, we want the acceptor to actually run in `:permanent` mode, while we preserve the `Task.Supervisor` as `:temporary`. Luckily Elixir has an API that allows us to change an existing child specification, which we use below.

Let's change `start/2` in `lib/kv.ex` once more to the following:

```elixir
  def start(_type, _args) do
    children = [
      {Registry, name: KV, keys: :unique},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one},
      {Task.Supervisor, name: KV.ServerSupervisor},
      Supervisor.child_spec({Task, fn -> KV.Server.accept(4040) end}, restart: :permanent)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
```

Now we have an always running acceptor that starts temporary task processes under an always running task supervisor.

## Leveraging the ecosystem

In this chapter, we implemented a basic TCP acceptor while exploring concurrency and fault-tolerance. Our acceptor can manage concurrent connections, but it is still not ready for production. Production-ready TCP servers run a pool of acceptors, each with their own supervisor. Elixir's `PartitionSupervisor` might be used to partition and scale the acceptor, but it is out of scope for this guide. In practice, you will use existing packages tailored for this use-case, such as [Ranch](https://github.com/ninenines/ranch) (in Erlang) or [Thousand Island](https://github.com/mtrudel/thousand_island) (in Elixir).

In the next chapter, we will start parsing the client requests and sending responses, finishing our server.
