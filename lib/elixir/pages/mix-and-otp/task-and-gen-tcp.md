# Task and gen_tcp

In this chapter, we are going to learn how to use [Erlang's `:gen_tcp` module](http://www.erlang.org/doc/man/gen_tcp.html) to serve requests. This provides a great opportunity to explore Elixir's `Task` module. In future chapters, we will expand our server so that it can actually serve the commands.

## Echo server

We will start our TCP server by first implementing an echo server. It will send a response with the text it received in the request. We will slowly improve our server until it is supervised and ready to handle multiple connections.

A TCP server, in broad strokes, performs the following steps:

  1. Listens to a port until the port is available and it gets hold of the socket
  2. Waits for a client connection on that port and accepts it
  3. Reads the client request and writes a response back

Let's implement those steps. Move to the `apps/kv_server` application, open up `lib/kv_server.ex`, and add the following functions:

```elixir
defmodule KVServer do
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

We are going to start our server by calling `KVServer.accept(4040)`, where 4040 is the port. The first step in `accept/1` is to listen to the port until the socket becomes available and then call `loop_acceptor/1`. `loop_acceptor/1` is a loop accepting client connections. For each accepted connection, we call `serve/1`.

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
iex> KVServer.accept(4040)
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

    ** (MatchError) no match of right hand side value: {:error, :closed}
        (kv_server) lib/kv_server.ex:45: KVServer.read_line/1
        (kv_server) lib/kv_server.ex:37: KVServer.serve/1
        (kv_server) lib/kv_server.ex:30: KVServer.loop_acceptor/1

That's because we were expecting data from `:gen_tcp.recv/2` but the client closed the connection. We need to handle such cases better in future revisions of our server.

For now, there is a more important bug we need to fix: what happens if our TCP acceptor crashes? Since there is no supervision, the server dies and we won't be able to serve more requests, because it won't be restarted. That's why we must move our server to a supervision tree.

## Tasks

We have learned about agents, generic servers, and supervisors. They are all meant to work with multiple messages or manage state. But what do we use when we only need to execute some task and that is it?

The `Task` module provides this functionality exactly. For example, it has a `Task.start_link/1` function that receives an anonymous function and executes it inside a new process that will be part of a supervision tree.

Let's give it a try. Open up `lib/kv_server/application.ex`, and let's change the supervisor in the `start/2` function to the following:

```elixir
  def start(_type, _args) do
    children = [
      {Task, fn -> KVServer.accept(4040) end}
    ]

    opts = [strategy: :one_for_one, name: KVServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
```

As usual, we've passed a two-element tuple as a child specification, which in turn will invoke `Task.start_link/1`.

With this change, we are saying that we want to run `KVServer.accept(4040)` as a task. We are hardcoding the port for now but this could be changed in a few ways, for example, by reading the port out of the system environment when starting the application:

```elixir
port = String.to_integer(System.get_env("PORT") || "4040")
# ...
{Task, fn -> KVServer.accept(port) end}
```

Insert these changes in your code and now you may start your application using the following command `PORT=4321 mix run --no-halt`, notice how we are passing the port as a variable, but still defaults to 4040 if none is given.

Now that the server is part of the supervision tree, it should start automatically when we run the application. Start your server, now passing the port, and once again use the `telnet` client to make sure that everything still works:

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

## Task supervisor

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
  Task.start_link(fn -> serve(client) end)
  loop_acceptor(socket)
end
```

We are starting a linked Task directly from the acceptor process. But we've already made this mistake once. Do you remember?

This is similar to the mistake we made when we called `KV.Bucket.start_link/1` straight from the registry. That meant a failure in any bucket would bring the whole registry down.

The code above would have the same flaw: if we link the `serve(client)` task to the acceptor, a crash when serving a request would bring the acceptor, and consequently all other connections, down.

We fixed the issue for the registry by using a simple one for one supervisor. We are going to use the same tactic here, except that this pattern is so common with tasks that `Task` already comes with a solution: a simple one for one supervisor that starts temporary tasks as part of our supervision tree.

Let's change `start/2` once again, to add a supervisor to our tree:

```elixir
  def start(_type, _args) do
    port = String.to_integer(System.get_env("PORT") || "4040")

    children = [
      {Task.Supervisor, name: KVServer.TaskSupervisor},
      {Task, fn -> KVServer.accept(port) end}
    ]

    opts = [strategy: :one_for_one, name: KVServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
```

We'll now start a `Task.Supervisor` process with name `KVServer.TaskSupervisor`. Remember, since the acceptor task depends on this supervisor, the supervisor must be started first.

Now we need to change `loop_acceptor/1` to use `Task.Supervisor` to serve each request:

```elixir
defp loop_acceptor(socket) do
  {:ok, client} = :gen_tcp.accept(socket)
  {:ok, pid} = Task.Supervisor.start_child(KVServer.TaskSupervisor, fn -> serve(client) end)
  :ok = :gen_tcp.controlling_process(client, pid)
  loop_acceptor(socket)
end
```

You might notice that we added a line, `:ok = :gen_tcp.controlling_process(client, pid)`. This makes the child process the "controlling process" of the `client` socket. If we didn't do this, the acceptor would bring down all the clients if it crashed because sockets would be tied to the process that accepted them (which is the default behavior).

Start a new server with `PORT=4040 mix run --no-halt` and we can now open up many concurrent telnet clients. You will also notice that quitting a client does not bring the acceptor down. Excellent!

Here is the full echo server implementation:

```elixir
defmodule KVServer do
  require Logger

  @doc """
  Starts accepting connections on the given `port`.
  """
  def accept(port) do
    {:ok, socket} = :gen_tcp.listen(port,
                      [:binary, packet: :line, active: false, reuseaddr: true])
    Logger.info "Accepting connections on port #{port}"
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = Task.Supervisor.start_child(KVServer.TaskSupervisor, fn -> serve(client) end)
    :ok = :gen_tcp.controlling_process(client, pid)
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

Since we have changed the supervisor specification, we need to ask: is our supervision strategy still correct?

In this case, the answer is yes: if the acceptor crashes, there is no need to crash the existing connections. On the other hand, if the task supervisor crashes, there is no need to crash the acceptor too.

However, there is still one concern left, which are the restart strategies. Tasks, by default, have the `:restart` value set to `:temporary`, which means they are not restarted. This is an excellent default for the connections started via the `Task.Supervisor`, as it makes no sense to restart a failed connection, but it is a bad choice for the acceptor. If the acceptor crashes, we want to bring the acceptor up and running again.

Let's fix this. We know that for a child of shape `{Task, fun}`, Elixir will invoke `Task.child_spec(fun)` to retrieve the underlying child specification. Therefore, one might imagine that to change the `{Task, fun}` specification to have a `:restart` of `:permanent`, we would need to change the `Task` module. However, that's impossible to do, as the `Task` module is defined as part of Elixir's standard library (and even if it was possible, it is unlikely it would be a good idea).
Luckily, this can be done by using `Supervisor.child_spec/2`, which allows us to configure a child specification with new values. Let's rewrite `start/2` in `KVServer.Application` once more:

```elixir
  def start(_type, _args) do
    port = String.to_integer(System.get_env("PORT") || "4040")

    children = [
      {Task.Supervisor, name: KVServer.TaskSupervisor},
      Supervisor.child_spec({Task, fn -> KVServer.accept(port) end}, restart: :permanent)
    ]

    opts = [strategy: :one_for_one, name: KVServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
```

Now we have an always running acceptor that starts temporary task processes under an always running task supervisor.

## Wrapping up

In this chapter we have implemented a straigh-forward TCP acceptor, which allowed is to explore tools for concurrency and fault-tolerance. While our acceptor can manage concurrent connections, it is still not ready for production. In practice, TCP servers run a pool of acceptors, instead of a single one, each of them with their own supervisor. While Elixir has tools to make it easier to partition and scale the accept, such as the `PartitionSupervisor`, they are out of scope for this guide. In any case, a better path forward may be to use existing packages tailored for this use case, such as [Ranch](https://github.com/ninenines/ranch) (in Erlang) or [Thousand Island](https://github.com/mtrudel/thousand_island) (in Elixir).

In the next chapter, we will start parsing the client requests and sending responses, finishing our server.
