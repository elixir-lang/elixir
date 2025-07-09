<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Configuration and distribution

So far we have hardcoded our applications to run a web server on port 4040. This has been somewhat problematic since we can't, for example, run our development server and tests at the same time. In this chapter, we will learn how to use the application environment for configuration, paving the way for us to enable distribution by running multiple development servers on the same machine (on different ports).

In this last guide, we will make the routing table for our distributed key-value store configurable, and then finally package the software for production.

Let's do this.

## Application environment

In the chapter [Registries, applications, and supervisors](supervisor-and-application.md), we have learned that our project is backed by an application, which bundles our modules and specifies how your supervision starts and shuts down. Each application can also have its own configuration, which in Erlang/OTP (and therefore Elixir) is called "application environment".

We can use the application environment to configure our own application, as well as others. Let's see the application environment in practice. Create a file `config/runtime.exs` with the following:

```elixir
import Config

port =
  cond do
    port_env = System.get_env("PORT") ->
      String.to_integer(port_env)

    config_env() == :test ->
      4040

    true ->
      4050
  end

config :kv, :port, port
```

The above is attempting to read the "PORT" environment variable and use it as the port if defined. Otherwise, we default to port `4040` for tests and port `4050` for other environments, eliminating the conflict between environments we have seen in the past. Then we store its value under the `:port` key of our `:kv` application.

Now we just need to read this configuration. Open up `lib/kv.ex` and the `start/2` function to the following:

```elixir
  def start(_type, _args) do
    port = Application.fetch_env!(:kv, :port)

    children = [
      {Registry, name: KV, keys: :unique},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one},
      {Task.Supervisor, name: KV.ServerSupervisor},
      Supervisor.child_spec({Task, fn -> KV.Server.accept(port) end}, restart: :permanent)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
```

Run `iex -S mix` and you will see the following message printed:

```
[info] Accepting connections on port 4050
```

Run tests, without killing the development server, and you will see it running on port 4040.

Our change was straight-forward. We used `Application.fetch_env!/2` to read the entry for `port` in `:kv`'s environment. We explicitly used `fetch_env!/2` (instead of `get_env/2` or `fetch_env`) because it will raise if the port wasn not configured (therefore our app wouldn't even boot).

## Compile vs runtime configuration

Configuration files provide a mechanism for us to configure the environment of any application. Elixir provides two configuration entry points:

  * `config/config.exs` — this file is read at build time, before we compile our application and before we even load our dependencies. This means we can't access the code in our application nor in our dependencies. However, it means we can control how they are compiled

  * `config/runtime.exs` — this file is read after our application and dependencies are compiled and therefore it can configure how our application works at runtime. If you want to read system environment variables (via `System.get_env/1`) or access external configuration, this is the appropriate place to do so

You can learn more about configuration in the `Config` and `Config.Provider` modules.

Generally speaking, we use `Application.fetch_env!/2` (and friends) to read runtime configuration. `Application.compile_env/2` is available for reading compile-time configuration. This allows Elixir to track which modules to recompile when the compilation environment changes.

Now that we can start multiple servers, let's explore distribution.

## Our first distributed code

Elixir ships with facilities to connect nodes and exchange information between them. In fact, we use the same concepts of processes, message passing and receiving messages when working in a distributed environment because Elixir processes are *location transparent*. This means that when sending a message, it doesn't matter if the recipient process is on the same node or on another node, the VM will be able to deliver the message in both cases.

In order to run distributed code, we need to start the VM with a name. The name can be short (when in the same network) or long (requires the full computer address). Let's start a new IEx session:

```console
$ iex --sname foo
```

You can see now the prompt is slightly different and shows the node name followed by the computer name:

    Interactive Elixir - press Ctrl+C to exit (type h() ENTER for help)
    iex(foo@jv)1>

My computer is named `jv`, so I see `foo@jv` in the example above, but you will get a different result. We will use `foo@computer-name` in the following examples and you should update them accordingly when trying out the code.

Let's define a module named `Hello` in this shell:

```elixir
iex> defmodule Hello do
...>   def world, do: IO.puts("hello world")
...> end
```

If you have another computer on the same network with both Erlang and Elixir installed, you can start another shell on it. If you don't, you can start another IEx session in another terminal. In either case, give it the short name of `bar`:

```console
$ iex --sname bar
```

Note that inside this new IEx session, we cannot access `Hello.world/0`:

```elixir
iex> Hello.world
** (UndefinedFunctionError) function Hello.world/0 is undefined (module Hello is not available)
    Hello.world()
```

However, we can spawn a new process on `foo@computer-name` from `bar@computer-name`! Let's give it a try (where `@computer-name` is the one you see locally):

```elixir
iex> Node.spawn_link(:"foo@computer-name", fn -> Hello.world() end)
#PID<9014.59.0>
hello world
```

Elixir spawned a process on another node and returned its PID. You can see the PID number no longer starts with zero, showing it belongs to another node. The code then executed on the other node where the `Hello.world/0` function exists and invoked that function. Note that the result of "hello world" was printed on the current node `bar` and not on `foo`. In other words, the message to be printed was sent back from `foo` to `bar`. This happens because the process spawned on the other node (`foo`) knows all the output should be sent back to the original node!

We can send and receive messages from the PID returned by `Node.spawn_link/2` as usual. Let's try a quick ping-pong example:

```elixir
iex> pid = Node.spawn_link(:"foo@computer-name", fn ->
...>   receive do
...>     {:ping, client} -> send(client, :pong)
...>   end
...> end)
#PID<9014.59.0>
iex> send(pid, {:ping, self()})
{:ping, #PID<0.73.0>}
iex> flush()
:pong
:ok
```

In other words, we can spawn processes in other nodes, hold to their PIDs, and then send messages to them as if they were running on the same machine. That's the *location transparency* principle. And because everything we have built so far was built on top of messaging passing, we should be able to adjust our key-value store to become a distributed one with little work.

## Distributed naming registry with `:global`

First, let's check that our code is not currently distributed. Start a new node like this:

```
$ PORT=4100 iex --sname foo -S mix
```

And the other like this:

```
$ PORT=4101 iex --sname bar -S mix
```

Now, within `foo@computer-name`, do this:

```elixir
iex> :erpc.call(:"bar@computer-name", KV, :create_bucket, ["shopping"])
{:ok, #PID<22121.164.0>}
```

Instead of using `Node.spawn_link/2`, we used [Erlang's builtin RPC module](`:erpc`) to call the function `create_bucket` in the `KV` module passing a one element list with the string "shopping" as the argument list. We could have used `Node.spawn_link/2`, but `:erpc.call/4` conveniently returns the result of the invocation.

Still in `foo@computer-name`, let's try to access the bucket:

```elixir
iex> KV.lookup_bucket("shopping")
nil
```

It returns `nil`. However, if you run `KV.lookup_bucket("shopping")` in `bar@computer-name`, it will return the proper bucket. In other words, the nodes can communicate with each other, but buckets spawned in one node are not visibly to the other.

This is because we are using [Elixir's Registry](`Registry`) to name our buckets, which is a **local** process registry. In other words, it is designed for processes running on a single node and not for distribution.

Luckily, Erlang ships with a distributed registry called [`:global`](`:global`), which is support by default as a naming registry. All we need to do is to replace the `via/1` function in `lib/kv.ex` from this:

```elixir
  defp via(name), do: {:via, Registry, {KV, name}}
```

to this:

```elixir
  defp via(name), do: {:global, name}
```

Do the change above and restart both `foo@computer-name` and `bar@computer-name`. Now, back on `foo@computer-name`, let's give it another try:

```elixir
iex> :erpc.call(:"bar@computer-name", KV, :create_bucket, ["shopping"])
{:ok, #PID<21821.179.0>}
iex> KV.lookup_bucket("shopping")
#PID<21821.179.0>
```

And there you go! By simply changing which naming registry we used, we now have a distributed key value store. You can even try using `telnet` to configure to the different ports and validate that changes in one session are visible to other one. Exciting!

## Node discovery and dependencies

There is one essential ingredient to wrap up our distributed key-value store. In other for the `:global` registry to work, we need to make sure the nodes are connected to each other. When we run `:erpc` call passing the node name:

```elixir
:erpc.call(:"bar@computer-name", KV, :create_bucket, ["shopping"])
```

Elixir automatically connected the nodes together. This is easy to do in an IEx session when both nodes are running on the same machine but it requires more work in a production environment, where nodes will be in different IP addresses and may be started at any time.

Luckily for us, this is also a well-solved problem. For example, if you are using [the Phoenix web framework](https://phoenixframework.org) in production, it ships with [the `dns_cluster` package](https://github.com/phoenixframework/dns_cluster), which automatically runs DNS queries to find new nodes and connect them. If you are using Kubernetes or cloud providers, [packages like `libcluster`](https://github.com/bitwalker/libcluster) ship with different strategies to discover and connect nodes.

Installing dependencies in Elixir is simple. Most commonly, we use the [Hex Package Manager](https://hex.pm), by listing the dependency inside the deps function in our `mix.exs` file:

```elixir
def deps do
  [{:dns_cluster, "~> 0.2"}]
end
```

This dependency refers to the latest version of `dns_cluster` in the 0.x version series that has been pushed to Hex. This is indicated by the `~>` preceding the version number. For more information on specifying version requirements, see the documentation for the `Version` module.

Typically, stable releases are pushed to Hex. If you want to depend on an external dependency still in development, Mix is able to manage Git dependencies too:

```elixir
def deps do
  [{:dns_cluster, git: "https://github.com/phoenixframework/dns_cluster.git"}]
end
```

You will notice that when you add a dependency to your project, Mix generates a `mix.lock` file that guarantees *repeatable builds*. The lock file must be checked in to your version control system, to guarantee that everyone who uses the project will use the same dependency versions as you.

Mix provides many tasks for working with dependencies, which can be seen in `mix help`:

```console
$ mix help
mix deps              # Lists dependencies and their status
mix deps.clean        # Deletes the given dependencies' files
mix deps.compile      # Compiles dependencies
mix deps.get          # Gets all out of date dependencies
mix deps.tree         # Prints the dependency tree
mix deps.unlock       # Unlocks the given dependencies
mix deps.update       # Updates the given dependencies
```

The most common tasks are `mix deps.get` and `mix deps.update`. Once fetched, dependencies are automatically compiled for you. You can read more about deps by running `mix help deps`.

To wrap up this chapter, we will build a very simple node discovery mechanism, where the name of the nodes we are should connect to are given on boot, using the lessons we learned in this chapter.

## `Node.connect/1`

We will change our application to support a "NODES" environment variable with the name of all nodes each instance should connect to.

Open up `config/runtime.exs` and add this to the bottom:

```elixir
nodes =
  System.get_env("NODES", "")
  |> String.split(",")
  |> Enum.reject(& &1 == "")
  |> Enum.map(&String.to_atom/1)

config :kv, :nodes, nodes
```

We fetch the environment variable, split it on ",", discard all empty strings, and then convert each entry to an atom, as node names are atoms.

Now, in your `start/2` callback, we will add this to of the `start/2` function:

```elixir
  def start(_type, _args) do
    for node <- Application.fetch_env!(:kv, :nodes) do
      Node.connect(node)
    end
```

Now we can start our nodes as:

```shell
$ NODES="foo@computer-name,bar@computer-name" PORT=4040 iex --sname foo -S mix
$ NODES="foo@computer-name,bar@computer-name" PORT=4041 iex --sname bar -S mix
```

And they should connect to each other. Give it a try!

In an actual production system, there is some additional care we must take. For example, `--sname` only allows instances on the same machine to connect. For production, we would use `--name` instead.

Furthermore, when connecting two instances, we must guarantee they have the same cookie, which is a secret Erlang uses to authorize the connection. When they run on the same machine, they share the same cookie by default, but it must be either explicitly set or shared in other ways when deploying in a cluster.

We will revisit these topics in the last chapter when we talk about releases.

## Distributed system trade-offs

In this chapter, we made our key-value store distributed by using the `:global` naming registry. However, it is important to keep in mind that every distributed system, be it a library or a full-blown database, is designed with a series of trade-offs in mind.

In particular, `:global` requires consistency across all known nodes whenever a new bucket is created. For example, if your cluster has three nodes, creating a new bucket will require all three nodes to agree on its name. This means if one node is unresponsive, perhaps due to a [network partition](https://en.wikipedia.org/wiki/Network_partition), the node will have to either reconnect or be kicked out before registration succeeds. This also means that, as your cluster grows in size, registration becomes more expensive, although lookups are always cheap and immediate. Within the ecosystem, there are other named registries, which explore different trade-offs, such as [Syn](https://github.com/ostinelli/syn).

Further complications arise when we consider storage. Today, when our nodes terminate, we lose all data stored in the buckets. In our current design, since we allow each node to store their own buckets, it means we would need to backup each node. And, if we don't want data losses, we would also need to replicate the data.

For those reasons, it is still very common to use a database (or any storage system) when writing production applications in Elixir, and use Elixir to implement the realtime and collaborative aspects of your applications that extend beyond storage. For example, we can use Elixir to track which clients are connected to the cluster at any given moment or implement a feed where users are notified in realtime whenever items are added or removed from a bucket.

In fact, that's exactly what we will build in the next chapter. Allowing us to wrap up everything we have learned so far and also talk about one of the essential building blocks in Elixir software: GenServers.
