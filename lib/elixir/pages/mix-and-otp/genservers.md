<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Client-server with GenServer

To wrap up our distributed key-value store, we will implement a feature where a client can subscribe to a bucket and receive realtime notifications of any modification happening in the bucket, regardless of where in the cluster the bucket is located.

We will do by adding a new command, called SUBSCRIBE, to be used like this:

```text
SUBSCRIBE shopping
milk SET TO 1
eggs SET TO 10
milk DELETED
```

To make this work, we must change our `KV.Bucket` implementation to track subscriptions and emit broadcasts. However, as we will see, we cannot implement such on top of agents, and we will need to rewrite our bucket implementation to a `GenServer`.

## Links and monitors

Processes in Elixir are isolated. When they need to communicate, they do so by sending messages. However, how do you know when a process terminates, either because it has completed or due to a crash?

We have two options: links and monitors.

We have used links extensively. Whenever we started a process, we typically did so by using `start_link` or similar. The idea behind links is that, if any of the processes crash, the other will crash due to the link. We talked about them in the [Process chapter of the Getting Started guide](../getting-started/processes.md). Here is a refresher:

```elixir
iex> self()
#PID<0.115.0>
iex> spawn_link(fn -> :nothing_bad_will_happen end)
#PID<0.116.0>
iex> self()
#PID<0.115.0>
```

```elixir
iex> spawn_link(fn -> raise "oops" end)
#PID<0.117.0>

12:37:33.229 [error] Process #PID<0.117.0> raised an exception
Interactive Elixir (1.18.4) - press Ctrl+C to exit (type h() ENTER for help)
iex> self()
#PID<0.118.0>
```

The reason why we links are so pervasive is because when we start a process inside a supervisor, we want our process to crash if the supervisor terminates. On the other hand, we don't want the supervisor to crash when a child terminates, and therefore supervisors trap exits from links by calling `Process.flag(:trap_exit, true)`.

In other words, links create an intrinsic relationship between the processes. If we simply want to track when a process dies, without tying their exit signals to each other, a better solution is to use monitors. When a monitored process terminates, we receive a message in our inbox, regardless of the reason:

```elixir
iex> pid = spawn(fn -> Process.sleep(5000) end)
#PID<0.119.0>
iex> Process.monitor(pid)
#Reference<0.1076459149.2159017989.118674>
iex> flush()
:ok
# Wait five seconds
iex> flush()
{:DOWN, #Reference<0.1076459149.2159017989.118674>, :process, #PID<0.119.0>, :normal}
:ok
```

Once the process terminates, we receive a "DOWN message", represented in a five-element tuple. The last element is the reason why it crashed (`:normal` means it terminated successfully).

Monitors will play a very important role in our subscribe feature. When a client subscribes to a bucket, the bucket will store the client PID and send messages to it on every change. However, if the client terminates (for example because it was disconnected), the bucket must remove the client from its list of subscribers (otherwise the list would keep on growing forever as clients connect and disconnect).

Unfortunately, we chose the `Agent` module to implement our `KV.Bucket` and, unfortunately, agents cannot receive messages. So the first step is to rewrite our `KV.Bucket` to a `GenServer`. The `GenServer` module documentation has a good overview on what they are and how to implement them. Give it a read and then we are ready to proceed.

## GenServer callbacks

A GenServer is a process that invokes a limited set of functions under specific conditions. When we used an `Agent`, we would keep both the client code and the server code side by side, like this:

```elixir
def put(bucket, key, value) do
  Agent.update(bucket, &Map.put(&1, key, value))
end
```

Let's break that code apart a bit:

```elixir
def put(bucket, key, value) do
  # Here is the client code
  Agent.update(bucket, fn state ->
    # Here is the server code
    Map.put(state, key, value)
  end)
  # Back to the client code
end
```

In the code above, we have a process, which we call "the client" sending a request to an agent, "the server". The request contains an anonymous function, which must be executed by the server.

In a GenServer, the code above would be two separate functions, roughly like this:

```elixir
def put(bucket, key, value) do
  # Send the server a :put "instruction"
  GenServer.call(bucket, {:put, key, value})
end

# Server callback

def handle_call({:put, key, value}, _from, state) do
  {:reply, :ok, Map.put(state, key, value)}
end
```

Let's go ahead and rewrite `KV.Bucket` at once. Open up `lib/kv/bucket.ex` and replace its contents with this new version:

```elixir
defmodule KV.Bucket do
  use GenServer

  @doc """
  Starts a new bucket.
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  @doc """
  Gets a value from the `bucket` by `key`.
  """
  def get(bucket, key) do
    GenServer.call(bucket, {:get, key})
  end

  @doc """
  Puts the `value` for the given `key` in the `bucket`.
  """
  def put(bucket, key, value) do
    GenServer.call(bucket, {:put, key, value})
  end

  @doc """
  Deletes `key` from `bucket`.

  Returns the current value of `key`, if `key` exists.
  """
  def delete(bucket, key) do
    GenServer.call(bucket, {:delete, key})
  end

  ### Callbacks

  @impl true
  def init(bucket) do
    state = %{
      bucket: bucket
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    value = get_in(state.bucket[key])
    {:reply, value, state}
  end

  def handle_call({:put, key, value}, _from, state) do
    state = put_in(state.bucket[key], value)
    {:reply, :ok, state}
  end

  def handle_call({:delete, key}, _from, state) do
    {value, state} = pop_in(state.bucket[key])
    {:reply, value, state}
  end
end
```

The first function is `start_link/1`, which starts a new GenServer passing a list of options. `GenServer.start_link/3`, which takes three arguments:

1. The module where the server callbacks are implemented, in this case `__MODULE__` (meaning the current module)

2. The initialization arguments, in this case the empty bucket `%{}`

3. A list of options which can be used to specify things like the name of the server. Once again, we forward the list of options that we receive on `start_link/1` to `GenServer.start_link/3`, as we did for agents

Once started, the GenServer will invoke the `init/1` callback, that receives the second argument given to `GenServer.start_link/3` and returns `{:ok, state}`, where state is a new map. We can already notice how the `GenServer` API makes the client/server segregation more apparent. `start_link/3` happens in the client, while `init/1` is the respective callback that runs on the server.

There are two types of requests you can send to a GenServer: calls and casts. Calls are synchronous and the server **must** send a response back to such requests. While the server computes the response, the client is **waiting**. Casts are asynchronous: the server won't send a response back and therefore the client won't wait for one. Both requests are messages sent to the server, and will be handled in sequence. So far we have only used `GenServer.call/2`, to keep the same semantics as the Agent, but we will give `cast` a try when implementing subscriptions. Given we kept the same behaviour, all tests will still pass.

Each request must be implemented a specific callback. For `call/2` requests, we implement a `handle_call/3` callback that receives the `request`, the process from which we received the request (`_from`), and the current server state (`state`). The `handle_call/3` callback returns a tuple in the format `{:reply, reply, updated_state}`. The first element of the tuple, `:reply`, indicates that the server should send a reply back to the client. The second element, `reply`, is what will be sent to the client while the third, `updated_state` is the new server state.

Another Elixir feature we used in the implementation above are the nested traversal functions: `get_in/1`, `put_in/2`, and `pop_in/1`. Instead of keeping the `bucket` as our GenServer state, we defined a state map with a `bucket` key inside. This will be important as we also need to track subscribers as part of the GenServer state. These new functions make it straight-forward to manipulate data structures nested in other data structures.

With our GenServer in place, let's work on subscription, starting with the tests.

## Implementing subscriptions

Our new test will subscribe to a bucket and then assert that, as operations are performed against the bucket, we receive messages of said events.

Open up `test/kv/bucket_test.exs` and key this in:

```elixir
  test "subscribes to puts and deletes" do
    {:ok, bucket} = start_supervised(KV.Bucket)
    KV.Bucket.subscribe(bucket)

    KV.Bucket.put(bucket, "milk", 3)
    assert_receive {:put, "milk", 3}

    # Also check it works even from another process
    spawn(fn -> KV.Bucket.delete(bucket, "milk") end)
    assert_receive {:delete, "milk"}
  end
```

In order to make the test pass, we need to implement the `KV.Bucket.subscribe/1`. So let's add these three new functions to `KV.Bucket`:

```elixir
  @doc """
  Subscribes the current process to the bucket.
  """
  def subscribe(bucket) do
    GenServer.cast(bucket, {:subscribe, self()})
  end

  @impl true
  def handle_cast({:subscribe, pid}, state) do
    Process.monitor(pid)
    state = update_in(state.subscribers, &MapSet.put(&1, pid))
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, _type, pid, _reason}, state) do
    state = update_in(state.subscribers, &MapSet.delete(&1, pid))
    {:noreply, state}
  end
```

On subscription, we send a `cast/2` request with the current process identifier and implement its `handle_cast/2` callback that receives the `request` and the current server state. We then proceed to monitor the given `pid` and add it to the list of subscribers, which we are implementing using `MapSet`. The `handle_cast/2` callback returns a tuple in the format `{:noreply, updated_state}`. Note that in a real application we would have probably implemented it with a synchronous call, as it provides back pressure, instead of an asynchronous cast. We are doing it this way to illustrate how to implement a cast callback.

Then, because we have monitored a process, once that process terminates, we will receive a "DOWN message". GenServers handle regular messages using the `handle_info/2` callback, which also typically return `{:noreply, updated_state}`. In this callback, we remove the PID that terminated from our list of subscribers.

We are almost there. We can see both `handle_cast/2` and `handle_info/2` callbacks assume there is a subscribers key in our state with a `MapSet`. So let's add it by updating the existing `init/1` to the following:

```elixir
  @impl true
  def init(bucket) do
    state = %{
      bucket: bucket,
      subscribers: MapSet.new()
    }

    {:ok, state}
  end
```

And finally let's update the callbacks for `put/3` and `delete/2` to broadcast messages whenever they are invoked, like this:

```elixir
  def handle_call({:put, key, value}, _from, state) do
    state = put_in(state.bucket[key], value)
    broadcast(state, {:put, key, value})
    {:reply, :ok, state}
  end

  def handle_call({:delete, key}, _from, state) do
    {value, state} = pop_in(state.bucket[key])
    broadcast(state, {:delete, key})
    {:reply, value, state}
  end

  defp broadcast(state, message) do
    for pid <- state.subscribers do
      send(pid, message)
    end
  end
```

There is no need to modify the callback for `get/2`. And that's it, run the tests again, and our new test should pass!

## Wiring it all up

Now that our bucket deals with subscriptions, we need to expose this new functionality in our server. Let's once again start with the test.

Open up `test/kv/server_test.exs` and add this new test:

```elixir
  test "subscribes to buckets", %{socket: socket, name: name} do
    assert send_and_recv(socket, "CREATE #{name}\r\n") == "OK\r\n"
    :gen_tcp.send(socket, "SUBSCRIBE #{name}\r\n")

    {:ok, other} = :gen_tcp.connect(~c"localhost", 4040, @socket_options)

    assert send_and_recv(other, "PUT #{name} milk 3\r\n") == "OK\r\n"
    assert :gen_tcp.recv(socket, 0, 1000) == {:ok, "milk SET TO 3\r\n"}

    assert send_and_recv(other, "DELETE #{name} milk\r\n") == "OK\r\n"
    assert :gen_tcp.recv(socket, 0, 1000) == {:ok, "milk DELETED\r\n"}
  end
```

The test creates a bucket and subscribes to it. Then it opens up another TCP connection to send commands. For each command sent, we expect the subscribed socket to receive a message.

To make the test pass, we need to change `KV.Command` to parse the new `SUBSCRIBE` command and then run it. Open up `lib/kv/commands.ex` and then first change the `parse/1` definition to the following:

```elixir
  def parse(line) do
    case String.split(line) do
      ["SUBSCRIBE", bucket] -> {:ok, {:subscribe, bucket}}
      ["CREATE", bucket] -> {:ok, {:create, bucket}}
      ["GET", bucket, key] -> {:ok, {:get, bucket, key}}
      ["PUT", bucket, key, value] -> {:ok, {:put, bucket, key, value}}
      ["DELETE", bucket, key] -> {:ok, {:delete, bucket, key}}
      _ -> {:error, :unknown_command}
    end
  end
```

We added a new clause that converts "SUBSCRIBE" into a tuple. Now we need to match on this tuple within `run/1`. We can do so by adding a new clause at the bottom of `run/1`, with the following code:

```elixir
  def run({:subscribe, bucket}, socket) do
    lookup(bucket, fn pid ->
      KV.Bucket.subscribe(pid)
      :inet.setopts(socket, active: true)
      receive_messages(socket)
    end)
  end

  defp receive_messages(socket) do
    receive do
      {:put, key, value} ->
        :gen_tcp.send(socket, "#{key} SET TO #{value}\r\n")
        receive_messages(socket)

      {:delete, key} ->
        :gen_tcp.send(socket, "#{key} DELETED\r\n")
        receive_messages(socket)

      {:tcp_closed, ^socket} ->
        {:error, :closed}

      # If we receive any message, including socket writes, we discard them
      _ ->
        receive_messages(socket)
    end
  end
```

Let's go over it by parts. We use the existing `lookup/2` private function to lookup for a bucket. If one is found, we subscribe the current process to the bucket. Then we call `:inet.setopts(socket, active: true)` (which we will explain soon) and `receive_messages/1`.

`receive_messages/1` awaits for messages from the bucket and then calls itself again, becoming a loop. We match on `{:put, key, value}` and `{:delete, key}` and write to those events to the socket. We also match on `{:tcp_closed, ^socket}`, which is a message that will be delivered if the TCP socket closes, and use it to abort the loop. We discard any other message.

At this point you may be wondering: where does `{:tcp_closed, ^socket}` come from?

So far, when receiving messages from the socket, we used `:gen_tcp.recv/3` to perform calls that will block the current process until content is available. This is known as "passive mode". However, we can also ask `:gen_tcp` to stream messages to the current process inbox as they arrive, which is known as "active mode", which is exactly what we configured when we called `:inet.setopts(socket, active: true)`. Those messages have the shape `{:tcp, socket, data}`. When the socket is in active mode and it is closed, it delivers a `{:tcp_closed, socket}` message. Once we receive this message, we exit the loop, which will exit the connection process. Since the bucket is monitoring the process, it will automatically remove the subscription too. You could verify this in practice by adding a `COUNT SUBSCRIPTIONS` command that returns the number of subscribers for a given bucket.

In practice, many systems would prefer to call `:inet.setopts(socket, active: :once)` to specify only a single TCP message should be delivered to avoid overflowing message queues. Once the message is received, they call `:inet.setopts/2` again. In our case, we are simply discarding anything that arrives over the socket, so setting `active: true` is equally fine. In all scenarios, the benefit of using active mode is that the process can receive TCP messages as well as messages from other processes at the same time, instead of blocking on `:gen_tcp.recv/3`.

To wrap it all up, you should give our new feature a try in a distributed setting too. Start two `NODES=... PORT=... iex --sname ... -S mix` instances. In one of them, create a bucket. In the other, subscribe to the same bucket. Once you go back to the first shell, you will see that, even as you send commands to the bucket in one machine, the messages will be streamed to the other one. In other words, our subscription system is also distributed, and all we had to do is to send messages!

## `call`, `cast` or `info`?

So far we have used three callbacks: `handle_call/3`, `handle_cast/2` and `handle_info/2`. Here is what we should consider when deciding when to use each:

1. `handle_call/3` must be used for synchronous requests. This should be the default choice as waiting for the server reply is a useful back-pressure mechanism.

2. `handle_cast/2` must be used for asynchronous requests, when you don't care about a reply. A cast does not guarantee the server has received the message and, for this reason, should be used sparingly. For example, the `subscribe/1` function we have defined in this chapter should have used `call/2`. We have used `cast/2` for educational purposes.

3. `handle_info/2` must be used for all other messages a server may receive that are not sent via `GenServer.call/2` or `GenServer.cast/2`, including regular messages sent with `send/2`. The monitoring `:DOWN` messages are an example of this.

To help developers remember the differences between call, cast and info, the supported return values and more, we have a tiny [GenServer cheat sheet](https://elixir-lang.org/downloads/cheatsheets/gen-server.pdf).

## Agents or GenServers?

Before moving forward to the last chapter, you may be wondering: in the future, should you use an `Agent` or a `GenServer`?

As we saw throughout this guide, agents are straight-forward to get started but they are limited in what they can do. Agents are effectively a subset of GenServers. In fact, agents are implemented on top of GenServers. As well as supervisors, the `Registry` module, and many other features you will find in both Erlang and Elixir.

In other words, GenServers are the most essential component for building concurrent and fault-tolerant systems in Elixir. They provide a robust and flexible framework for managing state and coordinating interactions between processes.

For those reasons, many adopt a rule of thumb to never use Agents and jump straight into GenServers instead. On the other hand, others are more than fine with using agents to store a bit of state here and there. Either way, you will be fine!

This is the last feature we have implemented for our distributed key-value store. In the next chapter, we will learn how to package our application before shipping it to production.
