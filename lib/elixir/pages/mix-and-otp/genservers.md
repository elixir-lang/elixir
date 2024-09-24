# Client-server communication with GenServer

In the [previous chapter](agents.md), we used agents to represent our buckets. In the [introduction to mix](introduction-to-mix.md), we specified we would like to name each bucket so we can do the following:

```elixir
CREATE shopping
OK

PUT shopping milk 1
OK

GET shopping milk
1
OK
```

In the session above we interacted with the "shopping" bucket.

Since agents are processes, each bucket has a process identifier (PID), but buckets do not have a name. Back [in the Process chapter](../getting-started/processes.md), we have learned that we can register processes in Elixir by giving them atom names:

```elixir
iex> Agent.start_link(fn -> %{} end, name: :shopping)
{:ok, #PID<0.43.0>}
iex> KV.Bucket.put(:shopping, "milk", 1)
:ok
iex> KV.Bucket.get(:shopping, "milk")
1
```

However, naming dynamic processes with atoms is a terrible idea! If we use atoms, we would need to convert the bucket name (often received from an external client) to atoms, and **we should never convert user input to atoms**. This is because atoms are not garbage collected. Once an atom is created, it is never reclaimed. Generating atoms from user input would mean the user can inject enough different names to exhaust our system memory!

In practice, it is more likely you will reach the Erlang VM limit for the maximum number of atoms before you run out of memory, which will bring your system down regardless.

Instead of abusing the built-in name facility, we will create our own *process registry* that associates the bucket name to the bucket process.

The registry needs to guarantee that it is always up to date. For example, if one of the bucket processes crashes due to a bug, the registry must notice this change and avoid serving stale entries. In Elixir, we say the registry needs to *monitor* each bucket. Because our *registry* needs to be able to receive and handle ad-hoc messages from the system, the `Agent` API is not enough.

We will use a `GenServer` to create a registry process that can monitor the bucket processes. GenServer provides industrial strength functionality for building servers in both Elixir and OTP.

Please read the `GenServer` module documentation for an overview if you haven't yet. Once you do so, we are ready to proceed.

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

There is quite a bit more ceremony in the GenServer code but, as we will see, it brings some benefits too.

For now, we will write only the server callbacks for our bucket registering logic, without providing a proper API, which we will do later.

Create a new file at `lib/kv/registry.ex` with the following contents:

```elixir
defmodule KV.Registry do
  use GenServer

  ## Missing Client API - will add this later

  ## Defining GenServer Callbacks

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:lookup, name}, _from, names) do
    {:reply, Map.fetch(names, name), names}
  end

  @impl true
  def handle_cast({:create, name}, names) do
    if Map.has_key?(names, name) do
      {:noreply, names}
    else
      {:ok, bucket} = KV.Bucket.start_link([])
      {:noreply, Map.put(names, name, bucket)}
    end
  end
end
```

There are two types of requests you can send to a GenServer: calls and casts. Calls are synchronous and the server **must** send a response back to such requests. While the server computes the response, the client is **waiting**. Casts are asynchronous: the server won't send a response back and therefore the client won't wait for one. Both requests are messages sent to the server, and will be handled in sequence. In the above implementation, we pattern-match on the `:create` messages, to be handled as cast, and on the `:lookup` messages, to be handled as call.

In order to invoke the callbacks above, we need to go through the corresponding `GenServer` functions. Let's start a registry, create a named bucket, and then look it up:

```elixir
iex> {:ok, registry} = GenServer.start_link(KV.Registry, :ok)
{:ok, #PID<0.136.0>}
iex> GenServer.cast(registry, {:create, "shopping"})
:ok
iex> {:ok, bk} = GenServer.call(registry, {:lookup, "shopping"})
{:ok, #PID<0.174.0>}
```

Our `KV.Registry` process received a cast with `{:create, "shopping"}` and a call with `{:lookup, "shopping"}`, in this sequence. `GenServer.cast` will immediately return, as soon as the message is sent to the `registry`. The `GenServer.call` on the other hand, is where we would be waiting for an answer, provided by the above `KV.Registry.handle_call` callback.

You may also have noticed that we have added `@impl true` before each callback. The `@impl true` informs the compiler that our intention for the subsequent function definition is to define a callback. If by any chance we make a mistake in the function name or in the number of arguments, like we define a `handle_call/2`, the compiler would warn us there isn't any `handle_call/2` to define, and would give us the complete list of known callbacks for the `GenServer` module.

This is all good and well, but we still want to offer our users an API that allows us to hide our implementation details.

## The Client API

A GenServer is implemented in two parts: the client API and the server callbacks. You can either combine both parts into a single module or you can separate them into a client module and a server module. The client is any process that invokes the client function. The server is always the process identifier or process name that we will explicitly pass as argument to the client API. Here we'll use a single module for both the server callbacks and the client API.

Edit the file at `lib/kv/registry.ex`, filling in the blanks for the client API:

```elixir
  ## Client API

  @doc """
  Starts the registry.
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @doc """
  Looks up the bucket pid for `name` stored in `server`.

  Returns `{:ok, pid}` if the bucket exists, `:error` otherwise.
  """
  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  @doc """
  Ensures there is a bucket associated with the given `name` in `server`.
  """
  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end
```

The first function is `start_link/1`, which starts a new GenServer passing a list of options. `start_link/1` calls out to `GenServer.start_link/3`, which takes three arguments:

1. The module where the server callbacks are implemented, in this case `__MODULE__` (meaning the current module)

2. The initialization arguments, in this case the atom `:ok`

3. A list of options which can be used to specify things like the name of the server. For now, we forward the list of options that we receive on `start_link/1` to `GenServer.start_link/3`

The next two functions, `lookup/2` and `create/2`, are responsible for sending these requests to the server.  In this case, we have used `{:lookup, name}` and `{:create, name}` respectively.  Requests are often specified as tuples, like this, in order to provide more than one "argument" in that first argument slot. It's common to specify the action being requested as the first element of a tuple, and arguments for that action in the remaining elements. Note that the requests must match the first argument to `handle_call/3` or `handle_cast/2`.

That's it for the client API. On the server side, we can implement a variety of callbacks to guarantee the server initialization, termination, and handling of requests. Those callbacks are optional and for now, we have only implemented the ones we care about. Let's recap.

The first is the `init/1` callback, that receives the second argument given to `GenServer.start_link/3` and returns `{:ok, state}`, where state is a new map. We can already notice how the `GenServer` API makes the client/server segregation more apparent. `start_link/3` happens in the client, while `init/1` is the respective callback that runs on the server.

For `call/2` requests, we implement a `handle_call/3` callback that receives the `request`, the process from which we received the request (`_from`), and the current server state (`names`). The `handle_call/3` callback returns a tuple in the format `{:reply, reply, new_state}`. The first element of the tuple, `:reply`, indicates that the server should send a reply back to the client. The second element, `reply`, is what will be sent to the client while the third, `new_state` is the new server state.

For `cast/2` requests, we implement a `handle_cast/2` callback that receives the `request` and the current server state (`names`). The `handle_cast/2` callback returns a tuple in the format `{:noreply, new_state}`. Note that in a real application we would have probably implemented the callback for `:create` with a synchronous call instead of an asynchronous cast. We are doing it this way to illustrate how to implement a cast callback.

There are other tuple formats both `handle_call/3` and `handle_cast/2` callbacks may return. There are other callbacks like `terminate/2` and `code_change/3` that we could implement. You are welcome to explore the full `GenServer` documentation to learn more about those.

For now, let's write some tests to guarantee our GenServer works as expected.

## Testing a GenServer

Testing a GenServer is not much different from testing an agent. We will spawn the server on a setup callback and use it throughout our tests. Create a file at `test/kv/registry_test.exs` with the following:

```elixir
defmodule KV.RegistryTest do
  use ExUnit.Case, async: true

  setup do
    registry = start_supervised!(KV.Registry)
    %{registry: registry}
  end

  test "spawns buckets", %{registry: registry} do
    assert KV.Registry.lookup(registry, "shopping") == :error

    KV.Registry.create(registry, "shopping")
    assert {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

    KV.Bucket.put(bucket, "milk", 1)
    assert KV.Bucket.get(bucket, "milk") == 1
  end
end
```

Our test case first asserts there are no buckets in our registry, creates a named bucket, looks it up, and asserts it behaves as a bucket.

There is one important difference between the `setup` block we wrote for `KV.Registry` and the one we wrote for `KV.Bucket`. Instead of starting the registry by hand by calling `KV.Registry.start_link/1`, we instead called the `ExUnit.Callbacks.start_supervised!/2` function, passing the `KV.Registry` module.

The `start_supervised!` function was injected into our test module by `use ExUnit.Case`. It does the job of starting the `KV.Registry` process, by calling its `start_link/1` function. The advantage of using `start_supervised!` is that ExUnit will guarantee that the registry process will be shutdown **before** the next test starts. In other words, it helps guarantee that the state of one test is not going to interfere with the next one in case they depend on shared resources.

When starting processes during your tests, we should always prefer to use `start_supervised!`. We recommend you to change the `setup` block in `bucket_test.exs` to use `start_supervised!` too.

Run the tests and they should all pass!

## The need for monitoring

Everything we have done so far could have been implemented with a `Agent`. In this section, we will see one of many things that we can achieve with a GenServer that is not possible with an Agent.

Let's start with a test that describes how we want the registry to behave if a bucket stops or crashes:

```elixir
test "removes buckets on exit", %{registry: registry} do
  KV.Registry.create(registry, "shopping")
  {:ok, bucket} = KV.Registry.lookup(registry, "shopping")
  Agent.stop(bucket)
  assert KV.Registry.lookup(registry, "shopping") == :error
end
```

The test above will fail on the last assertion as the bucket name remains in the registry even after we stop the bucket process.

In order to fix this bug, we need the registry to monitor every bucket it spawns. Once we set up a monitor, the registry will receive a notification every time a bucket process exits, allowing us to clean the registry up.

Let's first play with monitors by starting a new console with `iex -S mix`:

```elixir
iex> {:ok, pid} = KV.Bucket.start_link([])
{:ok, #PID<0.66.0>}
iex> Process.monitor(pid)
#Reference<0.0.0.551>
iex> Agent.stop(pid)
:ok
iex> flush()
{:DOWN, #Reference<0.0.0.551>, :process, #PID<0.66.0>, :normal}
```

Note `Process.monitor(pid)` returns a unique reference that allows us to match upcoming messages to that monitoring reference. After we stop the agent, we can `flush/0` all messages and notice a `:DOWN` message arrived, with the exact reference returned by `monitor`, notifying that the bucket process exited with reason `:normal`.

Let's reimplement the server callbacks to fix the bug and make the test pass. First, we will modify the GenServer state to two dictionaries: one that contains `name -> pid` and another that holds `ref -> name`. Then we need to monitor the buckets on `handle_cast/2` as well as implement a `handle_info/2` callback to handle the monitoring messages. The full server callbacks implementation is shown below:

```elixir
## Server callbacks

@impl true
def init(:ok) do
  names = %{}
  refs = %{}
  {:ok, {names, refs}}
end

@impl true
def handle_call({:lookup, name}, _from, state) do
  {names, _} = state
  {:reply, Map.fetch(names, name), state}
end

@impl true
def handle_cast({:create, name}, {names, refs}) do
  if Map.has_key?(names, name) do
    {:noreply, {names, refs}}
  else
    {:ok, bucket} = KV.Bucket.start_link([])
    ref = Process.monitor(bucket)
    refs = Map.put(refs, ref, name)
    names = Map.put(names, name, bucket)
    {:noreply, {names, refs}}
  end
end

@impl true
def handle_info({:DOWN, ref, :process, _pid, _reason}, {names, refs}) do
  {name, refs} = Map.pop(refs, ref)
  names = Map.delete(names, name)
  {:noreply, {names, refs}}
end

@impl true
def handle_info(msg, state) do
  require Logger
  Logger.debug("Unexpected message in KV.Registry: #{inspect(msg)}")
  {:noreply, state}
end
```

Observe that we were able to considerably change the server implementation without changing any of the client API. That's one of the benefits of explicitly segregating the server and the client.

Finally, different from the other callbacks, we have defined a "catch-all" clause for `handle_info/2` that discards and logs any unknown message. To understand why, let's move on to the next section.

## `call`, `cast` or `info`?

So far we have used three callbacks: `handle_call/3`, `handle_cast/2` and `handle_info/2`. Here is what we should consider when deciding when to use each:

1. `handle_call/3` must be used for synchronous requests. This should be the default choice as waiting for the server reply is a useful back-pressure mechanism.

2. `handle_cast/2` must be used for asynchronous requests, when you don't care about a reply. A cast does not guarantee the server has received the message and, for this reason, should be used sparingly. For example, the `create/2` function we have defined in this chapter should have used `call/2`. We have used `cast/2` for didactic purposes.

3. `handle_info/2` must be used for all other messages a server may receive that are not sent via `GenServer.call/2` or `GenServer.cast/2`, including regular messages sent with `send/2`. The monitoring `:DOWN` messages are an example of this.

Since any message, including the ones sent via `send/2`, go to `handle_info/2`, there is a chance that unexpected messages will arrive to the server. Therefore, if we don't define the catch-all clause, those messages could cause our registry to crash, because no clause would match. We don't need to worry about such cases for `handle_call/3` and `handle_cast/2` though. Calls and casts are only done via the `GenServer` API, so an unknown message is quite likely a developer mistake.

To help developers remember the differences between call, cast and info, the supported return values and more, we have a tiny [GenServer cheat sheet](https://elixir-lang.org/downloads/cheatsheets/gen-server.pdf).

## Monitors or links?

We have previously learned about links in the [Process chapter](../getting-started/processes.md). Now, with the registry complete, you may be wondering: when should we use monitors and when should we use links?

Links are bi-directional. If you link two processes and one of them crashes, the other side will crash too (unless it is trapping exits). A monitor is uni-directional: only the monitoring process will receive notifications about the monitored one. In other words: use links when you want linked crashes, and monitors when you just want to be informed of crashes, exits, and so on.

Returning to our `handle_cast/2` implementation, you can see the registry is both linking and monitoring the buckets:

```elixir
{:ok, bucket} = KV.Bucket.start_link([])
ref = Process.monitor(bucket)
```

This is a bad idea, as we don't want the registry to crash when a bucket crashes. The proper fix is to actually not link the bucket to the registry. Instead, we will link each bucket to a special type of process called Supervisors, which are explicitly designed to handle failures and crashes. We will learn more about them in the next chapter.
