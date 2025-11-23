<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Process-related anti-patterns

This document outlines potential anti-patterns related to processes and process-based abstractions.

## Code organization by process

### Problem

This anti-pattern refers to code that is unnecessarily organized by processes. A process itself does not represent an anti-pattern, but it should only be used to model runtime properties (such as concurrency, access to shared resources, error isolation, etc). When you use a process for code organization, it can create bottlenecks in the system.

### Example

An example of this anti-pattern, as shown below, is a module that implements arithmetic operations (like `add` and `subtract`) by means of a `GenServer` process. If the number of calls to this single process grows, this code organization can compromise the system performance, therefore becoming a bottleneck.

```elixir
defmodule Calculator do
  @moduledoc """
  Calculator that performs basic arithmetic operations.

  This code is unnecessarily organized in a GenServer process.
  """

  use GenServer

  def add(a, b, pid) do
    GenServer.call(pid, {:add, a, b})
  end

  def subtract(a, b, pid) do
    GenServer.call(pid, {:subtract, a, b})
  end

  @impl GenServer
  def init(init_arg) do
    {:ok, init_arg}
  end

  @impl GenServer
  def handle_call({:add, a, b}, _from, state) do
    {:reply, a + b, state}
  end

  def handle_call({:subtract, a, b}, _from, state) do
    {:reply, a - b, state}
  end
end
```

```elixir
iex> {:ok, pid} = GenServer.start_link(Calculator, :init)
{:ok, #PID<0.132.0>}
iex> Calculator.add(1, 5, pid)
6
iex> Calculator.subtract(2, 3, pid)
-1
```

#### Refactoring

In Elixir, as shown next, code organization must be done only through modules and functions. Whenever possible, a library should not impose specific behavior (such as parallelization) on its users. It is better to delegate this behavioral decision to the developers of clients, thus increasing the potential for code reuse of a library.

```elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end

  def subtract(a, b) do
    a - b
  end
end
```

```elixir
iex> Calculator.add(1, 5)
6
iex> Calculator.subtract(2, 3)
-1
```

## Scattered process interfaces

### Problem

In Elixir, the use of an `Agent`, a `GenServer`, or any other process abstraction is not an anti-pattern in itself. However, when the responsibility for direct interaction with a process is spread throughout the entire system, it can become problematic. This bad practice can increase the difficulty of code maintenance and make the code more prone to bugs.

### Example

The following code seeks to illustrate this anti-pattern. The responsibility for interacting directly with the `Agent` is spread across four different modules (`A`, `B`, `C`, and `D`).

```elixir
defmodule A do
  def update(process) do
    # Some other code...
    Agent.update(process, fn _list -> 123 end)
  end
end
```

```elixir
defmodule B do
  def update(process) do
    # Some other code...
    Agent.update(process, fn content -> %{a: content} end)
  end
end
```

```elixir
defmodule C do
  def update(process) do
    # Some other code...
    Agent.update(process, fn content -> [:atom_value | content] end)
  end
end
```

```elixir
defmodule D do
  def get(process) do
    # Some other code...
    Agent.get(process, fn content -> content end)
  end
end
```

This spreading of responsibility can generate duplicated code and make code maintenance more difficult. Also, due to the lack of control over the format of the shared data, complex composed data can be shared. This freedom to use any format of data is dangerous and can induce developers to introduce bugs.

```elixir
# start an agent with initial state of an empty list
iex> {:ok, agent} = Agent.start_link(fn -> [] end)
{:ok, #PID<0.135.0>}

# many data formats (for example, List, Map, Integer, Atom) are
# combined through direct access spread across the entire system
iex> A.update(agent)
iex> B.update(agent)
iex> C.update(agent)

# state of shared information
iex> D.get(agent)
[:atom_value, %{a: 123}]
```

For a `GenServer` and other behaviours, this anti-pattern will manifest when scattering calls to `GenServer.call/3` and `GenServer.cast/2` throughout multiple modules, instead of encapsulating all the interaction with the `GenServer` in a single place.

#### Refactoring

Instead of spreading direct access to a process abstraction, such as `Agent`, over many places in the code, it is better to refactor this code by centralizing the responsibility for interacting with a process in a single module. This refactoring improves maintainability by removing duplicated code; it also allows you to limit the accepted format for shared data, reducing bug-proneness. As shown below, the module `Foo.Bucket` is centralizing the responsibility for interacting with the `Agent`. Any other place in the code that needs to access shared data must now delegate this action to `Foo.Bucket`. Also, `Foo.Bucket` now only allows data to be shared in `Map` format.

```elixir
defmodule Foo.Bucket do
  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end)
  end

  def get(bucket, key) do
    Agent.get(bucket, &Map.get(&1, key))
  end

  def put(bucket, key, value) do
    Agent.update(bucket, &Map.put(&1, key, value))
  end
end
```

The following are examples of how to delegate access to shared data (provided by an `Agent`) to `Foo.Bucket`.

```elixir
# start an agent through `Foo.Bucket`
iex> {:ok, bucket} = Foo.Bucket.start_link(%{})
{:ok, #PID<0.114.0>}

# add shared values to the keys `milk` and `beer`
iex> Foo.Bucket.put(bucket, "milk", 3)
iex> Foo.Bucket.put(bucket, "beer", 7)

# access shared data of specific keys
iex> Foo.Bucket.get(bucket, "beer")
7
iex> Foo.Bucket.get(bucket, "milk")
3
```

#### Additional remarks

This anti-pattern was formerly known as [Agent obsession](https://github.com/lucasvegi/Elixir-Code-Smells/tree/main#agent-obsession).

## Sending unnecessary data

### Problem

Sending a message to a process can be an expensive operation if the message is big enough. That's because that message will be fully copied to the receiving process, which may be CPU and memory intensive. This is due to Erlang's "share nothing" architecture, where each process has its own memory, which simplifies and speeds up garbage collection.

This is more obvious when using `send/2`, `GenServer.call/3`, or the initial data in `GenServer.start_link/3`. Notably this also happens when using `spawn/1`, `Task.async/1`, `Task.async_stream/3`, and so on. It is more subtle here as the anonymous function passed to these functions captures the variables it references, and all captured variables will be copied over. By doing this, you can accidentally send way more data to a process than you actually need.

### Example

Imagine you were to implement some simple reporting of IP addresses that made requests against your application. You want to do this asynchronously and not block processing, so you decide to use `spawn/1`. It may seem like a good idea to hand over the whole connection because we might need more data later. However passing the connection results in copying a lot of unnecessary data like the request body, params, etc.

```elixir
# log_request_ip send the ip to some external service
spawn(fn -> log_request_ip(conn) end)
```

This problem also occurs when accessing only the relevant parts:

```elixir
spawn(fn -> log_request_ip(conn.remote_ip) end)
```

This will still copy over all of `conn`, because the `conn` variable is being captured inside the spawned function. The function then extracts the `remote_ip` field, but only after the whole `conn` has been copied over.

`send/2` and the `GenServer` APIs also rely on message passing. In the example below, the `conn` is once again copied to the underlying `GenServer`:

```elixir
GenServer.cast(pid, {:report_ip_address, conn})
```

#### Refactoring

This anti-pattern has many potential remedies:

  * Limit the data you send to the absolute necessary minimum instead of sending an entire struct. For example, don't send an entire `conn` struct if all you need is a couple of fields.

  * If the only process that needs data is the one you are sending to, consider making the process fetch that data instead of passing it.

  * Some abstractions, such as [`:persistent_term`](`:persistent_term`), allows you to share data between processes, as long as such data changes infrequently.

In our case, limiting the input data is a reasonable strategy. If all we need *right now* is the IP address, then let's only work with that and make sure we're only passing the IP address into the closure, like so:

```elixir
ip_address = conn.remote_ip
spawn(fn -> log_request_ip(ip_address) end)
```

Or in the `GenServer` case:

```elixir
GenServer.cast(pid, {:report_ip_address, conn.remote_ip})
```

## Unsupervised processes

### Problem

In Elixir, creating a process outside a supervision tree is not an anti-pattern in itself. However, when you spawn many long-running processes outside of supervision trees, this can make visibility and monitoring of these processes difficult, preventing developers from fully controlling their lifecycle.

### Example

The following code example seeks to illustrate a library responsible for maintaining a numerical `Counter` through a `Agent` process *outside a supervision tree*.

```elixir
defmodule Counter do
  @moduledoc """
  Global counter implemented as an Agent.
  """

  use Agent

  @doc "Starts a counter process."
  def start_link(opts \\ []) do
    initial_state = Keyword.get(opts, :initial_value, 0)
    name = Keyword.get(opts, :name, __MODULE__)
    Agent.start_link(fn -> initial_state end, name: name)
  end

  @doc "Gets the current value of the given counter."
  def get(name \\ __MODULE__) do
    Agent.get(name, fn state -> state end)
  end

  @doc "Bumps the value of the given counter."
  def bump(name \\ __MODULE__, value) do
    Agent.get_and_update(fn state -> {state, value + state} end)
  end
end
```

While it is possible to start the process outside of a supervision tree:

```elixir
iex> Counter.start_link()
{:ok, #PID<0.115.0>}
iex> Counter.bump(13)
0
iex> Counter.get()
13
```

Such processes are harder to observe and control their lifecycle. For example, if you have other processes that depend on the `Counter` above, you will need ad-hoc mechanisms to make sure they are initialized in order. Furthermore, when your application is shutting down, there is no guarantee when they are terminated.

#### Refactoring

To ensure that clients of a library have full control over their systems, regardless of the number of processes used and the lifetime of each one, all processes must be started inside a supervision tree. As shown below, this code uses a `Supervisor` as a supervision tree.

```elixir
defmodule SupervisedProcess.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # With the default values for counter and name
      Counter,
      # With custom values for counter, name, and a custom ID
      Supervisor.child_spec(
        {Counter, name: :other_counter, initial_value: 15},
        id: :other_counter
      )
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: App.Supervisor)
  end
end
```

Besides having a deterministic order in which processes are started, supervision trees also guarantee they are terminated in reverse order, allowing you to perform any necessary clean up during shut down. Furthermore, supervision strategies allows us to configure exactly how process should act in case of unexpected failures.

Finally, applications and supervision trees can be introspected through applications like the [Phoenix.LiveDashboard](http://github.com/phoenixframework/phoenix_live_dashboard) and [Erlang's built-in observer](https://www.erlang.org/doc/apps/observer/observer_ug):

<img src="assets/kv-observer.png" alt="Observer GUI screenshot" />
