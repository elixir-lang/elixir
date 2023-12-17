# Process-related anti-patterns

This document outlines potential anti-patterns related to processes and process-based abstractions.

## Code organization by process

#### Problem

This anti-pattern refers to code that is unnecessarily organized by processes. A process itself does not represent an anti-pattern, but it should only be used to model runtime properties (such as concurrency, access to shared resources, and event scheduling). When you use a process for code organization, it can create bottlenecks in the system.

#### Example

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

#### Problem

In Elixir, the use of an `Agent`, a `GenServer`, or any other process abstraction is not an anti-pattern in itself. However, when the responsibility for direct interaction with a process is spread throughout the entire system, it can become problematic. This bad practice can increase the difficulty of code maintenance and make the code more prone to bugs.

#### Example

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

## Unsupervised processes

#### Problem

In Elixir, creating a process outside a supervision tree is not an anti-pattern in itself. However, when you spawn many long-running processes outside of supervision trees, this can make visibility and monitoring of these processes difficult, preventing developers from fully controlling their applications.

#### Example

The following code example seeks to illustrate a library responsible for maintaining a numerical `Counter` through a `GenServer` process *outside a supervision tree*. Multiple counters can be created simultaneously by a client (one process for each counter), making these *unsupervised* processes difficult to manage. This can cause problems with the initialization, restart, and shutdown of a system.

```elixir
defmodule Counter do
  @moduledoc """
  Global counter implemented through a GenServer process.
  """

  use GenServer

  @doc "Starts a counter process."
  def start(initial_value, name \\ __MODULE__) when is_integer(initial_value) do
    GenServer.start(__MODULE__, initial_value, name: name)
  end

  @doc "Gets the current value of the given counter."
  def get(pid_name \\ __MODULE__) do
    GenServer.call(pid_name, :get)
  end

  @doc "Bumps the value of the given counter."
  def bump(value, pid_name \\ __MODULE__) do
    GenServer.call(pid_name, {:bump, value})
  end

  @impl true
  def init(counter) do
    {:ok, counter}
  end

  @impl true
  def handle_call(:get, _from, counter) do
    {:reply, counter, counter}
  end

  def handle_call({:bump, value}, _from, counter) do
    {:reply, counter, counter + value}
  end
end
```

```elixir
iex> Counter.start(0)
{:ok, #PID<0.115.0>}
iex> Counter.get()
0
iex> Counter.start(15, :other_counter)
{:ok, #PID<0.120.0>}
iex> Counter.get(:other_counter)
15
iex> Counter.bump(-3, :other_counter)
12
iex> Counter.bump(7)
7
```

#### Refactoring

To ensure that clients of a library have full control over their systems, regardless of the number of processes used and the lifetime of each one, all processes must be started inside a supervision tree. As shown below, this code uses a `Supervisor` as a supervision tree. When this Elixir application is started, two different counters (`Counter` and `:other_counter`) are also started as child processes of the `Supervisor` named `App.Supervisor`. Both are initialized to `0`. By means of this supervision tree, it is possible to manage the lifecycle of all child processes (stopping or restarting each one), improving the visibility of the entire app.

```elixir
defmodule SupervisedProcess.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      %{id: Counter, start: {Counter, :start, [0]}},
      %{id: :other_counter, start: {Counter, :start, [0, :other_counter]}}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: App.Supervisor)
  end
end
```

```elixir
iex> Supervisor.count_children(App.Supervisor)
%{active: 2, specs: 2, supervisors: 0, workers: 2}
iex> Counter.get(Counter)
0
iex> Counter.get(:other_counter)
0
iex> Counter.bump(7, Counter)
7
iex> Supervisor.terminate_child(App.Supervisor, Counter)
iex> Supervisor.count_children(App.Supervisor) # Only one active child
%{active: 1, specs: 2, supervisors: 0, workers: 2}
iex> Counter.get(Counter) # The process was terminated
** (EXIT) no process: the process is not alive...
iex> Supervisor.restart_child(App.Supervisor, Counter)
iex> Counter.get(Counter) # After the restart, this process can be used again
0
```

## Sending unnecessary data

#### Problem

Sending a message to a process can be an expensive operation if it is big enough, as messages will be fully copied to the receiving process, which is both CPU and memory intensive. This is due to erlang's "shared nothing" architecture where each process has its own memory, simplifying and speeding up garbage collection.
That this happens is more obvious when using `send/2`, `GenServer.call/3` or the initial data in `GenServer.start_link/3`, however notably this also happens when using `spawn/1`, `Task.async/1`, `Task.async_stream/3` & friends. It is more subtle here as the anonymous function passed to these captures the variables it references in its closure - meaning that data will also be copied over. Hence, you can accidentally send way more data to a process than you actually need.

#### Example

To depict the problem let's imagine you were to implement some simple reporting of ip addresses that made requests against your application. You want to do this asynchronously to not block processing, so you decide to use `spawn/1`. It may seem like a good idea to hand over the whole connection ("We might need more data later!"), however it results in copying a lot of unnecessary data (request body, params etc.).

```elixir
# log_request_ip send the ip to some external service
spawn(fn -> log_request_ip(conn) end)
```

Please note that this problem also occurs if you think you may just be accessing the relevant parts:

```elixir
spawn(fn -> log_request_ip(conn.remote_ip) end)
```

This will still copy over all of `conn`.

#### Refactoring

This anti-pattern has many potential remedies:

* Limiting the data you send to the absolute necessary minimum, instead of just sending the whole struct. For example, don't send an entire `Plug.Conn` struct if all you need is a couple of fields.
* If only the process you send the data to needs it, it may fetch the data itself instead.
* There are some data structures that are shared between processes and hence don't need copying, such as [ets](https://www.erlang.org/doc/man/ets) and [persistent_term](https://www.erlang.org/doc/man/persistent_term.html).

In our case the first, and most common, strategy is applicable. If all we need _right now_ is the ip address, then let's only work with that and make sure that's all we're passing into the closure:

```elixir
ip_address = conn.remote_ip
spawn(fn -> log_request_ip(ip_address) end)
```
