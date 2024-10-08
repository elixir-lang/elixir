# Processes

In Elixir, all code runs inside processes. Processes are isolated from each other, run concurrent to one another and communicate via message passing. Processes are not only the basis for concurrency in Elixir, but they also provide the means for building distributed and fault-tolerant programs.

Elixir's processes should not be confused with operating system processes. Processes in Elixir are extremely lightweight in terms of memory and CPU (even compared to threads as used in many other programming languages). Because of this, it is not uncommon to have tens or even hundreds of thousands of processes running simultaneously.

In this chapter, we will learn about the basic constructs for spawning new processes, as well as sending and receiving messages between processes.

## Spawning processes

The basic mechanism for spawning new processes is the auto-imported `spawn/1` function:

```elixir
iex> spawn(fn -> 1 + 2 end)
#PID<0.43.0>
```

`spawn/1` takes a function which it will execute in another process.

Notice `spawn/1` returns a PID (process identifier). At this point, the process you spawned is very likely dead. The spawned process will execute the given function and exit after the function is done:

```elixir
iex> pid = spawn(fn -> 1 + 2 end)
#PID<0.44.0>
iex> Process.alive?(pid)
false
```

> Note: you will likely get different process identifiers than the ones we are showing in our snippets.

We can retrieve the PID of the current process by calling `self/0`:

```elixir
iex> self()
#PID<0.41.0>
iex> Process.alive?(self())
true
```

Processes get much more interesting when we are able to send and receive messages.

## Sending and receiving messages

We can send messages to a process with `send/2` and receive them with `receive/1`:

```elixir
iex> send(self(), {:hello, "world"})
{:hello, "world"}
iex> receive do
...>   {:hello, msg} -> msg
...>   {:world, _msg} -> "won't match"
...> end
"world"
```

When a message is sent to a process, the message is stored in the process mailbox. The `receive/1` block goes through the current process mailbox searching for a message that matches any of the given patterns. `receive/1` supports guards and many clauses, exactly as `case/2`.

The process that sends the message does not block on `send/2`, it puts the message in the recipient's mailbox and continues. In particular, a process can send messages to itself.

If there is no message in the mailbox matching any of the patterns, the current process will wait until a matching message arrives. A timeout can also be specified:

```elixir
iex> receive do
...>   {:hello, msg}  -> msg
...> after
...>   1_000 -> "nothing after 1s"
...> end
"nothing after 1s"
```

A timeout of 0 can be given when you already expect the message to be in the mailbox.

Let's put it all together and send messages between processes:

```elixir
iex> parent = self()
#PID<0.41.0>
iex> spawn(fn -> send(parent, {:hello, self()}) end)
#PID<0.48.0>
iex> receive do
...>   {:hello, pid} -> "Got hello from #{inspect pid}"
...> end
"Got hello from #PID<0.48.0>"
```

The `inspect/1` function is used to convert a data structure's internal representation into a string, typically for printing. Notice that when the `receive` block gets executed the sender process we have spawned may already be dead, as its only instruction was to send a message.

While in the shell, you may find the helper `flush/0` quite useful. It flushes and prints all the messages in the mailbox.

```elixir
iex> send(self(), :hello)
:hello
iex> flush()
:hello
:ok
```

## Links

The majority of times we spawn processes in Elixir, we spawn them as linked processes. Before we show an example with `spawn_link/1`, let's see what happens when a process started with `spawn/1` fails:

```elixir
iex> spawn(fn -> raise "oops" end)
#PID<0.58.0>

[error] Process #PID<0.58.00> raised an exception
** (RuntimeError) oops
    (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
```

It merely logged an error but the parent process is still running. That's because processes are isolated. If we want the failure in one process to propagate to another one, we should link them. This can be done with `spawn_link/1`:

```elixir
iex> self()
#PID<0.41.0>
iex> spawn_link(fn -> raise "oops" end)

** (EXIT from #PID<0.41.0>) evaluator process exited with reason: an exception was raised:
    ** (RuntimeError) oops
        (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6

[error] Process #PID<0.289.0> raised an exception
** (RuntimeError) oops
    (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
```

Because processes are linked, we now see a message saying the parent process, which is the shell process, has received an EXIT signal from another process causing the shell to terminate. IEx detects this situation and starts a new shell session.

Linking can also be done manually by calling `Process.link/1`. We recommend that you take a look at the `Process` module for other functionality provided by processes.

Processes and links play an important role when building fault-tolerant systems. Elixir processes are isolated and don't share anything by default. Therefore, a failure in a process will never crash or corrupt the state of another process. Links, however, allow processes to establish a relationship in case of failure. We often link our processes to supervisors which will detect when a process dies and start a new process in its place.

While other languages would require us to catch/handle exceptions, in Elixir we are actually fine with letting processes fail because we expect supervisors to properly restart our systems. "Failing fast" (sometimes referred as "let it crash") is a common philosophy when writing Elixir software!

`spawn/1` and `spawn_link/1` are the basic primitives for creating processes in Elixir. Although we have used them exclusively so far, most of the time we are going to use abstractions that build on top of them. Let's see the most common one, called tasks.

## Tasks

Tasks build on top of the spawn functions to provide better error reports and introspection:

```elixir
iex> Task.start(fn -> raise "oops" end)
{:ok, #PID<0.55.0>}

15:22:33.046 [error] Task #PID<0.55.0> started from #PID<0.53.0> terminating
** (RuntimeError) oops
    (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
    (elixir) lib/task/supervised.ex:85: Task.Supervised.do_apply/2
    (stdlib) proc_lib.erl:247: :proc_lib.init_p_do_apply/3
Function: #Function<20.99386804/0 in :erl_eval.expr/5>
    Args: []
```

Instead of `spawn/1` and `spawn_link/1`, we use `Task.start/1` and `Task.start_link/1` which return `{:ok, pid}` rather than just the PID. This is what enables tasks to be used in supervision trees. Furthermore, `Task` provides convenience functions, like `Task.async/1` and `Task.await/1`, and functionality to ease distribution.

We will explore tasks and other abstractions around processes in the ["Mix and OTP guide"](../mix-and-otp/introduction-to-mix.md).

## State

We haven't talked about state so far. If you are building an application that requires state, for example, to keep your application configuration, or you need to parse a file and keep it in memory, where would you store it?

Processes are the most common answer to this question. We can write processes that loop infinitely, maintain state, and send and receive messages. As an example, let's write a module that starts new processes that work as a key-value store in a file named `kv.exs`:

```elixir
defmodule KV do
  def start_link do
    Task.start_link(fn -> loop(%{}) end)
  end

  defp loop(map) do
    receive do
      {:get, key, caller} ->
        send(caller, Map.get(map, key))
        loop(map)
      {:put, key, value} ->
        loop(Map.put(map, key, value))
    end
  end
end
```

Note that the `start_link` function starts a new process that runs the `loop/1` function, starting with an empty map. The `loop/1` (private) function then waits for messages and performs the appropriate action for each message. We made `loop/1` private by using `defp` instead of `def`. In the case of a `:get` message, it sends a message back to the caller and calls `loop/1` again, to wait for a new message. While the `:put` message actually invokes `loop/1` with a new version of the map, with the given `key` and `value` stored.

Let's give it a try by running `iex kv.exs`:

```elixir
iex> {:ok, pid} = KV.start_link()
{:ok, #PID<0.62.0>}
iex> send(pid, {:get, :hello, self()})
{:get, :hello, #PID<0.41.0>}
iex> flush()
nil
:ok
```

At first, the process map has no keys, so sending a `:get` message and then flushing the current process inbox returns `nil`. Let's send a `:put` message and try it again:

```elixir
iex> send(pid, {:put, :hello, :world})
{:put, :hello, :world}
iex> send(pid, {:get, :hello, self()})
{:get, :hello, #PID<0.41.0>}
iex> flush()
:world
:ok
```

Notice how the process is keeping a state and we can get and update this state by sending the process messages. In fact, any process that knows the `pid` above will be able to send it messages and manipulate the state.

It is also possible to register the `pid`, giving it a name, and allowing everyone that knows the name to send it messages:

```elixir
iex> Process.register(pid, :kv)
true
iex> send(:kv, {:get, :hello, self()})
{:get, :hello, #PID<0.41.0>}
iex> flush()
:world
:ok
```

Using processes to maintain state and name registration are very common patterns in Elixir applications. However, most of the time, we won't implement those patterns manually as above, but by using one of the many abstractions that ship with Elixir. For example, Elixir provides `Agent`s, which are simple abstractions around state. Our code above could be directly written as:

```elixir
iex> {:ok, pid} = Agent.start_link(fn -> %{} end)
{:ok, #PID<0.72.0>}
iex> Agent.update(pid, fn map -> Map.put(map, :hello, :world) end)
:ok
iex> Agent.get(pid, fn map -> Map.get(map, :hello) end)
:world
```

A `:name` option could also be given to `Agent.start_link/2` and it would be automatically registered. Besides agents, Elixir provides an API for building generic servers (called `GenServer`), registries, and more, all powered by processes underneath. Those, along with supervision trees, will be explored with more detail in the ["Mix and OTP guide"](../mix-and-otp/introduction-to-mix.md), which will build a complete Elixir application from start to finish.

For now, let's move on and explore the world of I/O in Elixir.
