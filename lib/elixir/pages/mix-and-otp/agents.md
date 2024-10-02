# Simple state management with agents

In this chapter, we will learn how to keep and share state between multiple entities. If you have previous programming experience, you may think of globally shared variables, but the model we will learn here is quite different. The next chapters will generalize the concepts introduced here.

If you have skipped the *Getting Started* guide or read it long ago, be sure to re-read the [Processes](../getting-started/processes.md) chapter. We will use it as a starting point.

## The trouble with (mutable) state

Elixir is an immutable language where nothing is shared by default. If we want to share information, which can be read and modified from multiple places, we have two main options in Elixir:

  * Using processes and message passing
  * [ETS (Erlang Term Storage)](http://www.erlang.org/doc/man/ets.html)

We covered processes in the *Getting Started* guide. ETS (Erlang Term Storage) is a new topic that we will explore in later chapters. When it comes to processes though, we rarely hand-roll our own, instead we use the abstractions available in Elixir and OTP:

  * `Agent` — Simple wrappers around state.
  * `GenServer` — "Generic servers" (processes) that encapsulate state, provide sync and async calls, support code reloading, and more.
  * `Task` — Asynchronous units of computation that allow spawning a process and potentially retrieving its result at a later time.

We will explore these abstractions as we move forward. Keep in mind that they are all implemented on top of processes using the basic features provided by the VM, like `send/2`, `receive/1`, `spawn/1` and `Process.link/1`.

Here, we will use agents, and create a module named `KV.Bucket`, responsible for storing our key-value entries in a way that allows them to be read and modified by other processes.

## Agents 101

`Agent`s are simple wrappers around state. If all you want from a process is to keep state, agents are a great fit. Let's start a `iex` session inside the project with:

```console
$ iex -S mix
```

And play a bit with agents:

```elixir
iex> {:ok, agent} = Agent.start_link(fn -> [] end)
{:ok, #PID<0.57.0>}
iex> Agent.update(agent, fn list -> ["eggs" | list] end)
:ok
iex> Agent.get(agent, fn list -> list end)
["eggs"]
iex> Agent.stop(agent)
:ok
```

We started an agent with an initial state of an empty list. We updated the agent's state, adding our new item to the head of the list. The second argument of `Agent.update/3` is a function that takes the agent's current state as input and returns its desired new state. Finally, we retrieved the whole list. The second argument of `Agent.get/3` is a function that takes the state as input and returns the value that `Agent.get/3` itself will return. Once we are done with the agent, we can call `Agent.stop/3` to terminate the agent process.

The `Agent.update/3` function accepts as a second argument any function that receives one argument and returns a value:

```elixir
iex> {:ok, agent} = Agent.start_link(fn -> [] end)
{:ok, #PID<0.338.0>}
iex> Agent.update(agent, fn _list -> 123 end)
:ok
iex> Agent.update(agent, fn content -> %{a: content} end)
:ok
iex> Agent.update(agent, fn content -> [12 | [content]] end)
:ok
iex> Agent.update(agent, fn list -> [:nop | list] end)
:ok
iex> Agent.get(agent, fn content -> content end)
[:nop, 12, %{a: 123}]
```

As you can see, we can modify the agent state in any way we want. Therefore, we most likely don't want to access the Agent API throughout many different places in our code. Instead, we want to encapsulate all Agent-related functionality in a single module, which we will call `KV.Bucket`. Before we implement it, let's write some tests which will outline the API exposed by our module.

Create a file at `test/kv/bucket_test.exs` (remember the `.exs` extension) with the following:

```elixir
defmodule KV.BucketTest do
  use ExUnit.Case, async: true

  test "stores values by key" do
    {:ok, bucket} = KV.Bucket.start_link([])
    assert KV.Bucket.get(bucket, "milk") == nil

    KV.Bucket.put(bucket, "milk", 3)
    assert KV.Bucket.get(bucket, "milk") == 3
  end
end
```

`use ExUnit.Case` is responsible for setting up our module for testing and imports many test-related functionality, such as the `test/2` macro.

Our first test starts a new `KV.Bucket` by calling the `start_link/1` and passing an empty list of options. Then we perform some `get/2` and `put/3` operations on it, asserting the result.

Also note the `async: true` option passed to `ExUnit.Case`. This option makes the test case run in parallel with other `:async` test cases by using multiple cores in our machine. This is extremely useful to speed up our test suite. However, `:async` must *only* be set if the test case does not rely on or change any global values. For example, if the test requires writing to the file system or access a database, keep it synchronous (omit the `:async` option) to avoid race conditions between tests.

Async or not, our new test should obviously fail, as none of the functionality is implemented in the module being tested:

```text
** (UndefinedFunctionError) function KV.Bucket.start_link/1 is undefined (module KV.Bucket is not available)
```

In order to fix the failing test, let's create a file at `lib/kv/bucket.ex` with the contents below. Feel free to give a try at implementing the `KV.Bucket` module yourself using agents before peeking at the implementation below.

```elixir
defmodule KV.Bucket do
  use Agent

  @doc """
  Starts a new bucket.
  """
  def start_link(_opts) do
    Agent.start_link(fn -> %{} end)
  end

  @doc """
  Gets a value from the `bucket` by `key`.
  """
  def get(bucket, key) do
    Agent.get(bucket, &Map.get(&1, key))
  end

  @doc """
  Puts the `value` for the given `key` in the `bucket`.
  """
  def put(bucket, key, value) do
    Agent.update(bucket, &Map.put(&1, key, value))
  end
end
```

The first step in our implementation is to call `use Agent`. Most of the functionality we will learn, such as `GenServer` and `Supervisor`, follow this pattern. For all of them, calling `use` generates a `child_spec/1` function with default configuration, which will be handy when we start supervising processes in chapter 4.

Then we define a `start_link/1` function, which will effectively start the agent. It is a convention to define a `start_link/1` function that always accepts a list of options. We don't plan on using any options right now, but we might later on. We then proceed to call `Agent.start_link/1`, which receives an anonymous function that returns the Agent's initial state.

We are keeping a map inside the agent to store our keys and values. Getting and putting values on the map is done with the Agent API and the capture operator `&`, introduced in [the Getting Started guide](../getting-started/anonymous-functions.md#the-capture-operator). The agent passes its state to the anonymous function via the `&1` argument when `Agent.get/2` and `Agent.update/2` are called.

Now that the `KV.Bucket` module has been defined, our test should pass! You can try it yourself by running: `mix test`.

## Test setup with ExUnit callbacks

Before moving on and adding more features to `KV.Bucket`, let's talk about ExUnit callbacks. As you may expect, all `KV.Bucket` tests will require a bucket agent to be up and running. Luckily, ExUnit supports callbacks that allow us to skip such repetitive tasks.

Let's rewrite the test case to use callbacks:

```elixir
defmodule KV.BucketTest do
  use ExUnit.Case, async: true

  setup do
    {:ok, bucket} = KV.Bucket.start_link([])
    %{bucket: bucket}
  end

  test "stores values by key", %{bucket: bucket} do
    assert KV.Bucket.get(bucket, "milk") == nil

    KV.Bucket.put(bucket, "milk", 3)
    assert KV.Bucket.get(bucket, "milk") == 3
  end
end
```

We have first defined a setup callback with the help of the `setup/1` macro. The `setup/1` macro defines a callback that is run before every test, in the same process as the test itself.

Note that we need a mechanism to pass the `bucket` PID from the callback to the test. We do so by using the *test context*. When we return `%{bucket: bucket}` from the callback, ExUnit will merge this map into the test context. Since the test context is a map itself, we can pattern match the bucket out of it, providing access to the bucket inside the test:

```elixir
test "stores values by key", %{bucket: bucket} do
  # `bucket` is now the bucket from the setup block
end
```

You can read more about ExUnit cases in the [`ExUnit.Case` module documentation](`ExUnit.Case`) and more about callbacks in `ExUnit.Callbacks`.

## Other agent actions

Besides getting a value and updating the agent state, agents allow us to get a value and update the agent state in one function call via `Agent.get_and_update/2`. Let's implement a `KV.Bucket.delete/2` function that deletes a key from the bucket, returning its current value:

```elixir
@doc """
Deletes `key` from `bucket`.

Returns the current value of `key`, if `key` exists.
"""
def delete(bucket, key) do
  Agent.get_and_update(bucket, &Map.pop(&1, key))
end
```

Now it is your turn to write a test for the functionality above! Also, be sure to explore [the documentation for the `Agent` module](`Agent`) to learn more about them.

## Client/server in agents

Before we move on to the next chapter, let's discuss the client/server dichotomy in agents. Let's expand the `delete/2` function we have just implemented:

```elixir
def delete(bucket, key) do
  Agent.get_and_update(bucket, fn map ->
    Map.pop(map, key)
  end)
end
```

Everything that is inside the function we passed to the agent happens in the agent process. In this case, since the agent process is the one receiving and responding to our messages, we say the agent process is the server. Everything outside the function is happening in the client.

This distinction is important. If there are expensive actions to be done, you must consider if it will be better to perform these actions on the client or on the server. For example:

```elixir
def delete(bucket, key) do
  Process.sleep(1000) # puts client to sleep
  Agent.get_and_update(bucket, fn map ->
    Process.sleep(1000) # puts server to sleep
    Map.pop(map, key)
  end)
end
```

When a long action is performed on the server, all other requests to that particular server will wait until the action is done, which may cause some clients to timeout.

In the next chapter, we will explore GenServers, where the segregation between clients and servers is made more apparent.
