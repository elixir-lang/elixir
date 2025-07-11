<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Simple state with agents

In this chapter, we will learn how to keep and share state between multiple entities. If you have previous programming experience, you may think of globally shared variables, but the model we will learn here is quite different. The next chapters will generalize the concepts introduced here.

If you have skipped the *Getting Started* guide or read it long ago, be sure to re-read the [Processes](../getting-started/processes.md) chapter. We will use it as a starting point.

## The trouble with (mutable) state

Elixir is an immutable language where nothing is shared by default. If we want to share information, this is typically done by sending messages between processes.

When it comes to processes though, we rarely hand-roll our own, instead we use the abstractions available in Elixir and OTP:

  * `Agent` — Simple wrappers around state.
  * `GenServer` — "Generic servers" (processes) that encapsulate state, provide sync and async calls, support code reloading, and more.
  * `Task` — Asynchronous units of computation that allow spawning a process and potentially retrieving its result at a later time.

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

We started an agent with an initial state of an empty list. The `start_link/1` function returned the `:ok` tuple with a process identifier (PID) of the agent. We will use this PID for all further interactions. We then updated the agent's state, adding our new item to the head of the list. The second argument of `Agent.update/3` is a function that takes the agent's current state as input and returns its desired new state. Finally, we retrieved the whole list. The second argument of `Agent.get/3` is a function that takes the state as input and returns the value that `Agent.get/3` itself will return. Once we are done with the agent, we can call `Agent.stop/3` to terminate the agent process.

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
1) test stores values by key (KV.BucketTest)
   test/kv/bucket_test.exs:4
   ** (UndefinedFunctionError) function KV.Bucket.start_link/1 is undefined (module KV.Bucket is not available)
```

In order to fix the failing test, let's create a file at `lib/kv/bucket.ex` with the contents below. Feel free to give a try at implementing the `KV.Bucket` module yourself using agents before peeking at the implementation below.

```elixir
defmodule KV.Bucket do
  use Agent

  @doc """
  Starts a new bucket.

  All options are forwarded to `Agent.start_link/2`.
  """
  def start_link(opts) do
    Agent.start_link(fn -> %{} end, opts)
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

The first step in our implementation is to call `use Agent`. This is a pattern we will see throughout the guides and understand in depth in the next chapter.

Then we define a `start_link/1` function, which will effectively start the agent. It is a convention to define a `start_link/1` function that always accepts a list of options. We then call `Agent.start_link/2` passing an anonymous function that returns the Agent's initial state and the same list of options we received.

We are keeping a map inside the agent to store our keys and values. Getting and putting values on the map is done with the Agent API and the capture operator `&`, introduced in [the Getting Started guide](../getting-started/anonymous-functions.md#the-capture-operator). The agent passes its state to the anonymous function via the `&1` argument when `Agent.get/2` and `Agent.update/2` are called.

Now that the `KV.Bucket` module has been defined, our test should pass! You can try it yourself by running: `mix test`.

## Naming processes

When starting `KV.Bucket`, we pass a list of options which we forward to `Agent.start_link/2`. One of the options accepted by `Agent.start_link/2` is a name option which allows us to name a process, so we can interact with it using its name instead of its PID.

Let's write a test as an example. Back on `KV.BucketTest`, add this:

```elixir
  test "stores values by key on a named process" do
    {:ok, _} = KV.Bucket.start_link(name: :shopping_list)
    assert KV.Bucket.get(:shopping_list, "milk") == nil

    KV.Bucket.put(:shopping_list, "milk", 3)
    assert KV.Bucket.get(:shopping_list, "milk") == 3
  end
```

However, keep in mind that names are shared in the current node. If two tests attempt to create two processes named `:shopping_list` at the same time, one would succeed and the other would fail. For this reason, it is a common practice in Elixir to name processes started during tests after the test itself, like this:

```elixir
  test "stores values by key on a named process", config do
    {:ok, _} = KV.Bucket.start_link(name: config.test)
    assert KV.Bucket.get(config.test, "milk") == nil

    KV.Bucket.put(config.test, "milk", 3)
    assert KV.Bucket.get(config.test, "milk") == 3
  end
```

The `config` argument, passed after the test name, is the *test context* and it includes configuration and metadata about the current test, which is useful in scenarios like these.

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

Some APIs, such as GenServers, make a clearer distiction between client and server, and we will explore them in future chapters. Next let's talk about naming things, applications, and supervisors.
