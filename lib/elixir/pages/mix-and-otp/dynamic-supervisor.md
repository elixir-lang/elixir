<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Supervising dynamic children

We have successfully learned how our supervision tree is automatically started (and stopped) as part of our application's life cycle. We can also name our buckets via the `:name` option. We also learned that, in practice, we should always start new processes inside supervisors. Let's apply these insights by ensuring our buckets are named and supervised.

## Child specs

Supervisors know how to start processes because they are given "child specifications". In our `lib/kv.ex` file, we defined a list of children with a single child spec:

```elixir
    children = [
      {Registry, name: KV, keys: :unique}
    ]
```

When the child specification is a tuple (as above) or module, then it is equivalent to calling the `child_spec/1` function on said module, which then returns the full specification. The pair above is equivalent to:

```elixir
iex> Registry.child_spec(name: KV, keys: :unique)
%{
  id: KV,
  start: {Registry, :start_link, [[name: KV, keys: :unique]]},
  type: :supervisor
}
```

The underlying map returns the `:id` (required), the module-function-args triplet to invoke to start the process (required), the type of the process (optional), among other optional keys. In other words, the `child_spec/1` function allows us to compose and encapsulate specifications in modules.

Therefore, if we want to supervise `KV.Bucket`, we only need to define a `child_spec/1` function. Luckily for us, whenever we invoke `use Agent` (or `use GenServer` or `use Supervisor` and so forth), an implementation with reasonable defaults is provided. So let's take it for a spin. Back on `iex -S mix`, try this:

```elixir
iex> KV.Bucket.child_spec([])
%{id: KV.Bucket, start: {KV.Bucket, :start_link, [[]]}}
iex> KV.Bucket.child_spec([name: :shopping])
%{id: KV.Bucket, start: {KV.Bucket, :start_link, [[name: :shopping]]}}
```

Let's try to start it as part of a supervisor then, using the `{module, options}` format to pass the bucket name (let's also use an atom as the name for convenience):

```elixir
iex> children = [{KV.Bucket, name: :shopping}]
iex> Supervisor.start_link(children, strategy: :one_for_one)
iex> KV.Bucket.put(:shopping, "milk", 1)
:ok
iex> KV.Bucket.get(:shopping, "milk")
1
```

What happens now if we explicitly kill the bucket process?

```elixir
# Find the pid for the given name
iex> pid = Process.whereis(:shopping)
#PID<0.48.0>
# Send it a kill exit signal
iex> Process.exit(pid, :kill)
true
# But a new process is alive in its place
iex> Process.whereis(:shopping)
#PID<0.50.0>
```

Given our buckets can already be supervised, it is time to hook them into our supervision tree.

## Dynamic supervisors

Given our buckets can already be supervised, you may be thinking to start them as part of our application `start/2` callback, such as:

```elixir
    children = [
      {Registry, name: KV, keys: :unique}
      {KV.Bucket, name: {:via, Registry, {KV, "shopping"}}}
    ]
```

And while the above would definitely work, it comes with a huge caveat: it only starts a single bucket. In practice, we want the user to be able to create new buckets at any time. In other words, we need to start and supervise processes dynamically.

While the `Supervisor` module has APIs for starting children after its initialization, it was not designed or optimized for the use case of having potentially millions of children. For this purpose, Elixir instead provides the `DynamicSupervisor` module. Using it is quite similar to `Supervisor` except that, instead of specifying the children during start, you do it afterwards. Let's take it for a spin:

```elixir
iex> {:ok, sup_pid} = DynamicSupervisor.start_link(strategy: :one_for_one)
iex> DynamicSupervisor.start_child(sup_pid, {KV.Bucket, name: :another_list})
iex> KV.Bucket.put(:another_list, "milk", 1)
:ok
iex> KV.Bucket.get(:another_list, "milk")
1
```

And it all works as expected. In fact, we can even give names to `DynamicSupervisor` themselves, instead of passing PIDs around and also use it to start buckets named using the registry:

```elixir
iex> DynamicSupervisor.start_link(strategy: :one_for_one, name: :dyn_sup)
iex> name = {:via, Registry, {KV, "yet_another_list"}}
iex> DynamicSupervisor.start_child(:dyn_sup, {KV.Bucket, name: name})
iex> KV.Bucket.put(name, "milk", 1)
:ok
iex> KV.Bucket.get(name, "milk")
1
```

Overall, processes can be named and supervised, regardless if they are supervisors, agents, etc, since all of Elixir standard library was designed around those capabilities.

With all ingredients in place to supervise and name buckets, open up the `lib/kv.ex` module and let's add a new function called `KV.lookup_bucket/1`, which receives a name and either create or returns a bucket for the given name:

```elixir
defmodule KV do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, name: KV, keys: :unique},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  @doc """
  Creates a bucket with the given name.
  """
  def create_bucket(name) do
    DynamicSupervisor.start_child(KV.BucketSupervisor, {KV.Bucket, name: via(name)})
  end

  @doc """
  Looks up the given bucket.
  """
  def lookup_bucket(name) do
    GenServer.whereis(via(name))
  end

  defp via(name), do: {:via, Registry, {KV, name}}
end
```

The code is relatively simple. First we changed `start/2` to also start a dynamic supervisor named `KV.BucketSupervisor`. Then, when implemented `KV.create_bucket/1` which receives a bucket and starts with using our registry and dynamic supervisor. And we also added `KV.lookup_bucket/1` that receives the same name and attempts to find its PID.

To make sure it all works as expected, let's write a test. Open up `test/kv_test.exs` and add this:

```elixir
defmodule KVTest do
  use ExUnit.Case, async: true

  test "creates and looks up buckets by any name" do
    name = "a unique name that won't be shared"
    assert is_nil(KV.lookup_bucket(name))

    assert {:ok, bucket} = KV.create_bucket(name)
    assert KV.lookup_bucket(name) == bucket

    assert KV.create_bucket(name) == {:error, {:already_started, bucket}}
  end
end
```

The test shows we are creating and locating buckets with any name, making sure we use a unique name to avoid conflicts between tests.

## The `start_supervised` test helper

Before we move on, let's do some clean up.

In `test/kv/bucket_test.exs`, we explicitly invoked `KV.Bucket.start_link/1` to start our buckets. However, we now know that we should avoid calling `start_link/1` directly and instead start processes as part of supervision trees.

In order to aid testing, `ExUnit` already starts a supervision tree per test and provides the `start_supervised` function to start processes within test-specific supervision tree. One advantage of this approach is that `ExUnit` guarantees any started process is shut down at the end of the test too. Let's rewrite our tests to use it instead:

```elixir
defmodule KV.BucketTest do
  use ExUnit.Case, async: true

  test "stores values by key" do
    {:ok, bucket} = start_supervised(KV.Bucket)
    assert KV.Bucket.get(bucket, "milk") == nil

    KV.Bucket.put(bucket, "milk", 3)
    assert KV.Bucket.get(bucket, "milk") == 3
  end

  test "stores values by key on a named process", config do
    {:ok, _} = start_supervised({KV.Bucket, name: config.test})
    assert KV.Bucket.get(config.test, "milk") == nil

    KV.Bucket.put(config.test, "milk", 3)
    assert KV.Bucket.get(config.test, "milk") == 3
  end
end
```

It is a small change, but our tests are now using all of the relevant best practices. Excellent!

## Observer

Now that we have defined our supervision tree, it is a great opportunity to introduce the Observer tool that ships with Erlang. Start your application with `iex -S mix` and key this in:

```elixir
iex> :observer.start()
```

> #### Missing dependencies {: .warning}
>
> When running `iex` inside a project with `iex -S mix`, `observer` won't be available as a dependency. To do so, you will need to call the following functions before:
>
> ```elixir
> iex> :observer.start()
> ```
>
> If the call above fails, here is what may have happened: some package managers default to installing a minimized Erlang without WX bindings for GUI support. In some package managers, you may be able to replace the headless Erlang with a more complete package (look for packages named `erlang` vs `erlang-nox` on Debian/Ubuntu/Arch). In others managers, you may need to install a separate `erlang-wx` (or similarly named) package.
>
> There are conversations to improve this experience in future releases.

A GUI should pop up containing all sorts of information about our system, from general statistics to load charts as well as a list of all running processes and applications.

In the Applications tab, you will see all applications currently running in your system alongside their supervision tree. You can select the `kv` application to explore it further:

<img src="assets/kv-observer.png" alt="Observer GUI screenshot" />

Not only that, as you create new buckets on the terminal, you should see new processes spawned in the supervision tree shown in Observer:

```elixir
iex> KV.lookup_bucket("shopping")
#PID<0.89.0>
```

We will leave it up to you to further explore what Observer provides. Note you can double-click any process in the supervision tree to retrieve more information about it, as well as right-click a process to send "a kill signal", a perfect way to emulate failures and see if your supervisor reacts as expected.

At the end of the day, tools like Observer are one of the reasons you want to always start processes inside supervision trees, even if they are temporary, to ensure they are always reachable and introspectable.

Now that our buckets are named and supervised, we are ready to start our server and start receiving requests.
