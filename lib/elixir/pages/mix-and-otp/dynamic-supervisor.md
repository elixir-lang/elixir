<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Supervising dynamic children

We have now successfully defined our supervisor which is automatically started (and stopped) as part of our application life cycle.

Remember, however, that our `KV.Registry` is both linking (via `start_link`) and monitoring (via `monitor`) bucket processes in the `handle_cast/2` callback:

```elixir
{:ok, bucket} = KV.Bucket.start_link([])
ref = Process.monitor(bucket)
```

Links are bidirectional, which implies that a crash in a bucket will crash the registry. Although we now have the supervisor, which guarantees the registry will be back up and running, crashing the registry still means we lose all data associating bucket names to their respective processes.

In other words, we want the registry to keep on running even if a bucket crashes. Let's write a new registry test:

```elixir
test "removes bucket on crash", %{registry: registry} do
  KV.Registry.create(registry, "shopping")
  {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

  # Stop the bucket with non-normal reason
  Agent.stop(bucket, :shutdown)
  assert KV.Registry.lookup(registry, "shopping") == :error
end
```

The test is similar to "removes bucket on exit" except that we are being a bit more harsh by sending `:shutdown` as the exit reason instead of `:normal`. If a process terminates with a reason other than `:normal`, all linked processes receive an EXIT signal, causing the linked process to also terminate unless it is trapping exits.

Since the bucket terminated, the registry also stopped, and our test fails when trying to `GenServer.call/3` it:

```text
  1) test removes bucket on crash (KV.RegistryTest)
     test/kv/registry_test.exs:26
     ** (exit) exited in: GenServer.call(#PID<0.148.0>, {:lookup, "shopping"}, 5000)
         ** (EXIT) no process: the process is not alive or there's no process currently associated with the given name, possibly because its application isn't started
     code: assert KV.Registry.lookup(registry, "shopping") == :error
     stacktrace:
       (elixir) lib/gen_server.ex:770: GenServer.call/3
       test/kv/registry_test.exs:33: (test)
```

We are going to solve this issue by defining a new supervisor that will spawn and supervise all buckets. Opposite to the previous Supervisor we defined, the children are not known upfront, but they are rather started dynamically. For those situations, we use a supervisor optimized to such use cases called `DynamicSupervisor`. The `DynamicSupervisor` does not expect a list of children during initialization; instead each child is started manually via `DynamicSupervisor.start_child/2`.

## The bucket supervisor

Since a `DynamicSupervisor` does not define any children during initialization, the `DynamicSupervisor` also allows us to skip the work of defining a whole separate module with the usual `start_link` function and the `init` callback. Instead, we can define a `DynamicSupervisor` directly in the supervision tree, by giving it a name and a strategy.

Open up `lib/kv/supervisor.ex` and add the dynamic supervisor as a child as follows:

```elixir
  def init(:ok) do
    children = [
      {KV.Registry, name: KV.Registry},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
```

Remember that the name of a process can be any atom. So far, we have named processes with the same name as the modules that define their implementation. For example, the process defined by `KV.Registry` was given a process name of `KV.Registry`. This is simply a convention: If later there is an error in your system that says, "process named KV.Registry crashed with reason", we know exactly where to investigate.

In this case, there is no module, so we picked the name `KV.BucketSupervisor`. It could have been any other name. We also chose the `:one_for_one` strategy, which is currently the only available strategy for dynamic supervisors.

Run `iex -S mix` so we can give our dynamic supervisor a try:

```elixir
iex> {:ok, bucket} = DynamicSupervisor.start_child(KV.BucketSupervisor, KV.Bucket)
{:ok, #PID<0.72.0>}
iex> KV.Bucket.put(bucket, "eggs", 3)
:ok
iex> KV.Bucket.get(bucket, "eggs")
3
```

`DynamicSupervisor.start_child/2` expects the name of the supervisor and the child specification of the child to be started.

The last step is to change the registry to use the dynamic supervisor:

```elixir
  def handle_cast({:create, name}, {names, refs}) do
    if Map.has_key?(names, name) do
      {:noreply, {names, refs}}
    else
      {:ok, pid} = DynamicSupervisor.start_child(KV.BucketSupervisor, KV.Bucket)
      ref = Process.monitor(pid)
      refs = Map.put(refs, ref, name)
      names = Map.put(names, name, pid)
      {:noreply, {names, refs}}
    end
  end
```

That's enough for our tests to pass but there is a resource leakage in our application. When a bucket terminates, the supervisor will start a new bucket in its place. After all, that's the role of the supervisor!

However, when the supervisor restarts the new bucket, the registry does not know about it. So we will have an empty bucket in the supervisor that nobody can access! To solve this, we want to say that buckets are actually temporary. If they crash, regardless of the reason, they should not be restarted.

We can do this by passing the `restart: :temporary` option to `use Agent` in `KV.Bucket`:

```elixir
defmodule KV.Bucket do
  use Agent, restart: :temporary
```

Let's also add a test to `test/kv/bucket_test.exs` that guarantees the bucket is temporary:

```elixir
  test "are temporary workers" do
    assert Supervisor.child_spec(KV.Bucket, []).restart == :temporary
  end
```

Our test uses the `Supervisor.child_spec/2` function to retrieve the child specification out of a module and then assert its restart value is `:temporary`. At this point, you may be wondering why use a supervisor if it never restarts its children. It happens that supervisors provide more than restarts, they are also responsible for guaranteeing proper startup and shutdown, especially in case of crashes in a supervision tree.

## Supervision trees

When we added `KV.BucketSupervisor` as a child of `KV.Supervisor`, we began to have supervisors that supervise other supervisors, forming so-called "supervision trees".

Every time you add a new child to a supervisor, it is important to evaluate if the supervisor strategy is correct as well as the order of child processes. In this case, we are using `:one_for_one` and the `KV.Registry` is started before `KV.BucketSupervisor`.

One flaw that shows up right away is the ordering issue. Since `KV.Registry` invokes `KV.BucketSupervisor`, then the `KV.BucketSupervisor` must be started before `KV.Registry`. Otherwise, it may happen that the registry attempts to reach the bucket supervisor before it has started.

The second flaw is related to the supervision strategy. If `KV.Registry` dies, all information linking `KV.Bucket` names to bucket processes is lost. Therefore the `KV.BucketSupervisor` and all children must terminate too - otherwise we will have orphan processes.

In light of this observation, we should consider moving to another supervision strategy. The two other candidates are `:one_for_all` and `:rest_for_one`. A supervisor using the `:rest_for_one` strategy will kill and restart child processes which were started *after* the crashed child. In this case, we would want `KV.BucketSupervisor` to terminate if `KV.Registry` terminates. This would require the bucket supervisor to be placed after the registry which violates the ordering constraints we have established two paragraphs above.

So our last option is to go all in and pick the `:one_for_all` strategy: the supervisor will kill and restart all of its children processes whenever any one of them dies. This is a completely reasonable approach for our application, since the registry can't work without the bucket supervisor, and the bucket supervisor should terminate without the registry. Let's reimplement `init/1` in `KV.Supervisor` to encode those properties:

```elixir
  def init(:ok) do
    children = [
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one},
      {KV.Registry, name: KV.Registry}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
```

There are two topics left before we move on to the next chapter.

## Shared state in tests

So far we have been starting one registry per test to ensure they are isolated:

```elixir
setup do
  registry = start_supervised!(KV.Registry)
  %{registry: registry}
end
```

Since we have changed our registry to use `KV.BucketSupervisor`, our tests are now relying on this shared supervisor even though each test has its own registry. The question is: should we?

It depends. It is ok to rely on shared state as long as we depend only on a non-shared partition of this state. Although multiple registries may start buckets on the shared bucket supervisor, those buckets and registries are isolated from each other. We would only run into concurrency issues if we used a function like `DynamicSupervisor.count_children(KV.BucketSupervisor)` which would count all buckets from all registries, potentially giving different results when tests run concurrently.

Since we have relied only on a non-shared partition of the bucket supervisor so far, we don't need to worry about concurrency issues in our test suite. In case it ever becomes a problem, we can start a supervisor per test and pass it as an argument to the registry `start_link` function.

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
> iex> Mix.ensure_application!(:wx)             # Not necessary on Erlang/OTP 27+
> iex> Mix.ensure_application!(:runtime_tools)  # Not necessary on Erlang/OTP 27+
> iex> Mix.ensure_application!(:observer)
> iex> :observer.start()
> ```
>
> If any of the calls above fail, here is what may have happened: some package managers default to installing a minimized Erlang without WX bindings for GUI support. In some package managers, you may be able to replace the headless Erlang with a more complete package (look for packages named `erlang` vs `erlang-nox` on Debian/Ubuntu/Arch). In others managers, you may need to install a separate `erlang-wx` (or similarly named) package.
>
> There are conversations to improve this experience in future releases.

A GUI should pop up containing all sorts of information about our system, from general statistics to load charts as well as a list of all running processes and applications.

In the Applications tab, you will see all applications currently running in your system alongside their supervision tree. You can select the `kv` application to explore it further:

<img src="assets/kv-observer.png" alt="Observer GUI screenshot" />

Not only that, as you create new buckets on the terminal, you should see new processes spawned in the supervision tree shown in Observer:

```elixir
iex> KV.Registry.create(KV.Registry, "shopping")
:ok
```

We will leave it up to you to further explore what Observer provides. Note you can double-click any process in the supervision tree to retrieve more information about it, as well as right-click a process to send "a kill signal", a perfect way to emulate failures and see if your supervisor reacts as expected.

At the end of the day, tools like Observer are one of the reasons you want to always start processes inside supervision trees, even if they are temporary, to ensure they are always reachable and introspectable.

Now that our buckets are properly linked and supervised, let's see how we can speed things up.
