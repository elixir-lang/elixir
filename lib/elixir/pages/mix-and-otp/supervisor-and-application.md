<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Supervision trees and applications

In the previous chapter about `GenServer`, we implemented `KV.Registry` to manage buckets. At some point, we started monitoring buckets so we were able to take action whenever a `KV.Bucket` crashed. Although the change was relatively small, it introduced a question which is frequently asked by Elixir developers: what happens when something fails?

Before we added monitoring, if a bucket crashed, the registry would forever point to a bucket that no longer exists. If a user tried to read or write to the crashed bucket, it would fail. Any attempt at creating a new bucket with the same name would just return the PID of the crashed bucket. In other words, that registry entry for that bucket would forever be in a bad state. Once we added monitoring, the registry automatically removes the entry for the crashed bucket. Trying to lookup the crashed bucket now (correctly) says the bucket does not exist and a user of the system can successfully create a new one if desired.

In practice, we are not expecting the processes working as buckets to fail. But, if it does happen, for whatever reason, we can rest assured that our system will continue to work as intended.

If you have prior programming experience, you may be wondering: "could we just guarantee the bucket does not crash in the first place?". As we will see, Elixir developers tend to refer to those practices as "defensive programming". That's because a live production system has dozens of different reasons why something can go wrong. The disk can fail, memory can be corrupted, bugs, the network may stop working for a second, etc. If we were to write software that attempted to protect or circumvent all of those errors, we would spend more time handling failures than writing our own software!

Therefore, an Elixir developer prefers to "let it crash" or "fail fast". And one of the most common ways we can recover from a failure is by restarting whatever part of the system crashed.

For example, imagine your computer, router, printer, or whatever device is not working properly. How often do you fix it by restarting it? Once we restart the device, we reset the device back to its initial state, which is well-tested and guaranteed to work. In Elixir, we apply this same approach to software: whenever a process crashes, we start a new process to perform the same job as the crashed process.

In Elixir, this is done by a Supervisor. A Supervisor is a process that supervises other processes and restarts them whenever they crash. To do so, Supervisors manage the whole life cycle of any supervised processes, including startup and shutdown.

In this chapter, we will learn how to put those concepts into practice by supervising the `KV.Registry` process. After all, if something goes wrong with the registry, the whole registry is lost and no bucket could ever be found! To address this, we will define a `KV.Supervisor` module that guarantees that our `KV.Registry` is up and running at any given moment.

At the end of the chapter, we will also talk about Applications. As we will see, Mix has been packaging all of our code into an application, and we will learn how to customize our application to guarantee that our Supervisor and the Registry are up and running whenever our system starts.

## Our first supervisor

A supervisor is a process which supervises other processes, which we refer to as child processes. The act of supervising a process includes three distinct responsibilities. The first one is to start child processes. Once a child process is running, the supervisor may restart a child process, either because it terminated abnormally or because a certain condition was reached. For example, a supervisor may restart all children if any child dies. Finally, a supervisor is also responsible for shutting down the child processes when the system is shutting down. Please see the `Supervisor` module for a more in-depth discussion.

Creating a supervisor is not much different from creating a GenServer. We are going to define a module named `KV.Supervisor`, which will use the Supervisor behaviour, inside the `lib/kv/supervisor.ex` file:

```elixir
defmodule KV.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    children = [
      KV.Registry
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

Our supervisor has a single child so far: `KV.Registry`. After we define a list of children, we call `Supervisor.init/2`, passing the children and the supervision strategy.

The supervision strategy dictates what happens when one of the children crashes. `:one_for_one` means that if a child dies, it will be the only one restarted. Since we have only one child now, that's all we need. The `Supervisor` behaviour supports several strategies, which we will discuss in this chapter.

Once the supervisor starts, it will traverse the list of children and it will invoke the `child_spec/1` function on each module.

The `child_spec/1` function returns the child specification which describes how to start the process, if the process is a worker or a supervisor, if the process is temporary, transient or permanent and so on. The `child_spec/1` function is automatically defined when we `use Agent`, `use GenServer`, `use Supervisor`, etc. Let's give it a try in the terminal with `iex -S mix`:

```elixir
iex> KV.Registry.child_spec([])
%{id: KV.Registry, start: {KV.Registry, :start_link, [[]]}}
```

We will learn those details as we move forward on this guide. If you would rather peek ahead, check the `Supervisor` docs.

After the supervisor retrieves all child specifications, it proceeds to start its children one by one, in the order they were defined, using the information in the `:start` key in the child specification. For our current specification, it will call `KV.Registry.start_link([])`.

Let's take the supervisor for a spin:

```elixir
iex> {:ok, sup} = KV.Supervisor.start_link([])
{:ok, #PID<0.148.0>}
iex> Supervisor.which_children(sup)
[{KV.Registry, #PID<0.150.0>, :worker, [KV.Registry]}]
```

So far we have started the supervisor and listed its children. Once the supervisor started, it also started all of its children.

What happens if we intentionally crash the registry started by the supervisor? Let's do so by sending it a bad input on `call`:

```elixir
iex> [{_, registry, _, _}] = Supervisor.which_children(sup)
[{KV.Registry, #PID<0.150.0>, :worker, [KV.Registry]}]
iex> GenServer.call(registry, :bad_input)
08:52:57.311 [error] GenServer #PID<0.150.0> terminating
** (FunctionClauseError) no function clause matching in KV.Registry.handle_call/3
iex> Supervisor.which_children(sup)
[{KV.Registry, #PID<0.157.0>, :worker, [KV.Registry]}]
```

Notice how the supervisor automatically started a new registry, with a new PID, in place of the first one once we caused it to crash due to a bad input.

In the previous chapters, we have always started processes directly. For example, we would call `KV.Registry.start_link([])`, which would return `{:ok, pid}`, and that would allow us to interact with the registry via its `pid`. Now that processes are started by the supervisor, we have to directly ask the supervisor who its children are, and fetch the PID from the returned list of children. In practice, doing so every time would be very expensive. To address this, we often give names to processes, allowing them to be uniquely identified in a single machine from anywhere in our code.

Let's learn how to do that.

## Naming processes

While our application will have many buckets, it will only have a single registry. Therefore, whenever we start the registry, we want to give it a unique name so we can reach out to it from anywhere. We do so by passing a `:name` option to `KV.Registry.start_link/1`.

Let's slightly change our children definition (in `KV.Supervisor.init/1`) to be a list of tuples instead of a list of atoms:

```elixir
  def init(:ok) do
    children = [
      {KV.Registry, name: KV.Registry}
    ]
```

With this in place, the supervisor will now start `KV.Registry` by calling `KV.Registry.start_link(name: KV.Registry)`.

If you revisit the `KV.Registry.start_link/1` implementation, you will remember it simply passes the options to GenServer:

```elixir
  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end
```

which in turn will register the process with the given name. The `:name` option expects an atom for locally named processes (locally named means it is available to this machine — there are other options, which we won't discuss here). Since module identifiers are atoms (try `i(KV.Registry)` in IEx), we can name a process after the module that implements it, provided there is only one process for that name. This helps when debugging and introspecting the system.

Let's give the updated supervisor a try inside `iex -S mix`:

```elixir
iex> KV.Supervisor.start_link([])
{:ok, #PID<0.66.0>}
iex> KV.Registry.create(KV.Registry, "shopping")
:ok
iex> KV.Registry.lookup(KV.Registry, "shopping")
{:ok, #PID<0.70.0>}
```

This time the supervisor started a named registry, allowing us to create buckets without having to explicitly fetch the PID from the supervisor. You should also know how to make the registry crash again, without looking up its PID: give it a try.

> At this point, you may be wondering: should you also locally name bucket processes? Remember buckets are started dynamically based on user input. Since local names MUST be atoms, we would have to dynamically create atoms, which is a bad idea since once an atom is defined, it is never erased nor garbage collected. This means that, if we create atoms dynamically based on user input, we will eventually run out of memory (or to be more precise, the VM will crash because it imposes a hard limit on the number of atoms). This limitation is precisely why we created our own registry (or why one would use Elixir's built-in `Registry` module).

We are getting closer and closer to a fully working system. The supervisor automatically starts the registry. But how can we automatically start the supervisor whenever our system starts? To answer this question, let's talk about applications.

## Understanding applications

We have been working inside an application this entire time. Every time we changed a file and ran `mix compile`, we could see a `Generated kv app` message in the compilation output.

We can find the generated `.app` file at `_build/dev/lib/kv/ebin/kv.app`. Let's have a look at its contents:

```erlang
{application,kv,
             [{applications,[kernel,stdlib,elixir,logger]},
              {description,"kv"},
              {modules,['Elixir.KV','Elixir.KV.Bucket','Elixir.KV.Registry',
                        'Elixir.KV.Supervisor']},
              {registered,[]},
              {vsn,"0.1.0"}]}.
```

This file contains Erlang terms (written using Erlang syntax). Even though we are not familiar with Erlang, it is easy to guess this file holds our application definition. It contains our application `version`, all the modules defined by it, as well as a list of applications we depend on, like Erlang's `kernel`, `elixir` itself, and `logger`.

> The `logger` application ships as part of Elixir. We stated that our application needs it by specifying it in the `:extra_applications` list in `mix.exs`. See the [official documentation](`Logger`) for more information.

In a nutshell, an application consists of all the modules defined in the `.app` file, including the `.app` file itself. An application has generally only two directories: `ebin`, for Elixir artifacts, such as `.beam` and `.app` files, and `priv`, with any other artifact or asset you may need in your application.

Although Mix generates and maintains the `.app` file for us, we can customize its contents by adding new entries to the `application/0` function inside the `mix.exs` project file. We are going to do our first customization soon.

### Starting applications

Each application in our system can be started and stopped. The rules for starting and stopping an application are also defined in the `.app` file. When we invoke `iex -S mix`, Mix compiles our application and then starts it.

Let's see this in practice. Start a console with `iex -S mix` and try:

```elixir
iex> Application.start(:kv)
{:error, {:already_started, :kv}}
```

Oops, it's already started. Mix starts the current application and all of its dependencies automatically. This is also true for `mix test` and many other Mix commands.

We can, however, stop our `:kv` application, as well as the `:logger` application:

```elixir
iex> Application.stop(:kv)
:ok
iex> Application.stop(:logger)
:ok
```

And let's try to start our application again:

```elixir
iex> Application.start(:kv)
{:error, {:not_started, :logger}}
```

Now we get an error because an application that `:kv` depends on (`:logger` in this case) isn't started. We need to either start each application manually in the correct order or call `Application.ensure_all_started/1` as follows:

```elixir
iex> Application.ensure_all_started(:kv)
{:ok, [:logger, :kv]}
```

In practice, our tools always start our applications for us, but there is an API available if you need fine-grained control.

## The application callback

Whenever we invoke `iex -S mix`, Mix automatically starts our application by calling `Application.start(:kv)`. But can we customize what happens when our application starts? As a matter of fact, we can! To do so, we define an application callback.

The first step is to tell our application definition (for example, our `.app` file) which module is going to implement the application callback. Let's do so by opening `mix.exs` and changing `def application` to the following:

```elixir
  def application do
    [
      extra_applications: [:logger],
      mod: {KV, []}
    ]
  end
```

The `:mod` option specifies the "application callback module", followed by the arguments to be passed on application start. The application callback module can be any module that implements the `Application` behaviour.

To implement the `Application` behaviour, we have to `use Application` and define a `start/2` function. The goal of `start/2` is to start a supervisor, which will then start any child services or execute any other code our application may need. Let's use this opportunity to start the `KV.Supervisor` we have implemented earlier in this chapter.

Since we have specified `KV` as the module callback, let's change the `KV` module defined in `lib/kv.ex` to implement a `start/2` function:

```elixir
defmodule KV do
  use Application

  @impl true
  def start(_type, _args) do
    # Although we don't use the supervisor name below directly,
    # it can be useful when debugging or introspecting the system.
    KV.Supervisor.start_link(name: KV.Supervisor)
  end
end
```

> Please note that by doing this, we are breaking the boilerplate test case which tested the `hello` function in `KV`. You can simply remove that test case.

When we `use Application`, we may define a couple of functions, similar to when we used `Supervisor` or `GenServer`. This time we only had to define a `start/2` function. The `Application` behaviour also has a `stop/1` callback, but it is rarely used in practice. You can check the documentation for more information.

Now that you have defined an application callback which starts our supervisor, we expect the `KV.Registry` process to be up and running as soon as we start `iex -S mix`. Let's give it another try:

```elixir
iex> KV.Registry.create(KV.Registry, "shopping")
:ok
iex> KV.Registry.lookup(KV.Registry, "shopping")
{:ok, #PID<0.88.0>}
```

Let's recap what is happening. Whenever we invoke `iex -S mix`, it automatically starts our application by calling `Application.start(:kv)`, which then invokes the application callback. The application callback's job is to start a **supervision tree**. Right now, our supervisor has a single child named `KV.Registry`, started with name `KV.Registry`. Our supervisor could have other children, and some of these children could be their own supervisors with their own children, leading to the so-called supervision trees.

## Projects or applications?

Mix makes a distinction between projects and applications. Based on the contents of our `mix.exs` file, we would say we have a Mix project that defines the `:kv` application. As we will see in later chapters, there are projects that don't define any application.

When we say "project" you should think about Mix. Mix is the tool that manages your project. It knows how to compile your project, test your project and more. It also knows how to compile and start the application relevant to your project.

When we talk about applications, we talk about OTP. Applications are the entities that are started and stopped as a whole by the runtime. You can learn more about applications and how they relate to booting and shutting down of your system as a whole in the documentation for the `Application` module.

## Next steps

Although this chapter was the first time we implemented a supervisor, it was not the first time we used one! In the previous chapter, when we used `start_supervised!` to start the registry during our tests, `ExUnit` started the registry under a supervisor managed by the ExUnit framework itself. By defining our own supervisor, we provide more structure on how we initialize, shutdown and supervise processes in our applications, aligning our production code and tests with best practices.

But we are not done yet. So far we are supervising the registry but our application is also starting buckets. Since buckets are started dynamically, we can use a special type of supervisor called `DynamicSupervisor`, which is optimized to handle such scenarios. Let's explore it next.
