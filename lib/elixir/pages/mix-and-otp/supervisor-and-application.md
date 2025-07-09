<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Registries and supervision trees

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

In the example session above we interacted with the "shopping" bucket. Therefore, an important feature in our key-value store to add names to processes.

We have also learned in the previous chapter we can already name our buckets. For example:

```elixir
iex> KV.Bucket.start_link(name: :shopping)
{:ok, #PID<0.43.0>}
iex> KV.Bucket.put(:shopping, "milk", 1)
:ok
iex> KV.Bucket.get(:shopping, "milk")
1
```

However, naming dynamic processes with atoms is a terrible idea! If we use atoms, we would need to convert the bucket name (often received from an external client) to atoms, and **we should never convert user input to atoms**. This is because atoms are not garbage collected. Once an atom is created, it is never reclaimed. Generating atoms from user input would mean the user can inject enough different names to exhaust our system memory!

In practice, it is more likely you will reach the Erlang VM limit for the maximum number of atoms before you run out of memory, which will bring your system down regardless.

Luckily, Elixir (and Erlang) comes with built-in abstractions for naming processes, called name registries, each with different trade-offs which we will explore throughout these guides.

## Local, decentralized, and scalable registry

Elixir ships with a single-node process registry module aptly called `Registry`. Its main feature is that you can use any Elixir value to name a process, not only atoms. Let's take it for a spin in `iex`:

```elixir
iex> Registry.start_link(name: KV, keys: :unique)
iex> name = {:via, Registry, {KV, "shopping"}}
iex> KV.Bucket.start_link(name: name)
{:ok, #PID<0.43.0>}
iex> KV.Bucket.put(name, "milk", 1)
:ok
iex> KV.Bucket.get(name, "milk")
1
```

As you can see, instead of passing an atom to the `:name` option, we pass a tuple of shape `{:via, registry_module, {registry_name, process_name}}`, and everything just worked. You could have used anything as the `process_name`, even an integer or a map! That's because all of Elixir built-in behaviours, agents, supervisors, tasks, etc. are compatible with naming registries, as long as you pass them using the "via" tuple format.

Therefore, all we need to do to name our buckets is to start a `Registry`, using `Registry.start_link/1`. But you may be wondering, where exactly should we place that?

## Understanding applications

Every Elixir project is an application. Elixir itself is defined in an application named `:elixir`. The `ExUnit.Case` module is part of the `:ex_unit` application. And so forth.

In fact, we have been working inside an application this entire time. Every time we changed a file and ran `mix compile`, we could see a `Generated kv app` message in the compilation output.

We can find the generated `.app` file at `_build/dev/lib/kv/ebin/kv.app`. Let's have a look at its contents:

```erlang
{application,kv,
             [{applications,[kernel,stdlib,elixir,logger]},
              {description,"kv"},
              {modules,['Elixir.KV','Elixir.KV.Bucket']},
              {registered,[]},
              {vsn,"0.1.0"}]}.
```

This file contains Erlang terms (written using Erlang syntax). Even though we are not familiar with Erlang, it is easy to guess this file holds our application definition. It contains our application `version`, all the modules defined by it, as well as a list of applications we depend on, like Erlang's `kernel`, `elixir` itself, and `logger`.

> The `logger` application ships as part of Elixir. We stated that our application needs it by specifying it in the `:extra_applications` list in `mix.exs`. See the [official documentation](`Logger`) for more information.

In a nutshell, an application consists of all the modules defined in the `.app` file, including the `.app` file itself. The application itself is located at the `_build/dev/lib/kv` folder and typically has only two directories: `ebin`, for Elixir artifacts, such as `.beam` and `.app` files, and `priv`, with any other artifact or asset you may need in your application.

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

In practice, our tools always start our applications for us, and you don't have to worry about the above, but it is good to know how it all works behind the scenes.

### The application callback

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

The `:mod` option specifies the "application callback module", followed by the arguments to be passed on application start. The application callback module can be any module that invokes `use Application`. Since we have specified `KV` as the module callback, let's change the `KV` module defined in `lib/kv.ex` to the following:

```elixir
defmodule KV do
  use Application
end
```

Now run `mix test` and you will see a couple things happening. First of all, you will get a compilation warning:

```
Compiling 1 file (.ex)
    warning: function start/2 required by behaviour Application is not implemented (in module KV)
    │
  1 │ defmodule KV do
    │ ~~~~~~~~~~~~~~~
    │
    └─ lib/kv.ex:1: KV (module)
```

This warning is telling us that `use Application` actually defines a behaviour, which expects us to implement to a `start/2` function in our `KV` module.

Then our application does not even boot because the `start/2` function is not actually implemented:

```
18:29:39.109 [notice] Application kv exited: exited in: KV.start(:normal, [])
    ** (EXIT) an exception was raised:
        ** (UndefinedFunctionError) function KV.start/2 is undefined or private
```

Implementing the `start/2` callback is relatively straight-forward, all we need to do is to start a supervision tree, and return `{:ok, root_supervisor_pid}`. The `Supervisor.start_link/2` function does precisely that, it only expects a list of children and the supervision strategy. Let's just pass an empty list of children for now:

```elixir
defmodule KV do
  use Application

  # The @impl true annotation says we are implementing a callback
  @impl true
  def start(_type, _args) do
    Supervisor.start_link([], strategy: :one_for_one)
  end
end
```

Now run `mix test` again and our app should boot but we should see one failure. When we changed the `KV` module, we broke the boilerplate test case which tested the `KV.hello/0` function. You can simply remove that test case and we are back to a green suite.

We wrote very little code but we did something incredibly powerful. We now have a function, `KV.start/2` that is invoked whenever your application starts. This gives us the perfect place to start our key-value registry. The `Application` module also allows us to define a `stop/1` callback and other funtionality. You can check the `Application` and `Supervisor` modules for extensive documentation on their uses.

Let's finally start our registry.

## Supervision trees

Now that we have the `start/2` callback, we can finally go ahead and start our registry. You may be tempted to do it like this:

```elixir
  def start(_type, _args) do
    Registry.start_link(name: KV, keys: :unique)
    Supervisor.start_link([], strategy: :one_for_one)
  end
```

However, this would not be a good idea. In Elixir, we typically start processes inside supervision trees. In fact, we rarely use the `start_link` functions to start processes (except at the root of the supervision tree itself). Instead, do this:

```elixir
  def start(_type, _args) do
    children = [
      {Registry, name: KV, keys: :unique}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
```

A supervisor receives one or more child specifications that tell exactly how to start each child. A child specification is typically represented by `{module, options}` pair, as shown above, and often as simply the module name. Sometimes, these children are supervisors themselves, giving ussupervision trees.

Let's take it for a spin and see if we can indeed name our buckets using our new registry. Let's make sure to start a new `iex -S mix` (`recompile()` is not enough, as it does not reload your supervision tree) and then:

```iex
iex> name = {:via, Registry, {KV, "shopping"}}
iex> KV.Bucket.start_link(name: name)
{:ok, #PID<0.43.0>}
iex> KV.Bucket.put(name, "milk", 1)
:ok
iex> KV.Bucket.get(name, "milk")
1
```

Perfect, this time we didn't need to start the registry inside `iex`, as it was started as part of the application itself.

By starting processes inside supervisors, we gain important properties such as:

  * **Introspection**: for each application, you can fully introspect and visualize each process in its supervision tree, its memory usage, message queue, etc

  * **Resilience**: when a process fails for an unexpected reason, its supervisor controls if and how those processes should be restarted, leading to self-healing systems

  * **Graceful shutdown**: when your application is shutting down, the children of a supervision tree is terminated in the opposite order it was started, leading to graceful shutdowns

## Projects or applications?

Mix makes a distinction between projects and applications. Based on the contents of our `mix.exs` file, we would say we have a Mix project that defines the `:kv` application.

When we say "project" you should think about Mix. Mix is the tool that manages your project. It knows how to compile your project, test your project and more. It also knows how to compile and start the application relevant to your project.

When we talk about applications, we talk about OTP. Applications are the entities that are started and stopped as a whole by the runtime. You can learn more about applications and how they relate to booting and shutting down of your system as a whole in the documentation for the `Application` module.

## Summing up

We learned important concepts in this chapter:

  * Naming registries allow us find processes in a given machine (or, as we will see in the future, even in a cluster)

  * Applications bundle our modules, its dependencies, and how code starts and stops

  * Processes are started as part of supervisors for introspection and fault-tolerance

In the next chapter, we will tie it all up by making sure all our buckets are named and supervised. To do so, we will learn a new tool called dynamic supervisors.
