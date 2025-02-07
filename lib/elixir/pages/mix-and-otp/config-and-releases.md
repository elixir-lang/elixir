<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Configuration and releases

In this last guide, we will make the routing table for our distributed key-value store configurable, and then finally package the software for production.

Let's do this.

## Application environment

So far we have hard-coded the routing table into the `KV.Router` module. However, we would like to make the table dynamic. This allows us not only to configure development/test/production, but also to allow different nodes to run with different entries in the routing table. There is a feature of OTP that does exactly that: the application environment.

Each application has an environment that stores the application's specific configuration by key. For example, we could store the routing table in the `:kv` application environment, giving it a default value and allowing other applications to change the table as needed.

Open up `apps/kv/mix.exs` and change the `application/0` function to return the following:

```elixir
def application do
  [
    extra_applications: [:logger],
    env: [routing_table: []],
    mod: {KV, []}
  ]
end
```

We have added a new `:env` key to the application. It returns the application default environment, which has an entry of key `:routing_table` and value of an empty list. It makes sense for the application environment to ship with an empty table, as the specific routing table depends on the testing/deployment structure.

In order to use the application environment in our code, we need to replace `KV.Router.table/0` with the definition below:

```elixir
@doc """
The routing table.
"""
def table do
  Application.fetch_env!(:kv, :routing_table)
end
```

We use `Application.fetch_env!/2` to read the entry for `:routing_table` in `:kv`'s environment. You can find more information and other functions to manipulate the app environment in the `Application` module.

Since our routing table is now empty, our distributed tests should fail. Restart the apps and re-run tests to see the failure:

```console
$ iex --sname bar -S mix
$ elixir --sname foo -S mix test --only distributed
```

We need a way to configure the application environment. That's when we use configuration files.

## Configuration

Configuration files provide a mechanism for us to configure the environment of any application. Elixir provides two configuration entry points:

  * `config/config.exs` — this file is read at build time, before we compile our application and before we even load our dependencies. This means we can't access the code in our application nor in our dependencies. However, it means we can control how they are compiled

  * `config/runtime.exs` — this file is read after our application and dependencies are compiled and therefore it can configure how our application works at runtime. If you want to read system environment variables (via `System.get_env/1`) or any sort of external configuration, this is the appropriate place to do so

For example, we can configure IEx default prompt to another value. Let's create the `config/runtime.exs` file with the following content:

```elixir
import Config
config :iex, default_prompt: ">>>"
```

Start IEx with `iex -S mix` and you can see that the IEx prompt has changed.

This means we can also configure our `:routing_table` directly in the `config/runtime.exs` file. However, which configuration value should we use?

Currently we have two tests tagged with `@tag :distributed`. The "server interaction" test in `KVServerTest`, and the "route requests across nodes" in `KV.RouterTest`. Both tests are failing since they require a routing table, which is currently empty.

For simplicity, we will define a routing table that always points to the current node. That's the table we will use for development and most of our tests. Back in `config/runtime.exs`, add this line:

```elixir
config :kv, :routing_table, [{?a..?z, node()}]
```

With such a simple table available, we can now remove `@tag :distributed` from the test in `test/kv_server_test.exs`. If you run the complete suite, the test should now pass.

However, for the tests in `KV.RouterTest`, we effectively need two nodes in our routing table. To do so, we will write a setup block that runs before all tests in that file. The setup block will change the application environment and revert it back once we are done, like this:

```elixir
defmodule KV.RouterTest do
  use ExUnit.Case

  setup_all do
    current = Application.get_env(:kv, :routing_table)

    Application.put_env(:kv, :routing_table, [
      {?a..?m, :"foo@computer-name"},
      {?n..?z, :"bar@computer-name"}
    ])

    on_exit fn -> Application.put_env(:kv, :routing_table, current) end
  end

  @tag :distributed
  test "route requests across nodes" do
```

Note we removed `async: true` from `use ExUnit.Case`. Since the application environment is a global storage, tests that modify it cannot run concurrently. With all changes in place, all tests should pass, including the distributed one.

## Releases

Now that our application runs distributed, you may be wondering how we can package our application to run in production. After all, all of our code so far depends on Erlang and Elixir versions that are installed in your current system. To achieve this goal, Elixir provides releases.

A release is a self-contained directory that consists of your application code, all of its dependencies, plus the whole Erlang Virtual Machine (VM) and runtime. Once a release is assembled, it can be packaged and deployed to a target as long as the target runs on the same operating system (OS) distribution and version as the machine that assembled the release.

In a regular project, we can assemble a release by simply running `mix release`. However, we have an umbrella project, and in such cases Elixir requires some extra input from us. Let's see what is necessary:

```shell
$ MIX_ENV=prod mix release
** (Mix) Umbrella projects require releases to be explicitly defined with a non-empty applications key that chooses which umbrella children should be part of the releases:

releases: [
  foo: [
    applications: [child_app_foo: :permanent]
  ],
  bar: [
    applications: [child_app_bar: :permanent]
  ]
]

Alternatively you can perform the release from the children applications
```

That's because an umbrella project gives us plenty of options when deploying the software. We can:

  * deploy all applications in the umbrella to a node that will work as both TCP server and key-value storage

  * deploy the `:kv_server` application to work only as a TCP server as long as the routing table points only to other nodes

  * deploy only the `:kv` application when we want a node to work only as storage (no TCP access)

As a starting point, let's define a release that includes both `:kv_server` and `:kv` applications. We will also add a version to it. Open up the `mix.exs` in the umbrella root and add inside `def project`:

```elixir
releases: [
  foo: [
    version: "0.0.1",
    applications: [kv_server: :permanent, kv: :permanent]
  ]
]
```

That defines a release named `foo` with both `kv_server` and `kv` applications. Their mode is set to `:permanent`, which means that, if those applications crash, the whole node terminates. That's reasonable since those applications are essential to our system.

Before we assemble the release, let's also define our routing table for production. Given we expect to have two nodes, we need to update `config/runtime.exs` to look like this:

```elixir
import Config

config :kv, :routing_table, [{?a..?z, node()}]

if config_env() == :prod do
  config :kv, :routing_table, [
    {?a..?m, :"foo@computer-name"},
    {?n..?z, :"bar@computer-name"}
  ]
end
```

We have hard-coded the table and node names, which is good enough for our example, but you would likely move it to an external configuration system in an actual production setup. We have also wrapped it in a `config_env() == :prod` check, so this configuration does not apply to other environments.

With the configuration in place, let's give assembling the release another try:

    $ MIX_ENV=prod mix release foo
    * assembling foo-0.0.1 on MIX_ENV=prod
    * skipping runtime configuration (config/runtime.exs not found)

    Release created at _build/prod/rel/foo!

        # To start your system
        _build/prod/rel/foo/bin/foo start

    Once the release is running:

        # To connect to it remotely
        _build/prod/rel/foo/bin/foo remote

        # To stop it gracefully (you may also send SIGINT/SIGTERM)
        _build/prod/rel/foo/bin/foo stop

    To list all commands:

        _build/prod/rel/foo/bin/foo

Excellent! A release was assembled in `_build/prod/rel/foo`. Inside the release, there will be a `bin/foo` file which is the entry point to your system. It supports multiple commands, such as:

  * `bin/foo start`, `bin/foo start_iex`, `bin/foo restart`, and `bin/foo stop` — for general management of the release

  * `bin/foo rpc COMMAND` and `bin/foo remote` — for running commands on the running system or to connect to the running system

  * `bin/foo eval COMMAND` — to start a fresh system that runs a single command and then shuts down

  * `bin/foo daemon` and `bin/foo daemon_iex` — to start the system as a daemon on Unix-like systems

  * `bin/foo install` — to install the system as a service on Windows machines

If you run `bin/foo start`, it will start the system using a short name (`--sname`) equal to the release name, which in this case is `foo`. The next step is to start a system named `bar`, so we can connect `foo` and `bar` together, like we did in the previous chapter. But before we achieve this, let's talk a bit about the benefits of releases.

## Why releases?

Releases allow developers to precompile and package all of their code and the runtime into a single unit. The benefits of releases are:

  * Code preloading. The VM has two mechanisms for loading code: interactive and embedded. By default, it runs in the interactive mode which dynamically loads modules when they are used for the first time. The first time your application calls `Enum.map/2`, the VM will find the `Enum` module and load it. There's a downside. When you start a new server in production, it may need to load many other modules, causing the first requests to have an unusual spike in response time. Releases run in embedded mode, which loads all available modules upfront, guaranteeing your system is ready to handle requests after booting.

  * Configuration and customization. Releases give developers fine grained control over system configuration and the VM flags used to start the system.

  * Self-contained. A release does not require the source code to be included in your production artifacts. All of the code is precompiled and packaged. Releases do not even require Erlang or Elixir on your servers, as they include the Erlang VM and its runtime by default. Furthermore, both Erlang and Elixir standard libraries are stripped to bring only the parts you are actually using.

  * Multiple releases. You can assemble different releases with different configuration per application or even with different applications altogether.

We have written extensive documentation on releases, so [please check the official documentation for more information](`mix release`). For now, we will continue exploring some of the features outlined above.

## Assembling multiple releases

So far, we have assembled a release named `foo`, but our routing table contains information for both `foo` and `bar`. Let's start `foo`:

    $ _build/prod/rel/foo/bin/foo start
    16:58:58.508 [info]  Accepting connections on port 4040

And let's connect to it and issue a request in another terminal:

    $ telnet 127.0.0.1 4040
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    CREATE bitsandpieces
    OK
    PUT bitsandpieces sword 1
    OK
    GET bitsandpieces sword
    1
    OK
    GET shopping foo
    Connection closed by foreign host.

Our application works already when we operate on the bucket named "bitsandpieces". But since the "shopping" bucket would be stored on `bar`, the request fails as `bar` is not available. If you go back to the terminal running `foo`, you will see:

    17:16:19.555 [error] Task #PID<0.622.0> started from #PID<0.620.0> terminating
    ** (stop) exited in: GenServer.call({KV.RouterTasks, :"bar@computer-name"}, {:start_task, [{:"foo@josemac-2", #PID<0.622.0>, #PID<0.622.0>}, [#PID<0.622.0>, #PID<0.620.0>, #PID<0.618.0>], :monitor, {KV.Router, :route, ["shopping", KV.Registry, :lookup, [KV.Registry, "shopping"]]}], :temporary, nil}, :infinity)
        ** (EXIT) no connection to bar@computer-name
        (elixir) lib/gen_server.ex:1010: GenServer.call/3
        (elixir) lib/task/supervisor.ex:454: Task.Supervisor.async/6
        (kv) lib/kv/router.ex:21: KV.Router.route/4
        (kv_server) lib/kv_server/command.ex:74: KVServer.Command.lookup/2
        (kv_server) lib/kv_server.ex:29: KVServer.serve/1
        (elixir) lib/task/supervised.ex:90: Task.Supervised.invoke_mfa/2
        (stdlib) proc_lib.erl:249: :proc_lib.init_p_do_apply/3
    Function: #Function<0.128611034/0 in KVServer.loop_acceptor/1>
        Args: []

Let's now define a release for `:bar`. One first step could be to define a release exactly like `foo` inside `mix.exs`. Additionally we will set the `cookie` option on both releases to `weknoweachother` in order for them to allow connections from each other. See the [Distributed Erlang Documentation](http://www.erlang.org/doc/reference_manual/distributed.html) for further information on this topic:

```elixir
releases: [
  foo: [
    version: "0.0.1",
    applications: [kv_server: :permanent, kv: :permanent],
    cookie: "weknoweachother"
  ],
  bar: [
    version: "0.0.1",
    applications: [kv_server: :permanent, kv: :permanent],
    cookie: "weknoweachother"
  ]
]
```

And now let's assemble both releases:

```shell
$ MIX_ENV=prod mix release foo
$ MIX_ENV=prod mix release bar
```

Stop `foo` if it's still running and re-start it to load the `cookie`:

```shell
$ _build/prod/rel/foo/bin/foo start
```

And start `bar` in another terminal:

```shell
$ _build/prod/rel/bar/bin/bar start
```

You should see an error like the error below happen 5 times, before the application finally shuts down:

```text
    17:21:57.567 [error] Task #PID<0.620.0> started from KVServer.Supervisor terminating
    ** (MatchError) no match of right hand side value: {:error, :eaddrinuse}
        (kv_server) lib/kv_server.ex:12: KVServer.accept/1
        (elixir) lib/task/supervised.ex:90: Task.Supervised.invoke_mfa/2
        (stdlib) proc_lib.erl:249: :proc_lib.init_p_do_apply/3
    Function: #Function<0.98032413/0 in KVServer.Application.start/2>
        Args: []
```

That's happening because the release `foo` is already listening on port `4040` and `bar` is trying to do the same! One option could be to move the `:port` configuration to the application environment, like we did for the routing table, and setup different ports per node.

But let's try something else. Let's make it so the `bar` release contains only the `:kv` application. So it works as a storage but it won't have a front-end. Change the `:bar` information to this:

```elixir
releases: [
  foo: [
    version: "0.0.1",
    applications: [kv_server: :permanent, kv: :permanent],
    cookie: "weknoweachother"
  ],
  bar: [
    version: "0.0.1",
    applications: [kv: :permanent],
    cookie: "weknoweachother"
  ]
]
```

And now let's assemble `bar` once more:

    $ MIX_ENV=prod mix release bar

And finally successfully boot it:

    $ _build/prod/rel/bar/bin/bar start

If you connect to localhost once again and perform another request, now everything should work, as long as the routing table contains the correct node names. Outstanding!

With releases, we were able to "cut different slices" of our project and prepared them to run in production, all packaged into a single directory.

## Configuring releases

Releases also provide built-in hooks for configuring almost every need of the production system:

  * `config/config.exs` — provides build-time application configuration, which is executed before our application compiles. This file often imports configuration files based on the environment, such as `config/dev.exs` and `config/prod.exs`.

  * `config/runtime.exs` — provides runtime application configuration. It is executed every time the release boots and is further extensible via config providers.

  * `rel/env.sh.eex` and `rel/env.bat.eex` — template files that are copied into every release and executed on every command to set up environment variables, including ones specific to the VM, and the general environment.

  * `rel/vm.args.eex` — a template file that is copied into every release and provides static configuration of the Erlang Virtual Machine and other runtime flags.

As we have seen, `config/config.exs` and `config/runtime.exs` are loaded during releases and regular Mix commands. On the other hand, `rel/env.sh.eex` and `rel/vm.args.eex` are specific to releases. Let's take a look.

### Operating System environment configuration

Every release contains an environment file, named `env.sh` on Unix-like systems and `env.bat` on Windows machines, that executes before the Elixir system starts. In this file, you can execute any OS-level code, such as invoke other applications, set environment variables and so on. Some of those environment variables can even configure how the release itself runs.

For instance, releases run using short-names (`--sname`). However, if you want to actually run a distributed key-value store in production, you will need multiple nodes and start the release with the `--name` option. We can achieve this by setting the `RELEASE_DISTRIBUTION` environment variable inside the `env.sh` and `env.bat` files. Mix already has a template for said files which we can customize, so let's ask Mix to copy them to our application:

    $ mix release.init
    * creating rel/vm.args.eex
    * creating rel/remote.vm.args.eex
    * creating rel/env.sh.eex
    * creating rel/env.bat.eex

If you open up `rel/env.sh.eex`, you will see:

```shell
#!/bin/sh

# # Sets and enables heart (recommended only in daemon mode)
# case $RELEASE_COMMAND in
#   daemon*)
#     HEART_COMMAND="$RELEASE_ROOT/bin/$RELEASE_NAME $RELEASE_COMMAND"
#     export HEART_COMMAND
#     export ELIXIR_ERL_OPTIONS="-heart"
#     ;;
#   *)
#     ;;
# esac

# # Set the release to load code on demand (interactive) instead of preloading (embedded).
# export RELEASE_MODE=interactive

# # Set the release to work across nodes.
# # RELEASE_DISTRIBUTION must be "sname" (local), "name" (distributed) or "none".
# export RELEASE_DISTRIBUTION=name
# export RELEASE_NODE=<%= @release.name %>
```

The steps necessary to work across nodes is already commented out as an example. You can enable full distribution by uncommenting the last two lines by removing the leading `# `.

If you are on Windows, you will have to open up `rel/env.bat.eex`, where you will find this:

```bat
@echo off
rem Set the release to load code on demand (interactive) instead of preloading (embedded).
rem set RELEASE_MODE=interactive

rem Set the release to work across nodes.
rem RELEASE_DISTRIBUTION must be "sname" (local), "name" (distributed) or "none".
rem set RELEASE_DISTRIBUTION=name
rem set RELEASE_NODE=<%= @release.name %>
```

Once again, uncomment the last two lines by removing the leading `rem ` to enable full distribution. And that's all!

### VM arguments

The `rel/vm.args.eex` allows you to specify low-level flags that control how the Erlang VM and its runtime operate. You specify entries as if you were specifying arguments in the command line with code comments also supported. Here is the default generated file:

    ## Customize flags given to the VM: https://www.erlang.org/doc/man/erl.html
    ## -mode/-name/-sname/-setcookie are configured via env vars, do not set them here

    ## Increase number of concurrent ports/sockets
    ##+Q 65536

    ## Tweak GC to run more often
    ##-env ERL_FULLSWEEP_AFTER 10

You can see [a complete list of VM arguments and flags in the Erlang documentation](http://www.erlang.org/doc/man/erl.html).

## Summing up

Throughout the guide, we have built a very simple distributed key-value store as an opportunity to explore many constructs like generic servers, supervisors, tasks, agents, applications and more. Not only that, we have written tests for the whole application, got familiar with ExUnit, and learned how to use the Mix build tool to accomplish a wide range of tasks.

If you are looking for a distributed key-value store to use in production, you should definitely look into [Riak](http://riak.com/products/riak-kv/), which also runs in the Erlang VM. In Riak, the buckets are replicated, to avoid data loss, and instead of a router, they use [consistent hashing](https://en.wikipedia.org/wiki/Consistent_hashing) to map a bucket to a node. A consistent hashing algorithm helps reduce the amount of data that needs to be migrated when new storage nodes are added to your live system.

Of course, Elixir can be used for much more than distributed key-value stores. Embedded systems, data-processing and data-ingestion, web applications, audio/video streaming systems, and others are many of the different domains Elixir excels at. We hope this guide has prepared you to explore any of those domains or any future domain you may desire to bring Elixir into.

Happy coding!
