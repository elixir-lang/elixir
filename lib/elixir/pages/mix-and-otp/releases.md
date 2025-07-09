<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Releases

Now that our application is ready, you may be wondering how we can package our application to run in production. After all, all of our code so far depends on Erlang and Elixir versions that are installed in your current system. To achieve this goal, Elixir provides releases.

A release is a self-contained directory that consists of your application code, all of its dependencies, plus the whole Erlang Virtual Machine (VM) and runtime. Once a release is assembled, it can be packaged and deployed to a target as long as the target runs on the same operating system (OS) distribution and version as the machine that assembled the release.

To get started, simply run `mix release` while setting `MIX_ENV=prod`:

```
$ MIX_ENV=prod mix release
Compiling 4 files (.ex)
Generated kv app
* assembling kv-0.1.0 on MIX_ENV=prod
* using config/runtime.exs to configure the release at runtime

Release created at _build/prod/rel/kv

    # To start your system
    _build/prod/rel/kv/bin/kv start

Once the release is running:

    # To connect to it remotely
    _build/prod/rel/kv/bin/kv remote

    # To stop it gracefully (you may also send SIGINT/SIGTERM)
    _build/prod/rel/kv/bin/kv stop

To list all commands:

    _build/prod/rel/kv/bin/kv
```

Excellent! A release was assembled in `_build/prod/rel/kv`. Everything you need to run your application is inside that directory. In particular, there is a `bin/kv` file which is the entry point to your system. It supports multiple commands, such as:

  * `bin/kv start`, `bin/kv start_iex`, `bin/kv restart`, and `bin/kv stop` — for general management of the release

  * `bin/kv rpc COMMAND` and `bin/kv remote` — for running commands on the running system or to connect to the running system

  * `bin/kv eval COMMAND` — to start a fresh system that runs a single command and then shuts down

  * `bin/kv daemon` and `bin/kv daemon_iex` — to start the system as a daemon on Unix-like systems

  * `bin/kv install` — to install the system as a service on Windows machines

If you run `bin/kv start_iex` inside the release directory, it will start the system using a short name (`--sname`) equal to the release name, which in this case is `kv`. The next step is to start two instances, on different ports and different names, as we did earlier on. But before we do this, let's talk a bit about the benefits of releases.

## Why releases?

Releases allow developers to precompile and package all of their code and the runtime into a single unit. The benefits of releases are:

  * Code preloading. The VM has two mechanisms for loading code: interactive and embedded. By default, it runs in the interactive mode which dynamically loads modules when they are used for the first time. The first time your application calls `Enum.map/2`, the VM will find the `Enum` module and load it. There's a downside. When you start a new server in production, it may need to load many other modules, causing the first requests to have an unusual spike in response time. Releases run in embedded mode, which loads all available modules upfront, guaranteeing your system is ready to handle requests after booting.

  * Configuration and customization. Releases give developers fine grained control over system configuration and the VM flags used to start the system.

  * Self-contained. A release does not require the source code to be included in your production artifacts. All of the code is precompiled and packaged. Releases do not even require Erlang or Elixir on your servers, as they include the Erlang VM and its runtime by default. Furthermore, both Erlang and Elixir standard libraries are stripped to bring only the parts you are actually using.

  * Multiple releases. You can assemble different releases with different configuration per application or even with different applications altogether.

We have written extensive documentation on releases, so [please check the official documentation for more information](`mix release`). For now, we will continue exploring some of the features outlined above.

## Configuring releases

Releases also provide built-in hooks for configuring almost every need of the production system:

  * `config/config.exs` — provides build-time application configuration, which is executed before our application compiles. This file often imports configuration files based on the environment, such as `config/dev.exs` and `config/prod.exs`.

  * `config/runtime.exs` — provides runtime application configuration. It is executed every time the release boots and is further extensible via config providers.

  * `rel/env.sh.eex` and `rel/env.bat.eex` — template files that are copied into every release and executed on every command to set up environment variables, including ones specific to the VM, and the general environment.

  * `rel/vm.args.eex` — a template file that is copied into every release and provides static configuration of the Erlang Virtual Machine and other runtime flags.

In this case, we already have specified a `config/runtime.exs` that deals with both `PORT` and `NODES` environment variables. Furthermore, while releases don't accept a `--sname` parameter, they do allow us to set the name via the `RELEASE_NODE` env var. Therefore, we can start two copies of the system by jumping into `_build/prod/rel/kv` and typing this (remember to adjust `@computer-name` to your actual computer name):

```shell
$ NODES="foo@computer-name,bar@computer-name" PORT=4040 RELEASE_NODE="foo" bin/kv start_iex
```

```shell
$ NODES="foo@computer-name,bar@computer-name" PORT=4041 RELEASE_NODE="bar" bin/kv start_iex
```

To verify it all worked out, you can type `Node.list` in the IEx section and see if it returns the other node. If it doesn't, you can start diagnosing, first by comparing the node names within each `iex>` prompt and calling `Node.connect/1` directly. With applications running, you can `telnet` into them as usual too.

While the above is enough to get started, you may want to perform advanced configuration based on the environment you are replying to. Releases provide scripts for that, which are great to automate based on host, network, or cloud settings.

## Operating System scripts

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

The steps necessary to work across nodes is already commented out as an example. You can enable full distribution by setting the `RELEASE_DISTRIBUTION` variable to `name`.

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

Once again, set the `RELEASE_DISTRIBUTION` variable to `name` and you are good to go!

## VM arguments

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

If you are looking for a distributed key-value store to use in production, you should definitely look into [Riak](http://riak.com/products/riak-kv/), which also runs in the Erlang VM. In Riak, the buckets are replicated and stored across several nodes to avoid data loss.

Of course, Elixir can be used for much more than distributed key-value stores. Embedded systems, data-processing and data-ingestion, web applications, audio/video streaming systems, machine learning, and others are many of the different domains Elixir excels at. We hope this guide has prepared you to explore any of those domains or any future domain you may desire to bring Elixir into.

Happy coding!
