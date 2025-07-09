<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Introduction to Mix

In this guide, we will build a complete Elixir application, with its own supervision tree, configuration, tests, and more.

The requirements for this guide are (see `elixir -v`):

  * Elixir 1.18.0 onwards
  * Erlang/OTP 27 onwards

The application works as a distributed key-value store. We are going to organize key-value pairs into buckets and distribute those buckets across multiple nodes. We will also build a simple client that allows us to connect to any of those nodes and send requests such as:

```text
CREATE shopping
OK

PUT shopping milk 1
OK

PUT shopping eggs 3
OK

GET shopping milk
1
OK

DELETE shopping eggs
OK
```

In order to build our key-value application, we are going to use three main tools:

  * ***OTP*** *(Open Telecom Platform)* is a set of libraries that ships with Erlang. Erlang developers use OTP to build robust, fault-tolerant applications. In this chapter we will explore how many aspects from OTP integrate with Elixir, including supervision trees, event managers and more;

  * ***[Mix](`Mix`)*** is a build tool that ships with Elixir that provides tasks for creating, compiling, testing your application, managing its dependencies and much more;

  * ***[ExUnit](`ExUnit`)*** is a unit-test based framework that ships with Elixir.

In this chapter, we will create our first project using Mix and explore different features in OTP, Mix, and ExUnit as we go.

> #### Source code {: .info}
>
> The final code for the application built in this guide is in [this repository](https://github.com/josevalim/kv) and can be used as a reference.

> #### Is this guide required reading? {: .info}
>
> This guide is not required reading in your Elixir journey. We'll explain.
>
> As an Elixir developer, you will most likely use one of the many existing frameworks when writing your Elixir code. [Phoenix](https://phoenixframework.org) covers web applications, [Ecto](https://github.com/elixir-ecto/ecto) communicates with databases, you can craft embedded software with [Nerves](https://nerves-project.org/), [Nx](https://github.com/elixir-nx) powers machine learning and AI projects, [Membrane](https://membrane.stream/) assembles audio/video processing pipelines, [Broadway](https://elixir-broadway.org/) handles data ingestion and processing, and many more. These frameworks handle the lower level details of concurrency, distribution, and fault-tolerance, so you, as a user, can focus on your own needs and demands.
>
> On the other hand, if you want to learn the foundations these frameworks are built upon, and the abstractions that power the Elixir ecosystem, this guide will give you a tour through several important concepts.

## Our first project

When you install Elixir, besides getting the `elixir`, `elixirc`, and `iex` executables, you also get an executable Elixir script named `mix`.

Let's create our first project by invoking `mix new` from the command line. We'll pass the project path as the argument (`kv`, in this case). By default, the application name and module name will be retrieved from the path. So we tell Mix that our main module should be the all-uppercase `KV`, instead of the default, which would have been `Kv`:

```console
$ mix new kv --module KV
```

Mix will create a directory named `kv` with a few files in it:

```text
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/kv.ex
* creating test
* creating test/test_helper.exs
* creating test/kv_test.exs
```

Let's take a brief look at those generated files.

> #### Executables in the `PATH` {: .info}
>
> Mix is an Elixir executable. This means that in order to run `mix`, you need to have both `mix` and `elixir` executables in your [`PATH`](https://en.wikipedia.org/wiki/PATH_(variable)). That's what happens when you install Elixir.

## Project compilation

A file named `mix.exs` was generated inside our new project folder (`kv`) and its main responsibility is to configure our project. Let's take a look at it:

```elixir
defmodule KV.MixProject do
  use Mix.Project

  def project do
    [
      app: :kv,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
    ]
  end
end
```

Our `mix.exs` defines two public functions: `project`, which returns project configuration like the project name and version, and `application`, which is used to generate an application file.

There is also a private function named `deps`, which is invoked from the `project` function, that defines our project dependencies. Defining `deps` as a separate function is not required, but it helps keep the project configuration tidy.

Mix also generates a file at `lib/kv.ex` with a module containing exactly one function, called `hello`:

```elixir
defmodule KV do
  @moduledoc """
  Documentation for KV.
  """

  @doc """
  Hello world.

  ## Examples

      iex> KV.hello()
      :world

  """
  def hello do
    :world
  end
end

```

This structure is enough to compile our project:

```console
$ cd kv
$ mix compile
```

Will output:

```text
Compiling 1 file (.ex)
Generated kv app
```

The `lib/kv.ex` file was compiled and an application manifest named `kv.app` was generated. All compilation artifacts are placed inside the `_build` directory using the options defined in the `mix.exs` file.

Once the project is compiled, you can start a `iex` session inside the project by running the command below. The `-S mix` is necessary to load the project in the interactive shell:

```console
$ iex -S mix
```

We are going to work on this `kv` project, making modifications and trying out the latest changes from a `iex` session. While you may start a new session whenever there are changes to the project source code, you can also recompile the project from within `iex` with the `recompile` helper, like this:

```elixir
iex> recompile()
Compiling 1 file (.ex)
:ok
iex> recompile()
:noop
```

If anything had to be compiled, you see some informative text, and get the `:ok` atom back, otherwise the function is silent, and returns `:noop`.

## Running tests

Mix also generated the appropriate structure for running our project tests. Mix projects usually follow the convention of having a `<filename>_test.exs` file in the `test` directory for each file in the `lib` directory. For this reason, we can already find a `test/kv_test.exs` corresponding to our `lib/kv.ex` file. It doesn't do much at this point:

```elixir
defmodule KVTest do
  use ExUnit.Case
  doctest KV

  test "greets the world" do
    assert KV.hello() == :world
  end
end
```

It is important to note a couple of things:

1. the test file is an Elixir script file (`.exs`). This is convenient because we don't need to compile test files before running them;

2. we define a test module named `KVTest`, in which we [`use ExUnit.Case`](`ExUnit.Case`) to inject the testing API;

3. we use one of the imported macros, `ExUnit.DocTest.doctest/1`, to indicate that the `KV` module contains doctests (we will discuss those in a later chapter);

4. we use the `ExUnit.Case.test/2` macro to define a simple test;

Mix also generated a file named `test/test_helper.exs` which is responsible for setting up the test framework:

```elixir
ExUnit.start()
```

This file will be required by Mix every time before we run our tests. We can run tests with:

```console
$ mix test
Compiled lib/kv.ex
Generated kv app
Running ExUnit with seed: 540224, max_cases: 16
..

Finished in 0.04 seconds
1 doctest, 1 test, 0 failures
```

Notice that by running `mix test`, Mix has compiled the source files and generated the application manifest once again. This happens because Mix supports multiple environments, which we will discuss later in this chapter.

Furthermore, you can see that ExUnit prints a dot for each successful test and automatically randomizes tests too. Let's make the test fail on purpose and see what happens.

Change the assertion in `test/kv_test.exs` to the following:

```elixir
assert KV.hello() == :oops
```

Now run `mix test` again (notice this time there will be no compilation):

```text
  1) test greets the world (KVTest)
     test/kv_test.exs:5
     Assertion with == failed
     code:  assert KV.hello() == :oops
     left:  :world
     right: :oops
     stacktrace:
       test/kv_test.exs:6: (test)

.

Finished in 0.05 seconds
1 doctest, 1 test, 1 failure
```

For each failure, ExUnit prints a detailed report, containing the test name with the test case, the code that failed and the values for the left side and right side (RHS) of the `==` operator.

In the second line of the failure, right below the test name, there is the location where the test was defined. If you copy the test location in full, including the file and line number, and append it to `mix test`, Mix will load and run just that particular test:

```console
$ mix test test/kv_test.exs:5
```

This shortcut will be extremely useful as we build our project, allowing us to quickly iterate by running a single test.

Finally, the stacktrace relates to the failure itself, giving information about the test and often the place the failure was generated from within the source files.

## Automatic code formatting

One of the files generated by `mix new` is the `.formatter.exs`. Elixir ships with a code formatter that is capable of automatically formatting our codebase according to a consistent style. The formatter is triggered with the `mix format` task. The generated `.formatter.exs` file configures which files should be formatted when `mix format` runs.

To give the formatter a try, change a file in the `lib` or `test` directories to include extra spaces or extra newlines, such as `def  hello  do`, and then run `mix format`.

Most editors provide built-in integration with the formatter, allowing a file to be formatted on save or via a chosen keybinding. If you are learning Elixir, editor integration gives you useful and quick feedback when learning the Elixir syntax.

For companies and teams, we recommend developers to run `mix format --check-formatted` on their continuous integration servers, ensuring all current and future code follows the standard.

You can learn more about the code formatter by checking [the format task documentation](`mix format`) or by reading [the release announcement for Elixir v1.6](https://elixir-lang.org/blog/2018/01/17/elixir-v1-6-0-released/), the first version to include the formatter.

## Environments

Mix provides the concept of "environments". They allow a developer to customize compilation and other options for specific scenarios. By default, Mix understands three environments:

  * `:dev` — the one in which Mix tasks (like `compile`) run by default
  * `:test` — used by `mix test`
  * `:prod` — the one you will use to run your project in production

The environment applies only to the current project. As we will see in future chapters, any dependency you add to your project will by default run in the `:prod` environment.

Customization per environment can be done by accessing the `Mix.env/0` in your `mix.exs` file, which returns the current environment as an atom. That's what we have used in the `:start_permanent` options:

```elixir
def project do
  [
    ...,
    start_permanent: Mix.env() == :prod,
    ...
  ]
end
```

When true, the `:start_permanent` option starts your application in permanent mode, which means the Erlang VM will crash if your application's supervision tree shuts down. Notice we don't want this behavior in dev and test because it is useful to keep the VM instance running in those environments for troubleshooting purposes.

Mix will default to the `:dev` environment, except for the `test` task that will default to the `:test` environment. The environment can be changed via the `MIX_ENV` environment variable:

```console
$ MIX_ENV=prod mix compile
```

Or on Windows:

```batch
> set "MIX_ENV=prod" && mix compile
```

> #### Mix in production {: .warning}
>
> Mix is a **build tool** and, as such, it is not expected to be available in production. Therefore, it is recommended to access `Mix.env/0` only in configuration files and inside `mix.exs`, never in your application code (`lib`).

## Exploring

There is much more to Mix, and we will continue to explore it as we build our project. A general overview is available on the [Mix documentation](`Mix`) and you can always invoke the help task to list all available tasks:

```console
$ mix help
$ mix help compile
```

Now let's move forward and add the first modules and functions to our application.
