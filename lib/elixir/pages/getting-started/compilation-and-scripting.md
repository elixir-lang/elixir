<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Compilation and Scripting

As our examples get longer in size, it can be tricky to type them all in the shell. It's about time for us to learn how to compile Elixir code and also how to run Elixir scripts.

## Compilation

Most of the time it is convenient to write [modules](modules-and-functions.md) (more closely looked at in the next chapter) into files so they can be compiled and reused. Let's assume we have a file named `math.ex` with the following contents:

```elixir
defmodule Math do
  def sum(a, b) do
    a + b
  end
end
```

This file can be compiled using `elixirc`:

```console
$ elixirc math.ex
```

This will generate a file named `Elixir.Math.beam` containing the bytecode for the defined module. If we start `iex` again, our module definition will be available (provided that `iex` is started in the same directory the bytecode file is in):

```elixir
iex> Math.sum(1, 2)
3
```

## Scripting mode

In addition to the Elixir file extension `.ex`, Elixir also supports `.exs` files for scripting. Elixir treats both files exactly the same way, the only difference is in intention. `.ex` files are meant to be compiled while `.exs` files are used for scripting. This convention is followed by projects like `mix`.

For instance, we can create a file called `math.exs`:

```elixir
defmodule Math do
  def sum(a, b) do
    a + b
  end
end

IO.puts Math.sum(1, 2)
```

And execute it as:

```console
$ elixir math.exs
```

Because we used `elixir` instead of `elixirc`, the module was compiled and loaded into memory, but no `.beam` file was written to disk.

Elixir projects are usually organized into three directories:

- `_build` - contains compilation artifacts
- `lib` - contains Elixir code (usually `.ex` files)
- `test` - contains tests (usually `.exs` files)

When working on actual projects, the build tool called `mix` will be responsible for compiling and setting up the proper paths for you. For learning and convenience purposes, we recommend you to write the following code into script files and execute them as shown above.
