<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Introduction

Welcome!

This guide will teach you about Elixir fundamentals - the language syntax, how to define modules, the common data structures in the language, and more. This chapter will focus on ensuring that Elixir is installed and that you can successfully run Elixir's Interactive Shell, called IEx.

Let's get started.

## Installation

If you haven't yet installed Elixir, visit our [installation page](https://elixir-lang.org/install.html). Once you are done, you can run `elixir --version` to get the current Elixir version. The requirements for this guide are:

  * Elixir 1.15.0 onwards
  * Erlang/OTP 26 onwards

If you are looking for other resources for learning Elixir, you can also consult the [learning page](https://elixir-lang.org/learning.html) of the official website.

## Interactive mode

When you install Elixir, you will have three new command line executables: `iex`, `elixir` and `elixirc`.

For now, let's start by running `iex` (or `iex.bat` if you are on Windows PowerShell, where `iex` is a PowerShell command) which stands for Interactive Elixir. In interactive mode, we can type any Elixir expression and get its result. Let's warm up with some basic expressions.

Open up `iex` and type the following expressions:

```elixir
Erlang/OTP 26 [64-bit] [smp:2:2] [...]

Interactive Elixir - press Ctrl+C to exit
iex(1)> 40 + 2
42
iex(2)> "hello" <> " world"
"hello world"
```

Please note that some details like version numbers may differ a bit in your session, that's not important. By executing the code above, you should evaluate expressions and see their results. To exit `iex` press `Ctrl+C` twice.

It seems we are ready to go! We will use the interactive shell quite a lot in the next chapters to get a bit more familiar with the language constructs and basic types, starting in the next chapter.

## Running scripts

After getting familiar with the basics of the language you may want to try writing simple programs. This can be accomplished by putting the following Elixir code into a file:

```elixir
IO.puts("Hello world from Elixir")
```

Save it as `simple.exs` and execute it with `elixir`:

```console
$ elixir simple.exs
Hello world from Elixir
```

Later on we will learn [how to compile Elixir code](modules-and-functions.md) and how to create and work within Elixir projects using the Mix build tool. For now, let's move on to learn the basic data types in the language.
