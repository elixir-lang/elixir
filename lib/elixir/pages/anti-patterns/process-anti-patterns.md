# Process-related anti-patterns

This document outlines anti-patterns related to processes and process-based abstractions.

## Code organization by process

* **Problem:** This anti-pattern refers to code that is unnecessarily organized by processes. A process itself does not represent an anti-pattern, but it should only be used to model runtime properties (e.g., concurrency, access to shared resources, and event scheduling). When a process is used for code organization, it can create bottlenecks in the system.

* **Example:** An example of this anti-pattern, as shown below, is a library that implements arithmetic operations (e.g., add, subtract) by means of a `GenSever` process. If the number of calls to this single process grows, this code organization can compromise the system performance, therefore becoming a bottleneck.

  ```elixir
  defmodule Calculator do
    use GenServer

    @moduledoc """
      Calculator that performs two basic arithmetic operations.
      This code is unnecessarily organized by a GenServer process.
    """

    @doc """
      Function to perform the sum of two values.
    """
    def add(a, b, pid) do
      GenServer.call(pid, {:add, a, b})
    end

    @doc """
      Function to perform subtraction of two values.
    """
    def subtract(a, b, pid) do
      GenServer.call(pid, {:subtract, a, b})
    end

    def init(init_arg) do
      {:ok, init_arg}
    end

    def handle_call({:add, a, b}, _from, state) do
      {:reply, a + b, state}
    end

    def handle_call({:subtract, a, b}, _from, state) do
      {:reply, a - b, state}
    end
  end

  # Start a generic server process
  iex(1)> {:ok, pid} = GenServer.start_link(Calculator, :init)
  {:ok, #PID<0.132.0>}

  #...Use examples...
  iex(2)> Calculator.add(1, 5, pid)
  6

  iex(3)> Calculator.subtract(2, 3, pid)
  -1
  ```

* **Refactoring:** In Elixir, as shown next, code organization must be done only by modules and functions. Whenever possible, a library should not impose specific behavior (such as parallelization) on its clients. It is better to delegate this behavioral decision to the developers of clients, thus increasing the potential for code reuse of a library.

  ```elixir
  defmodule Calculator do
    def add(a, b) do
      a + b
    end

    def subtract(a, b) do
      a - b
    end
  end

  #...Use examples...

  iex(1)> Calculator.add(1, 5)
  6

  iex(2)> Calculator.subtract(2, 3)
  -1
  ```

## Scattered process interfaces

TODO.

## Unsupervised processes

TODO.
