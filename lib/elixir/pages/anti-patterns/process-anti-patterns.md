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

* **Problem:** In Elixir, creating a process outside a supervision tree is not an anti-pattern in itself. However, when code creates a large number of long-running processes outside a supervision tree, this can make visibility and monitoring of these processes difficult, preventing developers from fully controlling their applications.

* **Example:** The following code example seeks to illustrate a library responsible for maintaining a numerical `Counter` through a `GenServer` process outside a supervision tree. Multiple counters can be created simultaneously by a client (one process for each counter), making these unsupervised processes difficult to manage. This can cause problems with the initialization, restart, and shutdown of a system.

  ```elixir
  defmodule Counter do
    use GenServer

    @moduledoc """
      Global counter implemented through a GenServer process
      outside a supervision tree.
    """

    @doc """
      Function to create a counter.
        initial_value: any integer value.
        pid_name: optional parameter to define the process name.
                  Default is Counter.
    """
    def start(initial_value, pid_name \\ __MODULE__)
      when is_integer(initial_value) do
      GenServer.start(__MODULE__, initial_value, name: pid_name)
    end

    @doc """
      Function to get the counter's current value.
        pid_name: optional parameter to inform the process name.
                  Default is Counter.
    """
    def get(pid_name \\ __MODULE__) do
      GenServer.call(pid_name, :get)
    end

    @doc """
      Function to changes the counter's current value.
      Returns the updated value.
        value: amount to be added to the counter.
        pid_name: optional parameter to inform the process name.
                  Default is Counter.
    """
    def bump(value, pid_name \\ __MODULE__) do
      GenServer.call(pid_name, {:bump, value})
      get(pid_name)
    end

    ## Callbacks

    @impl true
    def init(counter) do
      {:ok, counter}
    end

    @impl true
    def handle_call(:get, _from, counter) do
      {:reply, counter, counter}
    end

    def handle_call({:bump, value}, _from, counter) do
      {:reply, counter, counter + value}
    end
  end

  #...Use examples...

  iex(1)> Counter.start(0)
  {:ok, #PID<0.115.0>}

  iex(2)> Counter.get()
  0

  iex(3)> Counter.start(15, C2)
  {:ok, #PID<0.120.0>}

  iex(4)> Counter.get(C2)
  15

  iex(5)> Counter.bump(-3, C2)
  12

  iex(6)> Counter.bump(7)
  7
  ```

* **Refactoring:** To ensure that clients of a library have full control over their systems, regardless of the number of processes used and the lifetime of each one, all processes must be started inside a supervision tree. As shown below, this code uses a [`Supervisor`](https://hexdocs.pm/elixir/main/Supervisor.html) as a supervision tree. When this Elixir application is started, two different counters (`Counter` and `C2`) are also started as child processes of the `Supervisor` named `App.Supervisor`. Both are initialized with zero. By means of this supervision tree, it is possible to manage the lifecycle of all child processes (e.g., stopping or restarting each one), improving the visibility of the entire app.

  ```elixir
  defmodule SupervisedProcess.Application do
    use Application

    @impl true
    def start(_type, _args) do
      children = [
        # The counters are Supervisor children started via Counter.start(0).
        %{
          id: Counter,
          start: {Counter, :start, [0]}
        },
        %{
          id: C2,
          start: {Counter, :start, [0, C2]}
        }
      ]

      opts = [strategy: :one_for_one, name: App.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end

  #...Use examples...

  iex(1)> Supervisor.count_children(App.Supervisor)
  %{active: 2, specs: 2, supervisors: 0, workers: 2}

  iex(2)> Counter.get(Counter)
  0

  iex(3)> Counter.get(C2)
  0

  iex(4)> Counter.bump(7, Counter)
  7

  iex(5)> Supervisor.terminate_child(App.Supervisor, Counter)
  iex(6)> Supervisor.count_children(App.Supervisor)
  %{active: 1, specs: 2, supervisors: 0, workers: 2}  #only one active

  iex(7)> Counter.get(Counter)   #Error because it was previously terminated
  ** (EXIT) no process: the process is not alive...

  iex(8)> Supervisor.restart_child(App.Supervisor, Counter)
  iex(9)> Counter.get(Counter)   #after the restart, this process can be accessed again
  0
  ```
