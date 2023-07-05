# Process-related anti-patterns

This document outlines anti-patterns related to processes and process-based abstractions.

## Code organization by process

* **Problem:** This anti-pattern refers to code that is unnecessarily organized by processes. A process itself does not represent an anti-pattern, but it should only be used to model runtime properties (for example: concurrency, access to shared resources, and event scheduling). When a process is used for code organization, it can create bottlenecks in the system.

* **Example:** An example of this anti-pattern, as shown below, is a library that implements arithmetic operations (such as `add` and `subtract`) by means of a `GenSever` process. If the number of calls to this single process grows, this code organization can compromise the system performance, therefore becoming a bottleneck.

  ```elixir
  defmodule Calculator do
    @moduledoc """
    Calculator that performs two basic arithmetic operations.
    This code is unnecessarily organized by a GenServer process.
    """

    use GenServer

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

* **Problem:** In Elixir, creating a process outside a supervision tree is not an anti-pattern in itself. However, when code creates a large number of long-running processes outside a supervision tree, this can cause problems with the visibility, restart, and shutdown of a system, preventing developers from fully controlling their applications.

* **Example:** The following code example seeks to illustrate a module responsible for maintaining a numerical `Counter` through a `GenServer` process outside a supervision tree. Multiple counters can be created simultaneously by a client (one process for each counter), making these unsupervised processes difficult to manage.

  ```elixir
  defmodule Counter do
    @moduledoc """
    Global counter implemented through a GenServer process
    outside a supervision tree.
    """

    use GenServer

    @doc """
    Starts a counter.
    """
    def start_link(initial_value) when is_integer(initial_value) do
      GenServer.start(__MODULE__, initial_value, name: __MODULE__)
    end

    @doc """
    Get the counter's current value.
    """
    def get do
      GenServer.call(__MODULE__, :get)
    end

    @doc """
    Changes the counter's current value by `amount`.
    """
    def bump(amount) when is_integer(amount) do
      GenServer.call(__MODULE__, {:bump, amount})
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
      {:reply, counter + value, counter + value}
    end
  end

  #...Use examples...

  iex(1)> Counter.start_link(0)
  {:ok, #PID<0.115.0>}

  iex(2)> Counter.get()
  0

  iex(3)> Counter.bump(7)
  7

  iex(4)> Counter.bump(-3)
  4
  ```

* **Refactoring:** To ensure observability and control over the lifecycle of processes, all processes must be started inside a supervision tree. As shown below, this code uses a `Supervisor` as a supervision tree. When this Elixir application is started, two different counters (`Counter` and `C2`) are also started as child processes of the `Supervisor` named `MyApp.Supervisor`. Both are initialized with zero. By means of this supervision tree, it is possible to manage the lifecycle of all child processes (stopping or restarting each one), improving the visibility of the entire app.

  ```elixir
  defmodule MyApp.Application do
    use Application

    @impl true
    def start(_type, _args) do
      children = [
        {Counter, 0}
      ]

      opts = [strategy: :one_for_one, name: MyApp.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end

  #...Use examples...

  iex(1)> Supervisor.count_children(MyApp.Supervisor)
  %{active: 1, specs: 1, supervisors: 0, workers: 1}

  iex(2)> Counter.get()
  0

  iex(3)> Counter.bump(7)
  7

  iex(4)> Supervisor.terminate_child(MyApp.Supervisor, Counter)
  iex(5)> Supervisor.count_children(MyApp.Supervisor)
  %{active: 0, specs: 0, supervisors: 0, workers: 0}

  iex(6)> Counter.get()   #Error because it was previously terminated
  ** (EXIT) no process: the process is not alive...

  iex(7)> Supervisor.restart_child(MyApp.Supervisor, Counter)
  iex(8)> Counter.get()   #after the restart, this process can be accessed again
  0
  ```

  The application module above follows the same template as the one generated by `mix new my_app --sup`.

* **Additional remarks:** One of the few times where it is acceptable to start a process outside of a supervision tree is with `Task.async/1` and `Task.await/2`. Opposite to `Task.start_link/1`, the async/await mechanism gives you full control over the spawned process life cycle - which is also why you must always call `Task.await/2` after starting a task with `Task.async/1`. Even though, if your application is spawning multiple async processes, you should consider using `Task.Supervisor` for the reasons outlined above.

  Futhermore, if you are implementing a library, you should allow the users of your library to supervise processes inside their own supervision tree whenever possible. For example, the [Ecto](https://github.com/elixir-ecto/ecto) library requires you to start your database repository inside your supervision tree:

  ```elixir
  defmodule MyApp.Application do
    use Application

    @impl true
    def start(_type, _args) do
      children = [
        {MyApp.Repo, url: ...}
      ]

      ...
  ```

  This has benefits as it allows users of your library to configure and customize how the processes are started and supervised. Internally, the `Ecto` library also has its own supervision tree with its own processes, but those processes are not user-facing and instead are internal to Ecto's implementation.
