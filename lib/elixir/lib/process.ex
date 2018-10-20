defmodule Process do
  @moduledoc """
  Conveniences for working with processes and the process dictionary.

  Besides the functions available in this module, the `Kernel` module
  exposes and auto-imports some basic functionality related to processes
  available through the following functions:

    * `Kernel.spawn/1` and `Kernel.spawn/3`
    * `Kernel.spawn_link/1` and `Kernel.spawn_link/3`
    * `Kernel.spawn_monitor/1` and `Kernel.spawn_monitor/3`
    * `Kernel.self/0`
    * `Kernel.send/2`

  While this module provides low-level conveniences to work with processes,
  developers typically use abstractions such as `Agent`, `GenServer`,
  `Registry`, `Supervisor` and `Task` for building their systems and
  resort to this module for gathering information, trapping exits, links
  and monitoring.
  """

  @typedoc """
  A process destination.

  A remote or local PID, a local port, a locally registered name, or a tuple in
  the form of `{registered_name, node}` for a registered name at another node.
  """
  @type dest :: pid | port | (registered_name :: atom) | {registered_name :: atom, node}

  @doc """
  Tells whether the given process is alive on the local node.

  If the process identified by `pid` is alive (that is, it's not exiting and has
  not exited yet) than this function returns `true`. Otherwise, it returns
  `false`.

  `pid` must refer to a process running on the local node or `ArgumentError` is raised.

  Inlined by the compiler.
  """
  @spec alive?(pid) :: boolean
  defdelegate alive?(pid), to: :erlang, as: :is_process_alive

  @doc """
  Returns all key-value pairs in the process dictionary.

  Inlined by the compiler.
  """
  @spec get() :: [{term, term}]
  defdelegate get(), to: :erlang

  @doc """
  Returns the value for the given `key` in the process dictionary,
  or `default` if `key` is not set.

  ## Examples

      # Assuming :locale was not set
      iex> Process.get(:locale, "pt")
      "pt"
      iex> Process.put(:locale, "fr")
      nil
      iex> Process.get(:locale, "pt")
      "fr"

  """
  @spec get(term, default :: term) :: term
  def get(key, default \\ nil) do
    case :erlang.get(key) do
      :undefined -> default
      value -> value
    end
  end

  @doc """
  Returns all keys in the process dictionary.

  Inlined by the compiler.

  ## Examples

      # Assuming :locale was not set
      iex> :locale in Process.get_keys()
      false
      iex> Process.put(:locale, "pt")
      nil
      iex> :locale in Process.get_keys()
      true

  """
  @spec get_keys() :: [term]
  defdelegate get_keys(), to: :erlang

  @doc """
  Returns all keys in the process dictionary that have the given `value`.

  Inlined by the compiler.
  """
  @spec get_keys(term) :: [term]
  defdelegate get_keys(value), to: :erlang

  @doc """
  Stores the given `key`-`value` pair in the process dictionary.

  The return value of this function is the value that was previously stored
  under `key`, or `nil` in case no value was stored under it.

  ## Examples

      # Assuming :locale was not set
      iex> Process.put(:locale, "en")
      nil
      iex> Process.put(:locale, "fr")
      "en"

  """
  @spec put(term, term) :: term | nil
  def put(key, value) do
    nillify(:erlang.put(key, value))
  end

  @doc """
  Deletes the given `key` from the process dictionary.

  Returns the value that was under `key` in the process dictionary,
  or `nil` if `key` was not stored in the process dictionary.

  ## Examples

      iex> Process.put(:comments, ["comment", "other comment"])
      iex> Process.delete(:comments)
      ["comment", "other comment"]
      iex> Process.delete(:comments)
      nil

  """
  @spec delete(term) :: term | nil
  def delete(key) do
    nillify(:erlang.erase(key))
  end

  @doc """
  Sends an exit signal with the given `reason` to `pid`.

  The following behaviour applies if `reason` is any term except `:normal`
  or `:kill`:

    1. If `pid` is not trapping exits, `pid` will exit with the given
       `reason`.

    2. If `pid` is trapping exits, the exit signal is transformed into a
       message `{:EXIT, from, reason}` and delivered to the message queue
       of `pid`.

  If `reason` is the atom `:normal`, `pid` will not exit (unless `pid` is
  the calling process, in which case it will exit with the reason `:normal`).
  If it is trapping exits, the exit signal is transformed into a message
  `{:EXIT, from, :normal}` and delivered to its message queue.

  If `reason` is the atom `:kill`, that is if `Process.exit(pid, :kill)` is called,
  an untrappable exit signal is sent to `pid` which will unconditionally exit
  with reason `:killed`.

  Inlined by the compiler.

  ## Examples

      Process.exit(pid, :kill)
      #=> true

  """
  @spec exit(pid, term) :: true
  defdelegate exit(pid, reason), to: :erlang

  @doc """
  Sleeps the current process for the given `timeout`.

  `timeout` is either the number of milliseconds to sleep as an
  integer or the atom `:infinity`. When `:infinity` is given,
  the current process will sleep forever, and not
  consume or reply to messages.

  **Use this function with extreme care**. For almost all situations
  where you would use `sleep/1` in Elixir, there is likely a
  more correct, faster and precise way of achieving the same with
  message passing.

  For example, if you are waiting for a process to perform some
  action, it is better to communicate the progress of such action
  with messages.

  In other words, **do not**:

      Task.start_link(fn ->
        do_something()
        ...
      end)

      # Wait until work is done
      Process.sleep(2000)

  But **do**:

      parent = self()

      Task.start_link(fn ->
        do_something()
        send(parent, :work_is_done)
        ...
      end)

      receive do
        :work_is_done -> :ok
      after
        # Optional timeout
        30_000 -> :timeout
      end

  For cases like the one above, `Task.async/1` and `Task.await/2` are
  preferred.

  Similarly, if you are waiting for a process to terminate,
  monitor that process instead of sleeping. **Do not**:

      Task.start_link(fn ->
        ...
      end)

      # Wait until task terminates
      Process.sleep(2000)

  Instead **do**:

      {:ok, pid} =
        Task.start_link(fn ->
          ...
        end)

      ref = Process.monitor(pid)

      receive do
        {:DOWN, ^ref, _, _, _} -> :task_is_down
      after
        # Optional timeout
        30_000 -> :timeout
      end

  """
  @spec sleep(timeout) :: :ok
  def sleep(timeout)
      when is_integer(timeout) and timeout >= 0
      when timeout == :infinity do
    receive after: (timeout -> :ok)
  end

  @doc """
  Sends a message to the given `dest`.

  `dest` may be a remote or local PID, a local port, a locally
  registered name, or a tuple in the form of `{registered_name, node}` for a
  registered name at another node.

  Inlined by the compiler.

  ## Options

    * `:noconnect` - when used, if sending the message would require an
      auto-connection to another node the message is not sent and `:noconnect` is
      returned.

    * `:nosuspend` - when used, if sending the message would cause the sender to
      be suspended the message is not sent and `:nosuspend` is returned.

  Otherwise the message is sent and `:ok` is returned.

  ## Examples

      iex> Process.send({:name, :node_that_does_not_exist}, :hi, [:noconnect])
      :noconnect

  """
  @spec send(dest, msg, [option]) :: :ok | :noconnect | :nosuspend
        when dest: dest(),
             msg: any,
             option: :noconnect | :nosuspend
  defdelegate send(dest, msg, options), to: :erlang

  @doc """
  Sends `msg` to `dest` after `time` milliseconds.

  If `dest` is a PID, it must be the PID of a local process, dead or alive.
  If `dest` is an atom, it must be the name of a registered process
  which is looked up at the time of delivery. No error is produced if the name does
  not refer to a process.

  The message is not sent immediately. Therefore, `dest` can receive other messages
  in-between even when `time` is `0`.

  This function returns a timer reference, which can be read with `read_timer/1`
  or canceled with `cancel_timer/1`.

  The timer will be automatically canceled if the given `dest` is a PID
  which is not alive or when the given PID exits. Note that timers will not be
  automatically canceled when `dest` is an atom (as the atom resolution is done
  on delivery).

  Inlined by the compiler.

  ## Options

    * `:abs` - (boolean) when `false`, `time` is treated as relative to the
    current monotonic time. When `true`, `time` is the absolute value of the
    Erlang monotonic time at which `msg` should be delivered to `dest`.
    To read more about Erlang monotonic time and other time-related concepts,
    look at the documentation for the `System` module. Defaults to `false`.

  ## Examples

      timer_ref = Process.send_after(pid, :hi, 1000)

  """
  @spec send_after(pid | atom, term, non_neg_integer, [option]) :: reference
        when option: {:abs, boolean}
  def send_after(dest, msg, time, opts \\ []) do
    :erlang.send_after(time, dest, msg, opts)
  end

  @doc """
  Cancels a timer returned by `send_after/3`.

  When the result is an integer, it represents the time in milliseconds
  left until the timer would have expired.

  When the result is `false`, a timer corresponding to `timer_ref` could not be
  found. This can happen either because the timer expired, because it has
  already been canceled, or because `timer_ref` never corresponded to a timer.

  Even if the timer had expired and the message was sent, this function does not
  tell you if the timeout message has arrived at its destination yet.

  Inlined by the compiler.

  ## Options

    * `:async` - (boolean) when `false`, the request for cancellation is
      synchronous. When `true`, the request for cancellation is asynchronous,
      meaning that the request to cancel the timer is issued and `:ok` is
      returned right away. Defaults to `false`.

    * `:info` - (boolean) whether to return information about the timer being
      cancelled. When the `:async` option is `false` and `:info` is `true`, then
      either an integer or `false` (like described above) is returned. If
      `:async` is `false` and `:info` is `false`, `:ok` is returned. If `:async`
      is `true` and `:info` is `true`, a message in the form `{:cancel_timer,
      timer_ref, result}` (where `result` is an integer or `false` like
      described above) is sent to the caller of this function when the
      cancellation has been performed. If `:async` is `true` and `:info` is
      `false`, no message is sent. Defaults to `true`.

  """
  @spec cancel_timer(reference, options) :: non_neg_integer | false | :ok
        when options: [async: boolean, info: boolean]
  defdelegate cancel_timer(timer_ref, options \\ []), to: :erlang

  @doc """
  Reads a timer created by `send_after/3`.

  When the result is an integer, it represents the time in milliseconds
  left until the timer will expire.

  When the result is `false`, a timer corresponding to `timer_ref` could not be
  found. This can be either because the timer expired, because it has already
  been canceled, or because `timer_ref` never corresponded to a timer.

  Even if the timer had expired and the message was sent, this function does not
  tell you if the timeout message has arrived at its destination yet.

  Inlined by the compiler.
  """
  @spec read_timer(reference) :: non_neg_integer | false
  defdelegate read_timer(timer_ref), to: :erlang

  @type spawn_opt ::
          :link
          | :monitor
          | {:priority, :low | :normal | :high}
          | {:fullsweep_after, non_neg_integer}
          | {:min_heap_size, non_neg_integer}
          | {:min_bin_vheap_size, non_neg_integer}
  @type spawn_opts :: [spawn_opt]

  @doc """
  Spawns the given function according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  More options are available; for the comprehensive list of available options
  check `:erlang.spawn_opt/4`.

  Inlined by the compiler.

  ## Examples

      Process.spawn(fn -> 1 + 2 end, [:monitor])
      #=> {#PID<0.93.0>, #Reference<0.18808174.1939079169.202418>}
      Process.spawn(fn -> 1 + 2 end, [:link])
      #=> #PID<0.95.0>

  """
  @spec spawn((() -> any), spawn_opts) :: pid | {pid, reference}
  defdelegate spawn(fun, opts), to: :erlang, as: :spawn_opt

  @doc """
  Spawns the given function `fun` from module `mod`, passing the given `args`
  according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  It also accepts extra options, for the list of available options
  check `:erlang.spawn_opt/4`.

  Inlined by the compiler.
  """
  @spec spawn(module, atom, list, spawn_opts) :: pid | {pid, reference}
  defdelegate spawn(mod, fun, args, opts), to: :erlang, as: :spawn_opt

  @doc """
  Starts monitoring the given `item` from the calling process.

  Once the monitored process dies, a message is delivered to the
  monitoring process in the shape of:

      {:DOWN, ref, :process, object, reason}

  where:

    * `ref` is a monitor reference returned by this function;
    * `object` is either a `pid` of the monitored process (if monitoring
      a PID) or `{name, node}` (if monitoring a remote or local name);
    * `reason` is the exit reason.

  If the process is already dead when calling `Process.monitor/1`, a
  `:DOWN` message is delivered immediately.

  See [the need for monitoring](https://elixir-lang.org/getting-started/mix-otp/genserver.html#the-need-for-monitoring)
  for an example. See `:erlang.monitor/2` for more information.

  Inlined by the compiler.

  ## Examples

      pid = spawn(fn -> 1 + 2 end)
      #=> #PID<0.118.0>
      Process.monitor(pid)
      #=> #Reference<0.906660723.3006791681.40191>
      Process.exit(pid, :kill)
      #=> true
      receive do
        msg -> msg
      end
      #=> {:DOWN, #Reference<0.906660723.3006791681.40191>, :process, #PID<0.118.0>, :noproc}

  """
  @spec monitor(pid | {name, node} | name) :: reference when name: atom
  def monitor(item) do
    :erlang.monitor(:process, item)
  end

  @doc """
  Demonitors the monitor identified by the given `reference`.

  If `monitor_ref` is a reference which the calling process
  obtained by calling `monitor/1`, that monitoring is turned off.
  If the monitoring is already turned off, nothing happens.

  See `:erlang.demonitor/2` for more information.

  Inlined by the compiler.

  ## Examples

      pid = spawn(fn -> 1 + 2 end)
      ref = Process.monitor(pid)
      Process.demonitor(ref)
      #=> true

  """
  @spec demonitor(reference, options :: [:flush | :info]) :: boolean
  defdelegate demonitor(monitor_ref, options \\ []), to: :erlang

  @doc """
  Returns a list of PIDs corresponding to all the
  processes currently existing on the local node.

  Note that if a process is exiting, it is considered to exist but not be
  alive. This means that for such process, `alive?/1` will return `false` but
  its PID will be part of the list of PIDs returned by this function.

  See `:erlang.processes/0` for more information.

  Inlined by the compiler.

  ## Examples

      Process.list()
      #=> [#PID<0.0.0>, #PID<0.1.0>, #PID<0.2.0>, #PID<0.3.0>, ...]

  """
  @spec list() :: [pid]
  defdelegate list(), to: :erlang, as: :processes

  @doc """
  Creates a link between the calling process and the given item (process or
  port).

  Links are bidirectional. Linked processes can be unlinked by using `unlink/1`.

  If such a link exists already, this function does nothing since there can only
  be one link between two given processes. If a process tries to create a link
  to itself, nothing will happen.

  When two processes are linked, each one receives exit signals from the other
  (see also `exit/2`). Let's assume `pid1` and `pid2` are linked. If `pid2`
  exits with a reason other than `:normal` (which is also the exit reason used
  when a process finishes its job) and `pid1` is not trapping exits (see
  `flag/2`), then `pid1` will exit with the same reason as `pid2` and in turn
  emit an exit signal to all its other linked processes. The behaviour when
  `pid1` is trapping exits is described in `exit/2`.

  See `:erlang.link/1` for more information.

  Inlined by the compiler.
  """
  @spec link(pid | port) :: true
  defdelegate link(pid_or_port), to: :erlang

  @doc """
  Removes the link between the calling process and the given item (process or
  port).

  If there is no such link, this function does nothing. If `pid_or_port` does
  not exist, this function does not produce any errors and simply does nothing.

  The return value of this function is always `true`.

  See `:erlang.unlink/1` for more information.

  Inlined by the compiler.
  """
  @spec unlink(pid | port) :: true
  defdelegate unlink(pid_or_port), to: :erlang

  @doc """
  Registers the given `pid_or_port` under the given `name`.

  `name` must be an atom and can then be used instead of the
  PID/port identifier when sending messages with `Kernel.send/2`.

  `register/2` will fail with `ArgumentError` in any of the following cases:

    * the PID/Port is not existing locally and alive
    * the name is already registered
    * the `pid_or_port` is already registered under a different `name`

  The following names are reserved and cannot be assigned to
  processes nor ports:

    * `nil`
    * `false`
    * `true`
    * `:undefined`

  ## Examples

      Process.register(self(), :test)
      #=> true
      send(:test, :hello)
      #=> :hello
      send(:wrong_name, :hello)
      #=> ** (ArgumentError) argument error

  """
  @spec register(pid | port, atom) :: true
  def register(pid_or_port, name)
      when is_atom(name) and name not in [nil, false, true, :undefined] do
    :erlang.register(name, pid_or_port)
  catch
    :error, :badarg when node(pid_or_port) != node() ->
      message = "could not register #{inspect(pid_or_port)} because it belongs to another node"
      :erlang.error(ArgumentError.exception(message), [pid_or_port, name])

    :error, :badarg ->
      message =
        "could not register #{inspect(pid_or_port)} with " <>
          "name #{inspect(name)} because it is not alive, the name is already " <>
          "taken, or it has already been given another name"

      :erlang.error(ArgumentError.exception(message), [pid_or_port, name])
  end

  @doc """
  Removes the registered `name`, associated with a PID
  or a port identifier.

  Fails with `ArgumentError` if the name is not registered
  to any PID or port.

  Inlined by the compiler.

  ## Examples

      Process.register(self(), :test)
      #=> true
      Process.unregister(:test)
      #=> true
      Process.unregister(:wrong_name)
      #=> ** (ArgumentError) argument error

  """
  @spec unregister(atom) :: true
  defdelegate unregister(name), to: :erlang

  @doc """
  Returns the PID or port identifier registered under `name` or `nil` if the
  name is not registered.

  See `:erlang.whereis/1` for more information.

  ## Examples

      Process.register(self(), :test)
      Process.whereis(:test)
      #=> #PID<0.84.0>
      Process.whereis(:wrong_name)
      #=> nil

  """
  @spec whereis(atom) :: pid | port | nil
  def whereis(name) do
    nillify(:erlang.whereis(name))
  end

  @doc """
  Returns the PID of the group leader for the calling process.

  Inlined by the compiler.

  ## Examples

      Process.group_leader()
      #=> #PID<0.53.0>

  """
  @spec group_leader() :: pid
  defdelegate group_leader(), to: :erlang

  @doc """
  Sets the group leader of the given `pid` to `leader`.

  Typically, this is used when a process started from a certain shell should
  have a group leader other than `:init`.

  Inlined by the compiler.
  """
  @spec group_leader(pid, leader :: pid) :: true
  def group_leader(pid, leader) do
    :erlang.group_leader(leader, pid)
  end

  @doc """
  Returns a list of names which have been registered using `register/2`.

  Inlined by the compiler.

  ## Examples

      Process.register(self(), :test)
      Process.registered()
      #=> [:test, :elixir_config, :inet_db, ...]

  """
  @spec registered() :: [atom]
  defdelegate registered(), to: :erlang

  @typep heap_size ::
           non_neg_integer
           | %{size: non_neg_integer, kill: boolean, error_logger: boolean}

  @typep priority_level :: :low | :normal | :high | :max

  @doc """
  Sets the given `flag` to `value` for the calling process.

  Returns the old value of `flag`.

  See `:erlang.process_flag/2` for more information.

  Inlined by the compiler.
  """
  @spec flag(:error_handler, module) :: module
  @spec flag(:max_heap_size, heap_size) :: heap_size
  @spec flag(:message_queue_data, :erlang.message_queue_data()) :: :erlang.message_queue_data()
  @spec flag(:min_bin_vheap_size, non_neg_integer) :: non_neg_integer
  @spec flag(:min_heap_size, non_neg_integer) :: non_neg_integer
  @spec flag(:monitor_nodes, term) :: term
  @spec flag({:monitor_nodes, term()}, term) :: term
  @spec flag(:priority, priority_level) :: priority_level
  @spec flag(:save_calls, 0..10000) :: 0..10000
  @spec flag(:sensitive, boolean) :: boolean
  @spec flag(:trap_exit, boolean) :: boolean
  defdelegate flag(flag, value), to: :erlang, as: :process_flag

  @doc """
  Sets the given `flag` to `value` for the given process `pid`.

  Returns the old value of `flag`.

  It raises `ArgumentError` if `pid` is not a local process.

  The allowed values for `flag` are only a subset of those allowed in `flag/2`,
  namely `:save_calls`.

  See `:erlang.process_flag/3` for more information.

  Inlined by the compiler.
  """
  @spec flag(pid, :save_calls, 0..10000) :: 0..10000
  defdelegate flag(pid, flag, value), to: :erlang, as: :process_flag

  @doc """
  Returns information about the process identified by `pid`, or returns `nil` if the process
  is not alive.

  Use this only for debugging information.

  See `:erlang.process_info/1` for more information.
  """
  @spec info(pid) :: keyword | nil
  def info(pid) do
    nillify(:erlang.process_info(pid))
  end

  @doc """
  Returns information about the process identified by `pid`,
  or returns `nil` if the process is not alive.

  See `:erlang.process_info/2` for more information.
  """
  @spec info(pid, atom | [atom]) :: {atom, term} | [{atom, term}] | nil
  def info(pid, spec)

  def info(pid, :registered_name) do
    case :erlang.process_info(pid, :registered_name) do
      :undefined -> nil
      [] -> {:registered_name, []}
      other -> other
    end
  end

  def info(pid, spec) when is_atom(spec) or is_list(spec) do
    nillify(:erlang.process_info(pid, spec))
  end

  @doc """
  Puts the calling process into a "hibernation" state.

  The calling process is put into a waiting state
  where its memory allocation has been reduced as much as possible,
  which is useful if the process does not expect to receive any messages
  in the near future.

  See `:erlang.hibernate/3` for more information.

  Inlined by the compiler.
  """
  @spec hibernate(module, atom, list) :: no_return
  defdelegate hibernate(mod, fun_name, args), to: :erlang

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other), do: other
end
