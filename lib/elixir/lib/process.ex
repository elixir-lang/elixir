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

  """

  @doc """
  Tells whether the given process is alive.

  If the process identified by `pid` is alive (that is, it's not exiting and has
  not exited yet) than this function returns `true`. Otherwise, it returns
  `false`.

  `pid` must refer to a process running on the local node.

  Inlined by the compiler.
  """
  @spec alive?(pid) :: boolean
  def alive?(pid) do
    :erlang.is_process_alive(pid)
  end

  @doc """
  Returns all key-value pairs in the process dictionary.

  Inlined by the compiler.
  """
  @spec get :: [{term, term}]
  def get do
    :erlang.get()
  end

  @doc """
  Returns the value for the given `key` in the process dictionary,
  or `default` if `key` is not set.
  """
  @spec get(term) :: term
  @spec get(term, default :: term) :: term
  def get(key, default \\ nil) do
    case :erlang.get(key) do
      :undefined ->
        default
      value ->
        value
    end
  end

  @doc """
  Returns all keys in the process dictionary.

  Inlined by the compiler.
  """
  @spec get_keys() :: [term]
  def get_keys() do
    :erlang.get_keys()
  end

  @doc """
  Returns all keys in the process dictionary that have the given `value`.

  Inlined by the compiler.
  """
  @spec get_keys(term) :: [term]
  def get_keys(value) do
    :erlang.get_keys(value)
  end

  @doc """
  Stores the given `key`-`value` pair in the process dictionary.

  The return value of this function is the value that was previously stored
  under the key `key`, or `nil` in case no value was stored under `key`.

  ## Examples

      # Assuming :locale was not set
      Process.put(:locale, "en")
      #=> nil
      Process.put(:locale, "fr")
      #=> "en"

  """
  @spec put(term, term) :: term | nil
  def put(key, value) do
    nillify :erlang.put(key, value)
  end

  @doc """
  Deletes the given `key` from the process dictionary.
  """
  @spec delete(term) :: term | nil
  def delete(key) do
    nillify :erlang.erase(key)
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
  def exit(pid, reason) do
    :erlang.exit(pid, reason)
  end

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

  For example, if you are waiting a process to perform some
  action, it is better to communicate the progress of such action
  with messages.

  In other words, **do not**:

      Task.start_link fn ->
        do_something()
        ...
      end

      # Wait until work is done
      Process.sleep(2000)

  But **do**:

      parent = self()
      Task.start_link fn ->
        do_something()
        send parent, :work_is_done
        ...
      end

      receive do
        :work_is_done -> :ok
      after
        30_000 -> :timeout # Optional timeout
      end

  For cases like the one above, `Task.async/1` and `Task.await/2` are
  preferred.

  Similarly, if you are waiting for a process to terminate,
  monitor that process instead of sleeping. **Do not**:

      Task.start_link fn ->
        ...
      end

      # Wait until task terminates
      Process.sleep(2000)

  Instead **do**:

      {:ok, pid} =
        Task.start_link fn ->
          ...
        end

      ref = Process.monitor(pid)
      receive do
        {:DOWN, ^ref, _, _, _} -> :task_is_down
      after
        30_000 -> :timeout # Optional timeout
      end

  """
  def sleep(timeout)
      when is_integer(timeout) and timeout >= 0
      when timeout == :infinity do
    receive after: (timeout -> :ok)
  end

  @doc """
  Sends a message to the given process.

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
        when dest: pid | port | atom | {atom, node},
             msg: any,
             option: :noconnect | :nosuspend
  def send(dest, msg, options) do
    :erlang.send(dest, msg, options)
  end

  @doc """
  Sends `msg` to `dest` after `time` milliseconds.

  If `dest` is a PID, it must be the PID of a local process, dead or alive.
  If `dest` is an atom, it must be the name of a registered process
  which is looked up at the time of delivery. No error is produced if the name does
  not refer to a process.

  This function returns a timer reference, which can be read with `read_timer/1`
  or canceled with `cancel_timer/1`.

  The timer will be automatically canceled if the given `dest` is a PID
  which is not alive or when the given PID exits. Note that timers will not be
  automatically canceled when `dest` is an atom (as the atom resolution is done
  on delivery).

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
  """
  @spec cancel_timer(reference) :: non_neg_integer | false
  def cancel_timer(timer_ref) do
    :erlang.cancel_timer(timer_ref)
  end

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
  def read_timer(timer_ref) do
    :erlang.read_timer(timer_ref)
  end

  @type spawn_opt  :: :link | :monitor | {:priority, :low | :normal | :high} |
                      {:fullsweep_after, non_neg_integer} |
                      {:min_heap_size, non_neg_integer} |
                      {:min_bin_vheap_size, non_neg_integer}
  @type spawn_opts :: [spawn_opt]

  @doc """
  Spawns the given function according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  More options are available; for the comprehensive list of available options
  check [`:erlang.spawn_opt/4`](http://www.erlang.org/doc/man/erlang.html#spawn_opt-4).

  Inlined by the compiler.
  """
  @spec spawn((() -> any), spawn_opts) :: pid | {pid, reference}
  def spawn(fun, opts) do
    :erlang.spawn_opt(fun, opts)
  end

  @doc """
  Spawns the given function `fun` from module `mod`, passing the given `args`
  according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  It also accepts extra options, for the list of available options
  check [`:erlang.spawn_opt/4`](http://www.erlang.org/doc/man/erlang.html#spawn_opt-4).

  Inlined by the compiler.
  """
  @spec spawn(module, atom, list, spawn_opts) :: pid | {pid, reference}
  def spawn(mod, fun, args, opts) do
    :erlang.spawn_opt(mod, fun, args, opts)
  end

  @doc """
  Starts monitoring the given `item` from the calling process.

  This function returns the monitor reference.

  See [the need for monitoring](http://elixir-lang.org/getting-started/mix-otp/genserver.html#the-need-for-monitoring)
  for an example.
  See [`:erlang.monitor/2`](http://www.erlang.org/doc/man/erlang.html#monitor-2) for more info.

  Inlined by the compiler.
  """
  @spec monitor(pid | {reg_name :: atom, node :: atom} | reg_name :: atom) :: reference
  def monitor(item) do
    :erlang.monitor(:process, item)
  end

  @doc """
  Demonitors the monitor identifies by the given `reference`.

  If `monitor_ref` is a reference which the calling process
  obtained by calling `monitor/1`, that monitoring is turned off.
  If the monitoring is already turned off, nothing happens.

  See [`:erlang.demonitor/2`](http://www.erlang.org/doc/man/erlang.html#demonitor-2) for more info.

  Inlined by the compiler.
  """
  @spec demonitor(reference) :: true
  @spec demonitor(reference, options :: [:flush | :info]) :: boolean
  def demonitor(monitor_ref, options \\ []) do
    :erlang.demonitor(monitor_ref, options)
  end

  @doc """
  Returns a list of PIDs corresponding to all the
  processes currently existing on the local node.

  Note that if a process is exiting, it is considered to exist but not be
  alive. This means that for such process, `alive?/1` will return `false` but
  its PID will be part of the list of PIDs returned by this function.

  See [`:erlang.processes/0`](http://www.erlang.org/doc/man/erlang.html#processes-0) for more info.
  """
  @spec list :: [pid]
  def list do
    :erlang.processes()
  end

  @doc """
  Creates a link between the calling process and the given item (process or
  port).

  If such a link exists already, this function does nothing.

  See [`:erlang.link/1`](http://www.erlang.org/doc/man/erlang.html#link-1) for more info.

  Inlined by the compiler.
  """
  @spec link(pid | port) :: true
  def link(pid_or_port) do
    :erlang.link(pid_or_port)
  end

  @doc """

  Removes the link between the calling process and the given item (process or
  port).

  If there is no such link, this function does nothing. If `pid_or_port` does
  not exist, this function does not produce any errors and simply does nothing.

  The return value of this function is always `true`.

  See [`:erlang.unlink/1`](http://www.erlang.org/doc/man/erlang.html#unlink-1) for more info.

  Inlined by the compiler.
  """
  @spec unlink(pid | port) :: true
  def unlink(pid_or_port) do
    :erlang.unlink(pid_or_port)
  end

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

  """
  @spec register(pid | port, atom) :: true
  def register(pid_or_port, name) when is_atom(name) and name not in [nil, false, true, :undefined] do
    :erlang.register(name, pid_or_port)
  catch
    :error, :badarg when node(pid_or_port) != node()  ->
      message = "could not register the #{pid_or_port pid_or_port} because it belongs to another node"
      :erlang.error ArgumentError.exception(message), [pid_or_port, name]
    :error, :badarg ->
      message = "could not register the #{pid_or_port pid_or_port} with " <>
                "name #{inspect name}. Or it is not alive, or the name is already " <>
                "taken, or it has already been given another name"
      :erlang.error ArgumentError.exception(message), [pid_or_port, name]
  end

  defp pid_or_port(pid) when is_pid(pid), do: "pid #{inspect pid}"
  defp pid_or_port(port) when is_port(port), do: "port #{inspect port}"

  @doc """
  Removes the registered `name`, associated with a PID
  or a port identifier.

  Fails with `ArgumentError` if the name is not registered
  to any PID or port.
  """
  @spec unregister(atom) :: true
  def unregister(name) do
    :erlang.unregister(name)
  end

  @doc """
  Returns the PID or port identifier registered under `name` or `nil` if the
  name is not registered.

  See [`:erlang.whereis/1`](http://www.erlang.org/doc/man/erlang.html#whereis-1) for more info.
  """
  @spec whereis(atom) :: pid | port | nil
  def whereis(name) do
    nillify :erlang.whereis(name)
  end

  @doc """
  Returns the PID of the group leader for the calling process.
  """
  @spec group_leader :: pid
  def group_leader do
    :erlang.group_leader
  end

  @doc """
  Sets the group leader of the given `pid` to `leader`.

  Typically, this is used when a process started from a certain shell should
  have a group leader other than `:init`.
  """
  @spec group_leader(pid, leader :: pid) :: true
  def group_leader(pid, leader) do
    :erlang.group_leader(leader, pid)
  end

  @doc """
  Returns a list of names which have been registered using `register/2`.
  """
  @spec registered :: [atom]
  def registered do
    :erlang.registered()
  end

  @typep process_flag :: :trap_exit | :error_handler | :min_heap_size |
                         :min_bin_vheap_size | :priority | :save_calls |
                         :sensitive
  @doc """
  Sets the given `flag` to `value` for the calling process.

  Returns the old value of `flag`.

  See [`:erlang.process_flag/2`](http://www.erlang.org/doc/man/erlang.html#process_flag-2) for more info.
  """
  @spec flag(process_flag, term) :: term
  def flag(flag, value) do
    :erlang.process_flag(flag, value)
  end

  @doc """
  Sets the given `flag` to `value` for the given process `pid`.

  Returns the old value of `flag`.

  The allowed values for `flag` are only a subset of those allowed in `flag/2`,
  namely `:save_calls`.

  See [`:erlang.process_flag/3`](http://www.erlang.org/doc/man/erlang.html#process_flag-3) for more info.
  """
  @spec flag(pid, :save_calls, non_neg_integer) :: non_neg_integer
  def flag(pid, flag, value) do
    :erlang.process_flag(pid, flag, value)
  end

  @doc """
  Returns information about the process identified by `pid`, or returns `nil` if the process
  is not alive.

  Use this only for debugging information.

  See [`:erlang.process_info/1`](http://www.erlang.org/doc/man/erlang.html#process_info-1) for more info.
  """
  @spec info(pid) :: Keyword.t
  def info(pid) do
    nillify :erlang.process_info(pid)
  end

  @doc """
  Returns information about the process identified by `pid`,
  or returns `nil` if the process is not alive.

  See [`:erlang.process_info/2`](http://www.erlang.org/doc/man/erlang.html#process_info-2) for more info.
  """
  @spec info(pid, atom | [atom]) :: {atom, term} | [{atom, term}]  | nil
  def info(pid, spec)

  def info(pid, :registered_name) do
    case :erlang.process_info(pid, :registered_name) do
      :undefined -> nil
      [] -> {:registered_name, []}
      other -> other
    end
  end

  def info(pid, spec) when is_atom(spec) or is_list(spec) do
    nillify :erlang.process_info(pid, spec)
  end

  @doc """
  Puts the calling process into a "hibernation" state.

  The calling process is put into a waiting state
  where its memory allocation has been reduced as much as possible,
  which is useful if the process does not expect to receive any messages
  in the near future.

  See [`:erlang.hibernate/3`](http://www.erlang.org/doc/man/erlang.html#hibernate-3) for more info.

  Inlined by the compiler.
  """
  @spec hibernate(module, atom, list) :: no_return
  def hibernate(mod, fun, args) do
    :erlang.hibernate(mod, fun, args)
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other),      do: other
end
