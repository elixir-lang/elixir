defmodule Process do
  @moduledoc """
  Conveniences for working with processes and the process dictionary.

  Besides the functions available in this module, the `Kernel` module
  exposes and auto-imports some basic functionality related to processes
  available through the functions:

    * `Kernel.spawn/1` and `Kernel.spawn/3`
    * `Kernel.spawn_link/1` and `Kernel.spawn_link/3`
    * `Kernel.spawn_monitor/1` and `Kernel.spawn_monitor/3`
    * `Kernel.self/0`
    * `Kernel.send/2`

  """

  @doc """
  Returns `true` if the process exists and is alive, that is,
  is not exiting and has not exited. Otherwise, returns `false`.

  `pid` must refer to a process at the local node.
  """
  @spec alive?(pid) :: boolean
  def alive?(pid) do
    :erlang.is_process_alive(pid)
  end

  @doc """
  Returns all key-values in the dictionary.
  """
  @spec get :: [{term, term}]
  def get do
    :erlang.get()
  end

  @doc """
  Returns the value for the given `key`.
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
  Returns all keys that have the given `value`.
  """
  @spec get_keys(term) :: [term]
  def get_keys(value) do
    :erlang.get_keys(value)
  end

  @doc """
  Stores the given key-value in the process dictionary.
  """
  @spec put(term, term) :: term | nil
  def put(key, value) do
    nillify :erlang.put(key, value)
  end

  @doc """
  Deletes the given `key` from the dictionary.
  """
  @spec delete(term) :: term | nil
  def delete(key) do
    nillify :erlang.erase(key)
  end

  @doc """
  Sends an exit signal with the given reason to the pid.

  The following behaviour applies if reason is any term except `:normal` or `:kill`:

    1. If pid is not trapping exits, pid will exit with the given reason.

    2. If pid is trapping exits, the exit signal is transformed into a message
       `{:EXIT, from, reason}` and delivered to the message queue of pid.

    3. If reason is the atom `:normal`, pid will not exit (unless it is the calling
       process's pid, in which case it will exit with the reason `:normal`).
       If it is trapping exits, the exit signal is transformed into a message
       `{:EXIT, from, :normal}` and delivered to its message queue.

    4. If reason is the atom `:kill`, that is if `exit(pid, :kill)` is called,
       an untrappable exit signal is sent to pid which will unconditionally
       exit with exit reason `:killed`.

  Inlined by the compiler.

  ## Examples

      Process.exit(pid, :kill)

  """
  @spec exit(pid, term) :: true
  def exit(pid, reason) do
    :erlang.exit(pid, reason)
  end

  @doc """
  Sends a message to the given process.

  If the option `:noconnect` is used and sending the message would require an
  auto-connection to another node the message is not sent and `:noconnect` is
  returned.

  If the option `:nosuspend` is used and sending the message would cause the
  sender to be suspended the message is not sent and `:nosuspend` is returned.

  Otherwise the message is sent and `:ok` is returned.

  ## Examples

      iex> Process.send({:name, :node_does_not_exist}, :hi, [:noconnect])
      :noconnect

  """
  @spec send(dest, msg, [option]) ::  result when
        dest: pid | port | atom | {atom, node},
        msg: any,
        option: :noconnect | :nosuspend,
        result: :ok | :noconnect | :nosuspend
  def send(dest, msg, options) do
    :erlang.send(dest, msg, options)
  end

  @doc """
  Sends `msg` to `dest` after `time` milliseconds.

  If `dest` is a pid, it must be the pid of a local process, dead or alive.
  If `dest` is an atom, it must be the name of a registered process
  which is looked up at the time of delivery. No error is given if the name does
  not refer to a process.

  This function returns a timer reference, which can be read or canceled with
  `:erlang.read_timer/1`, `:erlang.start_timer/3` and `:erlang.cancel_timer/1`.
  Note `time` cannot be greater than `4294967295`.

  Finally, the timer will be automatically canceled if the given `dest` is a pid
  which is not alive or when the given pid exits. Note that timers will not be
  automatically canceled when `dest` is an atom (as the atom resolution is done
  on delivery).
  """
  @spec send_after(pid | atom, term, non_neg_integer) :: reference
  def send_after(dest, msg, time) do
    :erlang.send_after(time, dest, msg)
  end

  @type spawn_opt  :: :link | :monitor | {:priority, :low | :normal | :high} |
                      {:fullsweep_after, non_neg_integer} |
                      {:min_heap_size, non_neg_integer} |
                      {:min_bin_vheap_size, non_neg_integer}
  @type spawn_opts :: [spawn_opt]

  @doc """
  Spawns the given module and function passing the given args
  according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the pid and the monitoring reference, otherwise
  just the spawned process pid.

  It also accepts extra options, for the list of available options
  check http://www.erlang.org/doc/man/erlang.html#spawn_opt-4

  Inlined by the compiler.
  """
  @spec spawn((() -> any), spawn_opts) :: pid | {pid, reference}
  def spawn(fun, opts) do
    :erlang.spawn_opt(fun, opts)
  end

  @doc """
  Spawns the given module and function passing the given args
  according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the pid and the monitoring reference, otherwise
  just the spawned process pid.

  It also accepts extra options, for the list of available options
  check http://www.erlang.org/doc/man/erlang.html#spawn_opt-4

  Inlined by the compiler.
  """
  @spec spawn(module, atom, list, spawn_opts) :: pid | {pid, reference}
  def spawn(mod, fun, args, opts) do
    :erlang.spawn_opt(mod, fun, args, opts)
  end

  @doc """
  The calling process starts monitoring the item given.
  It returns the monitor reference.

  See http://www.erlang.org/doc/man/erlang.html#monitor-2 for more info.

  Inlined by the compiler.
  """
  @spec monitor(pid | {reg_name :: atom, node :: atom} | reg_name :: atom) :: reference
  def monitor(item) do
    :erlang.monitor(:process, item)
  end

  @doc """
  If `monitor_ref` is a reference which the calling process
  obtained by calling `monitor/1`, this monitoring is turned off.
  If the monitoring is already turned off, nothing happens.

  See http://www.erlang.org/doc/man/erlang.html#demonitor-2 for more info.

  Inlined by the compiler.
  """
  @spec demonitor(reference) :: true
  @spec demonitor(reference, options :: [:flush | :info]) :: boolean
  def demonitor(monitor_ref, options \\ []) do
    :erlang.demonitor(monitor_ref, options)
  end

  @doc """
  Returns a list of process identifiers corresponding to all the
  processes currently existing on the local node.

  Note that a process that is exiting, exists but is not alive, i.e.,
  `alive?/1` will return `false` for a process that is exiting,
  but its process identifier will be part of the result returned.

  See http://www.erlang.org/doc/man/erlang.html#processes-0 for more info.
  """
  @spec list :: [pid]
  def list do
    :erlang.processes()
  end

  @doc """
  Creates a link between the calling process and another process
  (or port) `pid`, if there is not such a link already.

  See http://www.erlang.org/doc/man/erlang.html#link-1 for more info.

  Inlined by the compiler.
  """
  @spec link(pid | port) :: true
  def link(pid) do
    :erlang.link(pid)
  end

  @doc """
  Removes the link, if there is one, between the calling process and
  the process or port referred to by `pid`. Returns `true` and does not
  fail, even if there is no link or `id` does not exist

  See http://www.erlang.org/doc/man/erlang.html#unlink-1 for more info.

  Inlined by the compiler.
  """
  @spec unlink(pid | port) :: true
  def unlink(pid) do
    :erlang.unlink(pid)
  end

  @doc """
  Associates the name with a pid or a port identifier. `name`, which must
  be an atom, can be used instead of the pid / port identifier with the
  `Kernel.send/2` function.

  `Process.register/2` will fail with `ArgumentError` if the pid supplied
  is no longer alive, (check with `alive?/1`) or if the name is
  already registered (check with `whereis/1`).
  """
  @spec register(pid | port, atom) :: true
  def register(pid, name) when not name in [nil, false, true] do
    :erlang.register(name, pid)
  end

  @doc """
  Removes the registered name, associated with a pid or a port identifier.

  See http://www.erlang.org/doc/man/erlang.html#unregister-1 for more info.
  """
  @spec unregister(atom) :: true
  def unregister(name) do
    :erlang.unregister(name)
  end

  @doc """
  Returns the pid or port identifier with the registered name.
  Returns `nil` if the name is not registered.

  See http://www.erlang.org/doc/man/erlang.html#whereis-1 for more info.
  """
  @spec whereis(atom) :: pid | port | nil
  def whereis(name) do
    nillify :erlang.whereis(name)
  end

  @doc """
  Returns the pid of the group leader for the process which evaluates the function.
  """
  @spec group_leader :: pid
  def group_leader do
    :erlang.group_leader
  end

  @doc """
  Sets the group leader of `pid` to `leader`. Typically, this is used when a processes
  started from a certain shell should have a group leader other than `:init`.
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
  Sets certain flags for the process which calls this function.
  Returns the old value of the flag.

  See http://www.erlang.org/doc/man/erlang.html#process_flag-2 for more info.
  """
  @spec flag(process_flag, term) :: term
  def flag(flag, value) do
    :erlang.process_flag(flag, value)
  end

  @doc """
  Sets certain flags for the process `pid`, in the same manner as `flag/2`.
  Returns the old value of the flag. The allowed values for `flag` are
  only a subset of those allowed in `flag/2`, namely: `save_calls`.

  See http://www.erlang.org/doc/man/erlang.html#process_flag-3 for more info.
  """
  @spec flag(pid, :save_calls, non_neg_integer) :: non_neg_integer
  def flag(pid, flag, value) do
    :erlang.process_flag(pid, flag, value)
  end

  @doc """
  Returns information about the process identified by `pid` or `nil` if the process
  is not alive.
  Use this only for debugging information.

  See http://www.erlang.org/doc/man/erlang.html#process_info-1 for more info.
  """
  @spec info(pid) :: Keyword.t
  def info(pid) do
    nillify :erlang.process_info(pid)
  end

  @doc """
  Returns information about the process identified by `pid`
  or `nil` if the process is not alive.

  See http://www.erlang.org/doc/man/erlang.html#process_info-2 for more info.
  """
  @spec info(pid, atom) :: {atom, term} | nil
  def info(pid, spec)

  def info(pid, :registered_name) do
    case :erlang.process_info(pid, :registered_name) do
      :undefined -> nil
      [] -> {:registered_name, []}
      other -> other
    end
  end

  def info(pid, spec) when is_atom(spec) do
    nillify :erlang.process_info(pid, spec)
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other),      do: other
end
