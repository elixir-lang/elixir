defmodule Process do
  @moduledoc """
  This module provides convenience functions around processes and
  the process dictionary. In Erlang, most of these functions are
  auto-imported, but in Elixir they are grouped in a module for
  convenience. Notice that these functions, different from Erlang's,
  always return nil instead of undefined. You can use their Erlang
  version if you want the undefined value.
  """

  @doc """
  Returns true if the process exists and is alive, that is,
  is not exiting and has not exited. Otherwise, returns false.

  `pid` must refer to a process at the local node.
  """
  def alive?(pid) do
    :erlang.is_process_alive(pid)
  end

  @doc """
  Returns the current process.
  """
  def self do
    :erlang.self()
  end

  @doc """
  Returns all key-values in the dictionary
  with no specific ordering (i.e. they are
  not a keyword list).
  """
  def get do
    :erlang.get()
  end

  @doc """
  Returns the value for the given key.
  """
  def get(key, default // nil) do
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
  def get_keys(value) do
    :erlang.get_keys(value)
  end

  @doc """
  Stores the given key-value in the process dictionary.
  """
  def put(key, value) do
    nillify :erlang.put(key, value)
  end

  @doc """
  Deletes all items in the dictionary.
  """
  def delete() do
    :erlang.erase()
  end

  @doc """
  Deletes the given key from the dictionary.
  """
  def delete(key) do
    nillify :erlang.erase(key)
  end

  @doc """
  Sends an exit signal with the given reason to the pid.

  The following behavior apply if reason is any term except `:normal` or `:kill`:

  1) If pid is not trapping exits, pid itself will exist with the given reason;

  2) If pid is trapping exits, the exit signal is transformed into a message
     {'EXIT', from, reason} and delivered to the message queue of pid;

  3) If reason is the atom `:normal`, pid will not exit. If it is trapping exits,
     the exit signal is transformed into a message {'EXIT', from, :normal} and
     delivered to its message queue;

  4) If reason is the atom `:kill`, that is if `exit(pid, :kill)` is called, an
     untrappable exit signal is sent to pid which will unconditionally exit with
     exit reason `:killed`.

  ## Examples

      Process.exit(pid, :kill)

  """
  def exit(pid, status) do
    :erlang.exit(pid, status)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`.
  It behaves exactly the same as `Kernel.spawn/1`.
  """
  def spawn(fun) do
    :erlang.spawn(fun)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`.

  It also accepts extra options, for the list of available options
  check http://www.erlang.org/doc/man/erlang.html#spawn_opt-2
  """
  def spawn(fun, opts) do
    :erlang.spawn_opt(fun, opts)
  end

  @doc """
  Returns the pid of a new process started by the application of
  `module.function(args)`. The new process created will be placed in the system
  scheduler queue and be run some time later.

  It behaves exactly the same as the `Kernel.spawn/3` function.
  """
  def spawn(mod, fun, args) do
    :erlang.spawn(mod, fun, args)
  end

  @doc """
  Returns the pid of a new process started by the application of
  `module.function(args)`. The new process created will be placed in the system
  scheduler queue and be run some time later.

  It also accepts extra options, for the list of available options
  check http://www.erlang.org/doc/man/erlang.html#spawn_opt-4

  """
  def spawn(mod, fun, args, opts) do
    :erlang.spawn_opt(mod, fun, args, opts)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`.
  A link is created between the calling process and the new
  process, atomically.
  """
  def spawn_link(fun) do
    :erlang.spawn_link(fun)
  end

  @doc """
  Returns the pid of a new process started by the application of
  `module.function(args)`. A link is created between the calling process
  and the new process, atomically. Otherwise works like spawn/3.
  """
  def spawn_link(mod, fun, args) do
    :erlang.spawn_link(mod, fun, args)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`
  and reference for a monitor created to the new process.
  """
  def spawn_monitor(fun) do
    :erlang.spawn_monitor(fun)
  end

  @doc """
  A new process is started by the application of `module.function(args)`
  and the process is monitored at the same time. Returns the pid and a
  reference for the monitor. Otherwise works like spawn/3.
  """
  def spawn_monitor(mod, fun, args) do
    :erlang.spawn_monitor(mod, fun, args)
  end

  @doc """
  The calling process starts monitoring the item given.
  It returns the monitor reference.

  See http://www.erlang.org/doc/man/erlang.html#monitor-2 for more info.
  """
  def monitor(item) do
    :erlang.monitor(:process, item)
  end

  @doc """
  If monitor_ref is a reference which the calling process
  obtained by calling monitor/1, this monitoring is turned off.
  If the monitoring is already turned off, nothing happens.

  See http://www.erlang.org/doc/man/erlang.html#demonitor-2 for more info.
  """
  def demonitor(monitor_ref, options // []) do
    :erlang.demonitor(monitor_ref, options)
  end

  @doc """
  Returns a list of process identifiers corresponding to all the
  processes currently existing on the local node.

  Note that a process that is exiting, exists but is not alive, i.e.,
  alive?/1 will return false for a process that is exiting,
  but its process identifier will be part of the result returned.

  See http://www.erlang.org/doc/man/erlang.html#processes-0 for more info.
  """
  def list do
    :erlang.processes()
  end

  @doc """
  Creates a link between the calling process and another process
  (or port) `pid`, if there is not such a link already.

  See http://www.erlang.org/doc/man/erlang.html#link-1 for more info.
  """
  def link(pid) do
    :erlang.link(pid)
  end

  @doc """
  Removes the link, if there is one, between the calling process and
  the process or port referred to by `pid`. Returns true and does not
  fail, even if there is no link or `id` does not exist

  See http://www.erlang.org/doc/man/erlang.html#unlink-1 for more info.
  """
  def unlink(pid) do
    :erlang.unlink(pid)
  end

  @doc """
  Associates the name with a pid or a port identifier. name, which must
  be an atom, can be used instead of the pid / port identifier in the
  send operator (name <- message).

  See http://www.erlang.org/doc/man/erlang.html#register-2 for more info.
  """
  def register(pid, name) do
    :erlang.register(name, pid)
  end

  @doc """
  Removes the registered name, associated with a pid or a port identifier.

  See http://www.erlang.org/doc/man/erlang.html#unregister-1 for more info.
  """
  def unregister(name) do
    :erlang.unregister(name)
  end

  @doc """
  Returns the pid or port identifier with the registered name.
  Returns undefined if the name is not registered.

  See http://www.erlang.org/doc/man/erlang.html#whereis-1 for more info.
  """
  def whereis(name) do
    nillify :erlang.whereis(name)
  end

  @doc """
  Returns the pid of the group leader for the process which evaluates the function.
  """
  def group_leader do
    :erlang.group_leader
  end

  @doc """
  Sets the group leader of Pid to GroupLeader. Typically, this is used when a processes
  started from a certain shell should have another group leader than `:init`.
  """
  def group_leader(leader, pid) do
    :erlang.group_leader(leader, pid)
  end

  @doc """
  Returns a list of names which have been registered using register/2.
  """
  def registered do
    :erlang.registered()
  end

  @doc """
  Sets certain flags for the process which calls this function.
  Returns the old value of the flag.

  See http://www.erlang.org/doc/man/erlang.html#process_flag-2 for more info.
  """
  def flag(flag, value) do
    :erlang.process_flag(flag, value)
  end

  @doc """
  Sets certain flags for the process Pid, in the same manner as flag/2.
  Returns the old value of the flag. The allowed values for Flag are
  only a subset of those allowed in flag/2, namely: save_calls.

  See http://www.erlang.org/doc/man/erlang.html#process_flag-3 for more info.
  """
  def flag(pid, flag, value) do
    :erlang.process_flag(pid, flag, value)
  end

  @doc """
  Returns information about the process identified by pid.
  Use this only for debugging information.

  See http://www.erlang.org/doc/man/erlang.html#process_info-1 for more info.
  """
  def info(pid) do
    :erlang.process_info(pid)
  end

  @doc """
  Returns information about the process identified by pid
  or undefined if the process is not alive.

  See http://www.erlang.org/doc/man/erlang.html#process_info-2 for more info.
  """
  def info(pid, spec) do
    :erlang.process_info(pid, spec)
  end

  defp nillify(:undefined), do: nil
  defp nillify(other),      do: other
end
