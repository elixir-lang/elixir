# This module provides convenience methods
# around processes and the process dictionary.
# In Erlang, most of these functions are auto-imported,
# but in Elixir they were moved to a module for convenience.
# Notice that Elixir helpers always return nil instead
# of undefined, you can use their Erlang version if you want
# the undefined value.
defmodule Process do
  # `pid` must refer to a process at the local node.
  # Returns true if the process exists and is alive, that is,
  # is not exiting and has not exited. Otherwise, returns false.
  def alive?(pid) do
    :erlang.is_process_alive(pid)
  end

  # Returns the current process.
  def self do
    :erlang.self
  end

  # Returns all key-values in the dictionary.
  def get do
    :erlang.get()
  end

  # Returns the value for the given key.
  def get(key, default // nil) do
    case :erlang.get(key) do
    match: :undefined
      default
    match: value
      value
    end
  end

  # Returns all keys that have the given `value`.
  def get_keys(value) do
    :erlang.get_keys(value)
  end

  # Stores the given key-value in the process dictionary.
  def put(key, value) do
    nillify :erlang.put(key, value)
  end

  # Erase all items in the dictionary.
  def erase() do
    :erlang.erase()
  end

  # Erase the given key from the dictionary.
  def erase(key) do
    nillify :erlang.erase(key)
  end

  # Returns a list of process identifiers corresponding to all the
  # processes currently existing on the local node.
  #
  # Note that a process that is exiting, exists but is not alive, i.e.,
  # alive?/1 will return false for a process that is exiting,
  # but its process identifier will be part of the result returned.
  #
  # See http://www.erlang.org/doc/man/erlang.html#processes-0 for more info.
  def list do
    :erlang.processes
  end

  # Creates a link between the calling process and another process
  # (or port) `pid`, if there is not such a link already.
  #
  # See http://www.erlang.org/doc/man/erlang.html#link-1 for more info.
  def link(pid) do
    :erlang.link(pid)
  end

  # Removes the link, if there is one, between the calling process and
  # the process or port referred to by `pid`. Returns true and does not
  # fail, even if there is no link or `id` does not exist
  #
  # See http://www.erlang.org/doc/man/erlang.html#unlink-1 for more info.
  def unlink(pid) do
    :erlang.unlink(pid)
  end

  # Associates the name with a pid or a port identifier. name, which must
  # be an atom, can be used instead of the pid / port identifier in the
  # send operator (name <- message).
  #
  # See http://www.erlang.org/doc/man/erlang.html#register-2 for more info.
  def register(name, pid) do
    :erlang.register(name, pid)
  end

  # Removes the registered name, associated with a pid or a port identifier.
  #
  # See http://www.erlang.org/doc/man/erlang.html#unregister-1 for more info.
  def unregister(name) do
    :erlang.unregister(name)
  end

  # Returns the pid or port identifier with the registered name.
  # Returns undefined if the name is not registered.
  #
  # See http://www.erlang.org/doc/man/erlang.html#whereis-1 for more info.
  def whereis(name) do
    :erlang.whereis(name)
  end

  # Returns a list of names which have been registered using register/2.
  def registered() do
    :erlang.registered
  end

  # Sets certain flags for the process which calls this function.
  # Returns the old value of the flag.
  #
  # See http://www.erlang.org/doc/man/erlang.html#process_flag-2 for more info.
  def flag(flag, value) do
    :erlang.process_flag(flag, value)
  end

  # Sets certain flags for the process Pid, in the same manner as flag/2.
  # Returns the old value of the flag. The allowed values for Flag are
  # only a subset of those allowed in flag/2, namely: save_calls.
  #
  # See http://www.erlang.org/doc/man/erlang.html#process_flag-3 for more info.
  def flag(pid, flag, value) do
    :erlang.process_flag(pid, flag, value)
  end

  # Returns information about the process identified by pid
  # or undefined if the process is not alive.
  #
  # See http://www.erlang.org/doc/man/erlang.html#process_info-2 for more info.
  def info(pid, spec) do
    :erlang.process_info(pid, spec)
  end

  defp nillify(:undefined), do: nil
  defp nillify(else),       do: else
end