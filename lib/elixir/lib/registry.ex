defmodule Registry do
  @moduledoc ~S"""
  A local, decentralized and scalable key-value process storage.

  It allows developers to lookup one or more processes with a given key.
  If the registry has `:unique` keys, a key points to 0 or 1 processes.
  If the registry allows `:duplicate` keys, a single key may point to any
  number of processes. In both cases, different keys could identify the
  same process.

  Each entry in the registry is associated to the process that has
  registered the key. If the process crashes, the keys associated to that
  process are automatically removed. All key comparisons in the registry
  are done using the match operation (`===`).

  The registry can be used for different purposes, such as name lookups (using
  the `:via` option), storing properties, custom dispatching rules, or a pubsub
  implementation. We explore some of those use cases below.

  The registry may also be transparently partitioned, which provides
  more scalable behaviour for running registries on highly concurrent
  environments with thousands or millions of entries.

  ## Using in `:via`

  Once the registry is started with a given name (using
  `Registry.start_link/2`), it can be used to register and access named
  processes using the `{:via, Registry, {registry, key}}` tuple:

      {:ok, _} = Registry.start_link(:unique, Registry.ViaTest)
      name = {:via, Registry, {Registry.ViaTest, "agent"}}
      {:ok, _} = Agent.start_link(fn -> 0 end, name: name)
      Agent.get(name, & &1)
      #=> 0
      Agent.update(name, & &1 + 1)
      Agent.get(name, & &1)
      #=> 1

  Typically the registry is started as part of a supervision tree though:

      supervisor(Registry, [:unique, Registry.ViaTest])

  Only registries with unique keys can be used in `:via`. If the name is
  already taken, the case-specific `start_link` function (`Agent.start_link/2`
  in the example above) will return `{:error, {:already_started, current_pid}}`.

  ## Using as a dispatcher

  `Registry` has a dispatch mechanism that allows developers to implement custom
  dispatch logic triggered from the caller. For example, let's say we have a
  duplicate registry started as so:

      {:ok, _} = Registry.start_link(:duplicate, Registry.DispatcherTest)

  By calling `register/3`, different processes can register under a given key
  and associate any value under that key. In this case, let's register the
  current process under the key `"hello"` and attach the `{IO, :inspect}` tuple
  to it:

      {:ok, _} = Registry.register(Registry.DispatcherTest, "hello", {IO, :inspect})

  Now, an entity interested in dispatching events for a given key may call
  `dispatch/3` passing in the key and a callback. This callback will be invoked
  with a list of all the values registered under the requested key, alongside
  the pid of the process that registered each value, in the form of `{pid,
  value}` tuples. In our example, `value` will be the `{module, function}` tuple
  in the code above:

      Registry.dispatch(Registry.DispatcherTest, "hello", fn entries ->
        for {pid, {module, function}} <- entries, do: apply(module, function, [pid])
      end)
      # Prints #PID<...> where the pid is for the process that called register/3 above
      #=> :ok

  Dispatching happens in the process that calls `dispatch/3` either serially or
  concurrently in case of multiple partitions (via spawned tasks). The
  registered processes are not involved in dispatching unless involving them is
  done explicitly (for example, by sending them a message in the callback).

  Furthermore, if there is a failure when dispatching, due to a bad
  registration, dispatching will always fail and the registered process will not
  be notified. Therefore let's make sure we at least wrap and report those
  errors:

      require Logger
      Registry.dispatch(Registry.DispatcherTest, "hello", fn entries ->
        for {pid, {module, function}} <- entries do
          try do
            apply(module, function, [pid])
          catch
            kind, reason ->
              formatted = Exception.format(kind, reason, System.stacktrace)
              Logger.error "Registry.dispatch/3 failed with #{formatted}"
          end
        end
      end)
      # Prints #PID<...>
      #=> :ok

  You could also replace the whole `apply` system by explicitly sending
  messages. That's the example we will see next.

  ## Using as a PubSub

  Registries can also be used to implement a local, non-distributed, scalable
  PubSub by relying on the `dispatch/3` function, similarly to the previous
  section: in this case, however, we will send messages to each associated
  process, instead of invoking a given module-function.

  In this example, we will also set the number of partitions to the number of
  schedulers online, which will make the registry more performant on highly
  concurrent environments as each partition will spawn a new process, allowing
  dispatching to happen in parallel:

      {:ok, _} = Registry.start_link(:duplicate, Registry.PubSubTest,
                                     partitions: System.schedulers_online)
      {:ok, _} = Registry.register(Registry.PubSubTest, "hello", [])
      Registry.dispatch(Registry.PubSubTest, "hello", fn entries ->
        for {pid, _} <- entries, do: send(pid, {:broadcast, "world"})
      end)
      #=> :ok

  The example above broadcasted the message `{:broadcast, "world"}` to all
  processes registered under the "topic" (or "key" as we called it until now)
  `"hello"`.

  The third argument given to `register/3` is a value associated to the
  current process. While in the previous section we used it when dispatching,
  in this particular example we are not interested in it, so we have set it
  to an empty list. You could store a more meaningful value if necessary.

  ## Registrations

  Looking up, dispatching and registering are efficient and immediate at
  the cost of delayed unsubscription. For example, if a process crashes,
  its keys are automatically removed from the registry but the change may
  not propagate immediately. This means certain operations may return processes
  that are already dead. When such may happen, it will be explicitly stated
  in the function documentation.

  However, keep in mind those cases are typically not an issue. After all, a
  process referenced by a pid may crash at any time, including between getting
  the value from the registry and sending it a message. Many parts of the standard
  library are designed to cope with that, such as `Process.monitor/1` which will
  deliver the `:DOWN` message immediately if the monitored process is already dead
  and `Kernel.send/2` which acts as a no-op for dead processes.

  ## ETS

  Note that the registry uses one ETS table plus two ETS tables per partition.
  """

  # TODO: Decide if it should be started as part of Elixir's supervision tree.

  @kind [:unique, :duplicate]
  @all_info -1
  @key_info -2

  @typedoc "The registry identifier"
  @type registry :: atom

  @typedoc "The type of the registry"
  @type kind :: :unique | :duplicate

  @typedoc "The type of keys allowed on registration"
  @type key :: term

  @typedoc "The type of values allowed on registration"
  @type value :: term

  @typedoc "The type of registry metadata keys"
  @type meta_key :: atom | tuple

  @typedoc "The type of registry metadata values"
  @type meta_value :: term

  ## Via callbacks

  @doc false
  def whereis_name({registry, key}) do
    case key_info!(registry) do
      {:unique, partitions, key_ets} ->
        key_ets = key_ets || key_ets!(registry, key, partitions)
        case safe_lookup_second(key_ets, key) do
          {pid, _} ->
            if Process.alive?(pid), do: pid, else: :undefined
          _ ->
            :undefined
        end
      {kind, _, _} ->
        raise ArgumentError, ":via is not supported for #{kind} registries"
    end
  end

  @doc false
  def register_name({registry, key}, pid) when pid == self() do
    case register(registry, key, nil) do
      {:ok, _} -> :yes
      {:error, _} -> :no
    end
  end

  @doc false
  def send({registry, key}, msg) do
    case lookup(registry, key) do
      [{pid, _}] -> Kernel.send(pid, msg)
      [] -> :erlang.error(:badarg, [{registry, key}, msg])
    end
  end

  @doc false
  def unregister_name({registry, key}) do
    unregister(registry, key)
  end

  ## Registry API

  @doc """
  Starts the registry as a supervisor process.

  Manually it can be started as:

      Registry.start_link(:unique, MyApp.Registry)

  In your supervisor tree, you would write:

      supervisor(Registry, [:unique, MyApp.Registry])

  For intensive workloads, the registry may also be partitioned (by specifying
  the `:partitions` option). If partitioning is required then a good default is to
  set the number of partitions to the number of schedulers available:

      Registry.start_link(:unique, MyApp.Registry, partitions: System.schedulers_online())

  or:

      supervisor(Registry, [:unique, MyApp.Registry, [partitions: System.schedulers_online()]])

  ## Options

  The registry supports the following options:

    * `:partitions` - the number of partitions in the registry. Defaults to `1`.
    * `:listeners` - a list of named processes which are notified of `:register`
      and `:unregister` events. The registered process must be monitored by the
      listener if the listener wants to be notified if the registered process
      crashes.
    * `:meta` - a keyword list of metadata to be attached to the registry.

  """
  @spec start_link(kind, registry, options) :: {:ok, pid} | {:error, term}
        when options: [partitions: pos_integer, listeners: [atom], meta: [{meta_key, meta_value}]]
  def start_link(kind, registry, options \\ []) when kind in @kind and is_atom(registry) do
    meta = Keyword.get(options, :meta, [])
    unless Keyword.keyword?(meta) do
      raise ArgumentError, "expected :meta to be a keyword list, got: #{inspect meta}"
    end

    partitions = Keyword.get(options, :partitions, 1)
    unless is_integer(partitions) and partitions >= 1 do
      raise ArgumentError, "expected :partitions to be a positive integer, got: #{inspect partitions}"
    end

    listeners = Keyword.get(options, :listeners, [])
    unless is_list(listeners) and Enum.all?(listeners, &is_atom/1) do
      raise ArgumentError, "expected :listeners to be a list of named processes, got: #{inspect listeners}"
    end

    # The @info format must be kept in sync with Registry.Partition optimization.
    entries = [{@all_info, {kind, partitions, nil, nil, listeners}},
               {@key_info, {kind, partitions, nil}} | meta]
    Registry.Supervisor.start_link(kind, registry, partitions, listeners, entries)
  end

  @doc """
  Updates the value for `key` for the current process in the unique `registry`.

  Returns a `{new_value, old_value}` tuple or `:error` if there
  is no such key assigned to the current process.

  If a non-unique registry is given, an error is raised.

  ## Examples

      iex> Registry.start_link(:unique, Registry.UpdateTest)
      iex> {:ok, _} = Registry.register(Registry.UpdateTest, "hello", 1)
      iex> Registry.lookup(Registry.UpdateTest, "hello")
      [{self(), 1}]
      iex> Registry.update_value(Registry.UpdateTest, "hello", & &1 + 1)
      {2, 1}
      iex> Registry.lookup(Registry.UpdateTest, "hello")
      [{self(), 2}]

  """
  @spec update_value(registry, key, (value -> value)) :: {new_value :: term, old_value :: term} | :error
  def update_value(registry, key, callback) when is_atom(registry) and is_function(callback, 1) do
    case key_info!(registry) do
      {:unique, partitions, key_ets} ->
        key_ets = key_ets || key_ets!(registry, key, partitions)
        try do
          :ets.lookup_element(key_ets, key, 2)
        catch
          :error, :badarg -> :error
        else
          {pid, old_value} when pid == self() ->
            new_value = callback.(old_value)
            :ets.insert(key_ets, {key, {pid, new_value}})
            {new_value, old_value}
          {_, _} ->
            :error
        end
      {kind, _, _} ->
        raise ArgumentError, "Registry.update_value/3 is not supported for #{kind} registries"
    end
  end

  @doc """
  Invokes the callback with all entries under `key` in each partition
  for the given `registry`.

  The list of `entries` is a non-empty list of two-element tuples where
  the first element is the pid and the second element is the value
  associated to the pid. If there are no entries for the given key,
  the callback is never invoked.

  If the registry is not partitioned, the callback is invoked in the process
  that calls `dispatch/3`. If the registry is partitioned, the callback is
  invoked concurrently per partition by starting a task linked to the
  caller. The callback, however, is only invoked if there are entries for that
  partition.

  See the module documentation for examples of using the `dispatch/3`
  function for building custom dispatching or a pubsub system.
  """
  @spec dispatch(registry, key, (entries :: [{pid, value}] -> term)) :: :ok
  def dispatch(registry, key, mfa_or_fun)
      when is_atom(registry) and is_function(mfa_or_fun, 1)
      when is_atom(registry) and tuple_size(mfa_or_fun) == 3 do
    case key_info!(registry) do
      {:unique, partitions, key_ets} ->
        (key_ets || key_ets!(registry, key, partitions))
        |> safe_lookup_second(key)
        |> List.wrap()
        |> apply_non_empty_to_mfa_or_fun(mfa_or_fun)
      {:duplicate, 1, key_ets} ->
        key_ets
        |> safe_lookup_second(key)
        |> apply_non_empty_to_mfa_or_fun(mfa_or_fun)
      {:duplicate, partitions, _} ->
        registry
        |> dispatch_task(key, mfa_or_fun, partitions)
        |> Enum.each(&Task.await(&1, :infinity))
    end
    :ok
  end

  defp dispatch_task(_registry, _key, _mfa_or_fun, 0) do
    []
  end
  defp dispatch_task(registry, key, mfa_or_fun, partition) do
    partition = partition - 1
    task = Task.async(fn ->
      registry
      |> key_ets!(partition)
      |> safe_lookup_second(key)
      |> apply_non_empty_to_mfa_or_fun(mfa_or_fun)
      :ok
    end)
    [task | dispatch_task(registry, key, mfa_or_fun, partition)]
  end

  defp apply_non_empty_to_mfa_or_fun([], _mfa_or_fun) do
    :ok
  end
  defp apply_non_empty_to_mfa_or_fun(entries, {module, function, args}) do
    apply(module, function, [entries | args])
  end
  defp apply_non_empty_to_mfa_or_fun(entries, fun) do
    fun.(entries)
  end

  @doc """
  Finds the `{pid, value}` pair for the given `key` in `registry` in no particular order.

  An empty list if there is no match.

  For unique registries, a single partition lookup is necessary. For
  duplicate registries, all partitions must be looked up.

  ## Examples

  In the example below we register the current process and look it up
  both from itself and other processes:

      iex> Registry.start_link(:unique, Registry.UniqueLookupTest)
      iex> Registry.lookup(Registry.UniqueLookupTest, "hello")
      []
      iex> {:ok, _} = Registry.register(Registry.UniqueLookupTest, "hello", :world)
      iex> Registry.lookup(Registry.UniqueLookupTest, "hello")
      [{self(), :world}]
      iex> Task.async(fn -> Registry.lookup(Registry.UniqueLookupTest, "hello") end) |> Task.await
      [{self(), :world}]

  The same applies to duplicate registries:

      iex> Registry.start_link(:duplicate, Registry.DuplicateLookupTest)
      iex> Registry.lookup(Registry.DuplicateLookupTest, "hello")
      []
      iex> {:ok, _} = Registry.register(Registry.DuplicateLookupTest, "hello", :world)
      iex> Registry.lookup(Registry.DuplicateLookupTest, "hello")
      [{self(), :world}]
      iex> {:ok, _} = Registry.register(Registry.DuplicateLookupTest, "hello", :another)
      iex> Enum.sort(Registry.lookup(Registry.DuplicateLookupTest, "hello"))
      [{self(), :another}, {self(), :world}]

  """
  @spec lookup(registry, key) :: [{pid, value}]
  def lookup(registry, key) when is_atom(registry) do
    case key_info!(registry) do
      {:unique, partitions, key_ets} ->
        key_ets = key_ets || key_ets!(registry, key, partitions)
        case safe_lookup_second(key_ets, key) do
          {_, _} = pair ->
            [pair]
          _ ->
            []
        end

      {:duplicate, 1, key_ets} ->
        safe_lookup_second(key_ets, key)

      {:duplicate, partitions, _key_ets} ->
        for partition <- 0..(partitions - 1),
            pair <- safe_lookup_second(key_ets!(registry, partition), key),
            do: pair
    end
  end

  @doc """
  Returns `{pid, value}` pairs under the given `key` in `registry` that match `pattern`.

  Pattern must be an atom or a tuple that will match the structure of the
  value stored in the registry. The atom `:_` can be used to ignore a given
  value or tuple element, while :"$1" can be used to temporarily assign part
  of pattern to a variable for a subsequent comparison.

  An empty list will be returned if there is no match.

  For unique registries, a single partition lookup is necessary. For
  duplicate registries, all partitions must be looked up.

  ## Examples

  In the example below we register the current process under the same
  key in a duplicate registry but with different values:

      iex> Registry.start_link(:duplicate, Registry.MatchTest)
      iex> {:ok, _} = Registry.register(Registry.MatchTest, "hello", {1, :atom, 1})
      iex> {:ok, _} = Registry.register(Registry.MatchTest, "hello", {2, :atom, 2})
      iex> Registry.match(Registry.MatchTest, "hello", {1, :_, :_})
      [{self(), {1, :atom, 1}}]
      iex> Registry.match(Registry.MatchTest, "hello", {2, :_, :_})
      [{self(), {2, :atom, 2}}]
      iex> Registry.match(Registry.MatchTest, "hello", {:_, :atom, :_}) |> Enum.sort()
      [{self(), {1, :atom, 1}}, {self(), {2, :atom, 2}}]
      iex> Registry.match(Registry.MatchTest, "hello", {:"$1", :_, :"$1"}) |> Enum.sort()
      [{self(), {1, :atom, 1}}, {self(), {2, :atom, 2}}]

  """
  @spec match(registry, key, match_pattern :: atom() | tuple()) :: [{pid, term}]
  def match(registry, key, pattern) when is_atom(registry) do
    spec = [{{key, {:_, pattern}}, [], [{:element, 2, :"$_"}]}]

    case key_info!(registry) do
      {:unique, partitions, key_ets} ->
        key_ets = key_ets || key_ets!(registry, key, partitions)
        :ets.select(key_ets, spec)

      {:duplicate, 1, key_ets} ->
        :ets.select(key_ets, spec)

      {:duplicate, partitions, _key_ets} ->
        for partition <- 0..(partitions - 1),
            pair <- :ets.select(key_ets!(registry, partition), spec),
            do: pair
    end
  end

  @doc """
  Returns the known keys for the given `pid` in `registry` in no particular order.

  If the registry is unique, the keys are unique. Otherwise
  they may contain duplicates if the process was registered
  under the same key multiple times. The list will be empty
  if the process is dead or it has no keys in this registry.

  ## Examples

  Registering under a unique registry does not allow multiple entries:

      iex> Registry.start_link(:unique, Registry.UniqueKeysTest)
      iex> Registry.keys(Registry.UniqueKeysTest, self())
      []
      iex> {:ok, _} = Registry.register(Registry.UniqueKeysTest, "hello", :world)
      iex> Registry.register(Registry.UniqueKeysTest, "hello", :later) # registry is :unique
      {:error, {:already_registered, self()}}
      iex> Registry.keys(Registry.UniqueKeysTest, self())
      ["hello"]

  Such is possible for duplicate registries though:

      iex> Registry.start_link(:duplicate, Registry.DuplicateKeysTest)
      iex> Registry.keys(Registry.DuplicateKeysTest, self())
      []
      iex> {:ok, _} = Registry.register(Registry.DuplicateKeysTest, "hello", :world)
      iex> {:ok, _} = Registry.register(Registry.DuplicateKeysTest, "hello", :world)
      iex> Registry.keys(Registry.DuplicateKeysTest, self())
      ["hello", "hello"]

  """
  @spec keys(registry, pid) :: [key]
  def keys(registry, pid) when is_atom(registry) and is_pid(pid) do
    {kind, partitions, _, pid_ets, _} = info!(registry)
    {_, pid_ets} = pid_ets || pid_ets!(registry, pid, partitions)
    keys = safe_lookup_second(pid_ets, pid)

    cond do
      kind == :unique -> Enum.uniq(keys)
      true -> keys
    end
  end

  @doc """
  Unregisters all entries for the given `key` associated to the current
  process in `registry`.

  Always returns `:ok` and automatically unlinks the current process from
  the owner if there are no more keys associated to the current process. See
  also `register/3` to read more about the "owner".

  ## Examples

  It unregister all entries for `key` for unique registries:

      iex> Registry.start_link(:unique, Registry.UniqueUnregisterTest)
      iex> Registry.register(Registry.UniqueUnregisterTest, "hello", :world)
      iex> Registry.keys(Registry.UniqueUnregisterTest, self())
      ["hello"]
      iex> Registry.unregister(Registry.UniqueUnregisterTest, "hello")
      :ok
      iex> Registry.keys(Registry.UniqueUnregisterTest, self())
      []

  As well as duplicate registries:

      iex> Registry.start_link(:duplicate, Registry.DuplicateUnregisterTest)
      iex> Registry.register(Registry.DuplicateUnregisterTest, "hello", :world)
      iex> Registry.register(Registry.DuplicateUnregisterTest, "hello", :world)
      iex> Registry.keys(Registry.DuplicateUnregisterTest, self())
      ["hello", "hello"]
      iex> Registry.unregister(Registry.DuplicateUnregisterTest, "hello")
      :ok
      iex> Registry.keys(Registry.DuplicateUnregisterTest, self())
      []

  """
  @spec unregister(registry, key) :: :ok
  def unregister(registry, key) when is_atom(registry) do
    self = self()
    {kind, partitions, key_ets, pid_ets, listeners} = info!(registry)
    {key_partition, pid_partition} = partitions(kind, key, self, partitions)
    key_ets = key_ets || key_ets!(registry, key_partition)
    {pid_server, pid_ets} = pid_ets || pid_ets!(registry, pid_partition)

    # Remove first from the key_ets because in case of crashes
    # the pid_ets will still be able to clean up. The last step is
    # to clean if we have no more entries.
    true = :ets.match_delete(key_ets, {key, {self, :_}})
    true = :ets.delete_object(pid_ets, {self, key, key_ets})

    unlink_if_unregistered(pid_server, pid_ets, self)

    for listener <- listeners do
      Kernel.send(listener, {:unregister, registry, key, self})
    end

    :ok
  end

  @doc """
  Registers the current process under the given `key` in `registry`.

  A value to be associated with this registration must also be given.
  This value will be retrieved whenever dispatching or doing a key
  lookup.

  This function returns `{:ok, owner}` or `{:error, reason}`.
  The `owner` is the pid is the registry partition responsible for
  the pid. The owner is automatically linked to the caller.

  If the registry has unique keys, it will return `{:ok, owner}` unless
  the key is already associated to a pid, in which case it returns
  `{:error, {:already_registered, pid}}`.

  If the registry has duplicate keys, multiple registrations from the
  current process under the same key are allowed.

  ## Examples

  Registering under a unique registry does not allow multiple entries:

      iex> Registry.start_link(:unique, Registry.UniqueRegisterTest)
      iex> {:ok, _} = Registry.register(Registry.UniqueRegisterTest, "hello", :world)
      iex> Registry.register(Registry.UniqueRegisterTest, "hello", :later)
      {:error, {:already_registered, self()}}
      iex> Registry.keys(Registry.UniqueRegisterTest, self())
      ["hello"]

  Such is possible for duplicate registries though:

      iex> Registry.start_link(:duplicate, Registry.DuplicateRegisterTest)
      iex> {:ok, _} = Registry.register(Registry.DuplicateRegisterTest, "hello", :world)
      iex> {:ok, _} = Registry.register(Registry.DuplicateRegisterTest, "hello", :world)
      iex> Registry.keys(Registry.DuplicateRegisterTest, self())
      ["hello", "hello"]

  """
  @spec register(registry, key, value) :: {:ok, pid} | {:error, {:already_registered, pid}}
  def register(registry, key, value) when is_atom(registry) do
    self = self()
    {kind, partitions, key_ets, pid_ets, listeners} = info!(registry)
    {key_partition, pid_partition} = partitions(kind, key, self, partitions)
    key_ets = key_ets || key_ets!(registry, key_partition)
    {pid_server, pid_ets} = pid_ets || pid_ets!(registry, pid_partition)

    # Notice we write first to the pid ets table because it will
    # always be able to do the clean up. If we register first to the
    # key one and the process crashes, the key will stay there forever.
    Process.link(pid_server)
    true = :ets.insert(pid_ets, {self, key, key_ets})
    case register_key(kind, pid_server, key_ets, key, {key, {self, value}}) do
      {:ok, _} = ok ->
        for listener <- listeners do
          Kernel.send(listener, {:register, registry, key, self, value})
        end
        ok
      {:error, {:already_registered, ^self}} = error ->
        error
      {:error, _} = error ->
        true = :ets.delete_object(pid_ets, {self, key, key_ets})
        unlink_if_unregistered(pid_server, pid_ets, self)
        error
    end
  end

  defp register_key(:duplicate, pid_server, key_ets, _key, entry) do
    true = :ets.insert(key_ets, entry)
    {:ok, pid_server}
  end
  defp register_key(:unique, pid_server, key_ets, key, entry) do
    if :ets.insert_new(key_ets, entry) do
      {:ok, pid_server}
    else
      # Notice we have to call register_key recursively
      # because we are always at odds of a race condition.
      case :ets.lookup(key_ets, key) do
        [{^key, {pid, _}} = current] ->
          if Process.alive?(pid) do
            {:error, {:already_registered, pid}}
          else
            :ets.delete_object(key_ets, current)
            register_key(:unique, pid_server, key_ets, key, entry)
          end
        [] ->
          register_key(:unique, pid_server, key_ets, key, entry)
      end
    end
  end

  @doc """
  Reads registry metadata given on `start_link/3`.

  Atoms and tuples are allowed as keys.

  ## Examples

      iex> Registry.start_link(:unique, Registry.MetaTest, meta: [custom_key: "custom_value"])
      iex> Registry.meta(Registry.MetaTest, :custom_key)
      {:ok, "custom_value"}
      iex> Registry.meta(Registry.MetaTest, :unknown_key)
      :error

  """
  @spec meta(registry, meta_key) :: {:ok, meta_value} | :error
  def meta(registry, key) when is_atom(registry) and (is_atom(key) or is_tuple(key)) do
    try do
      :ets.lookup(registry, key)
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect registry}"
    else
      [{^key, value}] -> {:ok, value}
      _ -> :error
    end
  end

  @doc """
  Stores registry metadata.

  Atoms and tuples are allowed as keys.

  ## Examples

      iex> Registry.start_link(:unique, Registry.PutMetaTest)
      iex> Registry.put_meta(Registry.PutMetaTest, :custom_key, "custom_value")
      :ok
      iex> Registry.meta(Registry.PutMetaTest, :custom_key)
      {:ok, "custom_value"}
      iex> Registry.put_meta(Registry.PutMetaTest, {:tuple, :key}, "tuple_value")
      :ok
      iex> Registry.meta(Registry.PutMetaTest, {:tuple, :key})
      {:ok, "tuple_value"}

  """
  @spec put_meta(registry, meta_key, meta_value) :: :ok
  def put_meta(registry, key, value) when is_atom(registry) and (is_atom(key) or is_tuple(key)) do
    try do
      :ets.insert(registry, {key, value})
      :ok
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect registry}"
    end
  end

  ## Helpers

  @compile {:inline, hash: 2}

  defp hash(term, limit) do
    :erlang.phash2(term, limit)
  end

  defp info!(registry) do
    try do
      :ets.lookup_element(registry, @all_info, 2)
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect registry}"
    end
  end

  defp key_info!(registry) do
    try do
      :ets.lookup_element(registry, @key_info, 2)
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect registry}"
    end
  end

  defp key_ets!(registry, key, partitions) do
    :ets.lookup_element(registry, hash(key, partitions), 2)
  end

  defp key_ets!(registry, partition) do
    :ets.lookup_element(registry, partition, 2)
  end

  defp pid_ets!(registry, key, partitions) do
    :ets.lookup_element(registry, hash(key, partitions), 3)
  end

  defp pid_ets!(registry, partition) do
    :ets.lookup_element(registry, partition, 3)
  end

  defp safe_lookup_second(ets, key) do
    try do
      :ets.lookup_element(ets, key, 2)
    catch
      :error, :badarg -> []
    end
  end

  defp partitions(:unique, key, pid, partitions) do
    {hash(key, partitions), hash(pid, partitions)}
  end
  defp partitions(:duplicate, _key, pid, partitions) do
    partition = hash(pid, partitions)
    {partition, partition}
  end

  defp unlink_if_unregistered(pid_server, pid_ets, self) do
    unless :ets.member(pid_ets, self) do
      Process.unlink(pid_server)
    end
  end
end

defmodule Registry.Supervisor do
  @moduledoc false
  use Supervisor

  def start_link(kind, registry, partitions, listeners, entries) do
    Supervisor.start_link(__MODULE__, {kind, registry, partitions, listeners, entries}, name: registry)
  end

  def init({kind, registry, partitions, listeners, entries}) do
    ^registry = :ets.new(registry, [:set, :public, :named_table, read_concurrency: true])
    true = :ets.insert(registry, entries)

    children =
      for i <- 0..partitions-1 do
        key_partition = Registry.Partition.key_name(registry, i)
        pid_partition = Registry.Partition.pid_name(registry, i)
        arg = {kind, registry, i, partitions, key_partition, pid_partition, listeners}
        worker(Registry.Partition, [pid_partition, arg], id: pid_partition)
      end

    supervise(children, strategy: strategy_for_kind(kind))
  end

  # Unique registries have their key partition hashed by key.
  # This means that, if a pid partition crashes, it may have
  # entries from all key partitions, so we need to crash all.
  defp strategy_for_kind(:unique), do: :one_for_all

  # Duplicate registries have both key and pid partitions hashed
  # by pid. This means that, if a pid partition crashes, all of
  # its associated entries are in its sibling table, so we crash one.
  defp strategy_for_kind(:duplicate), do: :one_for_one
end

defmodule Registry.Partition do
  @moduledoc false

  # This process owns the equivalent key and pid ets tables
  # and is responsible for monitoring processes that map to
  # the its own pid table.
  use GenServer
  @all_info -1
  @key_info -2

  @doc """
  Returns the name of key partition table.
  """
  @spec key_name(atom, non_neg_integer) :: atom
  def key_name(registry, partition) do
    Module.concat(registry, "KeyPartition" <> Integer.to_string(partition))
  end

  @doc """
  Returns the name of pid partition table.
  """
  @spec pid_name(atom, non_neg_integer) :: atom
  def pid_name(name, partition) do
    Module.concat(name, "PIDPartition" <> Integer.to_string(partition))
  end

  @doc """
  Starts the registry partition.

  The process is only responsible for monitoring, demonitoring
  and cleaning up when monitored processes crash.
  """
  def start_link(registry, arg) do
    GenServer.start_link(__MODULE__, arg, name: registry)
  end

  ## Callbacks

  def init({kind, registry, i, partitions, key_partition, pid_partition, listeners}) do
    Process.flag(:trap_exit, true)
    key_ets = init_key_ets(kind, key_partition)
    pid_ets = init_pid_ets(kind, pid_partition)

    # If we have only one partition, we do an optimization which
    # is to write the table information alongside the registry info.
    if partitions == 1 do
      entries =
        [{@key_info, {kind, partitions, key_ets}},
         {@all_info, {kind, partitions, key_ets, {self(), pid_ets}, listeners}}]
      true = :ets.insert(registry, entries)
    else
      true = :ets.insert(registry, {i, key_ets, {self(), pid_ets}})
    end

    {:ok, pid_ets}
  end

  # The key partition is a set for unique keys,
  # duplicate bag for duplicate ones.
  defp init_key_ets(:unique, key_partition) do
    :ets.new(key_partition, [:set, :public, read_concurrency: true, write_concurrency: true])
  end
  defp init_key_ets(:duplicate, key_partition) do
    :ets.new(key_partition, [:duplicate_bag, :public, read_concurrency: true, write_concurrency: true])
  end

  # A process can always have multiple keys, so the
  # pid partition is always a duplicate bag.
  defp init_pid_ets(_, pid_partition) do
    :ets.new(pid_partition, [:duplicate_bag, :public, read_concurrency: true, write_concurrency: true])
  end

  def handle_call(:sync, _, state) do
    {:reply, :ok, state}
  end

  def handle_info({:EXIT, pid, _reason}, ets) do
    entries = :ets.take(ets, pid)
    for {_pid, key, key_ets} <- entries do
      try do
        :ets.match_delete(key_ets, {key, {pid, :_}})
      catch
        :error, :badarg -> :badarg
      end
    end
    {:noreply, ets}
  end
  def handle_info(msg, state) do
    super(msg, state)
  end
end
