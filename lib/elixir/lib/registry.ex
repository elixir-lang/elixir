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
  are done using the match operation (`===/2`).

  The registry can be used for different purposes, such as name lookups (using
  the `:via` option), storing properties, custom dispatching rules, or a pubsub
  implementation. We explore some of those use cases below.

  The registry may also be transparently partitioned, which provides
  more scalable behaviour for running registries on highly concurrent
  environments with thousands or millions of entries.

  ## Using in `:via`

  Once the registry is started with a given name using
  `Registry.start_link/1`, it can be used to register and access named
  processes using the `{:via, Registry, {registry, key}}` tuple:

      {:ok, _} = Registry.start_link(keys: :unique, name: Registry.ViaTest)
      name = {:via, Registry, {Registry.ViaTest, "agent"}}
      {:ok, _} = Agent.start_link(fn -> 0 end, name: name)
      Agent.get(name, & &1)
      #=> 0
      Agent.update(name, &(&1 + 1))
      Agent.get(name, & &1)
      #=> 1

  In the previous example, we were not interested in associating a value to the
  process:

      Registry.lookup(Registry.ViaTest, "agent")
      #=> [{self(), nil}]

  However, in some cases it may be desired to associate a value to the process
  using the alternate `{:via, Registry, {registry, key, value}}` tuple:

      {:ok, _} = Registry.start_link(keys: :unique, name: Registry.ViaTest)
      name = {:via, Registry, {Registry.ViaTest, "agent", :hello}}
      {:ok, _} = Agent.start_link(fn -> 0 end, name: name)
      Registry.lookup(Registry.ViaTest, "agent")
      #=> [{self(), :hello}]

  To this point, we have been starting `Registry` using `start_link/1`.
  Typically the registry is started as part of a supervision tree though:

      {Registry, keys: :unique, name: Registry.ViaTest}

  Only registries with unique keys can be used in `:via`. If the name is
  already taken, the case-specific `start_link` function (`Agent.start_link/2`
  in the example above) will return `{:error, {:already_started, current_pid}}`.

  ## Using as a dispatcher

  `Registry` has a dispatch mechanism that allows developers to implement custom
  dispatch logic triggered from the caller. For example, let's say we have a
  duplicate registry started as so:

      {:ok, _} = Registry.start_link(keys: :duplicate, name: Registry.DispatcherTest)

  By calling `register/3`, different processes can register under a given key
  and associate any value under that key. In this case, let's register the
  current process under the key `"hello"` and attach the `{IO, :inspect}` tuple
  to it:

      {:ok, _} = Registry.register(Registry.DispatcherTest, "hello", {IO, :inspect})

  Now, an entity interested in dispatching events for a given key may call
  `dispatch/3` passing in the key and a callback. This callback will be invoked
  with a list of all the values registered under the requested key, alongside
  the PID of the process that registered each value, in the form of `{pid,
  value}` tuples. In our example, `value` will be the `{module, function}` tuple
  in the code above:

      Registry.dispatch(Registry.DispatcherTest, "hello", fn entries ->
        for {pid, {module, function}} <- entries, do: apply(module, function, [pid])
      end)
      # Prints #PID<...> where the PID is for the process that called register/3 above
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
              formatted = Exception.format(kind, reason, __STACKTRACE__)
              Logger.error("Registry.dispatch/3 failed with #{formatted}")
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
  concurrent environments:

      {:ok, _} =
        Registry.start_link(
          keys: :duplicate,
          name: Registry.PubSubTest,
          partitions: System.schedulers_online()
        )

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
  process referenced by a PID may crash at any time, including between getting
  the value from the registry and sending it a message. Many parts of the standard
  library are designed to cope with that, such as `Process.monitor/1` which will
  deliver the `:DOWN` message immediately if the monitored process is already dead
  and `Kernel.send/2` which acts as a no-op for dead processes.

  ## ETS

  Note that the registry uses one ETS table plus two ETS tables per partition.
  """

  @keys [:unique, :duplicate]
  @all_info -1
  @key_info -2

  @typedoc "The registry identifier"
  @type registry :: atom

  @typedoc "The type of the registry"
  @type keys :: :unique | :duplicate

  @typedoc "The type of keys allowed on registration"
  @type key :: term

  @typedoc "The type of values allowed on registration"
  @type value :: term

  @typedoc "The type of registry metadata keys"
  @type meta_key :: atom | tuple

  @typedoc "The type of registry metadata values"
  @type meta_value :: term

  @typedoc "A pattern to match on objects in a registry"
  @type match_pattern :: atom | term

  @typedoc "A guard to be evaluated when matching on objects in a registry"
  @type guard :: atom | tuple

  @typedoc "A list of guards to be evaluated when matching on objects in a registry"
  @type guards :: [guard]

  @typedoc "A pattern used to representing the output format part of a match spec"
  @type body :: [atom | tuple]

  @typedoc "A full match spec used when selecting objects in the registry"
  @type spec :: [{match_pattern, guards, body}]

  @typedoc "Options used for `child_spec/1` and `start_link/1`"
  @type start_option ::
          {:keys, keys}
          | {:name, registry}
          | {:partitions, pos_integer}
          | {:listeners, [atom]}
          | {:meta, [{meta_key, meta_value}]}

  ## Via callbacks

  @doc false
  def whereis_name({registry, key}), do: whereis_name(registry, key)
  def whereis_name({registry, key, _value}), do: whereis_name(registry, key)

  defp whereis_name(registry, key) do
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
  def register_name({registry, key}, pid), do: register_name(registry, key, nil, pid)
  def register_name({registry, key, value}, pid), do: register_name(registry, key, value, pid)

  defp register_name(registry, key, value, pid) when pid == self() do
    case register(registry, key, value) do
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

      Registry.start_link(keys: :unique, name: MyApp.Registry)

  In your supervisor tree, you would write:

      Supervisor.start_link([
        {Registry, keys: :unique, name: MyApp.Registry}
      ], strategy: :one_for_one)

  For intensive workloads, the registry may also be partitioned (by specifying
  the `:partitions` option). If partitioning is required then a good default is to
  set the number of partitions to the number of schedulers available:

      Registry.start_link(
        keys: :unique,
        name: MyApp.Registry,
        partitions: System.schedulers_online()
      )

  or:

      Supervisor.start_link([
        {Registry, keys: :unique, name: MyApp.Registry, partitions: System.schedulers_online()}
      ], strategy: :one_for_one)

  ## Options

  The registry requires the following keys:

    * `:keys` - choose if keys are `:unique` or `:duplicate`
    * `:name` - the name of the registry and its tables

  The following keys are optional:

    * `:partitions` - the number of partitions in the registry. Defaults to `1`.
    * `:listeners` - a list of named processes which are notified of `:register`
      and `:unregister` events. The registered process must be monitored by the
      listener if the listener wants to be notified if the registered process
      crashes.
    * `:meta` - a keyword list of metadata to be attached to the registry.

  """
  @doc since: "1.5.0"
  @spec start_link([start_option]) :: {:ok, pid} | {:error, term}
  def start_link(options) do
    keys = Keyword.get(options, :keys)

    unless keys in @keys do
      raise ArgumentError,
            "expected :keys to be given and be one of :unique or :duplicate, got: #{inspect(keys)}"
    end

    name =
      case Keyword.fetch(options, :name) do
        {:ok, name} when is_atom(name) ->
          name

        {:ok, other} ->
          raise ArgumentError, "expected :name to be an atom, got: #{inspect(other)}"

        :error ->
          raise ArgumentError, "expected :name option to be present"
      end

    meta = Keyword.get(options, :meta, [])

    unless Keyword.keyword?(meta) do
      raise ArgumentError, "expected :meta to be a keyword list, got: #{inspect(meta)}"
    end

    partitions = Keyword.get(options, :partitions, 1)

    unless is_integer(partitions) and partitions >= 1 do
      raise ArgumentError,
            "expected :partitions to be a positive integer, got: #{inspect(partitions)}"
    end

    listeners = Keyword.get(options, :listeners, [])

    unless is_list(listeners) and Enum.all?(listeners, &is_atom/1) do
      raise ArgumentError,
            "expected :listeners to be a list of named processes, got: #{inspect(listeners)}"
    end

    # The @info format must be kept in sync with Registry.Partition optimization.
    entries = [
      {@all_info, {keys, partitions, nil, nil, listeners}},
      {@key_info, {keys, partitions, nil}} | meta
    ]

    Registry.Supervisor.start_link(keys, name, partitions, listeners, entries)
  end

  @doc false
  @deprecated "Use Registry.start_link/1 instead"
  def start_link(keys, name, options \\ []) when keys in @keys and is_atom(name) do
    start_link([keys: keys, name: name] ++ options)
  end

  @doc """
  Returns a specification to start a registry under a supervisor.

  See `Supervisor`.
  """
  @doc since: "1.5.0"
  @spec child_spec([start_option]) :: Supervisor.child_spec()
  def child_spec(options) do
    %{
      id: Keyword.get(options, :name, Registry),
      start: {Registry, :start_link, [options]},
      type: :supervisor
    }
  end

  @doc """
  Updates the value for `key` for the current process in the unique `registry`.

  Returns a `{new_value, old_value}` tuple or `:error` if there
  is no such key assigned to the current process.

  If a non-unique registry is given, an error is raised.

  ## Examples

      iex> Registry.start_link(keys: :unique, name: Registry.UpdateTest)
      iex> {:ok, _} = Registry.register(Registry.UpdateTest, "hello", 1)
      iex> Registry.lookup(Registry.UpdateTest, "hello")
      [{self(), 1}]
      iex> Registry.update_value(Registry.UpdateTest, "hello", &(&1 + 1))
      {2, 1}
      iex> Registry.lookup(Registry.UpdateTest, "hello")
      [{self(), 2}]

  """
  @doc since: "1.4.0"
  @spec update_value(registry, key, (value -> value)) ::
          {new_value :: term, old_value :: term} | :error
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
  the first element is the PID and the second element is the value
  associated to the PID. If there are no entries for the given key,
  the callback is never invoked.

  If the registry is partitioned, the callback is invoked multiple times
  per partition. If the registry is partitioned and `parallel: true` is
  given as an option, the dispatching happens in parallel. In both cases,
  the callback is only invoked if there are entries for that partition.

  See the module documentation for examples of using the `dispatch/3`
  function for building custom dispatching or a pubsub system.
  """
  @doc since: "1.4.0"
  @spec dispatch(registry, key, dispatcher, keyword) :: :ok
        when dispatcher: (entries :: [{pid, value}] -> term) | {module(), atom(), [any()]}
  def dispatch(registry, key, mfa_or_fun, opts \\ [])
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
        if Keyword.get(opts, :parallel, false) do
          registry
          |> dispatch_parallel(key, mfa_or_fun, partitions)
          |> Enum.each(&Task.await(&1, :infinity))
        else
          dispatch_serial(registry, key, mfa_or_fun, partitions)
        end
    end

    :ok
  end

  defp dispatch_serial(_registry, _key, _mfa_or_fun, 0) do
    :ok
  end

  defp dispatch_serial(registry, key, mfa_or_fun, partition) do
    partition = partition - 1

    registry
    |> key_ets!(partition)
    |> safe_lookup_second(key)
    |> apply_non_empty_to_mfa_or_fun(mfa_or_fun)

    dispatch_serial(registry, key, mfa_or_fun, partition)
  end

  defp dispatch_parallel(_registry, _key, _mfa_or_fun, 0) do
    []
  end

  defp dispatch_parallel(registry, key, mfa_or_fun, partition) do
    partition = partition - 1
    parent = self()

    task =
      Task.async(fn ->
        registry
        |> key_ets!(partition)
        |> safe_lookup_second(key)
        |> apply_non_empty_to_mfa_or_fun(mfa_or_fun)

        Process.unlink(parent)
        :ok
      end)

    [task | dispatch_parallel(registry, key, mfa_or_fun, partition)]
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

      iex> Registry.start_link(keys: :unique, name: Registry.UniqueLookupTest)
      iex> Registry.lookup(Registry.UniqueLookupTest, "hello")
      []
      iex> {:ok, _} = Registry.register(Registry.UniqueLookupTest, "hello", :world)
      iex> Registry.lookup(Registry.UniqueLookupTest, "hello")
      [{self(), :world}]
      iex> Task.async(fn -> Registry.lookup(Registry.UniqueLookupTest, "hello") end) |> Task.await()
      [{self(), :world}]

  The same applies to duplicate registries:

      iex> Registry.start_link(keys: :duplicate, name: Registry.DuplicateLookupTest)
      iex> Registry.lookup(Registry.DuplicateLookupTest, "hello")
      []
      iex> {:ok, _} = Registry.register(Registry.DuplicateLookupTest, "hello", :world)
      iex> Registry.lookup(Registry.DuplicateLookupTest, "hello")
      [{self(), :world}]
      iex> {:ok, _} = Registry.register(Registry.DuplicateLookupTest, "hello", :another)
      iex> Enum.sort(Registry.lookup(Registry.DuplicateLookupTest, "hello"))
      [{self(), :another}, {self(), :world}]

  """
  @doc since: "1.4.0"
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
  value or tuple element, while the atom `:"$1"` can be used to temporarily assign part
  of pattern to a variable for a subsequent comparison.

  Optionally, it is possible to pass a list of guard conditions for more precise matching.
  Each guard is a tuple, which describes checks that should be passed by assigned part of pattern.
  For example the `$1 > 1` guard condition would be expressed as the `{:>, :"$1", 1}` tuple.
  Please note that guard conditions will work only for assigned variables like `:"$1"`, `:"$2"`, etc.
  Avoid usage of special match variables `:"$_"` and `:"$$"`, because it might not work as expected.

  An empty list will be returned if there is no match.

  For unique registries, a single partition lookup is necessary. For
  duplicate registries, all partitions must be looked up.

  ## Examples

  In the example below we register the current process under the same
  key in a duplicate registry but with different values:

      iex> Registry.start_link(keys: :duplicate, name: Registry.MatchTest)
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
      iex> guards = [{:>, :"$1", 1}]
      iex> Registry.match(Registry.MatchTest, "hello", {:_, :_, :"$1"}, guards)
      [{self(), {2, :atom, 2}}]
      iex> guards = [{:is_atom, :"$1"}]
      iex> Registry.match(Registry.MatchTest, "hello", {:_, :"$1", :_}, guards) |> Enum.sort()
      [{self(), {1, :atom, 1}}, {self(), {2, :atom, 2}}]

  """
  @doc since: "1.4.0"
  @spec match(registry, key, match_pattern, guards) :: [{pid, term}]
  def match(registry, key, pattern, guards \\ []) when is_atom(registry) and is_list(guards) do
    guards = [{:"=:=", {:element, 1, :"$_"}, {:const, key}} | guards]
    spec = [{{:_, {:_, pattern}}, guards, [{:element, 2, :"$_"}]}]

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

      iex> Registry.start_link(keys: :unique, name: Registry.UniqueKeysTest)
      iex> Registry.keys(Registry.UniqueKeysTest, self())
      []
      iex> {:ok, _} = Registry.register(Registry.UniqueKeysTest, "hello", :world)
      iex> Registry.register(Registry.UniqueKeysTest, "hello", :later) # registry is :unique
      {:error, {:already_registered, self()}}
      iex> Registry.keys(Registry.UniqueKeysTest, self())
      ["hello"]

  Such is possible for duplicate registries though:

      iex> Registry.start_link(keys: :duplicate, name: Registry.DuplicateKeysTest)
      iex> Registry.keys(Registry.DuplicateKeysTest, self())
      []
      iex> {:ok, _} = Registry.register(Registry.DuplicateKeysTest, "hello", :world)
      iex> {:ok, _} = Registry.register(Registry.DuplicateKeysTest, "hello", :world)
      iex> Registry.keys(Registry.DuplicateKeysTest, self())
      ["hello", "hello"]

  """
  @doc since: "1.4.0"
  @spec keys(registry, pid) :: [key]
  def keys(registry, pid) when is_atom(registry) and is_pid(pid) do
    {kind, partitions, _, pid_ets, _} = info!(registry)
    {_, pid_ets} = pid_ets || pid_ets!(registry, pid, partitions)

    keys =
      try do
        spec = [{{pid, :"$1", :"$2", :_}, [], [{{:"$1", :"$2"}}]}]
        :ets.select(pid_ets, spec)
      catch
        :error, :badarg -> []
      end

    # Handle the possibility of fake keys
    keys = gather_keys(keys, [], false)

    cond do
      kind == :unique -> Enum.uniq(keys)
      true -> keys
    end
  end

  defp gather_keys([{key, {_, remaining}} | rest], acc, _fake) do
    gather_keys(rest, [key | acc], {key, remaining})
  end

  defp gather_keys([{key, _} | rest], acc, fake) do
    gather_keys(rest, [key | acc], fake)
  end

  defp gather_keys([], acc, {key, remaining}) do
    List.duplicate(key, remaining) ++ Enum.reject(acc, &(&1 === key))
  end

  defp gather_keys([], acc, false) do
    acc
  end

  @doc """
  Unregisters all entries for the given `key` associated to the current
  process in `registry`.

  Always returns `:ok` and automatically unlinks the current process from
  the owner if there are no more keys associated to the current process. See
  also `register/3` to read more about the "owner".

  ## Examples

  For unique registries:

      iex> Registry.start_link(keys: :unique, name: Registry.UniqueUnregisterTest)
      iex> Registry.register(Registry.UniqueUnregisterTest, "hello", :world)
      iex> Registry.keys(Registry.UniqueUnregisterTest, self())
      ["hello"]
      iex> Registry.unregister(Registry.UniqueUnregisterTest, "hello")
      :ok
      iex> Registry.keys(Registry.UniqueUnregisterTest, self())
      []

  For duplicate registries:

      iex> Registry.start_link(keys: :duplicate, name: Registry.DuplicateUnregisterTest)
      iex> Registry.register(Registry.DuplicateUnregisterTest, "hello", :world)
      iex> Registry.register(Registry.DuplicateUnregisterTest, "hello", :world)
      iex> Registry.keys(Registry.DuplicateUnregisterTest, self())
      ["hello", "hello"]
      iex> Registry.unregister(Registry.DuplicateUnregisterTest, "hello")
      :ok
      iex> Registry.keys(Registry.DuplicateUnregisterTest, self())
      []

  """
  @doc since: "1.4.0"
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
    true = __unregister__(key_ets, {key, {self, :_}}, 1)
    true = __unregister__(pid_ets, {self, key, key_ets, :_}, 2)

    unlink_if_unregistered(pid_server, pid_ets, self)

    for listener <- listeners do
      Kernel.send(listener, {:unregister, registry, key, self})
    end

    :ok
  end

  @doc """
  Unregister entries for a given key matching a pattern.

  ## Examples

  For unique registries it can be used to conditionally unregister a key on
  the basis of whether or not it matches a particular value.

      iex> Registry.start_link(keys: :unique, name: Registry.UniqueUnregisterMatchTest)
      iex> Registry.register(Registry.UniqueUnregisterMatchTest, "hello", :world)
      iex> Registry.keys(Registry.UniqueUnregisterMatchTest, self())
      ["hello"]
      iex> Registry.unregister_match(Registry.UniqueUnregisterMatchTest, "hello", :foo)
      :ok
      iex> Registry.keys(Registry.UniqueUnregisterMatchTest, self())
      ["hello"]
      iex> Registry.unregister_match(Registry.UniqueUnregisterMatchTest, "hello", :world)
      :ok
      iex> Registry.keys(Registry.UniqueUnregisterMatchTest, self())
      []

  For duplicate registries:

      iex> Registry.start_link(keys: :duplicate, name: Registry.DuplicateUnregisterMatchTest)
      iex> Registry.register(Registry.DuplicateUnregisterMatchTest, "hello", :world_a)
      iex> Registry.register(Registry.DuplicateUnregisterMatchTest, "hello", :world_b)
      iex> Registry.register(Registry.DuplicateUnregisterMatchTest, "hello", :world_c)
      iex> Registry.keys(Registry.DuplicateUnregisterMatchTest, self())
      ["hello", "hello", "hello"]
      iex> Registry.unregister_match(Registry.DuplicateUnregisterMatchTest, "hello", :world_a)
      :ok
      iex> Registry.keys(Registry.DuplicateUnregisterMatchTest, self())
      ["hello", "hello"]
      iex> Registry.lookup(Registry.DuplicateUnregisterMatchTest, "hello")
      [{self(), :world_b}, {self(), :world_c}]

  """
  @doc since: "1.5.0"
  @spec unregister_match(registry, key, match_pattern, guards) :: :ok
  def unregister_match(registry, key, pattern, guards \\ []) when is_list(guards) do
    self = self()

    {kind, partitions, key_ets, pid_ets, listeners} = info!(registry)
    {key_partition, pid_partition} = partitions(kind, key, self, partitions)
    key_ets = key_ets || key_ets!(registry, key_partition)
    {pid_server, pid_ets} = pid_ets || pid_ets!(registry, pid_partition)

    # Remove first from the key_ets because in case of crashes
    # the pid_ets will still be able to clean up. The last step is
    # to clean if we have no more entries.

    # Here we want to count all entries for this pid under this key, regardless of pattern.
    underscore_guard = {:"=:=", {:element, 1, :"$_"}, {:const, key}}
    total_spec = [{{:_, {self, :_}}, [underscore_guard], [true]}]
    total = :ets.select_count(key_ets, total_spec)

    # We only want to delete things that match the pattern
    delete_spec = [{{:_, {self, pattern}}, [underscore_guard | guards], [true]}]

    case :ets.select_delete(key_ets, delete_spec) do
      # We deleted everything, we can just delete the object
      ^total ->
        true = __unregister__(pid_ets, {self, key, key_ets, :_}, 2)
        unlink_if_unregistered(pid_server, pid_ets, self)

        for listener <- listeners do
          Kernel.send(listener, {:unregister, registry, key, self})
        end

      0 ->
        :ok

      deleted ->
        # There are still entries remaining for this pid. delete_object/2 with
        # duplicate_bag tables will remove every entry, but we only want to
        # remove those we have deleted. The solution is to introduce a temp_entry
        # that indicates how many keys WILL be remaining after the delete operation.
        counter = System.unique_integer()
        remaining = total - deleted
        temp_entry = {self, key, {key_ets, remaining}, counter}
        true = :ets.insert(pid_ets, temp_entry)
        true = __unregister__(pid_ets, {self, key, key_ets, :_}, 2)
        real_keys = List.duplicate({self, key, key_ets, counter}, remaining)
        true = :ets.insert(pid_ets, real_keys)
        # We've recreated the real remaining key entries, so we can now delete
        # our temporary entry.
        true = :ets.delete_object(pid_ets, temp_entry)
    end

    :ok
  end

  @doc """
  Registers the current process under the given `key` in `registry`.

  A value to be associated with this registration must also be given.
  This value will be retrieved whenever dispatching or doing a key
  lookup.

  This function returns `{:ok, owner}` or `{:error, reason}`.
  The `owner` is the PID in the registry partition responsible for
  the PID. The owner is automatically linked to the caller.

  If the registry has unique keys, it will return `{:ok, owner}` unless
  the key is already associated to a PID, in which case it returns
  `{:error, {:already_registered, pid}}`.

  If the registry has duplicate keys, multiple registrations from the
  current process under the same key are allowed.

  ## Examples

  Registering under a unique registry does not allow multiple entries:

      iex> Registry.start_link(keys: :unique, name: Registry.UniqueRegisterTest)
      iex> {:ok, _} = Registry.register(Registry.UniqueRegisterTest, "hello", :world)
      iex> Registry.register(Registry.UniqueRegisterTest, "hello", :later)
      {:error, {:already_registered, self()}}
      iex> Registry.keys(Registry.UniqueRegisterTest, self())
      ["hello"]

  Such is possible for duplicate registries though:

      iex> Registry.start_link(keys: :duplicate, name: Registry.DuplicateRegisterTest)
      iex> {:ok, _} = Registry.register(Registry.DuplicateRegisterTest, "hello", :world)
      iex> {:ok, _} = Registry.register(Registry.DuplicateRegisterTest, "hello", :world)
      iex> Registry.keys(Registry.DuplicateRegisterTest, self())
      ["hello", "hello"]

  """
  @doc since: "1.4.0"
  @spec register(registry, key, value) :: {:ok, pid} | {:error, {:already_registered, pid}}
  def register(registry, key, value) when is_atom(registry) do
    self = self()
    {kind, partitions, key_ets, pid_ets, listeners} = info!(registry)
    {key_partition, pid_partition} = partitions(kind, key, self, partitions)
    key_ets = key_ets || key_ets!(registry, key_partition)
    {pid_server, pid_ets} = pid_ets || pid_ets!(registry, pid_partition)

    # Notice we write first to the pid_ets table because it will
    # always be able to do the cleanup. If we register first to the
    # key one and the process crashes, the key will stay there forever.
    Process.link(pid_server)

    counter = System.unique_integer()
    true = :ets.insert(pid_ets, {self, key, key_ets, counter})

    case register_key(kind, pid_server, key_ets, key, {key, {self, value}}) do
      {:ok, _} = ok ->
        for listener <- listeners do
          Kernel.send(listener, {:register, registry, key, self, value})
        end

        ok

      {:error, {:already_registered, ^self}} = error ->
        true = :ets.delete_object(pid_ets, {self, key, key_ets, counter})
        error

      {:error, _} = error ->
        true = :ets.delete_object(pid_ets, {self, key, key_ets, counter})
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
  Reads registry metadata given on `start_link/1`.

  Atoms and tuples are allowed as keys.

  ## Examples

      iex> Registry.start_link(keys: :unique, name: Registry.MetaTest, meta: [custom_key: "custom_value"])
      iex> Registry.meta(Registry.MetaTest, :custom_key)
      {:ok, "custom_value"}
      iex> Registry.meta(Registry.MetaTest, :unknown_key)
      :error

  """
  @doc since: "1.4.0"
  @spec meta(registry, meta_key) :: {:ok, meta_value} | :error
  def meta(registry, key) when is_atom(registry) and (is_atom(key) or is_tuple(key)) do
    try do
      :ets.lookup(registry, key)
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect(registry)}"
    else
      [{^key, value}] -> {:ok, value}
      _ -> :error
    end
  end

  @doc """
  Stores registry metadata.

  Atoms and tuples are allowed as keys.

  ## Examples

      iex> Registry.start_link(keys: :unique, name: Registry.PutMetaTest)
      iex> Registry.put_meta(Registry.PutMetaTest, :custom_key, "custom_value")
      :ok
      iex> Registry.meta(Registry.PutMetaTest, :custom_key)
      {:ok, "custom_value"}
      iex> Registry.put_meta(Registry.PutMetaTest, {:tuple, :key}, "tuple_value")
      :ok
      iex> Registry.meta(Registry.PutMetaTest, {:tuple, :key})
      {:ok, "tuple_value"}

  """
  @doc since: "1.4.0"
  @spec put_meta(registry, meta_key, meta_value) :: :ok
  def put_meta(registry, key, value) when is_atom(registry) and (is_atom(key) or is_tuple(key)) do
    try do
      :ets.insert(registry, {key, value})
      :ok
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect(registry)}"
    end
  end

  @doc """
  Returns the number of registered keys in a registry.
  It runs in constant time.

  ## Examples
  In the example below we register the current process and ask for the
  number of keys in the registry:

      iex> Registry.start_link(keys: :unique, name: Registry.UniqueCountTest)
      iex> Registry.count(Registry.UniqueCountTest)
      0
      iex> {:ok, _} = Registry.register(Registry.UniqueCountTest, "hello", :world)
      iex> {:ok, _} = Registry.register(Registry.UniqueCountTest, "world", :world)
      iex> Registry.count(Registry.UniqueCountTest)
      2

  The same applies to duplicate registries:

      iex> Registry.start_link(keys: :duplicate, name: Registry.DuplicateCountTest)
      iex> Registry.count(Registry.DuplicateCountTest)
      0
      iex> {:ok, _} = Registry.register(Registry.DuplicateCountTest, "hello", :world)
      iex> {:ok, _} = Registry.register(Registry.DuplicateCountTest, "hello", :world)
      iex> Registry.count(Registry.DuplicateCountTest)
      2

  """
  @doc since: "1.7.0"
  @spec count(registry) :: non_neg_integer()
  def count(registry) when is_atom(registry) do
    case key_info!(registry) do
      {_kind, partitions, nil} ->
        Enum.reduce(0..(partitions - 1), 0, fn partition_index, acc ->
          acc + safe_size(key_ets!(registry, partition_index))
        end)

      {_kind, 1, key_ets} ->
        safe_size(key_ets)
    end
  end

  defp safe_size(ets) do
    try do
      :ets.info(ets, :size)
    catch
      :error, :badarg -> 0
    end
  end

  @doc """
  Returns the number of `{pid, value}` pairs under the given `key` in `registry`
  that match `pattern`.

  Pattern must be an atom or a tuple that will match the structure of the
  value stored in the registry. The atom `:_` can be used to ignore a given
  value or tuple element, while the atom `:"$1"` can be used to temporarily assign part
  of pattern to a variable for a subsequent comparison.

  Optionally, it is possible to pass a list of guard conditions for more precise matching.
  Each guard is a tuple, which describes checks that should be passed by assigned part of pattern.
  For example the `$1 > 1` guard condition would be expressed as the `{:>, :"$1", 1}` tuple.
  Please note that guard conditions will work only for assigned variables like `:"$1"`, `:"$2"`, etc.
  Avoid usage of special match variables `:"$_"` and `:"$$"`, because it might not work as expected.

  Zero will be returned if there is no match.

  For unique registries, a single partition lookup is necessary. For
  duplicate registries, all partitions must be looked up.

  ## Examples

  In the example below we register the current process under the same
  key in a duplicate registry but with different values:

      iex> Registry.start_link(keys: :duplicate, name: Registry.CountMatchTest)
      iex> {:ok, _} = Registry.register(Registry.CountMatchTest, "hello", {1, :atom, 1})
      iex> {:ok, _} = Registry.register(Registry.CountMatchTest, "hello", {2, :atom, 2})
      iex> Registry.count_match(Registry.CountMatchTest, "hello", {1, :_, :_})
      1
      iex> Registry.count_match(Registry.CountMatchTest, "hello", {2, :_, :_})
      1
      iex> Registry.count_match(Registry.CountMatchTest, "hello", {:_, :atom, :_})
      2
      iex> Registry.count_match(Registry.CountMatchTest, "hello", {:"$1", :_, :"$1"})
      2
      iex> Registry.count_match(Registry.CountMatchTest, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 1}])
      1
      iex> Registry.count_match(Registry.CountMatchTest, "hello", {:_, :"$1", :_}, [{:is_atom, :"$1"}])
      2

  """
  @doc since: "1.7.0"
  @spec count_match(registry, key, match_pattern, guards) :: non_neg_integer()
  def count_match(registry, key, pattern, guards \\ [])
      when is_atom(registry) and is_list(guards) do
    guards = [{:"=:=", {:element, 1, :"$_"}, {:const, key}} | guards]
    spec = [{{:_, {:_, pattern}}, guards, [true]}]

    case key_info!(registry) do
      {:unique, partitions, key_ets} ->
        key_ets = key_ets || key_ets!(registry, key, partitions)
        :ets.select_count(key_ets, spec)

      {:duplicate, 1, key_ets} ->
        :ets.select_count(key_ets, spec)

      {:duplicate, partitions, _key_ets} ->
        Enum.reduce(0..(partitions - 1), 0, fn partition_index, acc ->
          count = :ets.select_count(key_ets!(registry, partition_index), spec)
          acc + count
        end)
    end
  end

  @doc """
  Select key, pid, and values registered using full match specs.

  The `spec` consists of a list of three part tuples, in the shape of `[{match_pattern, guards, body}]`.

  The first part, the match pattern, must be a tuple that will match the structure of the
  the data stored in the registry, which is `{key, pid, value}`. The atom `:_` can be used to
  ignore a given value or tuple element, while the atom `:"$1"` can be used to temporarily
  assign part of pattern to a variable for a subsequent comparison. This can be combined
  like `{:"$1", :_, :_}`.

  The second part, the guards, is a list of conditions that allow filtering the results.
  Each guard is a tuple, which describes checks that should be passed by assigned part of pattern.
  For example the `$1 > 1` guard condition would be expressed as the `{:>, :"$1", 1}` tuple.
  Please note that guard conditions will work only for assigned variables like `:"$1"`, `:"$2"`, etc.

  The third part, the body, is a list of shapes of the returned entries. Like guards, you have access to
  assigned variables like `:"$1"`, which you can combine with hardcoded values to freely shape entries
  Note that tuples have to be wrapped in an additional tuple. To get a result format like
  `%{key: key, pid: pid, value: value}`, assuming you bound those variables in order in the match part,
  you would provide a body like `[%{key: :"$1", pid: :"$2", value: :"$3"}]`. Like guards, you can use
  some operations like `:element` to modify the output format.

  Do not use special match variables `:"$_"` and `:"$$"`, because they might not work as expected.

  Note that for large registries with many partitions this will be costly as it builds the result by
  concatenating all the partitions.

  ## Examples

  This example shows how to get everything from the registry.

      iex> Registry.start_link(keys: :unique, name: Registry.SelectAllTest)
      iex> {:ok, _} = Registry.register(Registry.SelectAllTest, "hello", :value)
      iex> {:ok, _} = Registry.register(Registry.SelectAllTest, "world", :value)
      iex> Registry.select(Registry.SelectAllTest, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])
      [{"world", self(), :value}, {"hello", self(), :value}]

  Get all keys in the registry.

      iex> Registry.start_link(keys: :unique, name: Registry.SelectAllTest)
      iex> {:ok, _} = Registry.register(Registry.SelectAllTest, "hello", :value)
      iex> {:ok, _} = Registry.register(Registry.SelectAllTest, "world", :value)
      iex> Registry.select(Registry.SelectAllTest, [{{:"$1", :_, :_}, [], [:"$1"]}])
      ["world", "hello"]

  """
  @doc since: "1.9.0"
  @spec select(registry, spec) :: [term]
  def select(registry, spec)
      when is_atom(registry) and is_list(spec) do
    spec =
      for part <- spec do
        case part do
          {{key, pid, value}, guards, select} ->
            {{key, {pid, value}}, guards, select}

          _ ->
            raise ArgumentError,
                  "invalid match specification in Registry.select/2: #{inspect(spec)}"
        end
      end

    case key_info!(registry) do
      {_kind, partitions, nil} ->
        Enum.flat_map(0..(partitions - 1), fn partition_index ->
          :ets.select(key_ets!(registry, partition_index), spec)
        end)

      {_kind, 1, key_ets} ->
        :ets.select(key_ets, spec)
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
        raise ArgumentError, "unknown registry: #{inspect(registry)}"
    end
  end

  defp key_info!(registry) do
    try do
      :ets.lookup_element(registry, @key_info, 2)
    catch
      :error, :badarg ->
        raise ArgumentError, "unknown registry: #{inspect(registry)}"
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

  @doc false
  def __unregister__(table, match, pos) do
    key = :erlang.element(pos, match)

    # We need to perform an element comparison if we have an special atom key.
    if is_atom(key) and reserved_atom?(Atom.to_string(key)) do
      match = :erlang.setelement(pos, match, :_)
      guard = {:"=:=", {:element, pos, :"$_"}, {:const, key}}
      :ets.select_delete(table, [{match, [guard], [true]}]) >= 0
    else
      :ets.match_delete(table, match)
    end
  end

  defp reserved_atom?("_"), do: true
  defp reserved_atom?("$" <> _), do: true
  defp reserved_atom?(_), do: false
end

defmodule Registry.Supervisor do
  @moduledoc false
  use Supervisor

  def start_link(kind, registry, partitions, listeners, entries) do
    arg = {kind, registry, partitions, listeners, entries}
    Supervisor.start_link(__MODULE__, arg, name: registry)
  end

  def init({kind, registry, partitions, listeners, entries}) do
    ^registry = :ets.new(registry, [:set, :public, :named_table, read_concurrency: true])
    true = :ets.insert(registry, entries)

    children =
      for i <- 0..(partitions - 1) do
        key_partition = Registry.Partition.key_name(registry, i)
        pid_partition = Registry.Partition.pid_name(registry, i)
        arg = {kind, registry, i, partitions, key_partition, pid_partition, listeners}

        %{
          id: pid_partition,
          start: {Registry.Partition, :start_link, [pid_partition, arg]}
        }
      end

    Supervisor.init(children, strategy: strategy_for_kind(kind))
  end

  # Unique registries have their key partition hashed by key.
  # This means that, if a PID partition crashes, it may have
  # entries from all key partitions, so we need to crash all.
  defp strategy_for_kind(:unique), do: :one_for_all

  # Duplicate registries have both key and pid partitions hashed
  # by pid. This means that, if a PID partition crashes, all of
  # its associated entries are in its sibling table, so we crash one.
  defp strategy_for_kind(:duplicate), do: :one_for_one
end

defmodule Registry.Partition do
  @moduledoc false

  # This process owns the equivalent key and pid ETS tables
  # and is responsible for monitoring processes that map to
  # its own pid table.
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
      entries = [
        {@key_info, {kind, partitions, key_ets}},
        {@all_info, {kind, partitions, key_ets, {self(), pid_ets}, listeners}}
      ]

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
    :ets.new(key_partition, [
      :duplicate_bag,
      :public,
      read_concurrency: true,
      write_concurrency: true
    ])
  end

  # A process can always have multiple keys, so the
  # pid partition is always a duplicate bag.
  defp init_pid_ets(_, pid_partition) do
    :ets.new(pid_partition, [
      :duplicate_bag,
      :public,
      read_concurrency: true,
      write_concurrency: true
    ])
  end

  def handle_call(:sync, _, state) do
    {:reply, :ok, state}
  end

  def handle_info({:EXIT, pid, _reason}, ets) do
    entries = :ets.take(ets, pid)

    for {_pid, key, key_ets, _counter} <- entries do
      key_ets =
        case key_ets do
          # In case the fake key_ets is being used. See unregister_match/2.
          {key_ets, _} ->
            key_ets

          _ ->
            key_ets
        end

      try do
        Registry.__unregister__(key_ets, {key, {pid, :_}}, 1)
      catch
        :error, :badarg -> :badarg
      end
    end

    {:noreply, ets}
  end
end
