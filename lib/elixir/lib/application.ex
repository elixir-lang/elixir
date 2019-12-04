defmodule Application do
  @moduledoc """
  A module for working with applications and defining application callbacks.

  Applications are the idiomatic way to package software in Erlang/OTP. To get
  the idea, they are similar to the "library" concept common in other
  programming languages, but with some additional characteristics.

  An application is a component implementing some specific functionality, with a
  standardized directory structure, configuration, and lifecycle. Applications
  are *loaded*, *started*, and *stopped*. Each application also has its own
  environment, which provides a unified API for configuring each application.

  Developers typically interact with the application environment and its
  callback module. Therefore those will be the topics we will cover first
  before jumping into details about the application resource file and life-cycle.

  ## The application environment

  Each application has its own environment. The environment is a keyword list
  that maps atoms to terms. Note that this environment is unrelated to the
  operating system environment.

  By default, the environment of an application is an empty list. In a Mix
  project `mix.exs` file, you can set the `:env` key in `application/0`:

      def application do
        [env: [redis_host: "localhost"]]
      end

  The environment is available after loading the application. This is typically
  done automatically for you by tools like Mix, but it can also be done explicitly
  like so:

      Application.load(:APP_NAME)
      #=> :ok

      Application.fetch_env!(:APP_NAME, :redis_host)
      #=> "localhost"

  In Mix projects, the environment of the application and its dependencies can
  be overridden via the `config/config.exs` file. For example, someone using
  your application can override its `:redis_host` environment variable as follows:

      import Config
      config :APP_NAME, :redis_host, "redis.local"

  The function `put_env/3` allows dynamic configuration of the application
  environment, but as a rule of thumb each application is responsible for its
  own environment. Please do not use the functions in this module for directly
  accessing or modifying the environment of other applications.

  ### Compile time environment

  The application environment is available both at compile time and at runtime.
  For example, you can use the application environment and the configuration
  system to specify a value that is read when the code is being compiled:

      config :APP_NAME, :redis_host, "redis.local"

  And then in the code:

      defmodule RedisClient do
        @host Application.fetch_env!(:APP_NAME, :redis_host)

        def connect do
          # access @host here
        end
      end

  This approach, however, has one big limitation: if the redis host is set
  dynamically, for example through `System.get_env/2` or only when booting
  the application, the code won't be recompiled. This can lead into situations
  where the configuration the code was compield with is different from the
  code running, which can be cumbersome to debug.

  Therefore, whenever reading the environment at compilation time, we
  recommend using `Application.compile_env!/2` instead:

      @host Application.compile_env!(:APP_NAME, :redis_host)

  When `compile_env!/2` is used, Elixir is capable of notifying whenever
  those values differ between compilation time and runtime.

  ## The application callback module

  Applications can be loaded, started, and stopped. Generally, build tools
  like Mix take care of starting an application and all of its dependencies
  for you, but you can also do it manually by calling:

      {:ok, _} = Application.ensure_all_started(:some_app)

  When an application starts, developers may configure a callback module
  that executes custom code. Developers use this callback to start the
  application supervision tree.

  The first step to do so is to add a `:mod` key to the `application/0`
  definition in your `mix.exs` file. It expects a tuple, with the application
  callback module and start argument (commonly an empty list):

      def application do
        [mod: {MyApp, []}]
      end

  The `MyApp` module given to `:mod` needs to implement the `Application` behaviour.
  This can be done by putting `use Application` in that module and implementing the
  `c:start/2` callback, for example:

      defmodule MyApp do
        use Application

        def start(_type, _args) do
          children = []
          Supervisor.start_link(children, strategy: :one_for_one)
        end
      end

  The `c:start/2` callback has to spawn and link a supervisor and return `{:ok,
  pid}` or `{:ok, pid, state}`, where `pid` is the PID of the supervisor, and
  `state` is an optional application state. `args` is the second element of the
  tuple given to the `:mod` option.

  The `type` argument passed to `c:start/2` is usually `:normal` unless in a
  distributed setup where application takeovers and failovers are configured.
  Distributed applications are beyond the scope of this documentation.

  When an application is shutting down, its `c:stop/1` callback is called after
  the supervision tree has been stopped by the runtime. This callback allows the
  application to do any final cleanup. The argument is the state returned by
  `c:start/2`, if it did, or `[]` otherwise. The return value of `c:stop/1` is
  ignored.

  By using `Application`, modules get a default implementation of `c:stop/1`
  that ignores its argument and returns `:ok`, but it can be overridden.

  Application callback modules may also implement the optional callback
  `c:prep_stop/1`. If present, `c:prep_stop/1` is invoked before the supervision
  tree is terminated. Its argument is the state returned by `c:start/2`, if it did,
  or `[]` otherwise, and its return value is passed to `c:stop/1`.

  ## The application resource file

  In the sections above, we have configured an application in the
  `application/0` section of the `mix.exs` file. Ultimately, Mix will use
  this configuration to create an [*application resource
  file*](http://erlang.org/doc/man/app.html), which is a file called
  `APP_NAME.app`. For example, the application resource file of the OTP
  application `ex_unit` is called `ex_unit.app`.

  You can learn more about the generation of application resource files in
  the documentation of `Mix.Tasks.Compile.App`, available as well by running
  `mix help compile.app`.

  ## The application lifecycle

  ### Loading applications

  Applications are *loaded*, which means that the runtime finds and processes
  their resource files:

      Application.load(:ex_unit)
      #=> :ok

  When an application is loaded, the environment specified in its resource file
  is merged with any overrides from config files.

  Loading an application *does not* load its modules.

  In practice, you rarely load applications by hand because that is part of the
  start process, explained next.

  ### Starting applications

  Applications are also *started*:

      Application.start(:ex_unit)
      #=> :ok

  Once your application is compiled, running your system is a matter of starting
  your current application and its dependencies. Differently from other languages,
  Elixir does not have a `main` procedure that is responsible for starting your
  system. Instead, you start one or more applications, each with their own
  initialization and termination logic.

  When an application is started, the `Application.load/1` is automatically
  invoked if it hasn't been done yet. Then, it checks if the dependencies listed
  in the `applications` key of the resource file are already started. Having at
  least one dependency not started is an error condition. Functions like
  `ensure_all_started/1` takes care of starting an application and all of its
  dependencies for you.

  If the application does not have a callback module configured, starting is
  done at this point. Otherwise, its `c:start/2` callback if invoked. The PID of
  the top-level supervisor returned by this function is stored by the runtime
  for later use, and the returned application state is saved too, if any.

  ### Stopping applications

  Started applications are, finally, *stopped*:

      Application.stop(:ex_unit)
      #=> :ok

  Stopping an application without a callback module is defined, but except for
  some system tracing, it is in practice a no-op.

  Stopping an application with a callback module has three steps:

    1. If present, invoke the optional callback `c:prep_stop/1`.
    2. Terminate the top-level supervisor.
    3. Invoke the required callback `c:stop/1`.

  The arguments passed to the callbacks are related to the state optionally
  returned by `c:start/2`, and are documented in the section about the callback
  module above.

  It is important to highlight that step 2 is a blocking one. Termination of a
  supervisor triggers a recursive chain of children terminations, therefore
  orderly shutting down all descendant processes. The `c:stop/1` callback is
  invoked only after termination of the whole supervision tree.

  Shutting down a live system cleanly can be done by calling `System.stop/1`. It
  will shut down every application in the opposite order they had been started.

  By default, a SIGTERM from the operating system will automatically translate to
  `System.stop/0`. You can also have more explicit control over operating system
  signals via the `:os.set_signal/2` function.

  ## Tooling

  The Mix build tool automates most of the application management tasks. For example,
  `mix test` automatically starts your application dependencies and your application
  itself before your test runs. `mix run --no-halt` boots your current project and
  can be used to start a long running system. See `mix help run`.

  Developers can also use `mix release` to build **releases**. Releases are able to
  package all of your source code as well as the Erlang VM into a single directory.
  Releases also give you explicit control over how each application is started and in
  which order. They also provide a more streamlined mechanism for starting and
  stopping systems, debugging, logging, as well as system monitoring.

  Finally, Elixir provides tools such as escripts and archives, which are
  different mechanisms for packaging your application. Those are typically used
  when tools must be shared between developers and not as deployment options.
  See `mix help archive.build` and `mix help escript.build` for more detail.

  ## Further information

  For further details on applications please check the documentation of the
  [`application`](http://www.erlang.org/doc/man/application.html) Erlang module,
  and the
  [Applications](http://www.erlang.org/doc/design_principles/applications.html)
  section of the [OTP Design Principles User's
  Guide](http://erlang.org/doc/design_principles/users_guide.html).
  """

  @doc """
  Called when an application is started.

  This function is called when an application is started using
  `Application.start/2` (and functions on top of that, such as
  `Application.ensure_started/2`). This function should start the top-level
  process of the application (which should be the top supervisor of the
  application's supervision tree if the application follows the OTP design
  principles around supervision).

  `start_type` defines how the application is started:

    * `:normal` - used if the startup is a normal startup or if the application
      is distributed and is started on the current node because of a failover
      from another node and the application specification key `:start_phases`
      is `:undefined`.
    * `{:takeover, node}` - used if the application is distributed and is
      started on the current node because of a failover on the node `node`.
    * `{:failover, node}` - used if the application is distributed and is
      started on the current node because of a failover on node `node`, and the
      application specification key `:start_phases` is not `:undefined`.

  `start_args` are the arguments passed to the application in the `:mod`
  specification key (for example, `mod: {MyApp, [:my_args]}`).

  This function should either return `{:ok, pid}` or `{:ok, pid, state}` if
  startup is successful. `pid` should be the PID of the top supervisor. `state`
  can be an arbitrary term, and if omitted will default to `[]`; if the
  application is later stopped, `state` is passed to the `stop/1` callback (see
  the documentation for the `c:stop/1` callback for more information).

  `use Application` provides no default implementation for the `start/2`
  callback.
  """
  @callback start(start_type, start_args :: term) ::
              {:ok, pid}
              | {:ok, pid, state}
              | {:error, reason :: term}

  @doc """
  Called before stopping the application.

  This function is called before the top-level supervisor is terminated. It
  receives the state returned by `c:start/2`, if it did, or `[]` otherwise.
  The return value is later passed to `c:stop/1`.
  """
  @callback prep_stop(state) :: state

  @doc """
  Called after an application has been stopped.

  This function is called after an application has been stopped, i.e., after its
  supervision tree has been stopped. It should do the opposite of what the
  `c:start/2` callback did, and should perform any necessary cleanup. The return
  value of this callback is ignored.

  `state` is the state returned by `c:start/2`, if it did, or `[]` otherwise.
  If the optional callback `c:prep_stop/1` is present, `state` is its return
  value instead.

  `use Application` defines a default implementation of this function which does
  nothing and just returns `:ok`.
  """
  @callback stop(state) :: term

  @doc """
  Starts an application in synchronous phases.

  This function is called after `start/2` finishes but before
  `Application.start/2` returns. It will be called once for every start phase
  defined in the application's (and any included applications') specification,
  in the order they are listed in.
  """
  @callback start_phase(phase :: term, start_type, phase_args :: term) ::
              :ok | {:error, reason :: term}

  @doc """
  Callback invoked after code upgrade, if the application environment
  has changed.

  `changed` is a keyword list of keys and their changed values in the
  application environment. `new` is a keyword list with all new keys
  and their values. `removed` is a list with all removed keys.
  """
  @callback config_change(changed, new, removed) :: :ok
            when changed: keyword, new: keyword, removed: [atom]

  @optional_callbacks start_phase: 3, prep_stop: 1, config_change: 3

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Application

      @doc false
      def stop(_state) do
        :ok
      end

      defoverridable Application
    end
  end

  @application_keys [
    :description,
    :id,
    :vsn,
    :modules,
    :maxP,
    :maxT,
    :registered,
    :included_applications,
    :applications,
    :mod,
    :start_phases
  ]

  application_key_specs = Enum.reduce(@application_keys, &{:|, [], [&1, &2]})

  @type app :: atom
  @type key :: atom
  @type application_key :: unquote(application_key_specs)
  @type value :: term
  @type state :: term
  @type start_type :: :normal | {:takeover, node} | {:failover, node}
  @type restart_type :: :permanent | :transient | :temporary
  @type compile_env_notification ::
          {app :: atom, path :: [atom() | term()], {:ok, value :: term} | :error}

  @doc """
  Returns the spec for `app`.

  The following keys are returned:

    * #{Enum.map_join(@application_keys, "\n  * ", &"`#{inspect(&1)}`")}

  Note the environment is not returned as it can be accessed via
  `fetch_env/2`. Returns `nil` if the application is not loaded.
  """
  @spec spec(app) :: [{application_key, value}] | nil
  def spec(app) when is_atom(app) do
    case :application.get_all_key(app) do
      {:ok, info} -> :lists.keydelete(:env, 1, info)
      :undefined -> nil
    end
  end

  @doc """
  Returns the value for `key` in `app`'s specification.

  See `spec/1` for the supported keys. If the given
  specification parameter does not exist, this function
  will raise. Returns `nil` if the application is not loaded.
  """
  @spec spec(app, application_key) :: value | nil
  def spec(app, key) when is_atom(app) and key in @application_keys do
    case :application.get_key(app, key) do
      {:ok, value} -> value
      :undefined -> nil
    end
  end

  @doc """
  Gets the application for the given module.

  The application is located by analyzing the spec
  of all loaded applications. Returns `nil` if
  the module is not listed in any application spec.
  """
  @spec get_application(atom) :: atom | nil
  def get_application(module) when is_atom(module) do
    case :application.get_application(module) do
      {:ok, app} -> app
      :undefined -> nil
    end
  end

  @doc """
  Returns all key-value pairs for `app`.
  """
  @spec get_all_env(app) :: [{key, value}]
  def get_all_env(app) when is_atom(app) do
    :application.get_all_env(app)
  end

  @doc """
  Gets all compile env listeners.

  A compile env listener is a process that receives a message in the shape of
  `{:compile_env, compile_env_notification}`, with `compile_env_notification`
  given by the the type `t:compile_env_notification/0`, every time `compile_env/3`
  or `compile_env!/2` is called.

  Listeners can be added with `put_compile_env_listener/1` and removed
  with `delete_compile_env_listener/1`.
  """
  @doc since: "1.10.0"
  @spec get_compile_env_listeners() :: [pid]
  def get_compile_env_listeners() do
    :elixir_config.get(:compile_env_listeners)
  end

  @doc """
  Puts a `pid` as a compile env listener.

  See `get_compile_env_listeners/0` for more information.
  """
  @doc since: "1.10.0"
  @spec put_compile_env_listener(pid) :: :ok
  def put_compile_env_listener(pid) when is_pid(pid) do
    :elixir_config.update(:compile_env_listeners, &[pid | &1])
    :ok
  end

  @doc """
  Deletes a `pid` from compile env listeners.

  See `get_compile_env_listeners/0` for more information.
  """
  @doc since: "1.10.0"
  @spec delete_compile_env_listener(pid) :: :ok
  def delete_compile_env_listener(pid) when is_pid(pid) do
    :elixir_config.update(:compile_env_listeners, &List.delete(&1, pid))
    :ok
  end

  @doc """
  Reads the application environment at compilation time.

  Similar to `get_env/3`, except it must be used to read values
  at compile time. This allows Elixir to track when configuration
  values between compile time and runtime differ.

  The first argument is the application name. The second argument
  `key_or_path`, is either an atom key or a path to traverse in
  search of the configuration, starting with an atom key.

  For example, imagine the following configuration:

      config :my_app, :key, [foo: [bar: :baz]]

  We can access it during compile time as:

      Application.compile_env(:my_app, :key)
      #=> [foo: [bar: :baz]]

      Application.compile_env(:my_app, [:key, :foo])
      #=> [bar: :baz]

      Application.compile_env(:my_app, [:key, :foo, :bar])
      #=> :baz

  A default value can also be given as third argument. If
  any of the keys in the path along the way is missing, the
  default value is used:

      Application.compile_env(:my_app, [:unknown, :foo, :bar], :default)
      #=> :default

      Application.compile_env(:my_app, [:key, :unknown, :bar], :default)
      #=> :default

      Application.compile_env(:my_app, [:key, :foo, :unknown], :default)
      #=> :default

  Giving a path is useful to let Elixir know that only certain paths
  in a large configuration are compile time dependent.
  """
  @doc since: "1.10.0"
  @spec compile_env(app, key, value) :: value
  def compile_env(app, key_or_path, default \\ nil) when is_atom(app) do
    case fetch_compile_env(app, key_or_path) do
      {:ok, value} -> value
      :error -> default
    end
  end

  @doc """
  Reads the application environment at compilation time or raises.

  This is the same as `compile_env/3` but it raises an
  ArgumentError if the configuration is ot available.
  """
  @doc since: "1.10.0"
  @spec compile_env!(app, key) :: value
  def compile_env!(app, key_or_path) when is_atom(app) do
    case fetch_compile_env(app, key_or_path) do
      {:ok, value} ->
        value

      :error ->
        raise ArgumentError,
              "could not fetch application environment #{inspect(key_or_path)} for application " <>
                "#{inspect(app)} #{fetch_env_failed_reason(app, key_or_path)}"
    end
  end

  defp fetch_compile_env(app, key) when is_atom(key),
    do: fetch_compile_env(app, key, [])

  defp fetch_compile_env(app, [key | paths]) when is_atom(key),
    do: fetch_compile_env(app, key, paths)

  defp fetch_compile_env(app, key, path) do
    return = traverse_env(fetch_env(app, key), path)

    for listener <- get_compile_env_listeners() do
      send(listener, {:compile_env, {app, [key | path], return}})
    end

    return
  end

  @doc """
  Validates the given compile env entries against the loaded application environment.

  It expects a list of entries in the format sent to compile env listeners,
  as defined in `t:compile_env_notification/0`.

  An error is raised if a mismatch is found.
  """
  @doc since: "1.10.0"
  @spec validate_compile_env!([compile_env_notification()]) :: :ok
  def validate_compile_env!(compile_env) do
    Enum.each(compile_env, fn {app, [key | path], compile_return} ->
      try do
        traverse_env(fetch_env(app, key), path)
      rescue
        e ->
          raise """
          could not read compile env at #{inspect([key | path])} for application #{inspect(app)}:

          #{Exception.format(:error, e, __STACKTRACE__)}

          Expected it to match the compile time value of #{return_to_text(compile_return)}.

          #{compile_env_tips(app)}
          """
      else
        ^compile_return ->
          :ok

        runtime_return ->
          raise """
          the application #{inspect(app)} has a different value set at #{inspect([key | path])} \
          during runtime compared to compile time. Since this application environment entry was \
          marked as compile time, this difference can lead to different behaviour than expected:

            * Compile time value #{return_to_text(compile_return)}
            * Runtime value #{return_to_text(runtime_return)}

          #{compile_env_tips(app)}
          """
      end
    end)

    :ok
  end

  defp compile_env_tips(app),
    do: """
    To fix this error, you might:

      * Make the runtime value match the compile time one

      * Recompile your project. If the misconfigured application is a dependency, \
        you may need to run "mix deps.compile #{app} --force"

      * Alternatively, you can disable this check. If you are using releases, you can \
        set :validate_compile_env to false in your release configuration. If you are \
        using Mix to start your system, you can pass the --no-validate-compile-env flag
    """

  defp return_to_text({:ok, value}), do: "was set to: #{inspect(value)}"
  defp return_to_text(:error), do: "was not set"

  defp traverse_env(return, []), do: return
  defp traverse_env(:error, _paths), do: :error
  defp traverse_env({:ok, value}, [key | keys]), do: traverse_env(Access.fetch(value, key), keys)

  @doc """
  Returns the value for `key` in `app`'s environment.

  If the configuration parameter does not exist, the function returns the
  `default` value.

  **Important:** if you are reading the application environment at compilation
  time, for example, inside the module definition instead of inside of a
  function, see `compile_env/3` instead.

  **Important:** if you are writing a library to be used by other developers,
  it is generally recommended to avoid the application environment, as the
  application environment is effectively a global storage. For more information,
  read our [library guidelines](library-guidelines.html).

  ## Examples

  `get_env/3` is commonly used to read the configuration of your OTP applications.
  Since Mix configurations are commonly used to configure applications, we will use
  this as a point of illustration.

  Consider a new application `:my_app`. `:my_app` contains a database engine which
  supports a pool of databases. The database engine needs to know the configuration for
  each of those databases, and that configuration is supplied by key-value pairs in
  environment of `:my_app`.

      config :my_app, Databases.RepoOne,
        # A database configuration
        ip: "localhost",
        port: 5433

      config :my_app, Databases.RepoTwo,
        # Another database configuration (for the same OTP app)
        ip: "localhost",
        port: 20717

      config :my_app, my_app_databases: [Databases.RepoOne, Databases.RepoTwo]

  Our database engine used by `:my_app` needs to know what databases exist, and
  what the database configurations are. The database engine can make a call to
  `get_env(:my_app, :my_app_databases)` to retrieve the list of databases (specified
  by module names). Our database engine can then traverse each repository in the
  list and then call `get_env(:my_app, Databases.RepoOne)` and so forth to retrieve
  the configuration of each one.
  """
  @spec get_env(app, key, value) :: value
  def get_env(app, key, default \\ nil) when is_atom(app) do
    :application.get_env(app, key, default)
  end

  @doc """
  Returns the value for `key` in `app`'s environment in a tuple.

  If the configuration parameter does not exist, the function returns `:error`.
  """
  @spec fetch_env(app, key) :: {:ok, value} | :error
  def fetch_env(app, key) when is_atom(app) do
    case :application.get_env(app, key) do
      {:ok, value} -> {:ok, value}
      :undefined -> :error
    end
  end

  @doc """
  Returns the value for `key` in `app`'s environment.

  If the configuration parameter does not exist, raises `ArgumentError`.

  **Important:** if you are reading the application environment at compilation
  time, for example, inside the module definition instead of inside of a
  function, see `compile_env!/2` instead.
  """
  @spec fetch_env!(app, key) :: value
  def fetch_env!(app, key) when is_atom(app) do
    case fetch_env(app, key) do
      {:ok, value} ->
        value

      :error ->
        raise ArgumentError,
              "could not fetch application environment #{inspect(key)} for application " <>
                "#{inspect(app)} #{fetch_env_failed_reason(app, key)}"
    end
  end

  defp fetch_env_failed_reason(app, key) do
    vsn = :application.get_key(app, :vsn)

    case vsn do
      {:ok, _} ->
        "because configuration at #{inspect(key)} was not set"

      :undefined ->
        "because the application was not loaded/started. If your application " <>
          "depends on #{inspect(app)} at runtime, make sure to load/start it or " <>
          "list it under :extra_applications in your mix.exs file"
    end
  end

  @doc """
  Puts the `value` in `key` for the given `app`.

  ## Options

    * `:timeout` - the timeout for the change (defaults to `5_000` milliseconds)
    * `:persistent` - persists the given value on application load and reloads

  If `put_env/4` is called before the application is loaded, the application
  environment values specified in the `.app` file will override the ones
  previously set.

  The `:persistent` option can be set to `true` when there is a need to guarantee
  parameters set with this function will not be overridden by the ones defined
  in the application resource file on load. This means persistent values will
  stick after the application is loaded and also on application reload.
  """
  @spec put_env(app, key, value, timeout: timeout, persistent: boolean) :: :ok
  def put_env(app, key, value, opts \\ []) when is_atom(app) do
    :application.set_env(app, key, value, opts)
  end

  # TODO: Remove this once we support Erlang/OTP 22+ exclusively.
  @compile {:no_warn_undefined, {:application, :set_env, 2}}

  @doc """
  Puts the environment for multiple apps at the same time.

  The given config should not:

    * have the same application listed more than once
    * have the same key inside the same application listed more than once

  If those conditions are not met, the behaviour is undefined
  (on Erlang/OTP 21 and earlier) or will raise (on Erlang/OTP 22
  and later).

  It receives the same options as `put_env/4`. Returns `:ok`.
  """
  @doc since: "1.9.0"
  @spec put_all_env([{app, [{key, value}]}], timeout: timeout, persistent: boolean) :: :ok
  def put_all_env(config, opts \\ []) when is_list(config) and is_list(opts) do
    # TODO: Remove function exported? check when we require Erlang/OTP 22+
    if function_exported?(:application, :set_env, 2) do
      :application.set_env(config, opts)
    else
      for app_keyword <- config,
          {app, keyword} = app_keyword,
          key_value <- keyword,
          {key, value} = key_value do
        :application.set_env(app, key, value, opts)
      end

      :ok
    end
  end

  @doc """
  Deletes the `key` from the given `app` environment.

  It receives the same options as `put_env/4`. Returns `:ok`.
  """
  @spec delete_env(app, key, timeout: timeout, persistent: boolean) :: :ok
  def delete_env(app, key, opts \\ []) when is_atom(app) do
    :application.unset_env(app, key, opts)
  end

  @doc """
  Ensures the given `app` is started.

  Same as `start/2` but returns `:ok` if the application was already
  started. This is useful in scripts and in test setup, where test
  applications need to be explicitly started:

      :ok = Application.ensure_started(:my_test_dep)

  """
  @spec ensure_started(app, restart_type) :: :ok | {:error, term}
  def ensure_started(app, type \\ :temporary) when is_atom(app) do
    :application.ensure_started(app, type)
  end

  @doc """
  Ensures the given `app` and its applications are started.

  Same as `start/2` but also starts the applications listed under
  `:applications` in the `.app` file in case they were not previously
  started.
  """
  @spec ensure_all_started(app, restart_type) :: {:ok, [app]} | {:error, {app, term}}
  def ensure_all_started(app, type \\ :temporary) when is_atom(app) do
    :application.ensure_all_started(app, type)
  end

  @doc """
  Starts the given `app`.

  If the `app` is not loaded, the application will first be loaded using `load/1`.
  Any included application, defined in the `:included_applications` key of the
  `.app` file will also be loaded, but they won't be started.

  Furthermore, all applications listed in the `:applications` key must be explicitly
  started before this application is. If not, `{:error, {:not_started, app}}` is
  returned, where `app` is the name of the missing application.

  In case you want to automatically load **and start** all of `app`'s dependencies,
  see `ensure_all_started/2`.

  The `type` argument specifies the type of the application:

    * `:permanent` - if `app` terminates, all other applications and the entire
      node are also terminated.

    * `:transient` - if `app` terminates with `:normal` reason, it is reported
      but no other applications are terminated. If a transient application
      terminates abnormally, all other applications and the entire node are
      also terminated.

    * `:temporary` - if `app` terminates, it is reported but no other
      applications are terminated (the default).

  Note that it is always possible to stop an application explicitly by calling
  `stop/1`. Regardless of the type of the application, no other applications will
  be affected.

  Note also that the `:transient` type is of little practical use, since when a
  supervision tree terminates, the reason is set to `:shutdown`, not `:normal`.
  """
  @spec start(app, restart_type) :: :ok | {:error, term}
  def start(app, type \\ :temporary) when is_atom(app) do
    :application.start(app, type)
  end

  @doc """
  Stops the given `app`.

  When stopped, the application is still loaded.
  """
  @spec stop(app) :: :ok | {:error, term}
  def stop(app) when is_atom(app) do
    :application.stop(app)
  end

  @doc """
  Loads the given `app`.

  In order to be loaded, an `.app` file must be in the load paths.
  All `:included_applications` will also be loaded.

  Loading the application does not start it nor load its modules, but
  it does load its environment.
  """
  @spec load(app) :: :ok | {:error, term}
  def load(app) when is_atom(app) do
    :application.load(app)
  end

  @doc """
  Unloads the given `app`.

  It will also unload all `:included_applications`.
  Note that the function does not purge the application modules.
  """
  @spec unload(app) :: :ok | {:error, term}
  def unload(app) when is_atom(app) do
    :application.unload(app)
  end

  @doc """
  Gets the directory for app.

  This information is returned based on the code path. Here is an
  example:

      File.mkdir_p!("foo/ebin")
      Code.prepend_path("foo/ebin")
      Application.app_dir(:foo)
      #=> "foo"

  Even though the directory is empty and there is no `.app` file
  it is considered the application directory based on the name
  "foo/ebin". The name may contain a dash `-` which is considered
  to be the app version and it is removed for the lookup purposes:

      File.mkdir_p!("bar-123/ebin")
      Code.prepend_path("bar-123/ebin")
      Application.app_dir(:bar)
      #=> "bar-123"

  For more information on code paths, check the `Code` module in
  Elixir and also Erlang's [`:code` module](http://www.erlang.org/doc/man/code.html).
  """
  @spec app_dir(app) :: String.t()
  def app_dir(app) when is_atom(app) do
    case :code.lib_dir(app) do
      lib when is_list(lib) -> IO.chardata_to_string(lib)
      {:error, :bad_name} -> raise ArgumentError, "unknown application: #{inspect(app)}"
    end
  end

  @doc """
  Returns the given path inside `app_dir/1`.

  If `path` is a string, then it will be used as the path inside `app_dir/1`. If
  `path` is a list of strings, it will be joined (see `Path.join/1`) and the result
  will be used as the path inside `app_dir/1`.

  ## Examples

      File.mkdir_p!("foo/ebin")
      Code.prepend_path("foo/ebin")

      Application.app_dir(:foo, "my_path")
      #=> "foo/my_path"

      Application.app_dir(:foo, ["my", "nested", "path"])
      #=> "foo/my/nested/path"

  """
  @spec app_dir(app, String.t() | [String.t()]) :: String.t()
  def app_dir(app, path)

  def app_dir(app, path) when is_atom(app) and is_binary(path) do
    Path.join(app_dir(app), path)
  end

  def app_dir(app, path) when is_atom(app) and is_list(path) do
    Path.join([app_dir(app) | path])
  end

  @doc """
  Returns a list with information about the applications which are currently running.
  """
  @spec started_applications(timeout) :: [{app, description :: charlist(), vsn :: charlist()}]
  def started_applications(timeout \\ 5000) do
    :application.which_applications(timeout)
  end

  @doc """
  Returns a list with information about the applications which have been loaded.
  """
  @spec loaded_applications :: [{app, description :: charlist(), vsn :: charlist()}]
  def loaded_applications do
    :application.loaded_applications()
  end

  @doc """
  Formats the error reason returned by `start/2`,
  `ensure_started/2`, `stop/1`, `load/1` and `unload/1`,
  returns a string.
  """
  @spec format_error(any) :: String.t()
  def format_error(reason) do
    try do
      do_format_error(reason)
    catch
      # A user could create an error that looks like a built-in one
      # causing an error.
      :error, _ ->
        inspect(reason)
    end
  end

  # exit(:normal) call is special cased, undo the special case.
  defp do_format_error({{:EXIT, :normal}, {mod, :start, args}}) do
    Exception.format_exit({:normal, {mod, :start, args}})
  end

  # {:error, reason} return value
  defp do_format_error({reason, {mod, :start, args}}) do
    Exception.format_mfa(mod, :start, args) <>
      " returned an error: " <> Exception.format_exit(reason)
  end

  # error or exit(reason) call, use exit reason as reason.
  defp do_format_error({:bad_return, {{mod, :start, args}, {:EXIT, reason}}}) do
    Exception.format_exit({reason, {mod, :start, args}})
  end

  # bad return value
  defp do_format_error({:bad_return, {{mod, :start, args}, return}}) do
    Exception.format_mfa(mod, :start, args) <> " returned a bad value: " <> inspect(return)
  end

  defp do_format_error({:already_started, app}) when is_atom(app) do
    "already started application #{app}"
  end

  defp do_format_error({:not_started, app}) when is_atom(app) do
    "not started application #{app}"
  end

  defp do_format_error({:bad_application, app}) do
    "bad application: #{inspect(app)}"
  end

  defp do_format_error({:already_loaded, app}) when is_atom(app) do
    "already loaded application #{app}"
  end

  defp do_format_error({:not_loaded, app}) when is_atom(app) do
    "not loaded application #{app}"
  end

  defp do_format_error({:invalid_restart_type, restart}) do
    "invalid application restart type: #{inspect(restart)}"
  end

  defp do_format_error({:invalid_name, name}) do
    "invalid application name: #{inspect(name)}"
  end

  defp do_format_error({:invalid_options, opts}) do
    "invalid application options: #{inspect(opts)}"
  end

  defp do_format_error({:badstartspec, spec}) do
    "bad application start specs: #{inspect(spec)}"
  end

  defp do_format_error({'no such file or directory', file}) do
    "could not find application file: #{file}"
  end

  defp do_format_error(reason) do
    Exception.format_exit(reason)
  end
end
