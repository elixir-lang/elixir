defmodule Application do
  @moduledoc """
  A module for working with applications and defining application callbacks.

  In Elixir (actually, in Erlang/OTP), an application is a component
  implementing some specific functionality, that can be started and stopped
  as a unit, and which can be re-used in other systems.

  Applications are defined with an application file named `APP.app` where
  `APP` is the application name, usually in `underscore_case`. The application
  file must reside in the same `ebin` directory as the compiled modules of the
  application.

  In Elixir, Mix is responsible for compiling your source code and
  generating your application `.app` file. Furthermore, Mix is also
  responsible for configuring, starting and stopping your application
  and its dependencies. For this reason, this documentation will focus
  on the remaining aspects of your application: the application environment
  and the application callback module.

  You can learn more about Mix generation of `.app` files by typing
  `mix help compile.app`.

  ## Application environment

  Once an application is started, OTP provides an application environment
  that can be used to configure the application.

  Assuming you are inside a Mix project, you can edit the `application/0`
  function in the `mix.exs` file to the following:

      def application do
        [env: [hello: :world]]
      end

  In the application function, we can define the default environment values
  for our application. By starting your application with `iex -S mix`, you
  can access the default value:

      Application.get_env(:APP_NAME, :hello)
      #=> :world

  It is also possible to put and delete values from the application value,
  including new values that are not defined in the environment file (although
  this should be avoided).

  Keep in mind that each application is responsible for its environment.
  Do not use the functions in this module for directly accessing or modifying
  the environment of other applications (as it may lead to inconsistent
  data in the application environment).

  ## Application module callback

  Often times, an application defines a supervision tree that must be started
  and stopped when the application starts and stops. For such, we need to
  define an application module callback. The first step is to define the
  module callback in the application definition in the `mix.exs` file:

      def application do
        [mod: {MyApp, []}]
      end

  Our application now requires the `MyApp` module to provide an application
  callback. This can be done by invoking `use Application` in that module and
  defining a `start/2` callback, for example:

      defmodule MyApp do
        use Application

        def start(_type, _args) do
          MyApp.Supervisor.start_link()
        end
      end

  `start/2` typically returns `{:ok, pid}` or `{:ok, pid, state}` where
  `pid` identifies the supervision tree and `state` is the application state.
  `args` is the second element of the tuple given to the `:mod` option.

  The `type` argument passed to `start/2` is usually `:normal` unless in a
  distributed setup where application takeovers and failovers are configured.
  This particular aspect of applications is explained in more detail in the
  OTP documentation:

    * [`:application` module](http://www.erlang.org/doc/man/application.html)
    * [Applications – OTP Design Principles](http://www.erlang.org/doc/design_principles/applications.html)

  A developer may also implement the `stop/1` callback (automatically defined
  by `use Application`) which does any application cleanup. It receives the
  application state and can return any value. Note that shutting down the
  supervisor is automatically handled by the VM.
  """

  @doc """
  Called when an application is started.

  This function is called when an the application is started using
  `Application.start/2` (and functions on top of that, such as
  `Application.ensure_started/2`). This function should start the top-level
  process of the application (which should be the top supervisor of the
  application's supervision tree if the application follows the OTP design
  principles around supervision).

  `start_type` defines how the application is started:

    * `:normal` - used if the startup is a normal startup or if the application
      is distributed and is started on the current node because of a failover
      from another mode and the application specification key `:start_phases`
      is `:undefined`.
    * `{:takeover, node}` - used if the application is distributed and is
      started on the current node because of a failover on the node `node`.
    * `{:failover, node}` - used if the application is distributed and is
      started on the current node because of a failover on node `node`, and the
      application specification key `:start_phases` is not `:undefined`.

  `start_args` are the arguments passed to the application in the `:mod`
  specification key (e.g., `mod: {MyApp, [:my_args]}`).

  This function should either return `{:ok, pid}` or `{:ok, pid, state}` if
  startup is successful. `pid` should be the PID of the top supervisor. `state`
  can be an arbitrary term, and if omitted will default to `[]`; if the
  application is later stopped, `state` is passed to the `stop/1` callback (see
  the documentation for the `c:stop/1` callback for more information).

  `use Application` provides no default implementation for the `start/2`
  callback.
  """
  @callback start(start_type, start_args :: term) ::
    {:ok, pid} |
    {:ok, pid, state} |
    {:error, reason :: term}

  @doc """
  Called when an application is stopped.

  This function is called when an application has stopped, i.e., when its
  supervision tree has been stopped. It should do the opposite of what the
  `start/2` callback did, and should perform any necessary cleanup. The return
  value of this callback is ignored.

  `state` is the return value of the `start/2` callback or the return value of
  the `prep_stop/1` function if the application module defines such a function.

  `use Application` defines a default implementation of this function which does
  nothing and just returns `:ok`.
  """
  @callback stop(state) :: term

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Application

      @doc false
      def stop(_state) do
        :ok
      end

      defoverridable [stop: 1]
    end
  end

  @type app :: atom
  @type key :: atom
  @type value :: term
  @type state :: term
  @type start_type :: :permanent | :transient | :temporary

  @application_keys [:description, :id, :vsn, :modules, :maxP, :maxT, :registered,
                     :included_applications, :applications, :mod, :start_phases]

  @doc """
  Returns the spec for `app`.

  The following keys are returned:

    * #{Enum.map_join @application_keys, "\n  * ", &inspect/1}

  Note the environment is not returned as it can be accessed via
  `fetch_env/2`. Returns `nil` if the application is not loaded.
  """
  @spec spec(app) :: [{key, value}] | nil
  def spec(app) do
    case :application.get_all_key(app) do
      {:ok, info} -> :lists.keydelete(:env, 1, info)
      :undefined  -> nil
    end
  end

  @doc """
  Returns the value for `key` in `app`'s specification.

  See `spec/1` for the supported keys. If the given
  specification parameter does not exist, this function
  will raise. Returns `nil` if the application is not loaded.
  """
  @spec spec(app, key) :: value | nil
  def spec(app, key) when key in @application_keys do
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
  def get_all_env(app) do
    :application.get_all_env(app)
  end

  @doc """
  Returns the value for `key` in `app`'s environment.

  If the configuration parameter does not exist, the function returns the
  `default` value.
  """
  @spec get_env(app, key, value) :: value
  def get_env(app, key, default \\ nil) do
    :application.get_env(app, key, default)
  end

  @doc """
  Returns the value for `key` in `app`'s environment in a tuple.

  If the configuration parameter does not exist, the function returns `:error`.
  """
  @spec fetch_env(app, key) :: {:ok, value} | :error
  def fetch_env(app, key) do
    case :application.get_env(app, key) do
      {:ok, value} -> {:ok, value}
      :undefined -> :error
    end
  end

  @doc """
  Returns the value for `key` in `app`'s environment.

  If the configuration parameter does not exist, raises `ArgumentError`.
  """
  @spec fetch_env!(app, key) :: value | no_return
  def fetch_env!(app, key) do
    case fetch_env(app, key) do
      {:ok, value} -> value
      :error ->
        raise ArgumentError,
          "application #{inspect app} is not loaded, " <>
          "or the configuration parameter #{inspect key} is not set"
    end
  end

  @doc """
  Puts the `value` in `key` for the given `app`.

  ## Options

    * `:timeout`    - the timeout for the change (defaults to 5000ms)
    * `:persistent` - persists the given value on application load and reloads

  If `put_env/4` is called before the application is loaded, the application
  environment values specified in the `.app` file will override the ones
  previously set.

  The persistent option can be set to `true` when there is a need to guarantee
  parameters set with this function will not be overridden by the ones defined
  in the application resource file on load. This means persistent values will
  stick after the application is loaded and also on application reload.
  """
  @spec put_env(app, key, value, [timeout: timeout, persistent: boolean]) :: :ok
  def put_env(app, key, value, opts \\ []) do
    :application.set_env(app, key, value, opts)
  end

  @doc """
  Deletes the `key` from the given `app` environment.

  See `put_env/4` for a description of the options.
  """
  @spec delete_env(app, key, [timeout: timeout, persistent: boolean]) :: :ok
  def delete_env(app, key, opts \\ []) do
    :application.unset_env(app, key, opts)
  end

  @doc """
  Ensures the given `app` is started.

  Same as `start/2` but returns `:ok` if the application was already
  started. This is useful in scripts and in test setup, where test
  applications need to be explicitly started:

      :ok = Application.ensure_started(:my_test_dep)

  """
  @spec ensure_started(app, start_type) :: :ok | {:error, term}
  def ensure_started(app, type \\ :temporary) when is_atom(app) do
    :application.ensure_started(app, type)
  end

  @doc """
  Ensures the given `app` and its applications are started.

  Same as `start/2` but also starts the applications listed under
  `:applications` in the `.app` file in case they were not previously
  started.
  """
  @spec ensure_all_started(app, start_type) :: {:ok, [app]} | {:error, {app, term}}
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

  In case you want to automatically  load **and start** all of `app`'s dependencies,
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
  @spec start(app, start_type) :: :ok | {:error, term}
  def start(app, type \\ :temporary) when is_atom(app) do
    :application.start(app, type)
  end

  @doc """
  Stops the given `app`.

  When stopped, the application is still loaded.
  """
  @spec stop(app) :: :ok | {:error, term}
  def stop(app) do
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
  Elixir and also Erlang's `:code` module.
  """
  @spec app_dir(app) :: String.t
  def app_dir(app) when is_atom(app) do
    case :code.lib_dir(app) do
      lib when is_list(lib) -> IO.chardata_to_string(lib)
      {:error, :bad_name} -> raise ArgumentError, "unknown application: #{inspect app}"
    end
  end

  @doc """
  Returns the given path inside `app_dir/1`.
  """
  @spec app_dir(app, String.t | [String.t]) :: String.t
  def app_dir(app, path) when is_binary(path) do
    Path.join(app_dir(app), path)
  end
  def app_dir(app, path) when is_list(path) do
    Path.join([app_dir(app) | path])
  end

  @doc """
  Returns a list with information about the applications which are currently running.
  """
  @spec started_applications(timeout) :: [tuple]
  def started_applications(timeout \\ 5000) do
    :application.which_applications(timeout)
  end

  @doc """
  Returns a list with information about the applications which have been loaded.
  """
  @spec loaded_applications :: [tuple]
  def loaded_applications do
    :application.loaded_applications
  end

  @doc """
  Formats the error reason returned by `start/2`,
  `ensure_started/2`, `stop/1`, `load/1` and `unload/1`,
  returns a string.
  """
  @spec format_error(any) :: String.t
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
    Exception.format_mfa(mod, :start, args) <> " returned an error: " <>
      Exception.format_exit(reason)
  end

  # error or exit(reason) call, use exit reason as reason.
  defp do_format_error({:bad_return, {{mod, :start, args}, {:EXIT, reason}}}) do
    Exception.format_exit({reason, {mod, :start, args}})
  end

  # bad return value
  defp do_format_error({:bad_return, {{mod, :start, args}, return}}) do
    Exception.format_mfa(mod, :start, args) <>
      " returned a bad value: " <> inspect(return)
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
