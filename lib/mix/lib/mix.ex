defmodule Mix do
  @moduledoc ~S"""
  Mix is a build tool that provides tasks for creating, compiling,
  and testing Elixir projects, managing its dependencies, and more.

  ## `Mix.Project`

  The foundation of Mix is a project. A project can be defined by using
  `Mix.Project` in a module, usually placed in a file named `mix.exs`:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0"
          ]
        end
      end

  See the `Mix.Project` module for detailed documentation on Mix projects.

  Once the project is defined, a number of default Mix tasks can be run
  directly from the command line:

    * `mix compile` - compiles the current project
    * `mix test` - runs tests for the given project
    * `mix run` - runs a particular command inside the project

  Each task has its own options and sometimes specific configuration
  to be defined in the `project/0` function. You can use `mix help`
  to list all available tasks and `mix help NAME` to show help for
  a particular task.

  The best way to get started with your first project is by calling
  `mix new my_project` from the command line.

  ## `Mix.Task`

  Tasks are what make Mix extensible.

  Projects can extend Mix behaviour by adding their own tasks. For
  example, adding the task below inside your project will
  make it available to everyone that uses your project:

      defmodule Mix.Tasks.Hello do
        use Mix.Task

        def run(_) do
          Mix.shell().info("Hello world")
        end
      end

  The task can now be invoked with `mix hello`.

  See the `Mix.Task` behaviour for detailed documentation on Mix tasks.

  ## Dependencies

  Mix also manages your dependencies and integrates nicely with the [Hex package
  manager](https://hex.pm).

  In order to use dependencies, you need to add a `:deps` key
  to your project configuration. We often extract the list of dependencies
  into its own function:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0",
            deps: deps()
          ]
        end

        defp deps do
          [
            {:ecto, "~> 2.0"},
            {:plug, github: "elixir-lang/plug"}
          ]
        end
      end

  You can run `mix help deps` to learn more about dependencies in Mix.

  ## Environments

  Mix supports different environments. Environments allow developers
  to prepare and organize their project specifically for different
  scenarios. By default, Mix provides three environments:

    * `:dev` - the default environment
    * `:test` - the environment `mix test` runs on
    * `:prod` - the environment your dependencies run on

  The environment can be changed via the command line by setting
  the `MIX_ENV` environment variable, for example:

  ```bash
  $ MIX_ENV=prod mix run server.exs
  ```

  You can also specify that certain dependencies are available only for
  certain environments:

      {:some_test_dependency, "~> 1.0", only: :test}

  When running Mix via the command line, you can configure the default
  environment or the preferred environment per task via the `def cli`
  function in your `mix.exs`. For example:

      def cli do
        [
          default_env: :local,
          preferred_envs: [docs: :docs]
        ]
      end

  The environment can be read via `Mix.env/0`.

  ## Targets

  Besides environments, Mix supports targets. Targets are useful when a
  project needs to compile to different architectures and some of the
  dependencies are only available to some of them. By default, the target
  is `:host` but it can be set via the `MIX_TARGET` environment variable.

  When running Mix via the command line, you can configure the default
  target or the preferred target per task via the `def cli` function
  in your `mix.exs`. For example:

      def cli do
        [
          default_target: :local,
          preferred_targets: [docs: :docs]
        ]
      end

  The target can be read via `Mix.target/0`.

  ## Configuration

  Mix allows you to configure the application environment of your application
  and of your dependencies. See the `Application` module to learn more about
  the application environment. On this section, we will focus on how to configure
  it at two distinct moments: build-time and runtime.

  > #### Avoiding the application environment {: .warning}
  >
  > The application environment is discouraged for libraries. See Elixir's
  > [Library Guidelines](https://hexdocs.pm/elixir/library-guidelines.html) for
  > more information.

  ### Build-time configuration

  Whenever you invoke a `mix` command, Mix loads the configuration
  in `config/config.exs`, if said file exists. It is common for the
  `config/config.exs` file itself to import other configuration based
  on the current `MIX_ENV`, such as `config/dev.exs`, `config/test.exs`,
  and `config/prod.exs`, by calling `Config.import_config/1`:

      import Config
      import_config "#{config_env()}.exs"

  We say `config/config.exs` and all imported files are build-time
  configuration as they are evaluated whenever you compile your code.
  In other words, if your configuration does something like:

      import Config
      config :my_app, :secret_key, System.fetch_env!("MY_APP_SECRET_KEY")

  The `:secret_key` key under `:my_app` will be computed on the host
  machine before your code compiles. This can be an issue if the machine
  compiling your code does not have access to all environment variables
  used to run your code, as loading the config above will fail due to the
  missing environment variable. Furthermore, even if the environment variable
  is set, changing the environment variable will require a full recompilation
  of your application by calling `mix compile --force` (otherwise your project
  won't start). Luckily, Mix also provides runtime configuration, which is
  preferred in such cases and we will see next.

  ### Runtime configuration

  To enable runtime configuration in your release, all you need to do is
  to create a file named `config/runtime.exs`:

      import Config
      config :my_app, :secret_key, System.fetch_env!("MY_APP_SECRET_KEY")

  This file is executed whenever your project runs. If you assemble
  a release with `mix release`, it also executes every time your release
  starts.

  ## Aliases

  Aliases are shortcuts or tasks specific to the current project.

  In the [Mix.Task section](#module-mix-task), we have defined a task that would be
  available to everyone using our project as a dependency. What if
  we wanted the task to only be available for our project? Just
  define an alias:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0",
            aliases: aliases()
          ]
        end

        defp aliases do
          [
            c: "compile",
            hello: &hello/1,
            paid_task: &paid_task/1
          ]
        end

        defp hello(_) do
          Mix.shell().info("Hello world")
        end

        defp paid_task(_) do
          Mix.Task.run("paid.task", [
            "first_arg",
            "second_arg",
            "--license-key",
            System.fetch_env!("SOME_LICENSE_KEY")
          ])
        end
      end

  In the example above, we have defined three aliases. One is `mix c`
  which is a shortcut for `mix compile`. Another is named
  `mix hello` and the third is named `mix paid_task`, which executes
  the code inside a custom function to invoke the `paid.task` task
  with several arguments, including one pulled from an environment
  variable.

  Aliases may also be lists, specifying multiple tasks to be run
  consecutively:

      [all: [&hello/1, "deps.get --only #{Mix.env()}", "compile"]]

  In the example above, we have defined an alias named `mix all`,
  that prints "Hello world", then fetches dependencies specific
  to the current environment, and compiles the project.

  Aliases can also be used to augment existing tasks. Let's suppose
  you want to augment `mix clean` to clean another directory Mix does
  not know about:

      [clean: ["clean", &clean_extra/1]]

  Where `&clean_extra/1` would be a function in your `mix.exs`
  with extra cleanup logic.

  If the alias is overriding an existing task, the arguments given
  to the alias will be forwarded to the original task in order to
  preserve semantics. Otherwise arguments given to the alias are
  appended to the arguments of the last task in the list.

  Another use case of aliases is to run Elixir scripts and shell
  commands, for example:

      # priv/hello1.exs
      IO.puts("Hello One")

      # priv/hello2.exs
      IO.puts("Hello Two")

      # priv/world.sh
      #!/bin/sh
      echo "world!"

      # mix.exs
      defp aliases do
        [
          some_alias: ["hex.info", "run priv/hello1.exs", "cmd priv/world.sh"]
        ]
      end

  In the example above we have created the alias `some_alias` that will
  run the task `mix hex.info`, then `mix run` to run an Elixir script,
  then `mix cmd` to execute a command line shell script. This shows how
  powerful aliases mixed with Mix tasks can be.

  One common pitfall of aliases comes when trying to invoke the same task
  multiple times. Mix tasks are designed to run only once. This prevents
  the same task from being executed multiple times. For example, if there
  are several tasks depending on `mix compile`, the code will be compiled
  only once.

  Similarly, `mix format` can only be invoked once. So if you have an alias
  that attempts to invoke `mix format` multiple times, it won't work unless
  it is explicitly reenabled using `Mix.Task.reenable/1`:

      another_alias: [
        "format --check-formatted priv/hello1.exs",
        "cmd priv/world.sh",
        fn _ -> Mix.Task.reenable("format") end,
        "format --check-formatted priv/hello2.exs"
      ]

  Some tasks are automatically reenabled though, as they are expected to
  be invoked multiple times, such as: `mix cmd`, `mix do`, `mix xref`, etc.

  Finally, aliases defined in the current project do not affect its
  dependencies and aliases defined in dependencies are not accessible
  from the current project, with the exception of umbrella projects.
  Umbrella projects will run the aliases of its children when the
  umbrella project itself does not define said alias and there is no
  task with said name.

  ## Environment variables

  Several environment variables can be used to modify Mix's behavior.

  Mix responds to the following variables:

    * `MIX_ARCHIVES` - specifies the directory into which the archives should be installed
      (default: `~/.mix/archives`)

    * `MIX_BUILD_PATH` - sets the project `Mix.Project.build_path/0` config.
      This option must always point to a subdirectory inside a temporary directory.
      For instance, never "/tmp" or "_build" but "_build/PROD" or "/tmp/PROD", as
      required by Mix. This environment variable is used mostly by external build
      tools. For your CI servers, you likely want to use `MIX_BUILD_ROOT` below.

    * `MIX_BUILD_ROOT` - sets the root directory where build artifacts should be
      written to. For example, "_build". If `MIX_BUILD_PATH` is set, this option
      is ignored.

    * `MIX_DEBUG` - outputs debug information about each task before running it

    * `MIX_DEPS_PATH` - sets the project `Mix.Project.deps_path/0` config for the
      current project (default: `deps`)

    * `MIX_ENV` - specifies which environment should be used. See [Environments](#module-environments)

    * `MIX_EXS` - changes the full path to the `mix.exs` file

    * `MIX_HOME` - path to Mix's home directory, stores configuration files and scripts used by Mix
      (default: `~/.mix`)

    * `MIX_INSTALL_DIR` *(since v1.12.0)* - specifies directory where `Mix.install/2` keeps
       install cache

    * `MIX_PATH` - appends extra code paths

    * `MIX_PROFILE` - a list of comma-separated Mix tasks to profile the time spent on
      functions by the process running the task

    * `MIX_QUIET` - does not print information messages to the terminal

    * `MIX_REBAR3` - path to rebar3 command that overrides the one Mix installs
      (default: `~/.mix/rebar3`)

    * `MIX_TARGET` - specifies which target should be used. See [Targets](#module-targets)

    * `MIX_XDG` - asks Mix to follow the [XDG Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
      for its home directory and configuration files. This behavior needs to
      be opt-in due to backwards compatibility. `MIX_HOME` has higher preference
      than `MIX_XDG`. If none of the variables are set, the default directory
      `~/.mix` will be used

  Environment variables that are not meant to hold a value (and act basically as
  flags) should be set to either `1` or `true`, for example:

      $ MIX_DEBUG=1 mix compile

  ## SSL certificates

  Mix and the Hex package manager use the default operating system certificates
  when downloading resources. In certain situations, such as when running behind
  proxies, you want to replace those certificates.

  If you simply want to change the certificates used by Mix and Hex, you may set
  the `HEX_CACERTS_PATH` environment variable, pointing to a CA certificate file.
  See [`mix hex.config`](https://hexdocs.pm/hex/Mix.Tasks.Hex.Config.html#module-config-keys).

  From Erlang/OTP 27.2, it is also possible to change the certificates for your
  project as a whole. To do so, you might add the following to your `config/config.exs`:

      config :public_key, :cacerts_path, "/path/to/certs.pem"

  You can also do so by setting the `ERL_AFLAGS` and `ERL_ZFLAGS` environment variables:

  ```bash
  ERL_AFLAGS="-public_key cacerts_path '\"/path/to/certs.pem\"'"
  ERL_ZFLAGS="-public_key cacerts_path '\"/path/to/certs.pem\"'"
  ```

  You can verify if the configuration has been properly set by calling the following
  inside `iex -S mix`:

      Application.load(:public_key)
      Application.get_env(:public_key, :cacerts_path)

  And by loading the certificates:

      :public_key.cacerts_get()

  """

  @mix_install_project __MODULE__.InstallProject
  @mix_install_app :mix_install
  @mix_install_app_string Atom.to_string(@mix_install_app)

  use Application

  import Kernel, except: [raise: 2]

  @doc false
  def start do
    {:ok, _} = Application.ensure_all_started(:mix)
    :ok
  end

  @impl true
  def start(_type, []) do
    Mix.Local.append_archives()
    Mix.Local.append_paths()
    children = [Mix.Sync.PubSub, Mix.State, Mix.TasksServer, Mix.ProjectStack]
    opts = [strategy: :one_for_one, name: Mix.Supervisor, max_restarts: 0]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_data) do
    Mix.Local.remove_archives()
    Mix.Local.remove_paths()
    :ok
  end

  @doc """
  Returns the current Mix environment.

  This function should not be used at runtime in application code (as opposed
  to infrastructure and build code like Mix tasks). Mix is a build tool and may
  not be available after the code is compiled (for example in a release).

  To differentiate the program behavior depending on the environment, it is
  recommended to use application environment through `Application.get_env/3`.
  Proper configuration can be set in config files, often per-environment
  (see the `Config` module for more information).
  """
  @spec env() :: atom()
  def env do
    # env is not available on bootstrapping, so set a :dev default
    Mix.State.get(:env, :dev)
  end

  @doc """
  Changes the current Mix environment to `env`.

  Be careful when invoking this function as any project
  configuration won't be reloaded.

  This function should not be used at runtime in application code
  (see `env/0` for more information).
  """
  @spec env(atom()) :: :ok
  def env(env) when is_atom(env) do
    Mix.State.put(:env, env)
  end

  @doc """
  Returns the Mix target.
  """
  @spec target() :: atom()
  def target do
    # target is not available on bootstrapping, so set a :host default
    Mix.State.get(:target, :host)
  end

  @doc """
  Changes the current Mix target to `target`.

  Be careful when invoking this function as any project
  configuration won't be reloaded.
  """
  @spec target(atom()) :: :ok
  def target(target) when is_atom(target) do
    Mix.State.put(:target, target)
  end

  @doc """
  Returns the default compilers used by Mix.

  It can be used in your `mix.exs` to prepend or
  append new compilers to Mix:

      def project do
        [compilers: Mix.compilers() ++ [:foo, :bar]]
      end

  """
  @spec compilers() :: [atom()]
  def compilers do
    [:erlang, :elixir, :app]
  end

  @doc """
  Returns the current shell.

  `shell/0` can be used as a wrapper for the current shell. It contains
  conveniences for requesting information from the user, printing to the
  shell and so forth. The Mix shell is swappable (see `shell/1`), allowing
  developers to use a test shell that simply sends messages to the current
  process instead of performing IO (see `Mix.Shell.Process`).

  By default, this returns `Mix.Shell.IO`.

  ## Examples

      Mix.shell().info("Preparing to do something dangerous...")

      if Mix.shell().yes?("Are you sure?") do
        # do something dangerous
      end

  """
  @spec shell() :: module
  def shell do
    Mix.State.get(:shell, Mix.Shell.IO)
  end

  @doc """
  Sets the current shell.

  As an argument you may pass `Mix.Shell.IO`, `Mix.Shell.Process`,
  `Mix.Shell.Quiet`, or any module that implements the `Mix.Shell`
  behaviour.

  After calling this function, `shell` becomes the shell that is
  returned by `shell/0`.

  ## Examples

      iex> Mix.shell(Mix.Shell.IO)
      :ok

  You can use `shell/0` and `shell/1` to temporarily switch shells,
  for example, if you want to run a Mix Task that normally produces
  a lot of output:

      shell = Mix.shell()
      Mix.shell(Mix.Shell.Quiet)

      try do
        Mix.Task.run("noisy.task")
      after
        Mix.shell(shell)
      end

  """
  @spec shell(module) :: :ok
  def shell(shell) do
    Mix.State.put(:shell, shell)
  end

  @doc """
  Returns `true` if Mix is in debug mode, `false` otherwise.
  """
  @spec debug?() :: boolean()
  def debug? do
    Mix.State.get(:debug, false)
  end

  @doc """
  Sets Mix debug mode.
  """
  @spec debug(boolean()) :: :ok
  def debug(debug) when is_boolean(debug) do
    Mix.State.put(:debug, debug)
  end

  @doc """
  Raises a Mix error that is nicely formatted, defaulting to exit status `1`.
  """
  @spec raise(binary) :: no_return
  def raise(message) do
    __MODULE__.raise(message, exit_status: 1)
  end

  @doc """
  Raises a Mix error that is nicely formatted.

  ## Options

    * `:exit_status` - defines exit status, defaults to `1`

  """
  @doc since: "1.12.3"
  @spec raise(binary, exit_status: non_neg_integer()) :: no_return
  def raise(message, opts) when is_binary(message) and is_list(opts) do
    status =
      opts[:exit_status] ||
        if exit_code = opts[:exit_code] do
          IO.warn(":exit_code is deprecated, use :exit_status instead")
          exit_code
        else
          1
        end

    Kernel.raise(Mix.Error, mix: status, message: message)
  end

  @doc """
  The path for local archives or escripts.
  """
  @doc since: "1.10.0"
  @spec path_for(:archives | :escripts) :: String.t()
  def path_for(:archives) do
    System.get_env("MIX_ARCHIVES") || Path.join(Mix.Utils.mix_home(), "archives")
  end

  def path_for(:escripts) do
    Path.join(Mix.Utils.mix_home(), "escripts")
  end

  @doc """
  Ensures the given application from Erlang/OTP or Elixir and its dependencies
  are available in the path.

  Generally speaking, you should list the Erlang application dependencies under
  the `:extra_applications` section of your `mix.exs`. This must only be used by
  Mix tasks which wish to avoid depending on Erlang/Elixir for certain reasons.

  This function does not start the given applications.
  """
  @doc since: "1.15.0"
  def ensure_application!(app) when is_atom(app) do
    ensure_application!(app, Mix.State.builtin_apps(), [], %{})
    :ok
  end

  defp ensure_application!(app, builtin_apps, optional, seen) do
    case seen do
      %{^app => _} ->
        seen

      %{} ->
        seen = Map.put(seen, app, true)

        case builtin_apps do
          %{^app => path} ->
            Code.prepend_path(path, cache: true)
            Application.load(app)

            required = List.wrap(Application.spec(app, :applications))
            optional = List.wrap(Application.spec(app, :optional_applications))

            Enum.reduce(
              required ++ optional,
              seen,
              &ensure_application!(&1, builtin_apps, optional, &2)
            )

          %{} ->
            if app not in optional do
              Mix.raise(
                "The application \"#{app}\" could not be found. This may happen if your " <>
                  "Operating System broke Erlang into multiple packages and may be fixed " <>
                  "by installing the missing \"erlang-dev\" and \"erlang-#{app}\" packages"
              )
            end

            seen
        end
    end
  end

  @doc """
  Installs and starts dependencies.

  The given `deps` should be in the same format as defined in a regular Mix
  project. See `mix help deps` for more information. As a shortcut, an atom
  can be given as dependency to mean the latest version. In other words,
  specifying `:decimal` is the same as `{:decimal, ">= 0.0.0"}`.

  After each successful installation, a given set of dependencies is cached
  so starting another VM and calling `Mix.install/2` with the same dependencies
  will avoid unnecessary downloads and compilations. The location of the cache
  directory can be controlled using the `MIX_INSTALL_DIR` environment variable.

  This function can only be called outside of a Mix project and only with the
  same dependencies in the given VM.

  ## Options

    * `:force` - if `true`, runs with empty install cache. This is useful when you want
      to update your dependencies or your install got into an inconsistent state.
      To use this option, you can also set the `MIX_INSTALL_FORCE` environment variable.
      (Default: `false`)

    * `:verbose` - if `true`, prints additional debugging information
      (Default: `false`)

    * `:consolidate_protocols` - if `true`, runs protocol
      consolidation via the `mix compile.protocols` task (Default: `true`)

    * `:elixir` - if set, ensures the current Elixir version matches the given
      version requirement (Default: `nil`)

    * `:system_env` *(since v1.13.0)* - a list or a map of system environment variable
      names with respective values as binaries. The system environment is made part
      of the `Mix.install/2` cache, so different configurations will lead to different apps

    * `:config` *(since v1.13.0)* - a keyword list of keyword lists of compile-time
      configuration. The configuration is part of the `Mix.install/2` cache, so
      different configurations will lead to different apps. For this reason, you
      want to minimize the amount of configuration set through this option.
      Use `Application.put_all_env/2` for setting other runtime configuration.

    * `:config_path` *(since v1.14.0)* - path to a configuration file. If a `runtime.exs`
      file exists in the same directory as the given path, it is loaded too.

    * `:lockfile` *(since v1.14.0)* - path to a lockfile to be used as a basis of
      dependency resolution.

    * `:start_applications` *(since v1.15.3)* - if `true`, ensures that installed app
      and its dependencies are started after install (Default: `true`)

  ## Examples

  Installing `:decimal` and `:jason`:

      Mix.install([
        :decimal,
        {:jason, "~> 1.0"}
      ])

  Installing `:nx` and `:exla`, and configuring the underlying applications
  and environment variables:

      Mix.install(
        [:nx, :exla],
        config: [
          nx: [default_backend: EXLA]
        ],
        system_env: [
          XLA_TARGET: "cuda111"
        ]
      )

  Installing a Mix project as a path dependency along with its configuration
  and deps:

      # $ git clone https://github.com/hexpm/hexpm /tmp/hexpm
      # $ cd /tmp/hexpm && mix setup

      Mix.install(
        [
          {:hexpm, path: "/tmp/hexpm", env: :dev},
        ],
        config_path: "/tmp/hexpm/config/config.exs",
        lockfile: "/tmp/hexpm/mix.lock"
      )

      Hexpm.Repo.query!("SELECT COUNT(1) from packages")
      #=> ...

  The example above can be simplified by passing the application
  name as an atom for `:config_path` and `:lockfile`:

      Mix.install(
        [
          {:hexpm, path: "/tmp/hexpm", env: :dev},
        ],
        config_path: :hexpm,
        lockfile: :hexpm
      )

  ## Limitations

  There is one limitation to `Mix.install/2`, which is actually an Elixir
  behavior. If you are installing a dependency that defines a struct or
  macro, you cannot use the struct or macro immediately after the install
  call. For example, this won't work:

      Mix.install([:decimal])
      %Decimal{} = Decimal.new(42)

  That's because Elixir first expands all structs and all macros, and then
  it executes the code. This means that, by the time Elixir tries to expand
  the `%Decimal{}` struct, the dependency has not been installed yet.

  Luckily this has a straightforward solution, which is to move the code
  inside a module:

      Mix.install([:decimal])

      defmodule Script do
        def run do
          %Decimal{} = Decimal.new(42)
        end
      end

      Script.run()

  The contents inside `defmodule` will only be expanded and executed
  after `Mix.install/2` runs, which means that any struct, macros,
  and imports will be correctly handled.

  ## Environment variables

  The `MIX_INSTALL_DIR` environment variable configures the directory that
  caches all `Mix.install/2`. It defaults to the "mix/install" folder in the
  default user cache of your operating system. You can use `install_project_dir/0`
  to access the directory of an existing install (alongside other installs):

      iex> Mix.install([])
      iex> Mix.install_project_dir()

  The `MIX_INSTALL_FORCE` is available since Elixir v1.13.0 and forces
  `Mix.install/2` to discard any previously cached entry of the current install.

  The `MIX_INSTALL_RESTORE_PROJECT_DIR` environment variable may be specified
  since Elixir v1.16.2. It should point to a previous installation directory,
  which can be obtained with `Mix.install_project_dir/0` (after calling `Mix.install/2`).
  Using a restore dir may speed up the installation, since matching dependencies
  do not need be refetched nor recompiled. This environment variable is ignored
  if `:force` is enabled.
  """
  @doc since: "1.12.0"
  def install(deps, opts \\ [])

  def install(deps, opts) when is_list(deps) and is_list(opts) do
    Mix.start()

    if Mix.Project.get() do
      Mix.raise("Mix.install/2 cannot be used inside a Mix project")
    end

    elixir_requirement = opts[:elixir]
    elixir_version = System.version()

    if !!elixir_requirement and not Version.match?(elixir_version, elixir_requirement) do
      Mix.raise(
        "Mix.install/2 declared it supports only Elixir #{elixir_requirement} " <>
          "but you're running on Elixir #{elixir_version}"
      )
    end

    deps =
      Enum.map(deps, fn
        dep when is_atom(dep) ->
          {dep, ">= 0.0.0"}

        {app, opts} when is_atom(app) and is_list(opts) ->
          {app, maybe_expand_path_dep(opts)}

        {app, requirement, opts} when is_atom(app) and is_binary(requirement) and is_list(opts) ->
          {app, requirement, maybe_expand_path_dep(opts)}

        other ->
          other
      end)

    opts =
      Keyword.validate!(opts,
        config: [],
        config_path: nil,
        consolidate_protocols: true,
        elixir: nil,
        force: false,
        lockfile: nil,
        runtime_config: [],
        start_applications: true,
        system_env: [],
        verbose: false
      )

    config_path = expand_path(opts[:config_path], deps, :config_path, "config/config.exs")
    config = Keyword.fetch!(opts, :config)
    system_env = Keyword.fetch!(opts, :system_env)
    consolidate_protocols? = Keyword.fetch!(opts, :consolidate_protocols)
    start_applications? = Keyword.fetch!(opts, :start_applications)

    id =
      {deps, config, system_env, consolidate_protocols?}
      |> :erlang.term_to_binary()
      |> :erlang.md5()
      |> Base.encode16(case: :lower)

    force? = System.get_env("MIX_INSTALL_FORCE") in ["1", "true"] or Keyword.fetch!(opts, :force)

    case Mix.State.get(:installed) do
      nil ->
        Application.put_all_env(config, persistent: true)
        System.put_env(system_env)

        install_project_dir = install_project_dir(id)

        if Keyword.fetch!(opts, :verbose) do
          Mix.shell().info("Mix.install/2 using #{install_project_dir}")
        end

        if force? do
          File.rm_rf!(install_project_dir)
        end

        dynamic_config = [
          deps: deps,
          consolidate_protocols: consolidate_protocols?,
          config_path: config_path
        ]

        config = install_project_config(dynamic_config)

        started_apps = Application.started_applications()
        :ok = Mix.ProjectStack.push(@mix_install_project, config, "nofile")
        build_dir = Path.join(install_project_dir, "_build")
        external_lockfile = expand_path(opts[:lockfile], deps, :lockfile, "mix.lock")

        try do
          first_build? = not File.dir?(build_dir)

          restore_dir = System.get_env("MIX_INSTALL_RESTORE_PROJECT_DIR")

          if first_build? and restore_dir != nil and not force? do
            File.cp_r(restore_dir, install_project_dir)
            remove_dep(install_project_dir, @mix_install_app_string)
          end

          File.mkdir_p!(install_project_dir)

          File.cd!(install_project_dir, fn ->
            if config_path do
              Mix.Task.rerun("loadconfig")
            end

            cond do
              external_lockfile ->
                md5_path = Path.join(install_project_dir, "merge.lock.md5")

                old_md5 =
                  case File.read(md5_path) do
                    {:ok, data} -> Base.decode64!(data)
                    _ -> nil
                  end

                new_md5 = external_lockfile |> File.read!() |> :erlang.md5()

                if old_md5 != new_md5 do
                  lockfile = Path.join(install_project_dir, "mix.lock")
                  old_lock = Mix.Dep.Lock.read(lockfile)
                  new_lock = Mix.Dep.Lock.read(external_lockfile)
                  Mix.Dep.Lock.write(Map.merge(old_lock, new_lock), file: lockfile)
                  File.write!(md5_path, Base.encode64(new_md5))
                  Mix.Task.rerun("deps.get")
                end

              first_build? ->
                Mix.Task.rerun("deps.get")

              true ->
                # We already have a cache. If the user by any chance uninstalled Hex,
                # we make sure it is installed back (which mix deps.get would do anyway)
                Mix.Hex.ensure_installed?(true)
                :ok
            end

            Mix.Task.rerun("deps.loadpaths")

            # Hex and SSL can use a good amount of memory after the registry fetching,
            # so we stop any app started during deps resolution.
            stop_apps(Application.started_applications() -- started_apps)

            Mix.Task.rerun("compile")

            if config_path do
              Mix.Task.rerun("app.config")
            end
          end)

          if start_applications? do
            for %{app: app, opts: opts} <- Mix.Dep.cached(),
                Keyword.get(opts, :runtime, true) and Keyword.get(opts, :app, true) do
              Application.ensure_all_started(app)
            end
          end

          if restore_dir do
            remove_leftover_deps(install_project_dir)
          end

          Mix.State.put(:installed, {id, dynamic_config})
          :ok
        after
          Mix.ProjectStack.pop()
          # Clear all tasks invoked during installation, since there
          # is no reason to keep this in memory. Additionally this
          # allows us to rerun tasks for the dependencies later on,
          # such as recompilation
          Mix.Task.clear()
        end

      {^id, _dynamic_config} when not force? ->
        :ok

      _ ->
        Mix.raise("Mix.install/2 can only be called with the same dependencies in the given VM")
    end
  end

  defp expand_path(_path = nil, _deps, _key, _), do: nil
  defp expand_path(path, _deps, _key, _) when is_binary(path), do: Path.expand(path)

  defp expand_path(app_name, deps, key, relative_path) when is_atom(app_name) do
    app_dir =
      case List.keyfind(deps, app_name, 0) do
        {_, _, opts} when is_list(opts) -> opts[:path]
        {_, opts} when is_list(opts) -> opts[:path]
        _ -> Mix.raise("unknown dependency #{inspect(app_name)} given to #{inspect(key)}")
      end

    if !app_dir do
      Mix.raise("#{inspect(app_name)} given to #{inspect(key)} must be a path dependency")
    end

    Path.join(app_dir, relative_path)
  end

  defp remove_leftover_deps(install_project_dir) do
    build_lib_dir = Path.join([install_project_dir, "_build", "dev", "lib"])

    deps = File.ls!(build_lib_dir)

    loaded_deps =
      for {app, _description, _version} <- Application.loaded_applications(),
          into: MapSet.new(),
          do: Atom.to_string(app)

    # We want to keep :mix_install, but it has no application
    loaded_deps = MapSet.put(loaded_deps, @mix_install_app_string)

    for dep <- deps, not MapSet.member?(loaded_deps, dep) do
      remove_dep(install_project_dir, dep)
    end
  end

  defp remove_dep(install_project_dir, dep) do
    build_lib_dir = Path.join([install_project_dir, "_build", "dev", "lib"])
    deps_dir = Path.join(install_project_dir, "deps")

    build_path = Path.join(build_lib_dir, dep)
    File.rm_rf(build_path)
    dep_path = Path.join(deps_dir, dep)
    File.rm_rf(dep_path)
  end

  defp install_project_dir(cache_id) do
    install_root =
      System.get_env("MIX_INSTALL_DIR") ||
        Path.join(Mix.Utils.mix_cache(), "installs")

    version = "elixir-#{System.version()}-erts-#{:erlang.system_info(:version)}"
    Path.join([install_root, version, cache_id])
  end

  defp install_project_config(dynamic_config) do
    [
      version: "0.1.0",
      build_embedded: false,
      build_per_environment: true,
      build_path: "_build",
      lockfile: "mix.lock",
      deps_path: "deps",
      app: @mix_install_app,
      erlc_paths: [],
      elixirc_paths: [],
      compilers: [],
      prune_code_paths: false
    ] ++ dynamic_config
  end

  @doc false
  def in_install_project(fun) do
    case safe_get_installed() do
      {id, dynamic_config} ->
        config = install_project_config(dynamic_config)

        install_project_dir = install_project_dir(id)

        File.cd!(install_project_dir, fn ->
          :ok = Mix.ProjectStack.push(@mix_install_project, config, "nofile")

          try do
            fun.()
          after
            Mix.ProjectStack.pop()
          end
        end)

      nil ->
        Mix.raise("trying to call Mix.in_install_project/1, but Mix.install/2 was never called")
    end
  end

  @doc """
  Returns the directory where the current `Mix.install/2` project
  resides.
  """
  @doc since: "1.16.2"
  @spec install_project_dir() :: Path.t() | nil
  def install_project_dir() do
    case safe_get_installed() do
      {id, _dynamic_config} -> install_project_dir(id)
      nil -> nil
    end
  end

  @doc """
  Returns whether `Mix.install/2` was called in the current node.
  """
  @doc since: "1.13.0"
  @spec installed?() :: boolean()
  def installed? do
    safe_get_installed() != nil
  end

  defp safe_get_installed() do
    if mix_started?() do
      Mix.State.get(:installed)
    end
  end

  defp mix_started?() do
    Process.whereis(Mix.State) != nil
  end

  defp stop_apps([]), do: :ok

  defp stop_apps(apps) do
    :logger.add_primary_filter(:silence_app_exit, {&silence_app_exit/2, []})
    Enum.each(apps, fn {app, _, _} -> Application.stop(app) end)
    :logger.remove_primary_filter(:silence_app_exit)
    :ok
  end

  defp silence_app_exit(
         %{
           msg:
             {:report,
              %{
                label: {:application_controller, :exit},
                report: [application: _, exited: :stopped] ++ _
              }}
         },
         _extra
       ) do
    :stop
  end

  defp silence_app_exit(_message, _extra) do
    :ignore
  end

  defp maybe_expand_path_dep(opts) do
    if Keyword.has_key?(opts, :path) do
      Keyword.update!(opts, :path, &Path.expand/1)
    else
      opts
    end
  end

  @doc false
  def install?, do: Mix.Project.get() == @mix_install_project
end
