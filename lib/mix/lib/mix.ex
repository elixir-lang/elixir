defmodule Mix do
  @moduledoc ~S"""
  Mix is a build tool that provides tasks for creating, compiling,
  and testing Elixir projects, managing its dependencies, and more.

  ## Mix.Project

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

  ## Mix.Task

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

      $ MIX_ENV=prod mix run server.exs

  You can also specify that certain dependencies are available only for
  certain environments:

      {:some_test_dependency, "~> 1.0", only: :test}

  The environment can be read via `Mix.env/0`.

  ## Targets

  Besides environments, Mix supports targets. Targets are useful when a
  project needs to compile to different architectures and some of the
  dependencies are only available to some of them. By default, the target
  is `:host` but it can be set via the `MIX_TARGET` environment variable.
  The target can be read via `Mix.target/0`.

  ## Aliases

  Aliases are shortcuts or tasks specific to the current project.

  In the "Mix.Task" section, we have defined a task that would be
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
            hello: &hello/1
          ]
        end

        defp hello(_) do
          Mix.shell().info("Hello world")
        end
      end

  In the example above, we have defined two aliases. One is `mix c`
  which is a shortcut for `mix compile`. The other is named
  `mix hello`, which is the equivalent to the `Mix.Tasks.Hello`
  we have defined in the "Mix.Task" section.

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

  Arguments given to the alias will be appended to the arguments of
  the last task in the list. Except when overriding an existing task.
  In this case, the arguments will be given to the original task,
  in order to preserve semantics. For example, in the `:clean` alias
  above, the arguments given to the alias will be passed to "clean"
  and not to `clean_extra/1`.

  Aliases defined in the current project do not affect its dependencies
  and aliases defined in dependencies are not accessible from the
  current project.

  Aliases can be used very powerfully to also run Elixir scripts and
  shell commands, for example:

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

  Mix tasks are designed to run only once. This prevents the same task
  to be executed multiple times. For example, if there are several tasks
  depending on `mix compile`, the code will be compiled once. Tasks can
  be executed again if they are explicitly reenabled using `Mix.Task.reenable/1`:

      another_alias: [
        "format --check-formatted priv/hello1.exs",
        "cmd priv/world.sh",
        fn _ -> Mix.Task.reenable("format") end,
        "format --check-formatted priv/hello2.exs"
      ]

  Some tasks are automatically reenabled though, as they are expected to
  be invoked multiple times. They are: `mix cmd`, `mix do`, `mix loadconfig`,
  `mix profile.cprof`, `mix profile.eprof`, `mix profile.fprof`, `mix run`,
  and `mix xref`.

  It is worth mentioning that some tasks, such as in the case of the
  `mix format` command in the example above, can accept multiple files so it
  could be rewritten as:

      another_alias: ["format --check-formatted priv/hello1.exs priv/hello2.exs"]

  ## Environment variables

  Several environment variables can be used to modify Mix's behaviour.

  Mix responds to the following variables:

    * `MIX_ARCHIVES` - specifies the directory into which the archives should be installed
      (default: `~/.mix/archives`)
    * `MIX_BUILD_PATH` - sets the project `Mix.Project.build_path/0` config. This option
      must always point to a subdirectory inside a temporary directory. For instance,
      never "/tmp" or "_build" but "_build/PROD" or "/tmp/PROD", as required by Mix
    * `MIX_DEPS_PATH` - sets the project `Mix.Project.deps_path/0` config (default: `deps`)
    * `MIX_DEBUG` - outputs debug information about each task before running it
    * `MIX_ENV` - specifies which environment should be used. See [Environments](#module-environments)
    * `MIX_TARGET` - specifies which target should be used. See [Targets](#module-targets)
    * `MIX_EXS` - changes the full path to the `mix.exs` file
    * `MIX_HOME` - path to Mix's home directory, stores configuration files and scripts used by Mix
      (default: `~/.mix`)
    * `MIX_PATH` - appends extra code paths
    * `MIX_QUIET` - does not print information messages to the terminal
    * `MIX_REBAR` - path to rebar command that overrides the one Mix installs
      (default: `~/.mix/rebar`)
    * `MIX_REBAR3` - path to rebar3 command that overrides the one Mix installs
      (default: `~/.mix/rebar3`)
    * `MIX_XDG` - asks Mix to follow the [XDG Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
      for its home directory and configuration files. This behaviour needs to
      be opt-in due to backwards compatibility. `MIX_HOME` has higher preference
      than `MIX_XDG`. If none of the variables are set, the default directory
      `~/.mix` will be used

  Environment variables that are not meant to hold a value (and act basically as
  flags) should be set to either `1` or `true`, for example:

      $ MIX_DEBUG=1 mix compile

  """

  use Application

  @doc false
  def start do
    {:ok, _} = Application.ensure_all_started(:mix)
    :ok
  end

  @doc false
  def start(_type, []) do
    children = [Mix.State, Mix.TasksServer, Mix.ProjectStack]
    opts = [strategy: :one_for_one, name: Mix.Supervisor, max_restarts: 0]
    Supervisor.start_link(children, opts)
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
    [:yecc, :leex, :erlang, :elixir, :app]
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
  Raises a Mix error that is nicely formatted.
  """
  @spec raise(binary) :: no_return
  def raise(message) when is_binary(message) do
    Kernel.raise(Mix.Error, mix: true, message: message)
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
end
