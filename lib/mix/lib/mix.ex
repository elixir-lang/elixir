defmodule Mix do
  @moduledoc ~S"""
  Mix is a build tool that provides tasks for creating, compiling,
  and testing Elixir projects, managing its dependencies, and more.

  ## Mix.Project

  The foundation of Mix is a project. A project can be defined by using
  `Mix.Project` in a module, usually placed in a file named `mix.exs`:

      defmodule MyApp.Mixfile do
        use Mix.Project

        def project do
          [app: :my_app,
           version: "1.0.0"]
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
          Mix.shell.info "hello"
        end
      end

  The task can now be invoked with `mix hello`.

  ## Dependencies

  Mix also manages your dependencies and integrates nicely with the [Hex package
  manager](https://hex.pm).

  In order to use dependencies, you need to add a `:deps` key
  to your project configuration. We often extract the list of dependencies
  into its own function:

      defmodule MyApp.Mixfile do
        use Mix.Project

        def project do
          [app: :my_app,
           version: "1.0.0",
           deps: deps()]
        end

        defp deps do
          [{:ecto, "~> 0.2.5"},
           {:plug, github: "elixir-lang/plug"}]
        end
      end

  You can run `mix help deps` to learn more about dependencies in Mix.

  ## Environments

  Mix supports different environments. Environments allow developers to prepare
  and organize their project specifically for different scenarios. By default,
  Mix provides three environments:

    * `:dev` - the default environment
    * `:test` - the environment `mix test` runs on
    * `:prod` - the environment your dependencies run on

  The environment can be changed via the command line by setting
  the `MIX_ENV` environment variable, for example:

      $ MIX_ENV=prod mix run server.exs

  ## Aliases

  Aliases are shortcuts or tasks specific to the current project.

  In the `Mix.Task` section, we have defined a task that would be
  available to everyone using our project as a dependency. What if
  we wanted the task to only be available for our project? Just
  define an alias:

      defmodule MyApp.Mixfile do
        use Mix.Project

        def project do
          [app: :my_app,
           version: "1.0.0",
           aliases: aliases()]
        end

        defp aliases do
          [c: "compile",
           hello: &hello/1]
        end

        defp hello(_) do
          Mix.shell.info "Hello world"
        end
      end

  In the example above, we have defined two aliases. One is `mix c`
  which is a shortcut for `mix compile`. The other is named
  `mix hello`, which is the equivalent to the `Mix.Tasks.Hello`
  we have defined in the `Mix.Task` section.

  Aliases may also be lists, specifying multiple tasks to be run
  consecutively:

      [all: [&hello/1, "deps.get --only #{Mix.env}", "compile"]]

  In the example above, we have defined an alias named `mix all`,
  that prints hello, then fetches dependencies specific to the
  current environment and compiles it.

  Arguments given to the alias will be appended to the arguments
  of the last task in the list, if the last task is a function
  they will be given as a list of strings to the function.

  Finally, aliases can also be used to augment existing tasks.
  Let's suppose you want to augment `mix clean` to clean another
  directory Mix does not know about:

      [clean: ["clean", &clean_extra/1]]

  Where `&clean_extra/1` would be a function in your `mix.exs`
  with extra clean up logic.

  Note aliases do not show up on `mix help`.
  Aliases defined in the current project do not affect its dependencies and aliases defined in dependencies are not accessible from the current project.

  ## Environment variables

  Several environment variables can be used to modify Mix's behaviour.

  Mix responds to the following variables:

    * `MIX_ARCHIVES` - specifies the directory into which the archives should be installed
    * `MIX_DEBUG` - outputs debug information about each task before running it
    * `MIX_ENV` - specifies which environment should be used. See [Environments](#module-environments)
    * `MIX_EXS` - changes the full path to the `mix.exs` file
    * `MIX_HOME` - path to mix's home directory, stores configuration files and scripts used by mix
    * `MIX_PATH` - appends extra code paths
    * `MIX_QUIET` - does not print information messages to the terminal
    * `MIX_REBAR` - path to rebar command that overrides the one mix installs
    * `MIX_REBAR3` - path to rebar3 command that overrides the one mix installs

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
  Returns the Mix environment.
  """
  def env do
    # env is not available on bootstrapping, so set a :dev default
    Mix.State.get(:env, :dev)
  end

  @doc """
  Changes the current Mix environment to `env`.

  Be careful when invoking this function as any project
  configuration won't be reloaded.
  """
  def env(env) when is_atom(env) do
    Mix.State.put(:env, env)
  end

  @doc """
  Returns the default compilers used by Mix.

  It can be used in your `mix.exs` to prepend or
  append new compilers to Mix:

      def project do
        [compilers: Mix.compilers ++ [:foo, :bar]]
      end

  """
  def compilers do
    [:yecc, :leex, :erlang, :elixir, :xref, :app]
  end

  @doc """
  Returns the current shell.

  `shell/0` can be used as a wrapper for the current shell. It contains
  conveniences for requesting information from the user, printing to the shell and so
  forth. The Mix shell is swappable (see `shell/1`), allowing developers to use
  a test shell that simply sends messages to the current process instead of
  performing IO (see `Mix.Shell.Process`).

  By default, this returns `Mix.Shell.IO`.
  """
  def shell do
    Mix.State.get(:shell, Mix.Shell.IO)
  end

  @doc """
  Sets the current shell.

  After calling this function, `shell` becomes the shell that is returned by
  `shell/0`.
  """
  def shell(shell) do
    Mix.State.put(:shell, shell)
  end

  @doc """
  Returns true if Mix is in debug mode.
  """
  def debug? do
    Mix.State.get(:debug, false)
  end

  @doc """
  Sets Mix debug mode.
  """
  def debug(debug) when is_boolean(debug) do
    Mix.State.put(:debug, debug)
  end

  @doc """
  Raises a Mix error that is nicely formatted.
  """
  @spec raise(binary) :: no_return
  def raise(message) when is_binary(message) do
    Kernel.raise Mix.Error, mix: true, message: message
  end
end
