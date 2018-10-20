defmodule Mix.Tasks.New do
  use Mix.Task

  import Mix.Generator

  @shortdoc "Creates a new Elixir project"

  @moduledoc """
  Creates a new Elixir project.
  It expects the path of the project as argument.

      mix new PATH [--sup] [--module MODULE] [--app APP] [--umbrella]

  A project at the given PATH will be created. The
  application name and module name will be retrieved
  from the path, unless `--module` or `--app` is given.

  A `--sup` option can be given to generate an OTP application
  skeleton including a supervision tree. Normally an app is
  generated without a supervisor and without the app callback.

  An `--umbrella` option can be given to generate an
  umbrella project.

  An `--app` option can be given in order to
  name the OTP application for the project.

  A `--module` option can be given in order
  to name the modules in the generated code skeleton.

  ## Examples

      mix new hello_world

  Is equivalent to:

      mix new hello_world --module HelloWorld

  To generate an app with a supervision tree and an application callback:

      mix new hello_world --sup

  To generate an umbrella application with sub applications:

      mix new hello_world --umbrella
      cd hello_world/apps
      mix new child_app

  """

  @switches [
    app: :string,
    module: :string,
    sup: :boolean,
    umbrella: :boolean
  ]

  def run(argv) do
    {opts, argv} = OptionParser.parse!(argv, strict: @switches)

    case argv do
      [] ->
        Mix.raise("Expected PATH to be given, please use \"mix new PATH\"")

      [path | _] ->
        app = opts[:app] || Path.basename(Path.expand(path))
        check_application_name!(app, !opts[:app])
        mod = opts[:module] || Macro.camelize(app)
        check_mod_name_validity!(mod)
        check_mod_name_availability!(mod)

        unless path == "." do
          check_directory_existence!(path)
          File.mkdir_p!(path)
        end

        File.cd!(path, fn ->
          if opts[:umbrella] do
            generate_umbrella(app, mod, path, opts)
          else
            generate(app, mod, path, opts)
          end
        end)
    end
  end

  defp generate(app, mod, path, opts) do
    assigns = [
      app: app,
      mod: mod,
      sup_app: sup_app(mod, !!opts[:sup]),
      version: get_version(System.version())
    ]

    create_file("README.md", readme_template(assigns))
    create_file(".formatter.exs", formatter_template(assigns))
    create_file(".gitignore", gitignore_template(assigns))

    if in_umbrella?() do
      create_file("mix.exs", mix_exs_apps_template(assigns))
    else
      create_file("mix.exs", mix_exs_template(assigns))
    end

    create_directory("config")
    create_file("config/config.exs", config_template(assigns))

    create_directory("lib")
    create_file("lib/#{app}.ex", lib_template(assigns))

    if opts[:sup] do
      create_file("lib/#{app}/application.ex", lib_app_template(assigns))
    end

    create_directory("test")
    create_file("test/test_helper.exs", test_helper_template(assigns))
    create_file("test/#{app}_test.exs", test_template(assigns))

    """

    Your Mix project was created successfully.
    You can use "mix" to compile it, test it, and more:

        #{cd_path(path)}mix test

    Run "mix help" for more commands.
    """
    |> String.trim_trailing()
    |> Mix.shell().info()
  end

  defp sup_app(_mod, false), do: ""
  defp sup_app(mod, true), do: ",\n      mod: {#{mod}.Application, []}"

  defp cd_path("."), do: ""
  defp cd_path(path), do: "cd #{path}\n    "

  defp generate_umbrella(_app, mod, path, _opts) do
    assigns = [app: nil, mod: mod]

    create_file("README.md", readme_template(assigns))
    create_file(".formatter.exs", formatter_umbrella_template(assigns))
    create_file(".gitignore", gitignore_template(assigns))
    create_file("mix.exs", mix_exs_umbrella_template(assigns))

    create_directory("apps")

    create_directory("config")
    create_file("config/config.exs", config_umbrella_template(assigns))

    """

    Your umbrella project was created successfully.
    Inside your project, you will find an apps/ directory
    where you can create and host many apps:

        #{cd_path(path)}cd apps
        mix new my_app

    Commands like "mix compile" and "mix test" when executed
    in the umbrella project root will automatically run
    for each application in the apps/ directory.
    """
    |> String.trim_trailing()
    |> Mix.shell().info()
  end

  defp check_application_name!(name, inferred?) do
    unless name =~ Regex.recompile!(~r/^[a-z][a-z0-9_]*$/) do
      Mix.raise(
        "Application name must start with a lowercase ASCII letter, followed by " <>
          "lowercase ASCII letters, numbers, or underscores, got: #{inspect(name)}" <>
          if inferred? do
            ". The application name is inferred from the path, if you'd like to " <>
              "explicitly name the application then use the \"--app APP\" option"
          else
            ""
          end
      )
    end
  end

  defp check_mod_name_validity!(name) do
    unless name =~ Regex.recompile!(~r/^[A-Z]\w*(\.[A-Z]\w*)*$/) do
      Mix.raise(
        "Module name must be a valid Elixir alias (for example: Foo.Bar), got: #{inspect(name)}"
      )
    end
  end

  defp check_mod_name_availability!(name) do
    name = Module.concat(Elixir, name)

    if Code.ensure_loaded?(name) do
      Mix.raise("Module name #{inspect(name)} is already taken, please choose another name")
    end
  end

  defp check_directory_existence!(path) do
    msg = "The directory #{inspect(path)} already exists. Are you sure you want to continue?"

    if File.dir?(path) and not Mix.shell().yes?(msg) do
      Mix.raise("Please select another directory for installation")
    end
  end

  defp get_version(version) do
    {:ok, version} = Version.parse(version)

    "#{version.major}.#{version.minor}" <>
      case version.pre do
        [h | _] -> "-#{h}"
        [] -> ""
      end
  end

  defp in_umbrella? do
    apps = Path.dirname(File.cwd!())

    try do
      Mix.Project.in_project(:umbrella_check, "../..", fn _ ->
        path = Mix.Project.config()[:apps_path]
        path && Path.expand(path) == apps
      end)
    catch
      _, _ -> false
    end
  end

  embed_template(:readme, """
  # <%= @mod %>

  **TODO: Add description**
  <%= if @app do %>
  ## Installation

  If [available in Hex](https://hex.pm/docs/publish), the package can be installed
  by adding `<%= @app %>` to your list of dependencies in `mix.exs`:

  ```elixir
  def deps do
    [
      {:<%= @app %>, "~> 0.1.0"}
    ]
  end
  ```

  Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
  and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
  be found at [https://hexdocs.pm/<%= @app %>](https://hexdocs.pm/<%= @app %>).
  <% end %>
  """)

  embed_template(:formatter, """
  # Used by "mix format"
  [
    inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"]
  ]
  """)

  embed_template(:formatter_umbrella, """
  # Used by "mix format"
  [
    inputs: ["mix.exs", "config/*.exs"],
    subdirectories: ["apps/*"]
  ]
  """)

  embed_template(:gitignore, """
  # The directory Mix will write compiled artifacts to.
  /_build/

  # If you run "mix test --cover", coverage assets end up here.
  /cover/

  # The directory Mix downloads your dependencies sources to.
  /deps/

  # Where third-party dependencies like ExDoc output generated docs.
  /doc/

  # Ignore .fetch files in case you like to edit your project deps locally.
  /.fetch

  # If the VM crashes, it generates a dump, let's ignore it too.
  erl_crash.dump

  # Also ignore archive artifacts (built via "mix archive.build").
  *.ez
  <%= if @app do %>
  # Ignore package tarball (built via "mix hex.build").
  <%= @app %>-*.tar
  <% end %>
  """)

  embed_template(:mix_exs, """
  defmodule <%= @mod %>.MixProject do
    use Mix.Project

    def project do
      [
        app: :<%= @app %>,
        version: "0.1.0",
        elixir: "~> <%= @version %>",
        start_permanent: Mix.env() == :prod,
        deps: deps()
      ]
    end

    # Run "mix help compile.app" to learn about applications.
    def application do
      [
        extra_applications: [:logger]<%= @sup_app %>
      ]
    end

    # Run "mix help deps" to learn about dependencies.
    defp deps do
      [
        # {:dep_from_hexpm, "~> 0.3.0"},
        # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      ]
    end
  end
  """)

  embed_template(:mix_exs_apps, """
  defmodule <%= @mod %>.MixProject do
    use Mix.Project

    def project do
      [
        app: :<%= @app %>,
        version: "0.1.0",
        build_path: "../../_build",
        config_path: "../../config/config.exs",
        deps_path: "../../deps",
        lockfile: "../../mix.lock",
        elixir: "~> <%= @version %>",
        start_permanent: Mix.env() == :prod,
        deps: deps()
      ]
    end

    # Run "mix help compile.app" to learn about applications.
    def application do
      [
        extra_applications: [:logger]<%= @sup_app %>
      ]
    end

    # Run "mix help deps" to learn about dependencies.
    defp deps do
      [
        # {:dep_from_hexpm, "~> 0.3.0"},
        # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
        # {:sibling_app_in_umbrella, in_umbrella: true},
      ]
    end
  end
  """)

  embed_template(:mix_exs_umbrella, """
  defmodule <%= @mod %>.MixProject do
    use Mix.Project

    def project do
      [
        apps_path: "apps",
        start_permanent: Mix.env() == :prod,
        deps: deps()
      ]
    end

    # Dependencies listed here are available only for this
    # project and cannot be accessed from applications inside
    # the apps folder.
    #
    # Run "mix help deps" for examples and options.
    defp deps do
      []
    end
  end
  """)

  embed_template(:config, ~S"""
  # This file is responsible for configuring your application
  # and its dependencies with the aid of the Mix.Config module.
  use Mix.Config

  # This configuration is loaded before any dependency and is restricted
  # to this project. If another project depends on this project, this
  # file won't be loaded nor affect the parent project. For this reason,
  # if you want to provide default values for your application for
  # third-party users, it should be done in your "mix.exs" file.

  # You can configure your application as:
  #
  #     config :<%= @app %>, key: :value
  #
  # and access this configuration in your application as:
  #
  #     Application.get_env(:<%= @app %>, :key)
  #
  # You can also configure a third-party app:
  #
  #     config :logger, level: :info
  #

  # It is also possible to import configuration files, relative to this
  # directory. For example, you can emulate configuration per environment
  # by uncommenting the line below and defining dev.exs, test.exs and such.
  # Configuration from the imported file will override the ones defined
  # here (which is why it is important to import them last).
  #
  #     import_config "#{Mix.env()}.exs"
  """)

  embed_template(:config_umbrella, ~S"""
  # This file is responsible for configuring your application
  # and its dependencies with the aid of the Mix.Config module.
  use Mix.Config

  # By default, the umbrella project as well as each child
  # application will require this configuration file, as
  # configuration and dependencies are shared in an umbrella
  # project. While one could configure all applications here,
  # we prefer to keep the configuration of each individual
  # child application in their own app, but all other
  # dependencies, regardless if they belong to one or multiple
  # apps, should be configured in the umbrella to avoid confusion.
  import_config "../apps/*/config/config.exs"

  # Sample configuration (overrides the imported configuration above):
  #
  #     config :logger, :console,
  #       level: :info,
  #       format: "$date $time [$level] $metadata$message\n",
  #       metadata: [:user_id]
  """)

  embed_template(:lib, """
  defmodule <%= @mod %> do
    @moduledoc \"""
    Documentation for <%= @mod %>.
    \"""

    @doc \"""
    Hello world.

    ## Examples

        iex> <%= @mod %>.hello()
        :world

    \"""
    def hello do
      :world
    end
  end
  """)

  embed_template(:lib_app, """
  defmodule <%= @mod %>.Application do
    # See https://hexdocs.pm/elixir/Application.html
    # for more information on OTP Applications
    @moduledoc false

    use Application

    def start(_type, _args) do
      # List all child processes to be supervised
      children = [
        # Starts a worker by calling: <%= @mod %>.Worker.start_link(arg)
        # {<%= @mod %>.Worker, arg},
      ]

      # See https://hexdocs.pm/elixir/Supervisor.html
      # for other strategies and supported options
      opts = [strategy: :one_for_one, name: <%= @mod %>.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end
  """)

  embed_template(:test, """
  defmodule <%= @mod %>Test do
    use ExUnit.Case
    doctest <%= @mod %>

    test "greets the world" do
      assert <%= @mod %>.hello() == :world
    end
  end
  """)

  embed_template(:test_helper, """
  ExUnit.start()
  """)
end
