defmodule Mix.Tasks.New do
  use Mix.Task

  import Mix.Generator
  import Mix.Utils, only: [camelize: 1, underscore: 1]

  @shortdoc "Create a new Elixir project"

  @moduledoc """
  Creates a new Elixir project.
  It expects the path of the project as argument.

      mix new PATH [--sup] [--module MODULE] [--app APP] [--umbrella]

  A project at the given PATH  will be created. The
  application name and module name will be retrieved
  from the path, unless `--module` or `--app` is given.

  A `--sup` option can be given to generate an OTP application
  skeleton including a supervision tree. Normally an app is
  generated without a supervisor and without the app callback.

  An `--umbrella` option can be given to generate an
  umbrella project.

  ## Examples

      mix new hello_world

  Is equivalent to:

      mix new hello_world --module HelloWorld

  To generate an app with supervisor and application callback:

      mix new hello_world --sup

  """
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: [sup: :boolean, umbrella: :boolean])

    case argv do
      [] ->
        Mix.raise "Expected PATH to be given, please use `mix new PATH`"
      [path|_] ->
        name = opts[:app] || Path.basename(Path.expand(path))
        check_project_name!(name)
        File.mkdir_p!(path)

        File.cd! path, fn ->
          if opts[:umbrella] do
            do_generate_umbrella(name, path, opts)
          else
            do_generate(name, path, opts)
          end
        end
    end
  end

  defp do_generate(app, path, opts) do
    mod     = opts[:module] || camelize(app)
    assigns = [app: app, mod: mod, otp_app: otp_app(mod, !!opts[:sup])]

    create_file "README.md",  readme_template(assigns)
    create_file ".gitignore", gitignore_text

    if in_umbrella? do
      create_file "mix.exs", mixfile_apps_template(assigns)
    else
      create_file "mix.exs", mixfile_template(assigns)
    end

    create_directory "config"
    create_file "config/config.exs", config_template(assigns)

    create_directory "lib"

    if opts[:sup] do
      create_file "lib/#{app}.ex", lib_sup_template(assigns)
    else
      create_file "lib/#{app}.ex", lib_template(assigns)
    end

    create_directory "test"
    create_file "test/test_helper.exs", test_helper_template(assigns)
    create_file "test/#{app}_test.exs", test_template(assigns)

    Mix.shell.info """

    Your mix project was created successfully.
    You can use mix to compile it, test it, and more:

        cd #{path}
        mix test

    Run `mix help` for more commands.
    """
  end

  defp otp_app(_mod, false) do
    "    [applications: [:logger]]"
  end

  defp otp_app(mod, true) do
    "    [applications: [:logger],\n     mod: {#{mod}, []}]"
  end

  defp do_generate_umbrella(app, path, _opts) do
    mod = camelize(app)
    assigns = [mod: mod]

    create_file ".gitignore", gitignore_text
    create_file "README.md", readme_template(assigns)
    create_file "mix.exs", mixfile_umbrella_template(assigns)

    create_directory "apps"

    create_directory "config"
    create_file "config/config.exs",
      config_template(assigns) <> config_umbrella_template(assigns)

    Mix.shell.info """

    Your umbrella project was created successfully.
    Inside your project, you will find an apps/ directory
    where you can create and host many apps:

        cd #{path}
        cd apps
        mix new my_app

    Commands like `mix compile` and `mix test` when executed
    in the umbrella project root will automatically run
    for each application in the apps/ directory.
    """
  end

  defp check_project_name!(name) do
    unless name =~ ~r/^[a-z][\w_]*$/ do
      Mix.raise "Project path must start with a letter and have only lowercase letters, numbers and underscore"
    end
  end

  defp in_umbrella? do
    apps = Path.dirname(File.cwd!)

    try do
      Mix.Project.in_project(:umbrella_check, "../..", fn _ ->
        path = Mix.Project.config[:apps_path]
        path && Path.expand(path) == apps
      end)
    catch
      _, _ -> false
    end
  end

   embed_template :readme, """
   <%= @mod %>
   <%= String.duplicate("=", String.length(@mod)) %>

   ** TODO: Add description **
   """

   embed_text :gitignore, """
   /_build
   /deps
   erl_crash.dump
   *.ez
   """

  embed_template :mixfile, """
  defmodule <%= @mod %>.Mixfile do
    use Mix.Project

    def project do
      [app: :<%= @app %>,
       version: "0.0.1",
       elixir: "~> <%= System.version %>",
       deps: deps]
    end

    # Configuration for the OTP application
    #
    # Type `mix help compile.app` for more information
    def application do
  <%= @otp_app %>
    end

    # Dependencies can be hex.pm packages:
    #
    #   {:mydep, "~> 0.3.0"}
    #
    # Or git/path repositories:
    #
    #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
    #
    # Type `mix help deps` for more examples and options
    defp deps do
      []
    end
  end
  """

  embed_template :mixfile_apps, """
  defmodule <%= @mod %>.Mixfile do
    use Mix.Project

    def project do
      [app: :<%= @app %>,
       version: "0.0.1",
       deps_path: "../../deps",
       lockfile: "../../mix.lock",
       elixir: "~> <%= System.version %>",
       deps: deps]
    end

    # Configuration for the OTP application
    #
    # Type `mix help compile.app` for more information
    def application do
  <%= @otp_app %>
    end

    # Dependencies can be hex.pm packages:
    #
    #   {:mydep, "~> 0.3.0"}
    #
    # Or git/path repositories:
    #
    #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
    #
    # To depend on another app inside the umbrella:
    #
    #   {:myapp, in_umbrella: true}
    #
    # Type `mix help deps` for more examples and options
    defp deps do
      []
    end
  end
  """

  embed_template :mixfile_umbrella, """
  defmodule <%= @mod %>.Mixfile do
    use Mix.Project

    def project do
      [apps_path: "apps",
       deps: deps]
    end

    # Dependencies can be hex.pm packages:
    #
    #   {:mydep, "~> 0.3.0"}
    #
    # Or git/path repositories:
    #
    #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
    #
    # Type `mix help deps` for more examples and options.
    #
    # Dependencies listed here are available only for this project
    # and cannot be accessed from applications inside the apps folder
    defp deps do
      []
    end
  end
  """

  embed_template :config, ~S"""
  # This file is responsible for configuring your application
  # and its dependencies with the aid of the Mix.Config module.
  use Mix.Config

  # This configuration is loaded before any dependency and is restricted
  # to this project. If another project depends on this project, this
  # file won't be loaded nor affect the parent project. For this reason,
  # if you want to provide default values for your application for third-
  # party users, it should be done in your mix.exs file.

  # Sample configuration:
  #
  #     config :logger,
  #       level: :info,
  #       format: "$time $metadata[$level] $message\n"

  # It is also possible to import configuration files, relative to this
  # directory. For example, you can emulate configuration per environment
  # by uncommenting the line below and defining dev.exs, test.exs and such.
  # Configuration from the imported file will override the ones defined
  # here (which is why it is important to import them last).
  #
  #     import_config "#{Mix.env}.exs"
  """

  embed_template :config_umbrella, ~S"""

  # Finally, note that configuration defined in children projects
  # inside apps/ are not automatically available to the umbrella parent.
  # They can, however, be easily imported:
  #
  #     import_config "../apps/foo/config/config.exs"
  #     import_config "../apps/bar/config/config.exs"
  """

  embed_template :lib, """
  defmodule <%= @mod %> do
  end
  """

  embed_template :lib_sup, """
  defmodule <%= @mod %> do
    use Application

    # See http://elixir-lang.org/docs/stable/elixir/Application.html
    # for more information on OTP Applications
    def start(_type, _args) do
      import Supervisor.Spec, warn: false

      children = [
        # Define workers and child supervisors to be supervised
        # worker(<%= @mod %>.Worker, [arg1, arg2, arg3])
      ]

      # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
      # for other strategies and supported options
      opts = [strategy: :one_for_one, name: <%= @mod %>.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end
  """

  embed_template :test, """
  defmodule <%= @mod %>Test do
    use ExUnit.Case

    test "the truth" do
      assert 1 + 1 == 2
    end
  end
  """

  embed_template :test_helper, """
  ExUnit.start()
  """
end
