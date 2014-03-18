defmodule Mix.Tasks.New do
  use Mix.Task

  import Mix.Generator
  import Mix.Utils, only: [camelize: 1, underscore: 1]

  @shortdoc "Create a new Elixir project"

  @moduledoc """
  Creates a new Elixir project.
  It expects the path of the project as argument.

      mix new PATH [--bare] [--module MODULE] [--umbrella]

  A project at the given PATH  will be created. The
  application name and module name will be retrieved
  from the path, unless `--module` is given.

  A `--bare` option can be given to not generate an OTP
  application skeleton. Normally an app is generated with
  a supervisor and an application module that starts the
  supervisor.

  An `--umbrella` option can be given to generate an
  umbrella project.

  ## Examples

      mix new hello_world

  Is equivalent to:

      mix new hello_world --module HelloWorld

  To generate an app without supervisor and application behaviours:

      mix new hello_world --bare

  """
  def run(argv) do
    { opts, argv, _ } = OptionParser.parse(argv, switches: [bare: :boolean, umbrella: :boolean])

    case argv do
      [] ->
        raise Mix.Error, message: "Expected PATH to be given, please use `mix new PATH`"
      [path|_] ->
        name = Path.basename(Path.expand(path))
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
    assigns = [app: app, mod: mod, otp_app: otp_app(mod, !!opts[:bare])]

    create_file "README.md",  readme_template(assigns)
    create_file ".gitignore", gitignore_text

    if in_umbrella? do
      create_file "mix.exs", mixfile_apps_template(assigns)
    else
      create_file "mix.exs", mixfile_template(assigns)
    end

    create_directory "lib"

    if opts[:bare] do
      create_file "lib/#{app}.ex", lib_template(assigns)
    else
      create_file "lib/#{app}.ex", lib_app_template(assigns)
      create_directory "lib/#{app}"
      create_file "lib/#{app}/supervisor.ex", lib_supervisor_template(assigns)
    end

    create_directory "test"
    create_file "test/test_helper.exs", test_helper_template(assigns)
    create_file "test/#{app}_test.exs", test_lib_template(assigns)

    Mix.shell.info """

    Your mix project was created successfully.
    You can use mix to compile it, test it, and more:

        cd #{path}
        mix test

    Run `mix help` for more commands.
    """
  end

  defp otp_app(_mod, true) do
    "    [ applications: [] ]"
  end

  defp otp_app(mod, false) do
    "    [ applications: [],\n      mod: { #{mod}, [] } ]"
  end

  defp do_generate_umbrella(app, path, _opts) do
    mod = camelize(app)
    assigns = [mod: mod]

    create_file "README.md", readme_template(assigns)
    create_file "mix.exs",   mixfile_umbrella_template(assigns)

    create_directory "apps"

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
      raise Mix.Error, message: "Project path must start with a letter and have only lowercase letters, numbers and underscore"
    end
  end

  defp in_umbrella? do
    apps = Path.dirname(File.cwd!)

    try do
      Mix.Project.in_project(:umbrella_check, "../..", fn _ ->
        path = Mix.project[:apps_path]
        path && Path.expand(path) == apps
      end)
    catch
      _, _ -> false
    end
  end

   embed_template :readme, """
   # <%= @mod %>

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

    # List all dependencies in the format:
    #
    # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
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

    # List all dependencies in the format:
    #
    # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
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

    # List all dependencies in the format:
    #
    # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
    #
    # Type `mix help deps` for more examples and options
    defp deps do
      []
    end
  end
  """

  embed_template :lib, """
  defmodule <%= @mod %> do
  end
  """

  embed_template :lib_app, """
  defmodule <%= @mod %> do
    use Application.Behaviour

    # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
    # for more information on OTP Applications
    def start(_type, _args) do
      <%= @mod %>.Supervisor.start_link
    end
  end
  """

  embed_template :lib_supervisor, """
  defmodule <%= @mod %>.Supervisor do
    use Supervisor.Behaviour

    def start_link do
      :supervisor.start_link(__MODULE__, [])
    end

    def init([]) do
      children = [
        # Define workers and child supervisors to be supervised
        # worker(<%= @mod %>.Worker, [arg1, arg2, arg3])
      ]

      # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
      # for other strategies and supported options
      supervise(children, strategy: :one_for_one)
    end
  end
  """

  embed_template :test_lib, """
  defmodule <%= @mod %>Test do
    use ExUnit.Case

    test "the truth" do
      assert 1 + 1 == 2
    end
  end
  """

  embed_template :test_helper, """
  ExUnit.start
  """
end
