defmodule Mix.Tasks.New do
  use Mix.Task

  import Mix.Generator, only: [ create_directory: 1, create_file: 2]
  
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

  """

  @switches [
    app:      :string,
    module:   :string,
    sup:      :boolean,
    template: :string,
    umbrella: :boolean,
  ]

  @spec run(OptionParser.argv) :: :ok
  def run(argv) do
    {opts, argv} = OptionParser.parse!(argv, strict: @switches)

    case argv do
      [] ->
        Mix.raise "Expected PATH to be given, please use \"mix new PATH\""
      [path | _] ->
        app = opts[:app] || Path.basename(Path.expand(path))
        check_application_name!(app, !opts[:app])
        mod = opts[:module] || Macro.camelize(app)
        check_mod_name_validity!(mod)
        check_mod_name_availability!(mod)
        template = find_template(opts[:template])

        unless path == "." do
          check_directory_existence!(path)
          File.mkdir_p!(path)
        end

        File.cd! path, fn ->
          if opts[:umbrella] do
            generate_umbrella(app, mod, path, template, opts)
          else
            generate(app, mod, path, template, opts)
          end
        end
    end
  end

  defp generate(app, mod, path, template, opts) do
    assigns = [app: app, mod: mod, otp_app: otp_app(mod, !!opts[:sup]),
               version: get_version(System.version)]

    create_file "README.md",  template.readme(assigns)
    create_file ".gitignore", template.gitignore()

    if in_umbrella?() do
      create_file "mix.exs",  template.mixfile_apps(assigns)
    else
      create_file "mix.exs",  template.mixfile(assigns)
    end

    create_directory "config"
    create_file "config/config.exs", template.config(assigns)

    create_directory "lib"
    create_file "lib/#{app}.ex", template.lib(assigns)

    if opts[:sup] do
      create_file "lib/#{app}/application.ex", template.lib_app(assigns)
    end

    create_directory "test"
    create_file "test/test_helper.exs", template.test_helper(assigns)
    create_file "test/#{app}_test.exs", template.test(assigns)

    """

    Your Mix project was created successfully.
    You can use "mix" to compile it, test it, and more:

        #{cd_path(path)}mix test

    Run "mix help" for more commands.
    """
    |> String.trim_trailing
    |> Mix.shell.info
  end

  defp otp_app(_mod, false) do
    "    [extra_applications: [:logger]]"
  end

  defp otp_app(mod, true) do
    "    [extra_applications: [:logger],\n     mod: {#{mod}.Application, []}]"
  end

  defp cd_path(".") do
    ""
  end

  defp cd_path(path) do
    "cd #{path}\n    "
  end

  defp generate_umbrella(_app, mod, path, template, _opts) do
    assigns = [app: nil, mod: mod]

    create_file ".gitignore", template.gitignore()
    create_file "README.md", template.readme(assigns)
    create_file "mix.exs", template.mixfile_umbrella(assigns)

    create_directory "apps"

    create_directory "config"
    create_file "config/config.exs", template.config_umbrella(assigns)

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
    |> String.trim_trailing
    |> Mix.shell.info
  end

  defp check_application_name!(name, inferred?) do
    unless name =~ ~r/^[a-z][a-z0-9_]*$/ do
      Mix.raise "Application name must start with a letter and have only lowercase " <>
                "letters, numbers and underscore, got: #{inspect name}" <>
                (if inferred? do
                  ". The application name is inferred from the path, if you'd like to " <>
                  "explicitly name the application then use the \"--app APP\" option"
                else
                  ""
                end)
    end
  end

  defp check_mod_name_validity!(name) do
    unless name =~ ~r/^[A-Z]\w*(\.[A-Z]\w*)*$/ do
      Mix.raise "Module name must be a valid Elixir alias (for example: Foo.Bar), got: #{inspect name}"
    end
  end

  defp check_mod_name_availability!(name) do
    name = Module.concat(Elixir, name)
    if Code.ensure_loaded?(name) do
      Mix.raise "Module name #{inspect name} is already taken, please choose another name"
    end
  end

  defp check_directory_existence!(path) do
    if File.dir?(path) and not Mix.shell.yes?("The directory #{inspect(path)} already exists. Are you sure you want to continue?") do
      Mix.raise "Please select another directory for installation"
    end
  end

  defp get_version(version) do
    {:ok, version} = Version.parse(version)
    "#{version.major}.#{version.minor}" <>
      case version.pre do
        [h | _] -> "-#{h}"
        []      -> ""
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

  defp find_template(nil),        do: Mix.Templates.Standard
  defp find_template("standard"), do: Mix.Templates.Standard
  defp find_template(name) do
    check_mod_name_validity!(name)
    [ Elixir, name ]
    |> Module.concat
    |> Code.ensure_loaded
    |> case do
         { :error, reason } ->
           Mix.raise "Template #{inspect name} isn't available. (#{reason})"
         { :module, module } ->
           module
       end
  end
  
end
