defmodule Mix.Tasks.App.Config do
  use Mix.Task

  @shortdoc "Configures all registered apps"

  @moduledoc """
  Loads and configures all registered apps.

  This is done by loading `config/runtime.exs` if one exists.
  The application will be compiled if it was not compiled before.

  ## Command line options

    * `--force` - forces compilation regardless of compilation times
    * `--preload-modules` - preloads all modules defined in applications
    * `--no-archives-check` - does not check archives
    * `--no-app-loading` - does not load .app resource file after compilation
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-validate-compile-env` - does not validate the application compile environment

  """

  @switches [preload_modules: :boolean]

  @impl true
  def run(args) do
    Mix.Project.get!()
    Mix.Task.run("compile", args)
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    config = Mix.Project.config()
    runtime = config[:config_path] |> Path.dirname() |> Path.join("runtime.exs")

    if File.exists?(runtime) do
      Mix.Tasks.Loadconfig.load_runtime(runtime)
    end

    if opts[:preload_modules] do
      for {app, _, _} <- Application.loaded_applications() do
        :code.ensure_modules_loaded(Application.spec(app, :modules))
      end
    end

    # If the build path is set, we assume it is being shared
    # (such as in an umbrella) and therefore we cannot check
    # for applications as we may have both false positives and
    # false negatives.
    #
    # Therefore we let the application that owns the build path
    # to ultimately perform the check.
    if !config[:build_path] do
      check_configured()
    end
  end

  defp check_configured() do
    for app <- Mix.ProjectStack.config_apps(),
        # Application.spec is a quick check that doesn't involve
        # a separate process, so we try that first.
        is_nil(Application.spec(app, :vsn)),
        # Then we fallback to checking the :code.lib_dir for apps
        # with runtime: false, which is still reasonably fast as
        # the code server also uses a ETS table
        :code.lib_dir(app) == {:error, :bad_name} do
      Mix.shell().error("""
      You have configured application #{inspect(app)} in your configuration file,
      but the application is not available.

      This usually means one of:

        1. You have not added the application as a dependency in a mix.exs file.

        2. You are configuring an application that does not really exist.

      Please ensure #{inspect(app)} exists or remove the configuration.
      """)
    end

    :ok
  end
end
