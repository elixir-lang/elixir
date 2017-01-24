defmodule Mix.Tasks.Compile.App do
  use Mix.Task

  @recursive true

  @moduledoc """
  Writes an .app file.

  An `.app` file is a file containing Erlang terms that defines
  your application. Mix automatically generates this file based on
  your `mix.exs` configuration. You can learn more about OTP
  applications by seeing the documentation for the `Application`
  module.

  In order to generate the `.app` file, Mix expects your application
  to have both `:app` and `:version` keys. Furthermore, you can
  configure the generated application by defining an `application/0`
  function in your `mix.exs` with the following options:

    * `:applications` - all applications your application depends
      on at runtime. By default, this list is automatically inferred
      from your dependencies. Any extra Erlang/Elixir dependency
      must be specified in `:extra_applications`. Mix and other tools
      use the application list in order to start your dependencies
      before starting the application itself.

    * `:extra_applications` - a list of Erlang/Elixir applications
      that you want started before your application. For example,
      Elixir's `:logger` or Erlang's `:crypto`.

    * `:registered` - the name of all registered processes in the
      application. If your application defines a local GenServer
      with name `MyServer`, it is recommended to add `MyServer`
      to this list. It is most useful in detecting conflicts
      between applications that register the same names.

    * `:mod` - specifies a module to invoke when the application
      is started. It must be in the format `{Mod, args}` where
      args is often an empty list. The module specified must
      implement the callbacks defined by the `Application`
      module.

    * `:env` - default values for the application environment.
      The application environment is one of the most common ways
      to configure applications.

    * `:start_phases` - specifies a list of phases and their arguments
      to be called after the application is started and before any
      included applications are started.

    * `:included_applications` - specifies a list of applications
      that will be included in the application. A process in an
      included application considers itself belonging to the
      primary application.

  Let's see an example `MyApp.application/0` function:

      def application do
        [mod: {MyApp, []},
         start_phases: [init: [], go: []],
         env: [default: :value],
         applications: [:crypto],
         included_applications: [MyIncludedApp]]
      end

  And an example `MyIncludedApp.application/0` function:

      def application do
        [mod: {MyIncludedApp, []},
         start_phases: [go: []]]
      end

  In this example, the order that the application callbacks are called in is:

      Application.start(MyApp)
      MyApp.start(:normal, [])
      MyApp.start_phase(:init, :normal, [])
      MyApp.start_phase(:go, :normal, [])
      MyIncludedApp.start_phase(:go, :normal, [])

  Besides the options above, `.app` files also expect other
  options like `:modules` and `:vsn`, but these are automatically
  added by Mix.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.Project.get!
    config  = Mix.Project.config

    app     = Keyword.get(config, :app)
    version = Keyword.get(config, :version)

    validate_app(app)
    validate_version(version)

    path = Mix.Project.compile_path
    mods = modules_from(Path.wildcard("#{path}/*.beam")) |> Enum.sort

    target  = Path.join(path, "#{app}.app")
    sources = Mix.Project.config_files

    if opts[:force] || Mix.Utils.stale?(sources, [target]) || modules_changed?(mods, target) do
      best_guess = [
        description: to_charlist(config[:description] || app),
        modules: mods,
        registered: [],
        vsn: to_charlist(version),
      ]

      properties = if function_exported?(project, :application, 0) do
        project_application = project.application()
        unless Keyword.keyword?(project_application) do
          Mix.raise "Application configuration returned from application/0 should be a keyword list"
        end
        Keyword.merge(best_guess, project_application)
      else
        best_guess
      end

      properties = ensure_correct_properties(properties, config)
      contents = :io_lib.format("~p.~n", [{:application, app, properties}])

      Mix.Project.ensure_structure()
      File.write!(target, IO.chardata_to_string(contents))
      Mix.shell.info "Generated #{app} app"
      :ok
    else
      :noop
    end
  end

  defp modules_changed?(mods, target) do
    case :file.consult(target) do
      {:ok, [{:application, _app, properties}]} ->
        properties[:modules] != mods
      _ ->
        false
    end
  end

  defp validate_app(app) when is_atom(app), do: :ok
  defp validate_app(app) do
    ensure_present(:app, app)
    Mix.raise("Expected :app to be an atom, got: #{inspect(app)}")
  end

  defp validate_version(version) do
    ensure_present(:version, version)
    unless is_binary(version) and match?({:ok, _}, Version.parse(version)) do
      Mix.raise("Expected :version to be a SemVer version, got: #{inspect(version)}")
    end
  end

  defp ensure_present(name, nil) do
    Mix.raise("Please ensure mix.exs file has the #{inspect(name)} in the project definition")
  end
  defp ensure_present(_name, _val), do: :ok

  defp modules_from(beams) do
    Enum.map beams, &(&1 |> Path.basename |> Path.rootname(".beam") |> String.to_atom)
  end

  defp language_app(config) do
    case Keyword.fetch(config, :language) do
      {:ok, :elixir} -> [:elixir]
      {:ok, :erlang} -> []
      :error -> [:elixir]
    end
  end

  defp ensure_correct_properties(properties, config) do
    properties
    |> validate_properties!
    |> Keyword.put_new_lazy(:applications, fn -> apps_from_prod_non_optional_deps(properties) end)
    |> Keyword.update!(:applications, fn apps -> normalize_apps(apps, properties, config) end)
  end

  defp validate_properties!(properties) do
    Enum.each properties, fn
      {:description, value} ->
        unless is_list(value) do
          Mix.raise "Application description (:description) is not a character list, got: #{inspect value}"
        end
      {:id, value} ->
        unless is_list(value) do
          Mix.raise "Application id (:id) is not a character list, got: #{inspect value}"
        end
      {:vsn, value} ->
        unless is_list(value) do
          Mix.raise "Application vsn (:vsn) is not a character list, got: #{inspect value}"
        end
      {:maxT, value} ->
        unless value == :infinity or is_integer(value) do
          Mix.raise "Application maximum time (:maxT) is not an integer or :infinity, got: #{inspect value}"
        end
      {:modules, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise "Application modules (:modules) should be a list of atoms, got: #{inspect value}"
        end
      {:registered, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise "Application registered processes (:registered) should be a list of atoms, got: #{inspect value}"
        end
      {:included_applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise "Application included applications (:included_applications) should be a list of atoms, got: #{inspect value}"
        end
      {:extra_applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise "Application extra applications (:extra_applications) should be a list of atoms, got: #{inspect value}"
        end
      {:applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise "Application applications (:applications) should be a list of atoms, got: #{inspect value}"
        end
      {:env, value} ->
        unless Keyword.keyword?(value) do
          Mix.raise "Application environment (:env) should be a keyword list, got: #{inspect value}"
        end
      {:start_phases, value} ->
        unless Keyword.keyword?(value) do
          Mix.raise "Application start phases (:start_phases) should be a keyword list, got: #{inspect value}"
        end
      {:mod, []} ->
        :ok
      {:mod, {module, _args}} when is_atom(module) ->
        :ok
      {:mod, value} ->
        Mix.raise "Application callback module (:mod) should be either [] or {module, start_args}, got: #{inspect value}"
      _ ->
        :ok
    end

    properties
  end

  defp apps_from_prod_non_optional_deps(properties) do
    included_applications = Keyword.get(properties, :included_applications, [])

    for %{app: app, opts: opts, top_level: true} <- Mix.Dep.cached,
        Keyword.get(opts, :app, true),
        Keyword.get(opts, :runtime, true),
        not Keyword.get(opts, :optional, false),
        app not in included_applications,
        do: app
  end

  defp normalize_apps(apps, properties, config) do
    extra = Keyword.get(properties, :extra_applications, [])
    Enum.uniq([:kernel, :stdlib] ++ language_app(config) ++ extra ++ apps)
  end
end
