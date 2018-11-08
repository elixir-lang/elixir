defmodule Mix.Tasks.Compile.App do
  use Mix.Task.Compiler

  @recursive true

  @moduledoc """
  Writes an .app file.

  An `.app` file is a file containing Erlang terms that defines
  your application. Mix automatically generates this file based on
  your `mix.exs` configuration.

  In order to generate the `.app` file, Mix expects your project
  to have both `:app` and `:version` keys. Furthermore, you can
  configure the generated application by defining an `application/0`
  function in your `mix.exs` that returns a keyword list.

  The most commonly used keys are:

    * `:extra_applications` - a list of OTP applications
      your application depends on which are not included in `:deps`
      (usually defined in `deps/0` in your `mix.exs`). For example,
      here you can declare a dependency on applications that ship with
      Erlang/OTP or Elixir, like `:crypto` or `:logger`, but anything in
      the code path works. Mix guarantees that these applications and
      the rest of your runtime dependencies are started before your
      application starts.

    * `:registered` - the name of all registered processes in the
      application. If your application defines a local GenServer
      with name `MyServer`, it is recommended to add `MyServer`
      to this list. It is most useful in detecting conflicts
      between applications that register the same names.

    * `:env` - the default values for the application environment.
      The application environment is one of the most common ways
      to configure applications. See the `Application` module for
      mechanisms to read and write to the application environment.

  For example:

      def application do
        [extra_applications: [:logger, :crypto],
         env: [key: :value],
         registered: [MyServer]]
      end

  Other options include:

    * `:applications` - all applications your application depends
      on at runtime. By default, this list is automatically inferred
      from your dependencies. Mix and other tools use the application
      list in order to start your dependencies before starting the
      application itself.

    * `:mod` - specifies a module to invoke when the application
      is started. It must be in the format `{Mod, args}` where
      args is often an empty list. The module specified must
      implement the callbacks defined by the `Application`
      module.

    * `:start_phases` - specifies a list of phases and their arguments
      to be called after the application is started. See the "Phases"
      section below.

    * `:included_applications` - specifies a list of applications
      that will be included in the application. It is the responsibility of
      the primary application to start the supervision tree of all included
      applications, as only the primary application will be started. A process
      in an included application considers itself belonging to the
      primary application.

    * `:maxT` - specifies the maximum time the application is allowed to run, in
      milliseconds. Applications are stopped if `:maxT` is reached, and their
      top-level supervisor terminated with reason `:normal`. This threshold is
      technically valid in any resource file, but it is only effective for
      applications with a callback module. Defaults to `:infinity`.

  Besides the options above, `.app` files also expect other options like
  `:modules` and `:vsn`, but these are automatically added by Mix.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  ## Phases

  Applications provide a start phases mechanism which will be called,
  in order, for the application and all included applications. If a phase
  is not defined for an included application, that application is skipped.

  Let's see an example `MyApp.application/0` function:

      def application do
        [start_phases: [init: [], go: [], finish: []],
         included_applications: [:my_included_app]]
      end

  And an example `:my_included_app` defines on its `mix.exs` the function:

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
      MyApp.start_phase(:finish, :normal, [])

  """

  @impl true
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.Project.get!()
    config = Mix.Project.config()

    app = Keyword.get(config, :app)
    version = Keyword.get(config, :version)

    validate_app(app)
    validate_version(version)

    path = Mix.Project.compile_path()
    mods = modules_from(Path.wildcard("#{path}/*.beam")) |> Enum.sort()

    target = Path.join(path, "#{app}.app")
    source = Mix.Project.config_mtime()

    if opts[:force] || Mix.Utils.stale?([source], [target]) || modules_changed?(mods, target) do
      best_guess = [
        description: to_charlist(config[:description] || app),
        modules: mods,
        registered: [],
        vsn: to_charlist(version)
      ]

      properties =
        if function_exported?(project, :application, 0) do
          project_application = project.application

          unless Keyword.keyword?(project_application) do
            Mix.raise(
              "Application configuration returned from application/0 should be a keyword list"
            )
          end

          Keyword.merge(best_guess, project_application)
        else
          best_guess
        end

      properties = ensure_correct_properties(properties, config)
      contents = :io_lib.format("~p.~n", [{:application, app, properties}])

      Mix.Project.ensure_structure()
      File.write!(target, IO.chardata_to_string(contents))
      Mix.shell().info("Generated #{app} app")
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
    Enum.map(beams, &(&1 |> Path.basename() |> Path.rootname(".beam") |> String.to_atom()))
  end

  defp ensure_correct_properties(properties, config) do
    validate_properties!(properties)
    {extra, properties} = Keyword.pop(properties, :extra_applications, [])

    apps =
      properties
      |> Keyword.get(:applications)
      |> Kernel.||(apps_from_prod_deps(properties, config))
      |> normalize_apps(extra, config)

    Keyword.put(properties, :applications, apps)
  end

  defp validate_properties!(properties) do
    Enum.each(properties, fn
      {:description, value} ->
        unless is_list(value) do
          Mix.raise(
            "Application description (:description) is not a character list, got: " <>
              inspect(value)
          )
        end

      {:id, value} ->
        unless is_list(value) do
          Mix.raise("Application id (:id) is not a character list, got: " <> inspect(value))
        end

      {:vsn, value} ->
        unless is_list(value) do
          Mix.raise("Application vsn (:vsn) is not a character list, got: " <> inspect(value))
        end

      {:maxT, value} ->
        unless value == :infinity or is_integer(value) do
          Mix.raise(
            "Application maximum time (:maxT) is not an integer or :infinity, got: " <>
              inspect(value)
          )
        end

      {:modules, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise(
            "Application modules (:modules) should be a list of atoms, got: " <> inspect(value)
          )
        end

      {:registered, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise(
            "Application registered processes (:registered) should be a list of atoms, got: " <>
              inspect(value)
          )
        end

      {:included_applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise(
            "Application included applications (:included_applications) should be a list of atoms, got: " <>
              inspect(value)
          )
        end

      {:extra_applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise(
            "Application extra applications (:extra_applications) should be a list of atoms, got: " <>
              inspect(value)
          )
        end

      {:applications, value} ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)) do
          Mix.raise(
            "Application applications (:applications) should be a list of atoms, got: " <>
              inspect(value)
          )
        end

      {:env, value} ->
        unless Keyword.keyword?(value) do
          Mix.raise(
            "Application environment (:env) should be a keyword list, got: " <> inspect(value)
          )
        end

      {:start_phases, value} ->
        unless Keyword.keyword?(value) do
          Mix.raise(
            "Application start phases (:start_phases) should be a keyword list, got: " <>
              inspect(value)
          )
        end

      {:mod, []} ->
        :ok

      {:mod, {module, _args}} when is_atom(module) ->
        :ok

      {:mod, value} ->
        Mix.raise(
          "Application callback module (:mod) should be either [] or {module, start_args}, got: " <>
            inspect(value)
        )

      _ ->
        :ok
    end)
  end

  defp apps_from_prod_deps(properties, config) do
    included_applications = Keyword.get(properties, :included_applications, [])
    non_runtime_deps = non_runtime_deps(config)

    for %{app: app, top_level: true} <- Mix.Dep.cached(),
        not Map.has_key?(non_runtime_deps, app),
        app not in included_applications,
        do: app
  end

  defp non_runtime_deps(config) do
    for config_dep <- Keyword.get(config, :deps, []),
        not runtime_dep?(config_dep),
        do: {elem(config_dep, 0), true},
        into: %{}
  end

  defp runtime_dep?({_app, opts}) when is_list(opts), do: runtime_opts?(opts)
  defp runtime_dep?({_app, _req, opts}) when is_list(opts), do: runtime_opts?(opts)
  defp runtime_dep?(_), do: true

  defp runtime_opts?(opts) do
    Keyword.get(opts, :runtime, true) and Keyword.get(opts, :app, true)
  end

  defp normalize_apps(apps, extra, config) do
    Enum.uniq([:kernel, :stdlib] ++ language_app(config) ++ extra ++ apps)
  end

  defp language_app(config) do
    case Keyword.fetch(config, :language) do
      {:ok, :elixir} -> [:elixir]
      {:ok, :erlang} -> []
      :error -> [:elixir]
    end
  end
end
