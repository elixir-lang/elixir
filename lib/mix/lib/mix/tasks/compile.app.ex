# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Compile.App do
  use Mix.Task.Compiler

  @recursive true

  @moduledoc """
  Writes a `.app` file.

  A `.app` file is a file containing Erlang terms that defines
  your application. Mix automatically generates this file based on
  your `mix.exs` configuration.

  In order to generate the `.app` file, Mix expects your project
  to have both the `:app` and `:version` keys. Furthermore, you can
  configure the generated application by defining an `application/0`
  function in your `mix.exs` that returns a keyword list.

  The most commonly used keys are:

    * `:extra_applications` - a list of OTP applications
      your application depends on which are not included in `:deps`
      (usually defined in `deps/0` in your `mix.exs`). For example,
      here you can declare a dependency on applications that ship
      with Erlang/OTP or Elixir, like `:crypto` or `:logger`.
      Optional extra applications can be declared as a tuple, such
      as `{:ex_unit, :optional}`. Mix guarantees all non-optional
      applications are started before your application starts.

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
        [
          extra_applications: [:logger, :crypto, ex_unit: :optional],
          env: [key: :value],
          registered: [MyServer]
        ]
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

  Besides the options above, `.app` files also expect other options
  like `:modules` and `:vsn`, but these are automatically added by Mix.
  The complete list can be found on [Erlang's application
  specification](https://www.erlang.org/doc/man/app).

  From Elixir v1.17 onwards, the application .app file is also loaded
  whenever the task runs.

  ## Command line options

    * `--force` - forces compilation regardless of modification times
    * `--compile-path` - where to find `.beam` files and write the
      resulting `.app` file, defaults to `Mix.Project.compile_path/0`

  ## Configuration

    * `:reliable_dir_mtime` - this task relies on the operating system
      changing the mtime on a directory whenever a file is added or removed.
      You can set this option to `false` if your system does not provide
      reliable mtimes. Defaults to `false` on Windows.

  ## Phases

  Applications provide a start phases mechanism which will be called,
  in order, for the application and all included applications. If a phase
  is not defined for an included application, that application is skipped.

  Let's see an example `MyApp.application/0` function:

      def application do
        [
          start_phases: [init: [], go: [], finish: []],
          included_applications: [:my_included_app]
        ]
      end

  And an example `:my_included_app` defines on its `mix.exs` the function:

      def application do
        [
          mod: {MyIncludedApp, []},
          start_phases: [go: []]
        ]
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
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean, compile_path: :string])
    project = Mix.Project.get!()
    config = Mix.Project.config()

    app = Keyword.get(config, :app)
    version = Keyword.get(config, :version)
    validate_app!(app)
    validate_version!(version)

    compile_path = Keyword.get_lazy(opts, :compile_path, &Mix.Project.compile_path/0)
    target = Path.join(compile_path, "#{app}.app")

    # If configurations changed, we may have changed compile_env.
    # If compile_path changed, we may have added or removed files.
    # If the project changed, we may have changed other properties.
    new_mtime =
      Mix.Project.config_mtime()
      |> max(Mix.Utils.last_modified(Mix.Project.project_file()))
      |> max(Mix.Utils.last_modified(compile_path))

    current_properties = current_app_properties(target)

    {changed?, modules} =
      cond do
        opts[:force] || new_mtime > Mix.Utils.last_modified(target) ->
          {true, nil}

        Keyword.get(config, :reliable_dir_mtime, fn -> not match?({:win32, _}, :os.type()) end) ->
          {false, nil}

        true ->
          modules = modules_from(compile_path)
          {modules != Keyword.get(current_properties, :modules, []), modules}
      end

    if changed? do
      properties =
        [
          description: to_charlist(config[:description] || app),
          registered: [],
          vsn: to_charlist(version)
        ]
        |> merge_project_application(project)
        |> handle_extra_applications(config)
        |> add_compile_env(current_properties)
        |> add_modules(modules, compile_path)

      contents =
        case Mix.Utils.consultable({:application, app, properties}) do
          {:ok, contents} ->
            contents

          {:error, term, reason} ->
            Mix.raise(
              "\"def application\" has a term which cannot be written to .app files: #{inspect(term)} (#{reason})"
            )
        end

      :application.unload(app)
      :application.load({:application, app, properties})

      Mix.Project.ensure_structure()
      File.write!(target, IO.chardata_to_string([contents, ?.]))
      File.touch!(target, new_mtime)

      # If we just created the .app file, it will have touched
      # the directory mtime, so we need to reset it.
      if current_properties == [] do
        File.touch!(compile_path, new_mtime)
      end

      Mix.shell().info("Generated #{app} app")
      {:ok, []}
    else
      :application.load({:application, app, current_properties})
      {:noop, []}
    end
  end

  defp current_app_properties(target) do
    case :file.consult(target) do
      {:ok, [{:application, _app, properties}]} -> properties
      _ -> []
    end
  end

  defp validate_app!(app) when is_atom(app), do: :ok

  defp validate_app!(app) do
    ensure_present!(:app, app)
    Mix.raise("Expected :app to be an atom, got: #{inspect(app)}")
  end

  defp validate_version!(version) do
    ensure_present!(:version, version)

    if not (is_binary(version) and match?({:ok, _}, Version.parse(version))) do
      Mix.raise(
        "Expected :version to be a valid Version, got: #{inspect(version)} (see the Version module for more information)"
      )
    end
  end

  defp ensure_present!(name, nil) do
    Mix.raise("Please ensure mix.exs file has the #{inspect(name)} in the project definition")
  end

  defp ensure_present!(_name, _val), do: :ok

  defp modules_from(path) do
    case File.ls(path) do
      {:ok, entries} ->
        Enum.sort(
          for entry <- entries,
              String.ends_with?(entry, ".beam"),
              do: entry |> binary_part(0, byte_size(entry) - 5) |> String.to_atom()
        )

      {:error, _} ->
        []
    end
  end

  defp merge_project_application(best_guess, project) do
    if function_exported?(project, :application, 0) do
      project_application = project.application()

      if not Keyword.keyword?(project_application) do
        Mix.raise(
          "Application configuration returned from application/0 should be a keyword list"
        )
      end

      Keyword.merge(best_guess, validate_properties!(project_application))
    else
      best_guess
    end
  end

  defp validate_properties!(properties) do
    Enum.each(properties, fn
      {:description, value} ->
        if not is_list(value) do
          Mix.raise(
            "Application description (:description) is not a character list, got: " <>
              inspect(value)
          )
        end

      {:id, value} ->
        if not is_list(value) do
          Mix.raise("Application ID (:id) is not a character list, got: " <> inspect(value))
        end

      {:vsn, value} ->
        if not is_list(value) do
          Mix.raise("Application vsn (:vsn) is not a character list, got: " <> inspect(value))
        end

      {:maxT, value} ->
        if not (value == :infinity or is_integer(value)) do
          Mix.raise(
            "Application maximum time (:maxT) is not an integer or :infinity, got: " <>
              inspect(value)
          )
        end

      {:modules, value} ->
        if not (is_list(value) and Enum.all?(value, &is_atom(&1))) do
          Mix.raise(
            "Application modules (:modules) should be a list of atoms, got: " <> inspect(value)
          )
        end

      {:registered, value} ->
        if not (is_list(value) and Enum.all?(value, &is_atom(&1))) do
          Mix.raise(
            "Application registered processes (:registered) should be a list of atoms, got: " <>
              inspect(value)
          )
        end

      {:included_applications, value} ->
        if not (is_list(value) and Enum.all?(value, &is_atom(&1))) do
          Mix.raise(
            "Application included applications (:included_applications) should be a list of atoms, got: " <>
              inspect(value)
          )
        end

      {:extra_applications, value} ->
        if not (is_list(value) and Enum.all?(value, &typed_app?(&1))) do
          Mix.raise(
            "Application extra applications (:extra_applications) should be a list of atoms or " <>
              "{app, :required | :optional} pairs, got: " <> inspect(value)
          )
        end

      {:applications, value} ->
        if not (is_list(value) and Enum.all?(value, &typed_app?(&1))) do
          Mix.raise(
            "Application applications (:applications) should be a list of atoms or " <>
              "{app, :required | :optional} pairs, got: " <> inspect(value)
          )
        end

      {:env, value} ->
        if not Keyword.keyword?(value) do
          Mix.raise(
            "Application environment (:env) should be a keyword list, got: " <> inspect(value)
          )
        end

      {:start_phases, value} ->
        if not Keyword.keyword?(value) do
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

    properties
  end

  defp typed_app?(app) when is_atom(app), do: true
  defp typed_app?({app, type}) when is_atom(app) and type in [:required, :optional], do: true
  defp typed_app?(_), do: false

  defp add_compile_env(properties, current_properties) do
    # If someone calls compile.elixir and then compile.app across two
    # separate OS calls, then the compile_env won't be properly reflected.
    # This is ok because compile_env is not used for correctness. It is
    # simply to catch possible errors early.
    case Mix.ProjectStack.compile_env(nil) do
      nil -> Keyword.take(current_properties, [:compile_env]) ++ properties
      [] -> properties
      compile_env -> Keyword.put(properties, :compile_env, compile_env)
    end
  end

  defp add_modules(properties, modules, compile_path) do
    Keyword.put_new_lazy(properties, :modules, fn -> modules || modules_from(compile_path) end)
  end

  defp handle_extra_applications(properties, config) do
    {extra, properties} = Keyword.pop(properties, :extra_applications, [])

    if extra != [] and Keyword.has_key?(properties, :applications) do
      Mix.shell().error(
        "both :extra_applications and :applications was found in your mix.exs. " <>
          "You most likely want to remove the :applications key, as all applications are derived from your dependencies"
      )
    end

    {all, optional} =
      Mix.Utils.project_apps(properties, config, extra, fn apps_opts ->
        apps_from_runtime_prod_deps(properties, apps_opts)
      end)

    properties
    |> Keyword.put(:applications, all)
    |> Keyword.put(:optional_applications, optional)
  end

  defp apps_from_runtime_prod_deps(properties, apps_opts) do
    included_applications = Keyword.get(properties, :included_applications, [])

    for {app, opts} <- apps_opts,
        runtime_app?(opts),
        app not in included_applications,
        do: {app, if(Keyword.get(opts, :optional, false), do: :optional, else: :required)}
  end

  defp runtime_app?(opts) do
    Keyword.get(opts, :runtime, true) and Keyword.get(opts, :app, true) and matching_only?(opts) and
      matching_target?(opts)
  end

  defp matching_only?(opts) do
    case Keyword.fetch(opts, :only) do
      {:ok, value} -> Mix.env() in List.wrap(value)
      :error -> true
    end
  end

  defp matching_target?(opts) do
    case Keyword.fetch(opts, :targets) do
      {:ok, value} -> Mix.target() in List.wrap(value)
      :error -> true
    end
  end
end
