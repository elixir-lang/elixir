defmodule Mix.Dep.Umbrella do
  @moduledoc false

  @doc """
  Gets all umbrella dependencies in unloaded format.
  """
  def unloaded do
    config = Mix.Project.config()

    if apps_paths = Mix.Project.apps_paths(config) do
      env = Mix.env()
      from = Path.absname("mix.exs")
      build = Mix.Project.build_path(config)

      for {app, path} <- apps_paths do
        dest_path = Path.expand(path)
        build_path = Path.join([build, "lib", Atom.to_string(app)])

        opts = [
          path: path,
          dest: dest_path,
          from_umbrella: true,
          env: env,
          build: build_path
        ]

        %Mix.Dep{
          scm: Mix.SCM.Path,
          app: app,
          requirement: nil,
          manager: :mix,
          status: {:ok, nil},
          from: from,
          opts: opts
        }
      end
    else
      []
    end
  end

  @doc """
  Gets all umbrella dependencies in the loaded format from cache (if available).
  """
  def cached do
    if project = Mix.Project.get() do
      key = {:umbrella_deps, Mix.env(), project}
      Mix.State.read_cache(key) || Mix.State.write_cache(key, loaded())
    else
      loaded()
    end
  end

  @doc """
  Gets all umbrella dependencies in the loaded format.
  """
  def loaded do
    deps = unloaded()
    apps = Enum.map(deps, & &1.app)

    Enum.map(deps, fn umbrella_dep ->
      umbrella_dep = Mix.Dep.Loader.load(umbrella_dep, nil, false)

      deps =
        Enum.filter(umbrella_dep.deps, fn dep ->
          Mix.Dep.available?(dep) and dep.app in apps
        end)

      %{umbrella_dep | deps: deps}
    end)
    |> Mix.Dep.Converger.topological_sort()
  end
end
