defmodule Mix.Dep.Umbrella do
  @moduledoc false

  @doc """
  Gets all umbrella dependencies in unloaded format.
  """
  def unloaded do
    config = Mix.project

    if apps_path = config[:apps_path] do
      paths = Path.wildcard(Path.join(apps_path, "*"))
      build = Mix.Project.build_path

      paths
      |> Enum.filter(&File.dir?(&1))
      |> extract_umbrella
      |> filter_umbrella(config[:apps])
      |> to_umbrella_dep(build)
    else
      []
    end
  end

  @doc """
  Gets all umbrella dependencies in the loaded format.
  """
  def loaded do
    deps = unloaded
    apps = Enum.map(deps, &(&1.app))

    Enum.map(deps, fn umbrella_dep ->
      umbrella_dep = Mix.Dep.Loader.load(umbrella_dep)
      deps = Enum.filter(umbrella_dep.deps, fn dep ->
        Mix.Dep.available?(dep) and dep.app in apps
      end)
      %{umbrella_dep | deps: deps}
    end) |> Mix.Dep.Converger.topsort
  end

  defp extract_umbrella(paths) do
    for path <- paths do
      app = path |> Path.basename |> String.downcase |> binary_to_atom
      { app, path }
    end
  end

  defp filter_umbrella(pairs, nil), do: pairs
  defp filter_umbrella(pairs, apps) when is_list(apps) do
    for { app, _ } = pair <- pairs, app in apps, do: pair
  end

  defp to_umbrella_dep(paths, build) do
    Enum.map paths, fn({ app, path }) ->
      opts = [path: path, dest: Path.expand(path),
              env: Mix.env, build: Path.join([build, "lib", app])]
      %Mix.Dep{
        scm: Mix.SCM.Path,
        app: app,
        requirement: nil,
        manager: :mix,
        status: { :ok, nil },
        opts: opts }
    end
  end
end
