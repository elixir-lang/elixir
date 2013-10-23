defmodule Mix.Deps.Umbrella do
  @moduledoc false

  @doc """
  Extracts the current project umbrella apps as dependencies.
  """
  def children do
    config = Mix.project

    if apps_path = config[:apps_path] do
      paths = Path.wildcard(Path.join(apps_path, "*"))

      paths
      |> Enum.filter(&File.dir?(&1))
      |> extract_umbrella
      |> filter_umbrella(config[:apps])
      |> to_umbrella_dep(Mix.Project.get)
    else
      []
    end
  end

  defp extract_umbrella(paths) do
    lc path inlist paths do
      app = path |> Path.basename |> String.downcase |> binary_to_atom
      { app, path }
    end
  end

  defp filter_umbrella(pairs, nil), do: pairs
  defp filter_umbrella(pairs, apps) when is_list(apps) do
    lc { app, _ } = pair inlist pairs, app in apps, do: pair
  end

  defp to_umbrella_dep(paths, source) do
    Enum.map paths, fn({ app, path }) ->
      Mix.Dep[scm: Mix.SCM.Path, app: app, requirement: nil, manager: :mix, source: source,
              status: { :ok, nil }, opts: [path: path, dest: Path.expand(path)]]
    end
  end
end
