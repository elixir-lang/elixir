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
      |> topsort_umbrella(Path.expand(apps_path))
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

  # Sort umbrella in dependency order
  defp topsort_umbrella(projects, apps_path) do
    graph = :digraph.new

    try do
      Enum.each projects, fn { app, app_path } ->
        :digraph.add_vertex(graph, app, app_path)
      end

      Enum.each projects, fn { app, app_path } ->
        Mix.Project.in_project app, app_path, fn _ ->
          lc dep inlist Mix.Deps.Retriever.to_deps(Mix.project[:deps]),
             Mix.Deps.available?(dep) and in_umbrella?(dep, apps_path),
             do: :digraph.add_edge(graph, dep.app, app)
        end
      end

      unless :digraph_utils.is_acyclic(graph) do
        raise Mix.Error, message: "Could not dependency sort umbrella projects. " <>
          "There are cycles in the dependency graph."
      end

      vertices = :digraph_utils.topsort(graph)
      Enum.map vertices, &:digraph.vertex(graph, &1)
    after
      :digraph.delete(graph)
    end
  end

  defp in_umbrella?(Mix.Dep[opts: opts], apps_path) do
    apps_path == Path.expand(Path.join(opts[:dest], ".."))
  end

  defp to_umbrella_dep(paths, source) do
    Enum.map paths, fn({ app, path }) ->
      Mix.Dep[scm: Mix.SCM.Path, app: app, requirement: nil, manager: :mix, source: source,
              status: { :ok, nil }, opts: [path: path, dest: Path.expand(path)]]
    end
  end
end
