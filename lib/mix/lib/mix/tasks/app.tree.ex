defmodule Mix.Tasks.App.Tree do
  use Mix.Task

  @shortdoc "Analyses the current Mix project and sends dependency tree to mix shell."

  @moduledoc """
  Analyses the current Mix project and sends dependency tree to mix shell.

  ## Command line options

    * `--exclude` - exclude dependencies, which you do not want to see in a tree

  ## Example of use

      mix deps.tree --exclude logger --exclude elixir

  Please note, that `kernel` and `stdlib` are always excluded from tree.
  """

  @default_excluded [:kernel, :stdlib]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Task.run "loadpaths"
    {opts, _, _} = OptionParser.parse(args, switches: [exclude: :keep])
    excluded = Keyword.get_values(opts, :exclude) |> Enum.map(&String.to_atom/1)
    excluded = @default_excluded ++ excluded
    [Mix.Project.get!.project[:app] |> deps] |> print_tree(excluded)
  end

  defp deps(app, type \\ :normal) do
    {apps, include_apps} = load(app)
    {app, type, Enum.map(apps, &deps(&1)) ++ Enum.map(include_apps, &deps(&1, :include))}
  end

  defp load(app) do
    case Application.load(app) do
      :ok -> :ok
      {:error, {:already_loaded, ^app}} -> :ok
    end
    {:ok, apps} = :application.get_key(app, :applications)
    {:ok, included_apps} = :application.get_key(app, :included_applications)
    {apps, included_apps}
  end

  defp print_tree(app_tree, depth \\ [], excluded)

  defp print_tree([], _depth, _excluded), do: :ok
  defp print_tree([{app, type, apps} | app_tree], depth, excluded) do
    unless app in excluded do
      prefix = if depth == [],
                 do: "",
                 else: Enum.reverse(depth) |> tl |> Enum.map(&(if &1, do: " | ", else: "   "))
      prefix = [prefix, prefix(depth, app_tree)]
      Mix.shell.info("#{prefix}#{app}#{type(type)}")
      print_tree(apps, [(app_tree != []) | depth], excluded)
    end
    print_tree(app_tree, depth, excluded)
  end

  defp prefix([], _), do: ""
  defp prefix(_, []), do: " `-- "
  defp prefix(_, _), do: " |-- "

  defp type(:normal), do: ""
  defp type(:include), do: " (included)"
end
