defmodule Mix.Tasks.App.Tree do
  use Mix.Task

  @shortdoc "Prints the application tree"

  @moduledoc """
  Prints the application tree.

      mix app.tree --exclude logger --exclude elixir

  If no application is given, it uses the current application defined
  in the `mix.exs` file.

  ## Command line options

    * `--exclude` - exclude dependencies which you do not want to see printed.
      `kernel`, `stdlib` and `compiler` are always excluded from the tree.

    * `--pretty` - use Unicode codepoints for formatting the tree.
      Defaults to true except on Windows.

  """

  @default_excluded [:kernel, :stdlib, :compiler]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Task.run "compile"

    {app, opts} =
      case OptionParser.parse(args, switches: [exclude: :keep, pretty: :boolean]) do
        {opts, [], _} ->
          app = Mix.Project.config[:app] || Mix.raise("no application given and none found in mix.exs file")
          {app, opts}
        {opts, [app], _} ->
          {String.to_atom(app), opts}
      end

    excluded = Keyword.get_values(opts, :exclude) |> Enum.map(&String.to_atom/1)
    excluded = @default_excluded ++ excluded

    Mix.Utils.print_tree([{:normal, app}], fn {type, app} ->
      load(app)
      if app in excluded do
        false
      else
        {"#{app}#{type(type)}", children_for(app)}
      end
    end, opts)
  end

  defp load(app) do
    case Application.load(app) do
      :ok -> :ok
      {:error, {:already_loaded, ^app}} -> :ok
    end
  end

  defp children_for(app) do
    apps = Application.spec(app, :applications)
    included_apps = Application.spec(app, :included_applications)
    Enum.map(apps, &{:normal, &1}) ++ Enum.map(included_apps, &{:included, &1})
  end

  defp type(:normal),  do: ""
  defp type(:include), do: " (included)"
end
