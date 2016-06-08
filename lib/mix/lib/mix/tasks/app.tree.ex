defmodule Mix.Tasks.App.Tree do
  use Mix.Task

  @shortdoc "Prints the application tree"
  @recursive true

  @moduledoc """
  Prints the application tree.

      mix app.tree --exclude logger --exclude elixir

  If no application is given, it uses the current application defined
  in the `mix.exs` file.

  ## Command line options

    * `--exclude` - exclude applications which you do not want to see printed.
      `kernel`, `stdlib` and `compiler` are always excluded from the tree.

    * `--format` - Can be set to one of either:

      * `pretty` - use Unicode codepoints for formatting the tree.
        This is the default except on Windows.

      * `plain` - do not use Unicode codepoints for formatting the tree.
        This is the default on Windows.

      * `dot` - produces a DOT graph description of the application tree
        in `app_tree.dot` in the current directory.
        Warning: this will override any previously generated file.

  """

  @default_excluded [:kernel, :stdlib, :compiler]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Task.run "compile"

    {app, opts} =
      case OptionParser.parse!(args, strict: [exclude: :keep, format: :string]) do
        {opts, []} ->
          app = Mix.Project.config[:app] || Mix.raise("no application given and none found in mix.exs file")
          {app, opts}
        {opts, [app]} ->
          {String.to_atom(app), opts}
      end

    excluded = Keyword.get_values(opts, :exclude) |> Enum.map(&String.to_atom/1)
    excluded = @default_excluded ++ excluded

    app_tree_callback =
      fn {type, app} ->
        load(app)
        {"#{app}#{type(type)}", children_for(app, excluded)}
      end

    if opts[:format] == "dot" do
      app_tree = Mix.Utils.build_dot_graph("application tree", [{:normal, app}], app_tree_callback, opts)
      filename = "app_tree.dot"
      File.write!(filename, app_tree <> "\n")
      Mix.shell.info("Generated `#{filename}` in current directory")
    else
      Mix.Utils.print_tree([{:normal, app}], app_tree_callback, opts)
    end
  end

  defp load(app) do
    case Application.load(app) do
      :ok -> :ok
      {:error, {:already_loaded, ^app}} -> :ok
      _ -> Mix.raise("could not find application #{app}")
    end
  end

  defp children_for(app, excluded) do
    apps = Application.spec(app, :applications) -- excluded
    included_apps = Application.spec(app, :included_applications) -- excluded
    Enum.map(apps, &{:normal, &1}) ++ Enum.map(included_apps, &{:included, &1})
  end

  defp type(:normal),   do: ""
  defp type(:included), do: " (included)"
end
