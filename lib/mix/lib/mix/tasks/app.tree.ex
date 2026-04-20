# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.App.Tree do
  use Mix.Task

  @shortdoc "Prints the application tree"
  @recursive true

  @moduledoc """
  Prints the application tree.

      $ mix app.tree --exclude logger --exclude elixir

  If no application is given, it uses the current application defined
  in the `mix.exs` file.

  ## Command line options

    * `--exclude` - exclude applications which you do not want to see printed.
      `kernel`, `stdlib` and `compiler` are always excluded from the tree.

    * `--format` - Can be set to one of either:

      * `pretty` - uses Unicode code points for formatting the tree.
        This is the default except on Windows.

      * `plain` - does not use Unicode code points for formatting the tree.
        This is the default on Windows.

      * `dot` - produces a DOT graph description of the application tree
        in `app_tree.dot` in the current directory. See the documentation
        for the `--output` option to learn how to control where the file
        is written and other related details.

    * `--output` *(since v1.20.0)* - can be used to override the location of
      the file created by the `dot` format. It can be set to

      * `-` - prints the output to standard output;

      * a path - writes the output graph to the given path

      If the output file already exists then it will be renamed in place
      to have a `.bak` suffix, possibly overwriting any existing `.bak` file.
      If this rename fails a fatal exception will be thrown.

  """

  @default_excluded [:kernel, :stdlib, :compiler]

  @impl true
  def run(args) do
    Mix.Task.run("compile", args)

    {app, opts} =
      case OptionParser.parse!(args,
             strict: [exclude: :keep, format: :string, output: :string]
           ) do
        {opts, []} ->
          app =
            Mix.Project.config()[:app] ||
              Mix.raise("no application given and none found in mix.exs file")

          {app, opts}

        {opts, [app]} ->
          {String.to_atom(app), opts}
      end

    excluded = Keyword.get_values(opts, :exclude) |> Enum.map(&String.to_atom/1)
    excluded = @default_excluded ++ excluded

    callback = fn {app, type} ->
      if load(app, type) do
        {{app, type(type)}, children_for(app, excluded)}
      else
        {{app, "(optional - missing)"}, []}
      end
    end

    if opts[:format] == "dot" do
      root = [{app, :normal}]

      file_spec =
        Mix.Utils.write_dot_graph!("app_tree.dot", "application tree", root, callback, opts)

      if file_spec != "-" do
        png_file_spec = (file_spec |> Path.rootname() |> Path.basename()) <> ".png"

        """
        Generated "#{Path.relative_to_cwd(file_spec)}". To generate a PNG:

           dot -Tpng #{inspect(file_spec)} -o #{inspect(png_file_spec)}

        For more options see https://www.graphviz.org/.
        """
        |> String.trim_trailing()
        |> Mix.shell().info()
      end
    else
      Mix.Utils.print_tree([{app, :normal}], callback, opts)
    end
  end

  defp load(app, type) do
    case Application.ensure_loaded(app) do
      :ok -> true
      _ when type == :optional -> false
      _ -> Mix.raise("could not find application #{app}")
    end
  end

  defp children_for(app, excluded) do
    apps = Application.spec(app, :applications) -- excluded
    included_apps = Application.spec(app, :included_applications) -- excluded
    optional_apps = Application.spec(app, :optional_applications) || []

    Enum.sort(
      Enum.map(apps, &{&1, if(&1 in optional_apps, do: :optional, else: :normal)}) ++
        Enum.map(included_apps, &{&1, :included})
    )
  end

  defp type(:normal), do: nil
  defp type(:included), do: "(included)"
  defp type(:optional), do: "(optional)"
end
