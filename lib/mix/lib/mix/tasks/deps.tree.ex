defmodule Mix.Tasks.Deps.Tree do
  use Mix.Task

  @shortdoc "Prints the dependency tree"
  @recursive true

  @moduledoc """
  Prints the dependency tree.

      mix deps.tree

  If no dependency is given, it uses the tree defined in the `mix.exs` file.

  ## Command line options

    * `--only` - the enviroment to show dependencies for

    * `--exclude` - exclude dependencies which you do not want to see printed.

    * `--format` - Can be set to one of either:

      * `pretty` - use Unicode codepoints for formatting the tree.
        This is the default except on Windows.

      * `plain` - do not use Unicode codepoints for formatting the tree.
        This is the default on Windows.

      * `dot` - produces a DOT graph description of the dependency tree
        in `deps_tree.dot` in the current directory.
        Warning: this will override any previously generated file.

  """
  @switches [only: :string, exclude: :keep, format: :string]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    deps_opts = if only = opts[:only], do: [env: :"#{only}"], else: []
    deps      = Mix.Dep.loaded(deps_opts)
    excluded  = Keyword.get_values(opts, :exclude) |> Enum.map(&String.to_atom/1)
    top_level = Enum.filter(deps, & &1.top_level)

    root =
      case args do
        []    ->
          Mix.Project.config[:app] || Mix.raise("no application given and none found in mix.exs file")
        [app] ->
          app = String.to_atom(app)
          find_dep(deps, app) || Mix.raise("could not find dependency #{app}")
      end

    deps_tree_callback =
      fn
        %Mix.Dep{app: app} = dep ->
          deps =
            # Do not show dependencies if they were
            # already shown at the top level
            if not dep.top_level && find_dep(top_level, app) do
              []
            else
              find_dep(deps, app).deps
            end
          {format_dep(dep, opts), exclude(deps, excluded)}
        app ->
          {Atom.to_string(app), exclude(top_level, excluded)}
      end

    if opts[:format] == "dot" do
      deps_tree = Mix.Utils.build_dot_graph("dependency tree", [root], deps_tree_callback, opts)
      filename = "deps_tree.dot"
      File.write!(filename, deps_tree <> "\n")
      Mix.shell.info("Generated `#{filename}` in current directory")
    else
      Mix.Utils.print_tree([root], deps_tree_callback, opts)
    end
  end

  defp exclude(deps, excluded) do
    Enum.reject deps, & &1.app in excluded
  end

  defp format_dep(%{app: app, scm: scm, requirement: requirement, opts: deps_opts}, opts) do
    override =
      if deps_opts[:override] do
        "#{IO.ANSI.bright} *override*#{IO.ANSI.normal}"
      else
        ""
      end
    if opts[:format] == "dot" do
      {app, "#{requirement(requirement)}#{override}"}
    else
      "#{app}#{requirement(requirement)} (#{scm.format(deps_opts)})#{override}"
    end
  end

  defp requirement(nil), do: ""
  defp requirement(%Regex{} = regex), do: " #{inspect regex}"
  defp requirement(binary) when is_binary(binary), do: " #{binary}"

  defp find_dep(deps, app) do
    Enum.find(deps, & &1.app == app)
  end
end
