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

    root =
      case args do
        []    ->
          Mix.Project.config[:app] || Mix.raise("no application given and none found in mix.exs file")
        [app] ->
          app = String.to_atom(app)
          find_dep(deps, app) || Mix.raise("could not find dependency #{app}")
      end

    if opts[:format] == "dot" do
      callback = callback(&format_dot/1, deps, opts)
      Mix.Utils.write_dot_graph!("deps_tree.dot", "dependency tree", [root], callback, opts)
      """
      Generated "deps_tree.dot" in the current directory. To generate a PNG:

          dot -Tpng deps_tree.dot -o deps_tree.png

      For more options see http://www.graphviz.org/.
      """
      |> String.trim_trailing
      |> Mix.shell.info
    else
      callback = callback(&format_tree/1, deps, opts)
      Mix.Utils.print_tree([root], callback, opts)
    end
  end

  defp callback(formatter, deps, opts) do
    excluded  = Keyword.get_values(opts, :exclude) |> Enum.map(&String.to_atom/1)
    top_level = Enum.filter(deps, & &1.top_level)

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
          {formatter.(dep), exclude(deps, excluded)}
        app ->
          {{Atom.to_string(app), nil}, exclude(top_level, excluded)}
    end
  end

  defp exclude(deps, excluded) do
    Enum.reject deps, & &1.app in excluded
  end

  defp format_dot(%{app: app, requirement: requirement, opts: opts}) do
    override =
      if opts[:override] do
        " *override*"
      else
        ""
      end

    requirement = requirement && requirement(requirement)
    {app, "#{requirement}#{override}"}
  end

  defp format_tree(%{app: app, scm: scm, requirement: requirement, opts: opts}) do
    override =
      if opts[:override] do
        IO.ANSI.format([:bright, " *override*"])
      else
        ""
      end

    requirement = requirement && "#{requirement(requirement)} "
    {app, "#{requirement}(#{scm.format(opts)})#{override}"}
  end

  defp requirement(%Regex{} = regex), do: "#{inspect regex}"
  defp requirement(binary) when is_binary(binary), do: binary

  defp find_dep(deps, app) do
    Enum.find(deps, & &1.app == app)
  end
end
