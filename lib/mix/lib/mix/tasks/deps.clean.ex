defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Remove the given dependencies' files"

  @moduledoc """
  Remove the given dependencies' files.

  Since this is a destructive action, cleaning of all dependencies
  can only happen by passing the `--all` command line option. It
  also works accross all environments, unless `--only` is given.

  Clean does not unlock the dependencies, unless `--unlock` is given.
  """
  @switches [unlock: :boolean, all: :boolean, only: :string]

  def run(args) do
    Mix.Project.get! # Require the project to be available
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    cond do
      opts[:all] ->
        # Clean all deps by default unless --only is given
        clean_opts = if only = opts[:only], do: [env: :"#{only}"], else: []
        apps = Mix.Dep.loaded(clean_opts) |> Enum.map(&(&1.app))
        do_clean apps, opts
      args != [] ->
        do_clean args, opts
      true ->
        raise Mix.Error, message: "mix deps.clean expects dependencies as arguments or " <>
                                  "the --all option to clean all dependencies"
    end
  end

  defp do_clean(apps, opts) do
    shell = Mix.shell
    build = Mix.Project.build_path
            |> Path.dirname
            |> Path.join("#{opts[:only] || :*}/lib")
    deps  = Mix.Project.deps_path

    Enum.each apps, fn(app) ->
      shell.info "* Cleaning #{app}"

      build
      |> Path.join(to_string(app))
      |> Path.wildcard
      |> Enum.each(&File.rm_rf!/1)

      deps
      |> Path.join(to_string(app))
      |> File.rm_rf!
    end

    if opts[:unlock] do
      Mix.Task.run "deps.unlock", apps
    end
  end
end
