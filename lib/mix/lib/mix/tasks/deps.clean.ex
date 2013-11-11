defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Remove the given dependencies' files"

  @moduledoc """
  Remove the given dependencies' files.

  Since this is a destructive action, cleaning of all dependencies
  can only happen by passing the `--all` command line option.

  Clean does not unlock the repositories, unless `--unlock` is given.
  """

  import Mix.Deps, only: [loaded: 0, loaded_by_name: 1, format_dep: 1]

  # TODO: Needs to take build_path into account
  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, args, _ } = OptionParser.parse(args, switches: [unlock: :boolean, all: :boolean])

    cond do
      opts[:all] ->
        do_clean loaded, opts
      args != [] ->
        do_clean loaded_by_name(args), opts
      true ->
        raise Mix.Error, message: "mix deps.clean expects dependencies as arguments or " <>
                                  "the --all option to clean all dependencies"
    end
  end

  defp do_clean(deps, opts) do
    shell = Mix.shell

    apps = Enum.map deps, fn(Mix.Dep[scm: scm, opts: opts] = dep) ->
      shell.info "* Cleaning #{format_dep(dep)}"
      File.rm_rf!(opts[:build])
      scm.clean opts
      dep.app
    end

    if opts[:unlock] do
      Mix.Task.run "deps.unlock", apps
    end
  end
end
