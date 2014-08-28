defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Remove the given dependencies' files"

  @moduledoc """
  Remove the given dependencies' files.

  Since this is a destructive action, cleaning of all dependencies
  can only happen by passing the `--all` command line option. It
  also works accross all environments, unless `--only` is given.

  Clean does not unlock the dependencies, unless `--unlock` is given.
  The `--unused` flag removes unused the dependencies.
  """
  @switches [unlock: :boolean, all: :boolean, only: :string, unused: :boolean]

  def run(args) do
    Mix.Project.get!
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    cond do
      opts[:all] ->
        # Clean all deps by default unless --only is given
        clean_opts = if only = opts[:only], do: [env: :"#{only}"], else: []
        apps = Mix.Dep.loaded(clean_opts) |> Enum.map(&(&1.app))
        do_clean apps, opts
      opts[:unused] ->
        Mix.Dep.loaded([]) |> Enum.map(&(&1.app)) |> unused_apps |> do_clean opts
      args != [] ->
        do_clean args, opts
      true ->
        Mix.raise "mix deps.clean expects dependencies as arguments or " <>
                  "a flag indicating which dependencies to clean " <>
                  "The --all option will clean all dependencies while"
                  "the --unused option cleans unused dependencies."
    end
  end

  defp unused_apps(loaded_apps) do
    case File.ls(Mix.Project.deps_path) do
      {:ok, deps} ->
        Enum.reject deps,
          fn(x) -> not File.dir?(x) and Enum.member?(loaded_apps, String.to_atom(x)) end
      {_, _} -> []
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
