defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Remove dependencies files"
  @recursive true

  @moduledoc """
  Clean dependencies.

  By default, cleans all dependencies. A list of deps can
  be given to clean specific ones. Clean does not unlock
  the repositories, unless --unlock is given.
  """

  import Mix.Deps, only: [all: 0, by_name: 1, format_dep: 1]

  def run(args) do
    case OptionParser.parse(args, switches: [unlock: :boolean]) do
      { opts, [] }   -> do_clean all, opts
      { opts, args } -> do_clean by_name(args), opts
    end
  end

  defp do_clean(deps, opts) do
    shell = Mix.shell

    apps = Enum.map deps, fn(Mix.Dep[scm: scm, opts: opts] = dep) ->
      shell.info "* Cleaning #{format_dep(dep)}"
      scm.clean opts
      dep.app
    end

    if opts[:unlock] do
      Mix.Task.run "deps.unlock", apps
    end
  end
end