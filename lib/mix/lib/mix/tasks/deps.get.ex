defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all out of date dependencies"
  @recursive :both

  @moduledoc """
  Get all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

  * `--no-compile` - skip compilation of dependencies
  * `--no-deps-check` - skip dependency check
  * `--quiet` - do not output success message

  """

  import Mix.Deps, only: [all: 2, by_name: 1, format_dep: 1, check_lock: 2, out_of_date?: 1]

  def run(args) do
    Mix.Project.get! # Require the project to be available

    { opts, rest } = OptionParser.parse(args, switches: [no_compile: :boolean, quiet: :boolean])

    if rest != [] do
      { _, acc } = Enum.map_reduce by_name(rest), init, deps_getter(&1, &2)
    else
      acc = all(init, deps_getter(&1, &2))
    end

    finalize_get(acc, opts)
  end

  defp init do
    { [], Mix.Deps.Lock.read }
  end

  defp finalize_get({ apps, lock }, opts) do
    if apps == [] do
      unless opts[:quiet], do: Mix.shell.info "All dependencies up to date"
    else
      Mix.Deps.Lock.write(lock)

      unless opts[:no_compile] do
        Mix.Task.run("deps.compile", apps)
        unless opts[:no_deps_check], do: Mix.Task.run("deps.check", [])
      end
    end
  end

  defp deps_getter(dep, { acc, lock }) do
    shell = Mix.shell
    Mix.Dep[app: app, scm: scm, opts: opts] = dep = check_lock(dep, lock)

    cond do
      # Path dependencies are specially handled because they cannot
      # be fetched although they are always compiled afterwards
      scm == Mix.SCM.Path ->
        { dep, { [app|acc], lock } }

      # If the dependency is not available or we have a lock mismatch
      out_of_date?(dep) ->
        shell.info "* Getting #{format_dep(dep)}"

        old  = lock[app]
        opts = Keyword.put(opts, :lock, old)

        new =
          if scm.checked_out?(opts) do
            scm.update(opts)
          else
            scm.checkout(opts)
          end

        if new do
          # Update the dependency returned so it is now
          # available and nested dependencies can be fetched
          { Mix.Deps.update(dep), { [app|acc], Keyword.put(lock, app, new) } }
        else
          { dep, { acc, lock } }
        end

      # The dependency is ok or has some other error
      true ->
        { dep, { acc, lock } }
    end
  end
end
