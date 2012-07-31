defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all out of date dependencies"

  @moduledoc """
  Get all out of date dependencies, i.e. dependencies
  that are not available or have a wrong lock.
  """

  import Mix.Deps, only: [all: 0, by_name: 1, format_dep: 1,
                          deps_path: 1, check_lock: 2, out_of_date?: 1]

  def run(args) do
    case OptionParser.parse(args) do
      { _, [] } ->
        lock = Mix.Deps.Lock.read
        deps = lc dep inlist all, dep = check_lock(dep, lock), out_of_date?(dep), do: dep
        do_get(deps)
      { _, tail } ->
        do_get(by_name(tail))
    end
  end

  defp do_get(_) do
    shell = Mix.shell

    apps = Mix.Deps.Lock.update_lock all, fn(dep, lock) ->
      Mix.Dep[scm: scm, opts: opts] = dep
      shell.info "* Getting #{format_dep(dep)}"
      path = deps_path(dep)
      File.mkdir_p!(path)

      opts = Keyword.put(opts, :lock, lock)
      new  = scm.get(path, opts)

      if new && lock && lock != new do
        Mix.shell.error "  dependency could not be updated to lock #{inspect lock}"
        nil
      else
        new
      end
    end

    Mix.Task.run "deps.compile", apps
  end
end