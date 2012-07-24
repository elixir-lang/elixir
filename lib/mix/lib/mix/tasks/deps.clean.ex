defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Clean dependencies"

  @moduledoc """
  Clean dependencies.

  By default, cleans all dependencies. A list of deps can
  be given to clean specific ones.
  """

  import Mix.Tasks.Deps, only: [all: 0, by_name: 1, format_dep: 1, deps_path: 1]

  def run([]) do
    do_clean all
  end

  def run(args) do
    do_clean by_name(args)
  end

  defp do_clean(deps) do
    shell = Mix.shell

    Enum.each deps, fn({ _scm, app, _req, _status, _opts } = dep) ->
      shell.info "* Cleaning #{format_dep(dep)}"
      File.rm_rf deps_path(app)
    end
  end
end