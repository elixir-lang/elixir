defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Dep, only: [loaded: 1, format_dep: 1, format_status: 1, check_lock: 2]

  @shortdoc "List dependencies and their status"

  @moduledoc ~S"""
  List all dependencies and their status.

  Dependencies must be specified in the `mix.exs` file in one of
  the following formats:

      { app, requirement }
      { app, opts }
      { app, requirement, opts }

  Where:

  * app is an atom
  * requirement is a version requirement or a regular expression
  * opts is a keyword list of options

  Here are some examples:

      {:foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1"}

  To specify particular versions, regardless of the tag, do:

      {:barbat, "~> 0.1", github: "elixir-lang/barbat"}

  When using a package manager, options can be skipped:

      {:pkgbaz, "~> 0.1"}

  When using umbrella applications, one may also specify:

      {:myapp, in_umbrella: true}

  The dependencies versions are expected to follow Semantic Versioning
  and the requirements must be specified as defined in the `Version`
  module.

  Below we provide a more detailed look into the available options.

  ## Mix options

  * `:app` - When set to false, does not read the app file for this dependency
  * `:env` - The environment to run the dependency on, defaults to :prod
  * `:compile` - A command to compile the dependency, defaults to a mix,
                 rebar or make command
  * `:optional` - The dependency is optional and used only to specify requirements
  * `:only` - The dependency will belongs only to the given environments, useful
              when declaring dev- or test-only dependencies
  * `:override` - If set to true the dependency will override any other
                  definitions of itself by other dependencies

  ## Git options (`:git`)

  * `:git`        - The git repository URI
  * `:github`     - A shortcut for specifying git repos from github, uses `git:`
  * `:ref`        - The reference to checkout (may be a branch, a commit sha or a tag)
  * `:branch`     - The git branch to checkout
  * `:tag`        - The git tag to checkout
  * `:submodules` - When true, initialize submodules for the repo

  ## Path options (`:path`)

  * `:path` - The path for the dependency
  * `:in_umbrella` - When true, sets a path dependency pointing to "../#{app}",
                     sharing the same environment as the current application

  ## mix deps task

  This taslk lists all dependencies in the following format:

    * APP VERSION (SCM)
      [locked at REF]
      STATUS

  It supports the following options:

  * `--all` - check all dependencies, regardless of specified environment

  """
  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, _, _ } = OptionParser.parse(args)

    if opts[:all] do
      loaded_opts = []
    else
      loaded_opts = [env: Mix.env]
    end

    shell = Mix.shell
    lock  = Mix.Dep.Lock.read

    Enum.each loaded(loaded_opts), fn %Mix.Dep{scm: scm} = dep ->
      dep = check_lock(dep, lock)
      shell.info "* #{format_dep(dep)}"
      if formatted = scm.format_lock(dep.opts) do
        shell.info "  locked at #{formatted}"
      end
      shell.info "  #{format_status dep}"
    end
  end
end
