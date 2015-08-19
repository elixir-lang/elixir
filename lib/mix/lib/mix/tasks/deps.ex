defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Dep, only: [loaded: 1, format_dep: 1, format_status: 1, check_lock: 2]

  @shortdoc "List dependencies and their status"

  @moduledoc ~S"""
  List all dependencies and their status.

  Dependencies must be specified in the `mix.exs` file in one of
  the following formats:

      {app, requirement}
      {app, opts}
      {app, requirement, opts}

  Where:

    * app is an atom
    * requirement is a version requirement or a regular expression
    * opts is a keyword list of options

  By default, dependencies are fetched using the [Hex package manager](https://hex.pm/):

      {:plug, ">= 0.4.0"}

  By specifying such dependencies, Mix will automatically install
  Hex (if it wasn't previously installed and download a package
  suitable to your project).

  Mix also supports git and path dependencies:

      {:foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1"}
      {:foobar, path: "path/to/foobar"}

  And also in umbrella dependencies:

      {:myapp, in_umbrella: true}

  Path and in umbrella dependencies are automatically recompiled by
  the parent project whenever they change. While fetchable dependencies
  like git are recompiled only when fetched/updated.

  The dependencies versions are expected to follow Semantic Versioning
  and the requirements must be specified as defined in the `Version`
  module.

  Below we provide a more detailed look into the available options.

  ## Mix options

    * `:app` - when set to `false`, does not read the app file for this
      dependency

    * `:env` - the environment to run the dependency on, defaults to :prod

    * `:compile` - a command to compile the dependency, defaults to a mix,
      rebar or make command

    * `:optional` - the dependency is optional and used only to specify
      requirements

    * `:only` - the dependency will belong only to the given environments,
      useful when declaring dev- or test-only dependencies

    * `:override` - if set to `true` the dependency will override any other
      definitions of itself by other dependencies

  ## Git options (`:git`)

    * `:git`        - the git repository URI
    * `:github`     - a shortcut for specifying git repos from github, uses `git:`
    * `:ref`        - the reference to checkout (may be a branch, a commit sha or a tag)
    * `:branch`     - the git branch to checkout
    * `:tag`        - the git tag to checkout
    * `:submodules` - when `true`, initialize submodules for the repo

  ## Mercurial options (`:hg`)

    * `:hg`         - the hg repository URI
    * `:bitbucket`  - a shortcut for specifying hg repos from Bitbucket, uses `hg:`
    * `:ref`        - the reference to checkout (may be a branch, a commit sha or a tag)
    * `:branch`     - the Hg branch to checkout
    * `:tag`        - the Hg tag to checkout

  ## Path options (`:path`)

    * `:path`        - the path for the dependency
    * `:in_umbrella` - when `true`, sets a path dependency pointing to
      "../#{app}", sharing the same environment as the current application

  ## mix deps task

  This task lists all dependencies in the following format:

      APP VERSION (SCM)
      [locked at REF]
      STATUS

  It supports the following options:

    * `--all` - check all dependencies, regardless of specified environment

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    {opts, _, _} = OptionParser.parse(args)

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
