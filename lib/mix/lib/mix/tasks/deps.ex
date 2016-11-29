defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Dep, only: [loaded: 1, format_dep: 1, format_status: 1, check_lock: 1]

  @shortdoc "Lists dependencies and their status"

  @moduledoc ~S"""
  Lists all dependencies and their status.

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
  Hex (if it wasn't previously installed) and download a package
  suitable to your project.

  Mix also supports Git and path dependencies:

      {:foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1"}
      {:foobar, path: "path/to/foobar"}

  And also in umbrella dependencies:

      {:my_app, in_umbrella: true}

  Path and in umbrella dependencies are automatically recompiled by
  the parent project whenever they change. While fetchable dependencies
  like the ones using `:git` are recompiled only when fetched/updated.

  The dependencies' versions are expected to follow Semantic Versioning
  and the requirements must be specified as defined in the `Version`
  module.

  ## Options

  Below we provide a more detailed look into the available options.

  ### Dependency definition options

    * `:app` - when set to `false`, does not read the app file for this
      dependency. By default, the app file is read

    * `:env` - the environment (as an atom) to run the dependency on; defaults to `:prod`

    * `:compile` - a command (string) to compile the dependency; defaults to a `mix`,
      `rebar` or `make` command

    * `:optional` - marks the dependency as optional. In such cases, the
      current project will always include the optional dependency but any
      other project that depends on the current project won't be forced to
      use the optional dependency. However, if the other project includes
      the optional dependency on its own, the requirements and options
      specified here will also be applied.

    * `:only` - the dependency is made available only in the given environments,
      useful when declaring dev- or test-only dependencies; by default the
      dependency will be available in all environments. The value of this option
      can either be a single environment (like `:dev`) or a list of environments
      (like `[:dev, :test]`)

    * `:override` - if set to `true` the dependency will override any other
      definitions of itself by other dependencies

    * `:manager` - Mix can also compile Rebar, Rebar3 and makefile projects
      and can fetch sub dependencies of Rebar and Rebar3 projects. Mix will
      try to infer the type of project but it can be overridden with this
      option by setting it to `:mix`, `:rebar`, `:rebar3` or `:make`

    * `:runtime` - whether the dependency is part of runtime applications.
      Defaults to `true` which automatically adds the application to the list
      of apps that are started automatically and included in releases

  ### Git options (`:git`)

    * `:git`        - the Git repository URI
    * `:github`     - a shortcut for specifying Git repos from GitHub, uses `git:`
    * `:ref`        - the reference to checkout (may be a branch, a commit SHA or a tag)
    * `:branch`     - the Git branch to checkout
    * `:tag`        - the Git tag to checkout
    * `:submodules` - when `true`, initialize submodules for the repo
    * `:sparse`     - checkout a single directory inside the Git repository and use it
      as your Mix dependency. Search "sparse git checkouts" for more information.

  ### Path options (`:path`)

    * `:path`        - the path for the dependency
    * `:in_umbrella` - when `true`, sets a path dependency pointing to
      "../#{app}", sharing the same environment as the current application

  ## Deps task

  `mix deps` task lists all dependencies in the following format:

      APP VERSION (SCM)
      [locked at REF]
      STATUS

  It supports the following options:

    * `--all` - checks all dependencies, regardless of specified environment

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    {opts, _, _} = OptionParser.parse(args)
    loaded_opts  = if opts[:all], do: [], else: [env: Mix.env]

    shell = Mix.shell

    Enum.each loaded(loaded_opts), fn %Mix.Dep{scm: scm, manager: manager} = dep ->
      dep   = check_lock(dep)
      extra = if manager, do: " (#{manager})", else: ""

      shell.info "* #{format_dep(dep)}#{extra}"
      if formatted = scm.format_lock(dep.opts) do
        shell.info "  locked at #{formatted}"
      end

      shell.info "  #{format_status dep}"
    end
  end
end
