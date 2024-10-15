defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  import Mix.Dep, only: [format_dep: 1, format_status: 1, check_lock: 1]

  @moduledoc """
  Checks, compiles, and loads all dependencies along the way.

  If there is an invalid dependency, its status is printed
  before aborting.

  Although this task does not show up in `mix help`, it is
  part of Mix public API and can be depended on.

  ## Configuration

    * `:listeners` - the list of listener modules. For more details
      see `Mix.Task.Compiler`

  ## Command line options

    * `--no-archives-check` - does not check archives
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check or compile deps, only load available ones
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-optional-deps` - does not compile or load optional deps

  """

  @impl true
  def run(args) do
    # Note that we need to ensure the dependencies are compiled first,
    # before we can start the pub/sub listeners, since those come from
    # the dependencies. Theoretically, between compiling dependencies
    # and starting the listeners, there may be a concurrent compilation
    # of the dependencies, which we would miss, and we would already
    # have modules from our compilation loaded. To avoid this race
    # condition we start the pub/sub beforehand and we accumulate all
    # events until the listeners are started. Alternatively we could
    # use a lock around compilation and sterning the listeners, however
    # the added benefit of the current approach is that we consistently
    # receive events for all dependency compilations. Also, if we ever
    # decide to start the listeners later (e.g. after loadspaths), the
    # accumulation approach still works.
    Mix.PubSub.start()

    if "--no-archives-check" not in args do
      Mix.Task.run("archive.check", args)
    end

    all = Mix.Dep.load_and_cache()

    all =
      if "--no-optional-deps" in args do
        for dep <- all, dep.opts[:optional] != true, do: dep
      else
        all
      end

    config = Mix.Project.config()

    if "--no-elixir-version-check" not in args do
      check_elixir_version(config)
    end

    if "--no-deps-check" not in args do
      deps_check(all, "--no-compile" in args)
    end

    Code.prepend_paths(Enum.flat_map(all, &Mix.Dep.load_paths/1), cache: true)

    # For now we only allow listeners defined in dependencies, so
    # we start them right after adding adding deps to the path
    Mix.PubSub.start_listeners()

    :ok
  end

  defp check_elixir_version(config) do
    if req = config[:elixir] do
      case Version.parse_requirement(req) do
        {:ok, req} ->
          if not Version.match?(System.version(), req) do
            raise Mix.ElixirVersionError,
              target: config[:app] || Mix.Project.get(),
              expected: req,
              actual: System.version()
          end

        :error ->
          Mix.raise("Invalid Elixir version requirement #{req} in mix.exs file")
      end
    end
  end

  defp deps_check(all, no_compile?) do
    all = Enum.map(all, &check_lock/1)
    {not_ok, compile} = partition(all, [], [])

    cond do
      not_ok != [] ->
        show_not_ok!(not_ok)

      compile == [] or no_compile? ->
        :ok

      true ->
        Mix.Tasks.Deps.Compile.compile(compile)

        compile
        |> Enum.map(& &1.app)
        |> Mix.Dep.filter_by_name(Mix.Dep.load_and_cache())
        |> Enum.filter(&(not Mix.Dep.ok?(&1)))
        |> show_not_ok!()
    end
  end

  defp partition([dep | deps], not_ok, compile) do
    cond do
      Mix.Dep.compilable?(dep) or (Mix.Dep.ok?(dep) and local?(dep)) ->
        if from_umbrella?(dep) do
          partition(deps, not_ok, compile)
        else
          partition(deps, not_ok, [dep | compile])
        end

      Mix.Dep.ok?(dep) ->
        partition(deps, not_ok, compile)

      true ->
        partition(deps, [dep | not_ok], compile)
    end
  end

  defp partition([], not_ok, compile) do
    {Enum.reverse(not_ok), Enum.reverse(compile)}
  end

  # Those are compiled by umbrella.
  defp from_umbrella?(dep) do
    dep.opts[:from_umbrella]
  end

  # Every local dependency (i.e. that are not fetchable)
  # are automatically recompiled if they are ok.
  defp local?(dep) do
    not dep.scm.fetchable?()
  end

  defp show_not_ok!([]) do
    :ok
  end

  defp show_not_ok!(deps) do
    shell = Mix.shell()
    shell.error("Unchecked dependencies for environment #{Mix.env()}:")

    Enum.each(deps, fn dep ->
      shell.error("* #{format_dep(dep)}")
      shell.error("  #{format_status(dep)}")
    end)

    Mix.raise("Can't continue due to errors on dependencies")
  end
end
