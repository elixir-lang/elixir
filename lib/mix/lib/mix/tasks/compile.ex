defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compiles source files"

  @moduledoc """
  A meta task that compiles source files.

  It simply runs the compilers registered in your project.

  ## Configuration

    * `:compilers` - compilers to run, defaults to:
      `[:yeec, :leex, :erlang, :elixir, :app]`

    * `:consolidate_protocols` - when `true`, runs protocol
      consolidation via the `compile.protocols` task

    * `:build_embedded` - when `true`, activates protocol
      consolidation and does not generate symlinks in builds

    * `:build_path` - the directory where build artifacts
      should be written to. This option is intended only for
      child apps within a larger umbrella application so that
      each child app can use the common `_build` directory of
      the parent umbrella. In a non-umbrella context, configuring
      this has undesirable side-effects (such as skipping some
      compiler checks) and should be avoided.

  ## Compilers

  To see documentation for each specific compiler, you must
  invoke `help` directly for the compiler command:

      mix help compile.elixir
      mix help compile.erlang

  You can get a list of all compilers by running:

      mix compile --list

  ## Command line options

    * `--list`          - list all enabled compilers
    * `--no-deps-check` - skip checking of dependencies
    * `--force`         - force compilation

  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(["--list"]) do
    loadpaths!
    _ = Mix.Task.load_all

    shell   = Mix.shell
    modules = Mix.Task.all_modules

    docs = for module <- modules,
               task = Mix.Task.task_name(module),
               match?("compile." <> _, task),
               doc = Mix.Task.moduledoc(module) do
      {task, first_line(doc)}
    end

    max = Enum.reduce docs, 0, fn({task, _}, acc) ->
      max(byte_size(task), acc)
    end

    sorted = Enum.sort(docs)

    Enum.each sorted, fn({task, doc}) ->
      shell.info format('mix ~-#{max}s # ~ts', [task, doc])
    end

    compilers = compilers() ++ if(consolidate_protocols?(), do: [:protocols], else: [])
    shell.info "\nEnabled compilers: #{Enum.join compilers, ", "}"
    :ok
  end

  def run(args) do
    Mix.Project.get!
    Mix.Task.run "loadpaths", args

    if local_deps_changed?() do
      Mix.Dep.Lock.touch_manifest
    end

    res = Mix.Task.run "compile.all", args
    res = if :ok in List.wrap(res), do: :ok, else: :noop

    if res == :ok && consolidate_protocols?() do
      Mix.Task.run "compile.protocols", args
    end

    res
  end

  # Loadpaths without checks because compilers may be defined in deps.
  defp loadpaths! do
    Mix.Task.run "loadpaths", ["--no-elixir-version-check", "--no-deps-check"]
    Mix.Task.reenable "loadpaths"
  end

  defp consolidate_protocols? do
    Mix.Project.config[:consolidate_protocols]
  end

  defp local_deps_changed? do
    manifest = Path.absname(Mix.Dep.Lock.manifest())

    Enum.any?(Mix.Dep.children(), fn %{scm: scm} = dep ->
      not scm.fetchable? and Mix.Dep.in_dependency(dep, fn _ ->
        files = Mix.Project.config_files ++ manifests()
        Mix.Utils.stale?(files, [manifest])
      end)
    end)
  end

  @doc """
  Returns all compilers.
  """
  def compilers do
    Mix.Project.config[:compilers] || Mix.compilers
  end

  @doc """
  Returns manifests for all compilers.
  """
  def manifests do
    Enum.flat_map(compilers(), fn(compiler) ->
      module = Mix.Task.get("compile.#{compiler}")
      if module && function_exported?(module, :manifests, 0) do
        module.manifests
      else
        []
      end
    end)
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> IO.iodata_to_binary
  end

  defp first_line(doc) do
    String.split(doc, "\n", parts: 2) |> hd |> String.strip |> String.rstrip(?.)
  end
end
