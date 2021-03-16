defmodule Mix.Tasks.Compile do
  use Mix.Task.Compiler

  @shortdoc "Compiles source files"

  @moduledoc """
  The main entry point to compile source files.

  It simply runs the compilers registered in your project and returns
  a tuple with the compilation status and a list of diagnostics.

  Before compiling code, it loads the code in all dependencies and
  perform a series of checks to ensure the project is up to date.

  ## Configuration

    * `:compilers` - compilers to run, defaults to `Mix.compilers/0`,
      which are `[:yecc, :leex, :erlang, :elixir, :app]`.

    * `:consolidate_protocols` - when `true`, runs protocol
      consolidation via the `mix compile.protocols` task. The default
      value is `true`.

    * `:build_embedded` - when `true`, embeds all code and priv
      content in the `_build` directory instead of using symlinks.

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

    * `--erl-config` - path to an Erlang term file that will be loaded as Mix config
    * `--force` - forces compilation
    * `--list` - lists all enabled compilers
    * `--no-app-loading` - does not load applications (including from deps) before compiling
    * `--no-archives-check` - skips checking of archives
    * `--no-compile` - does not actually compile, only loads code and perform checks
    * `--no-deps-check` - skips checking of dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-protocol-consolidation` - skips protocol consolidation
    * `--no-validate-compile-env` - does not validate the application compile environment
    * `--return-errors` - returns error status and diagnostics instead of exiting on error
    * `--warnings-as-errors` - exit with non-zero status if compilation has one or more warnings

  """

  @doc """
  Returns all compilers.
  """
  def compilers(config \\ Mix.Project.config()) do
    compilers = config[:compilers] || Mix.compilers()

    if :xref in compilers do
      IO.warn(
        "the :xref compiler is deprecated, please remove it from your mix.exs :compilers options"
      )

      List.delete(compilers, :xref)
    else
      compilers
    end
  end

  @impl true
  def run(["--list"]) do
    # Loadpaths without checks because compilers may be defined in deps.
    args = ["--no-elixir-version-check", "--no-deps-check", "--no-archives-check"]
    Mix.Task.run("loadpaths", args)
    Mix.Task.reenable("loadpaths")
    Mix.Task.reenable("deps.loadpaths")

    # Compilers are tasks, so load all tasks available.
    _ = Mix.Task.load_all()

    shell = Mix.shell()
    modules = Mix.Task.all_modules()

    docs =
      for module <- modules,
          task = Mix.Task.task_name(module),
          match?("compile." <> _, task),
          doc = Mix.Task.moduledoc(module) do
        {task, first_line(doc)}
      end

    max =
      Enum.reduce(docs, 0, fn {task, _}, acc ->
        max(byte_size(task), acc)
      end)

    sorted = Enum.sort(docs)

    Enum.each(sorted, fn {task, doc} ->
      shell.info(format('mix ~-#{max}s # ~ts', [task, doc]))
    end)

    compilers = compilers() ++ if(consolidate_protocols?(:ok), do: [:protocols], else: [])
    shell.info("\nEnabled compilers: #{Enum.join(compilers, ", ")}")
    :ok
  end

  @impl true
  def run(args) do
    Mix.Project.get!()
    Mix.Task.run("loadpaths", args)

    {opts, _, _} = OptionParser.parse(args, switches: [erl_config: :string])
    load_erl_config(opts)

    {res, diagnostics} =
      Mix.Task.run("compile.all", args)
      |> List.wrap()
      |> Enum.map(&Mix.Task.Compiler.normalize(&1, :all))
      |> Enum.reduce({:noop, []}, &merge_diagnostics/2)

    config = Mix.Project.config()

    cond do
      "--no-compile" in args ->
        Mix.Task.reenable("compile")
        {:noop, []}

      config[:consolidate_protocols] and "--no-protocol-consolidation" not in args ->
        {consolidate_and_load_protocols(args, config, res), diagnostics}

      true ->
        {res, diagnostics}
    end
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> IO.iodata_to_binary()
  end

  defp first_line(doc) do
    String.split(doc, "\n", parts: 2) |> hd |> String.trim() |> String.trim_trailing(".")
  end

  defp merge_diagnostics({status1, diagnostics1}, {status2, diagnostics2}) do
    new_status =
      cond do
        status1 == :error or status2 == :error -> :error
        status1 == :ok or status2 == :ok -> :ok
        true -> :noop
      end

    {new_status, diagnostics1 ++ diagnostics2}
  end

  defp load_erl_config(opts) do
    if path = opts[:erl_config] do
      {:ok, terms} = :file.consult(path)
      Application.put_all_env(terms, persistent: true)
    end
  end

  @impl true
  def manifests do
    Enum.flat_map(compilers(), fn compiler ->
      module = Mix.Task.get("compile.#{compiler}")

      if module && function_exported?(module, :manifests, 0) do
        module.manifests
      else
        []
      end
    end)
  end

  ## Consolidation handling

  defp consolidate_protocols?(:ok), do: true
  defp consolidate_protocols?(:noop), do: not Mix.Tasks.Compile.Protocols.consolidated?()
  defp consolidate_protocols?(:error), do: false

  defp consolidate_and_load_protocols(args, config, res) do
    res =
      if consolidate_protocols?(res) do
        Mix.Task.run("compile.protocols", args)
        :ok
      else
        res
      end

    path = Mix.Project.consolidation_path(config)

    with {:ok, protocols} <- File.ls(path) do
      Code.prepend_path(path)
      Enum.each(protocols, &load_protocol/1)
    end

    res
  end

  defp load_protocol(file) do
    case file do
      "Elixir." <> _ ->
        module = file |> Path.rootname() |> String.to_atom()
        :code.purge(module)
        :code.delete(module)

      _ ->
        :ok
    end
  end
end
