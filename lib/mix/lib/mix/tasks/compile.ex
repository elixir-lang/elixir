defmodule Mix.Tasks.Compile do
  use Mix.Task.Compiler

  @shortdoc "Compiles source files"

  @moduledoc """
  A meta task that compiles source files.

  It simply runs the compilers registered in your project and returns
  a tuple with the compilation status and a list of diagnostics.

  ## Configuration

    * `:compilers` - compilers to run, defaults to `Mix.compilers/0`,
      which are `[:yecc, :leex, :erlang, :elixir, :xref, :app]`.

    * `:consolidate_protocols` - when `true`, runs protocol
      consolidation via the `compile.protocols` task. The default
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

    * `--list` - lists all enabled compilers
    * `--no-archives-check` - skips checking of archives
    * `--no-deps-check` - skips checking of dependencies
    * `--no-protocol-consolidation` - skips protocol consolidation
    * `--force` - forces compilation
    * `--return-errors` - returns error status and diagnostics instead of exiting on error
    * `--erl-config` - path to an Erlang term file that will be loaded as Mix config

  """

  @impl true
  def run(["--list"]) do
    loadpaths!()
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

    if res == :error and "--return-errors" not in args do
      exit({:shutdown, 1})
    end

    res =
      if consolidate_protocols?(res) and "--no-protocol-consolidation" not in args do
        Mix.Task.run("compile.protocols", args)
        :ok
      else
        res
      end

    {res, diagnostics}
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

  # Loadpaths without checks because compilers may be defined in deps.
  defp loadpaths! do
    args = ["--no-elixir-version-check", "--no-deps-check", "--no-archives-check"]
    Mix.Task.run("loadpaths", args)
    Mix.Task.reenable("loadpaths")
    Mix.Task.reenable("deps.loadpaths")
  end

  defp consolidate_protocols?(:ok) do
    Mix.Project.config()[:consolidate_protocols]
  end

  defp consolidate_protocols?(:noop) do
    config = Mix.Project.config()
    config[:consolidate_protocols] and not Mix.Tasks.Compile.Protocols.consolidated?()
  end

  defp consolidate_protocols?(:error) do
    false
  end

  @doc """
  Returns all compilers.
  """
  def compilers do
    Mix.Project.config()[:compilers] || Mix.compilers()
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

  defp format(expression, args) do
    :io_lib.format(expression, args) |> IO.iodata_to_binary()
  end

  defp first_line(doc) do
    String.split(doc, "\n", parts: 2) |> hd |> String.trim() |> String.trim_trailing(".")
  end

  defp load_erl_config(opts) do
    if path = opts[:erl_config] do
      {:ok, terms} = :file.consult(path)
      Application.put_all_env(terms, persistent: true)
    end
  end
end
