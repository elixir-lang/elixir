defmodule Mix.Tasks.Compile.Xref do
  use Mix.Task.Compiler
  alias Mix.Tasks.Compile.Elixir, as: E

  @recursive true
  @manifest "compile.xref"
  @manifest_vsn 1

  @moduledoc """
  Performs remote dispatch checking.

  It uses `mix xref` to check if any remote call does not exist or is
  deprecated, and emits warnings in such cases. This task does not show
  deprecated local calls (a call to a deprecated function or macro in the
  same module) nor calls to deprecated functionality in Elixir itself.

  When this task runs, it will check if the source code has been modified.
  If it has changed, `mix xref` will be run to check remote dispatches. You
  can force checking regardless of modification time by passing the `--force`
  option.

  ## Command line options

    * `--force` - forces checking regardless of modification time
    * `--warnings-as-errors` - treats warnings as errors and returns a non-zero exit code
    * `--all-warnings` - prints warnings even from files that do not need to be recompiled

  """

  @switches [all_warnings: :boolean, force: :boolean, warnings_as_errors: :boolean]

  @impl true
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    Mix.Task.run("compile")

    warnings =
      if needs_xref?(opts) do
        run_xref()
      else
        read_manifest()
      end

    if warnings != [] and warnings_as_errors(opts) do
      {:error, to_diagnostics(warnings, :error)}
    else
      {:noop, to_diagnostics(warnings, :warning)}
    end
  end

  defp needs_xref?(opts) do
    # If all warnings is given, it is easier to check everything.
    !!opts[:force] or !!opts[:all_warnings] or Mix.Utils.stale?(E.manifests(), manifests())
  end

  defp run_xref do
    timestamp = System.os_time(:second)
    warnings = Mix.Tasks.Xref.warnings([])
    write_manifest(warnings, timestamp)
    warnings
  end

  defp warnings_as_errors(opts) do
    Keyword.get_lazy(opts, :warnings_as_errors, fn ->
      Mix.Project.config()[:elixirc_options][:warnings_as_errors]
    end)
  end

  @impl true
  def manifests, do: [manifest()]

  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  defp write_manifest(warnings, timestamp) do
    manifest = manifest()
    File.mkdir_p!(Path.dirname(manifest))
    File.write!(manifest, :erlang.term_to_binary({@manifest_vsn, warnings}))
    File.touch(manifest, timestamp)
  end

  defp read_manifest() do
    try do
      manifest() |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ -> []
    else
      {@manifest_vsn, data} when is_list(data) -> data
      _ -> []
    end
  end

  @impl true
  def clean do
    File.rm(manifest())
  end

  defp to_diagnostics(warnings, severity) do
    for {message, locations} <- warnings,
        {file, line} <- locations do
      %Mix.Task.Compiler.Diagnostic{
        compiler_name: "Xref",
        file: Path.absname(file),
        message: to_string(message),
        position: line,
        severity: severity
      }
    end
  end
end
