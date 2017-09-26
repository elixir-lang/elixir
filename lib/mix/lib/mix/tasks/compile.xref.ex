defmodule Mix.Tasks.Compile.Xref do
  use Mix.Task.Compiler
  alias Mix.Tasks.Compile.Elixir, as: E

  @recursive true
  @manifest ".compile.xref"
  @manifest_vsn :v1

  @moduledoc """
  Performs remote dispatch checking.

  When this task runs, it will check the modification time of the `:elixir`
  compiler manifest. If it has changed, `mix xref` will be run to check remote
  dispatches. You can force checking regardless of modification time by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces checking regardless of modification time
    * `--warnings-as-errors` - treats warnings as errors and returns a non-zero exit code

  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} =
      OptionParser.parse(args, switches: [force: :boolean, warnings_as_errors: :boolean])

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
    !!opts[:force] or Mix.Utils.stale?(E.manifests(), manifests())
  end

  defp run_xref do
    timestamp = :calendar.universal_time()
    case Mix.Task.run("xref", ["warnings"]) do
      :noop ->
        []
      {:ok, warnings} ->
        write_manifest(warnings, timestamp)
        warnings
    end
  end

  defp warnings_as_errors(opts) do
    Keyword.get_lazy(opts, :warnings_as_errors, fn ->
      Mix.Project.config()[:elixirc_options][:warnings_as_errors]
    end)
  end

  @doc """
  Returns xref manifests.
  """
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  defp write_manifest(warnings, timestamp) do
    File.write!(manifest(), :erlang.term_to_binary({@manifest_vsn, warnings}))
    File.touch(manifest(), timestamp)
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

  @doc """
  Cleans up xref manifest.
  """
  def clean do
    File.rm manifest()
  end

  defp to_diagnostics(warnings, severity) do
    for {file, lines, message} <- warnings,
        line <- lines do
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
