defmodule Mix.Tasks.Compile.Xref do
  use Mix.Task
  alias Mix.Tasks.Compile.Elixir, as: E

  @recursive true
  @manifest ".compile.xref"

  @moduledoc """
  Performs remote dispatch checking.

  When this task runs, it will check the modification time of the `:elixir`
  compiler manifest. If it has changed, `mix xref` will be run to check remote
  dispatches. You can force checking regardless of modification time by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces checking regardless of modification time
    * `--warnings-as-errors` - treat warnings as errors and return a non-zero exit code

  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} =
      OptionParser.parse(args, switches: [force: :boolean, warnings_as_errors: :boolean])

    config = Mix.Project.config()
    warnings_as_errors =
      (opts[:warnings_as_errors] != nil) or (config[:elixirc_options][:warnings_as_errors] != nil)

    if needs_xref?(opts) and should_exit?(Mix.Task.run("xref", args), warnings_as_errors) do
      exit({:shutdown, 1})
    end

    write_manifest()
  end

  defp needs_xref?(opts) do
    Mix.Utils.stale?(E.manifests(), manifests()) or (opts[:force] != nil)
  end

  defp should_exit?(:error, true),
    do: true
  defp should_exit?(_, _),
    do: false

  @doc """
  Returns xref manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  defp write_manifest do
    # Only write the manifest if the manifest path exists,
    # because if it doesn't we don't need one.
    if File.exists?(Mix.Project.manifest_path()),
      do: File.write!(manifest(), "")

    :noop
  end

  @doc """
  Cleans up xref manifest.
  """
  def clean do
    File.rm manifest()
  end
end
