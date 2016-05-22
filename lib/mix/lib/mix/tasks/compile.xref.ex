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

    warnings_as_errors =
      Keyword.get_lazy(opts, :warnings_as_errors, fn ->
        Mix.Project.config()[:elixirc_options][:warnings_as_errors]
      end)

    if needs_xref?(opts) && should_exit?(Mix.Task.run("xref", ["--warnings"]), warnings_as_errors) do
      exit({:shutdown, 1})
    end

    write_manifest()
  end

  defp needs_xref?(opts) do
    opts[:force] || Mix.Utils.stale?(E.manifests(), manifests())
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
    File.touch(manifest())
    :noop
  end

  @doc """
  Cleans up xref manifest.
  """
  def clean do
    File.rm manifest()
  end
end
