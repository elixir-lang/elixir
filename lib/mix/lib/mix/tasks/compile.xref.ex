defmodule Mix.Tasks.Compile.Xref do
  use Mix.Task
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
    * `--warnings-as-errors` - treat warnings as errors and return a non-zero exit code

  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} =
      OptionParser.parse(args, switches: [force: :boolean, warnings_as_errors: :boolean])

    if Mix.Utils.stale?(E.manifests(), manifests()) or opts[:force] do
      if Mix.Task.run("xref", args) == :ok do
        write_manifest()
      else
        if opts[:warnings_as_errors] do
          exit({:shutdown, 1})
        else
          write_manifest()
        end
      end
    else
      :noop
    end
  end

  @doc """
  Returns xref manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  def write_manifest do
    data = {@manifest_vsn, System.version}
    File.mkdir_p!(Mix.Project.manifest_path())
    File.write!(manifest(), :io_lib.format('~p.~n', [data]))
    :ok
  end

  @doc """
  Cleans up xref manifest.
  """
  def clean do
    File.rm manifest()
  end
end
