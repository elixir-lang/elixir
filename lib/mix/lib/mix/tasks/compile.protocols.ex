defmodule Mix.Tasks.Compile.Protocols do
  use Mix.Task

  @moduledoc ~S"""
  Consolidates all protocols in all paths.

  This task is automatically invoked whenever the project
  enables `:consolidate_protocols` or `:build_embedded` in
  its configuration.

  ## Consolidation

  Protocol consolidation is useful in production when no
  dynamic code loading will happen, effectively optimizing
  protocol dispatches by not accounting for code loading.

  This task consolidates all protocols in the code path
  and output the new binary files to the given directory
  (defaults to "_build/MIX_ENV/consolidated").

  In case you are manually compiling protocols or building
  releases, you need to take the generated protocols into
  account. This can be done with:

      $ elixir -pa _build/MIX_ENV/consolidated -S mix run

  You can verify a protocol is consolidated by checking
  its attributes:

      $ iex -pa _build/MIX_ENV/consolidated -S mix run
      iex> Protocol.consolidated?(Enumerable)
      true

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Task.run "compile", args
    {opts, _, _} = OptionParser.parse(args, switches: [output: :string], aliases: [o: :output])

    paths = filter_otp(:code.get_path, :code.lib_dir)
    paths
    |> Protocol.extract_protocols
    |> consolidate(paths, opts[:output] || default_path())

    :ok
  end

  @doc false
  def default_path, do: Path.join(Mix.Project.build_path, "consolidated")

  defp filter_otp(paths, otp) do
    Enum.filter(paths, &(not :lists.prefix(&1, otp)))
  end

  defp consolidate(protocols, paths, output) do
    File.mkdir_p!(output)

    _ = for protocol <- protocols do
      impls = Protocol.extract_impls(protocol, paths)
      maybe_reload(protocol)
      {:ok, binary} = Protocol.consolidate(protocol, impls)
      File.write!(Path.join(output, "#{protocol}.beam"), binary)
      Mix.shell.info "Consolidated #{inspect protocol}"
    end

    relative = Path.relative_to_cwd(output)
    Mix.shell.info "Consolidated protocols written to #{relative}"
  end

  defp maybe_reload(module) do
    case :code.which(module) do
      atom when is_atom(atom) ->
        # Module is likely in memory, we purge as an attempt to reload it
        :code.purge(module)
        :code.delete(module)
        :ok
      _file ->
        :ok
    end
  end

  def clean do
    File.rm_rf(default_path)
  end
end
