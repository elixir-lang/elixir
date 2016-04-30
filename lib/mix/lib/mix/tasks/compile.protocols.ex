defmodule Mix.Tasks.Compile.Protocols do
  use Mix.Task

  @manifest ".compile.protocols"
  @manifest_vsn :v1

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
    config = Mix.Project.config
    Mix.Task.run "compile", args
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    output = default_path()
    manifest = Path.join(output, @manifest)
    protocols_and_impls =
      unless Mix.Project.umbrella?(config) do
        Mix.Tasks.Compile.Elixir.protocols_and_impls
      end

    cond do
      opts[:force] || Mix.Utils.stale?(Mix.Project.config_files, [manifest]) ->
        clean()
        paths = consolidation_paths()
        paths
        |> Protocol.extract_protocols
        |> consolidate(paths, output, manifest, protocols_and_impls)

      protocols_and_impls ->
        manifest
        |> diff_manifest(protocols_and_impls, output)
        |> consolidate(consolidation_paths(), output, manifest, protocols_and_impls)

      true ->
        :noop
    end
  end

  @doc """
  Clean up consolidated protocols.
  """
  def clean do
    File.rm_rf(default_path)
  end

  @doc false
  def default_path, do: Path.join(Mix.Project.build_path, "consolidated")

  defp consolidation_paths do
    filter_otp(:code.get_path, :code.lib_dir)
  end

  defp filter_otp(paths, otp) do
    Enum.filter(paths, &(not :lists.prefix(&1, otp)))
  end

  defp consolidate([], _paths, output, manifest, metadata) do
    File.mkdir_p!(output)
    write_manifest(manifest, metadata)
    :noop
  end

  defp consolidate(protocols, paths, output, manifest, metadata) do
    File.mkdir_p!(output)

    protocols
    |> Enum.uniq()
    |> Enum.map(&Task.async(fn -> consolidate(&1, paths, output) end))
    |> Enum.map(&Task.await(&1, 30_000))

    write_manifest(manifest, metadata)
    :ok
  end

  defp consolidate(protocol, paths, output) do
    impls = Protocol.extract_impls(protocol, paths)
    reload(protocol)
    {:ok, binary} = Protocol.consolidate(protocol, impls)
    File.write!(Path.join(output, "#{protocol}.beam"), binary)
    Mix.shell.info "Consolidated #{inspect protocol}"
  end

  defp reload(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defp read_manifest(manifest) do
    case :file.consult(manifest) do
      {:ok, [@manifest_vsn|t]} -> t
      _ -> []
    end
  end

  defp write_manifest(_manifest, nil), do: :om
  defp write_manifest(manifest, metadata) do
    File.open!(manifest, [:write], fn device ->
      :io.format(device, '~p.~n', [@manifest_vsn])
      Enum.map metadata, fn entry ->
        :io.format(device, '~p.~n', [entry])
      end
      :ok
    end)
  end

  defp diff_manifest(manifest, new_metadata, output) do
    old_metadata = read_manifest(manifest)

    additions =
      Enum.flat_map(new_metadata -- old_metadata, fn
        {_, {:impl, protocol}} -> [protocol]
        {protocol, :protocol} -> [protocol]
      end)

    removals =
      Enum.flat_map(old_metadata -- new_metadata, fn
        {_, {:impl, protocol}} -> [protocol]
        {protocol, :protocol} ->
          remove_consolidated(protocol, output)
          []
      end)

    additions ++ removals
  end

  defp remove_consolidated(protocol, output) do
    File.rm Path.join(output, "#{protocol}.beam")
  end
end
