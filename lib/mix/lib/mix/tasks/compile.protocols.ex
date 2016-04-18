defmodule Mix.Tasks.Compile.Protocols do
  use Mix.Task

  @manifest ".compile.protocols"
  @manifest_vsn :v2

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

    output   = default_path(config)
    manifest = Path.join(output, @manifest)

    protocols_and_impls =
      unless Mix.Project.umbrella?(config) do
        protocols_and_impls(config)
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
  def default_path(config \\ Mix.Project.config) do
    Path.join(Mix.Project.build_path(config), "consolidated")
  end

  defp protocols_and_impls(config) do
    deps = for(%{scm: scm, opts: opts} <- Mix.Dep.cached(),
               not scm.fetchable?,
               do: opts[:build])

    app = Mix.Project.app_path(config)

    protocols_and_impls =
      for path <- [app | deps] do
        elixir = Path.join(path, ".compile.elixir")
        Mix.Compilers.Elixir.protocols_and_impls(elixir)
      end

    Enum.concat(protocols_and_impls)
  end

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
      {:ok, [@manifest_vsn | t]} -> t
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
    modified = Mix.Utils.last_modified(manifest)
    old_metadata = read_manifest(manifest)

    protocols =
      for {protocol, :protocol, beam} <- new_metadata,
          Mix.Utils.last_modified(beam) > modified,
          remove_consolidated(protocol, output),
          do: {protocol, true},
          into: %{}

    protocols =
      Enum.reduce(new_metadata -- old_metadata, protocols, fn
        {_, {:impl, protocol}, _beam}, protocols ->
          Map.put(protocols, protocol, true)
        {protocol, :protocol, _beam}, protocols ->
          Map.put(protocols, protocol, true)
      end)

    protocols =
      Enum.reduce(old_metadata -- new_metadata, protocols, fn
        {_, {:impl, protocol}, _beam}, protocols ->
          Map.put(protocols, protocol, true)
        {protocol, :protocol, _beam}, protocols ->
          remove_consolidated(protocol, output)
          protocols
      end)

    Map.keys(protocols)
  end

  defp remove_consolidated(protocol, output) do
    File.rm Path.join(output, "#{protocol}.beam")
  end
end
