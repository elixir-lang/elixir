defmodule Mix.Tasks.Compile.Protocols do
  use Mix.Task.Compiler

  @manifest "compile.protocols"
  @manifest_vsn 3

  @moduledoc ~S"""
  Consolidates all protocols in all paths.

  This task is automatically invoked unless the project
  disables the `:consolidate_protocols` option in their
  configuration.

  ## Consolidation

  Protocol consolidation is useful in production when no
  dynamic code loading will happen, effectively optimizing
  protocol dispatches by not accounting for code loading.

  This task consolidates all protocols in the code path
  and outputs the new binary files to the given directory.
  Defaults to "_build/MIX_ENV/lib/YOUR_APP/consolidated"
  for regular apps and "_build/MIX_ENV/consolidated" in
  umbrella projects.

  In case you are manually compiling protocols or building
  releases, you need to take the generated protocols into
  account. This can be done with:

      $ elixir -pa _build/MIX_ENV/lib/YOUR_APP/consolidated -S mix run

  Or in umbrellas:

      $ elixir -pa _build/MIX_ENV/consolidated -S mix run

  You can verify a protocol is consolidated by checking
  its attributes:

      iex> Protocol.consolidated?(Enumerable)
      true

  """

  @impl true
  def run(args) do
    config = Mix.Project.config()
    Mix.Task.run("compile")
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean, verbose: :boolean])

    manifest = manifest()
    output = Mix.Project.consolidation_path(config)
    config_mtime = Mix.Project.config_mtime()
    protocols_and_impls = protocols_and_impls(config)
    metadata = {config_mtime, protocols_and_impls}
    {old_config_mtime, old_protocols_and_impls} = read_manifest(manifest, output)

    cond do
      # We need to reconsolidate all protocols whenever the dependency changes
      # because we only track protocols from the current app and from local deps.
      #
      # We are only interested in the compile.lock from config_mtime (which is
      # a build artifact), so it would be fine to compare it directly against
      # the manifest, but let's follow best practices anyway.
      opts[:force] || config_mtime > old_config_mtime ->
        clean()
        paths = consolidation_paths()

        paths
        |> Protocol.extract_protocols()
        |> consolidate(paths, output, manifest, metadata, opts)

      protocols_and_impls ->
        protocols_and_impls
        |> diff_manifest(old_protocols_and_impls, output)
        |> consolidate(consolidation_paths(), output, manifest, metadata, opts)

      true ->
        :noop
    end
  end

  @impl true
  def clean do
    File.rm(manifest())
    File.rm_rf(Mix.Project.consolidation_path())
  end

  @impl true
  def manifests, do: [manifest()]

  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  @doc """
  Returns if protocols have been consolidated at least once.
  """
  def consolidated? do
    File.regular?(manifest())
  end

  defp protocols_and_impls(config) do
    deps = for %{scm: scm, opts: opts} <- Mix.Dep.cached(), not scm.fetchable?(), do: opts[:build]

    paths =
      if Mix.Project.umbrella?(config) do
        deps
      else
        [Mix.Project.app_path(config) | deps]
      end

    Mix.Compilers.Elixir.protocols_and_impls(paths)
  end

  defp consolidation_paths do
    filter_otp(:code.get_path(), :code.lib_dir())
  end

  defp filter_otp(paths, otp) do
    Enum.filter(paths, &(not :lists.prefix(otp, &1)))
  end

  defp consolidate([], _paths, output, manifest, metadata, _opts) do
    File.mkdir_p!(output)
    write_manifest(manifest, metadata)
    :noop
  end

  defp consolidate(protocols, paths, output, manifest, metadata, opts) do
    File.mkdir_p!(output)

    protocols
    |> Enum.uniq()
    |> Enum.map(&Task.async(fn -> consolidate(&1, paths, output, opts) end))
    |> Enum.map(&Task.await(&1, :infinity))

    write_manifest(manifest, metadata)
    :ok
  end

  defp consolidate(protocol, paths, output, opts) do
    impls = Protocol.extract_impls(protocol, paths)
    reload(protocol)

    case Protocol.consolidate(protocol, impls) do
      {:ok, binary} ->
        File.write!(Path.join(output, "#{Atom.to_string(protocol)}.beam"), binary)

        if opts[:verbose] do
          Mix.shell().info("Consolidated #{inspect_protocol(protocol)}")
        end

      # If we remove a dependency and we have implemented one of its
      # protocols locally, we will mark the protocol as needing to be
      # reconsolidated when the implementation is removed even though
      # the protocol no longer exists. Although most times removing a
      # dependency will trigger a full recompilation, such won't happen
      # in umbrella apps with shared build.
      {:error, :no_beam_info} ->
        remove_consolidated(protocol, output)

        if opts[:verbose] do
          Mix.shell().info("Unavailable #{inspect_protocol(protocol)}")
        end
    end
  end

  # We cannot use the inspect protocol while consolidating
  # since inspect may not be available.
  defp inspect_protocol(protocol) do
    Macro.inspect_atom(:literal, protocol)
  end

  defp reload(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defp read_manifest(manifest, output) do
    try do
      [@manifest_vsn | metadata] = manifest |> File.read!() |> :erlang.binary_to_term()
      metadata
    rescue
      _ ->
        # If there is no manifest or it is out of date, remove old files
        File.rm_rf(output)
        {0, {%{}, %{}}}
    end
  end

  defp write_manifest(manifest, metadata) do
    File.mkdir_p!(Path.dirname(manifest))
    manifest_data = :erlang.term_to_binary([@manifest_vsn | metadata], [:compressed])
    File.write!(manifest, manifest_data)
  end

  defp diff_manifest({new_protocols, new_impls}, {old_protocols, old_impls}, output) do
    protocols =
      new_protocols
      |> Enum.filter(fn {protocol, new_timestamp} ->
        case old_protocols do
          # There is a new version, removed the consolidated
          %{^protocol => old_timestamp} when new_timestamp > old_timestamp ->
            remove_consolidated(protocol, output)
            true

          # Nothing changed
          %{^protocol => _} ->
            false

          # New protocol
          %{} ->
            true
        end
      end)
      |> Map.new()

    protocols =
      for {impl, protocol} <- new_impls,
          not is_map_key(old_impls, impl),
          do: {protocol, true},
          into: protocols

    removed_protocols =
      for {protocol, _timestamp} <- old_protocols,
          not is_map_key(new_protocols, protocol),
          do: remove_consolidated(protocol, output)

    removed_protocols = Map.from_keys(removed_protocols, true)

    protocols =
      for {impl, protocol} <- old_impls,
          not is_map_key(new_impls, impl),
          not is_map_key(removed_protocols, protocol),
          do: {protocol, true},
          into: protocols

    Map.keys(protocols)
  end

  defp remove_consolidated(protocol, output) do
    File.rm(Path.join(output, "#{Atom.to_string(protocol)}.beam"))
    protocol
  end
end
