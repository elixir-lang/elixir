defmodule Mix.Compilers.Protocol do
  @moduledoc false
  @manifest "compile.protocols"
  @manifest_vsn 3

  ## Umbrella handling

  def umbrella(args, res) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean, verbose: :boolean])
    manifest = manifest()
    config_mtime = Mix.Project.config_mtime()
    {old_config_mtime, old_protocols_and_impls} = read_manifest(manifest)

    case status(config_mtime > old_config_mtime, opts) do
      :off ->
        res

      :on when res == :noop ->
        :noop

      on_or_force ->
        deps_paths =
          for %{scm: scm, opts: opts} <- Mix.Dep.cached(),
              not scm.fetchable?(),
              do: opts[:build]

        protocols_and_impls = Mix.Compilers.Elixir.protocols_and_impls_from_paths(deps_paths)

        case compile(on_or_force == :force, old_protocols_and_impls, protocols_and_impls, opts) do
          :ok ->
            write_manifest(manifest, {config_mtime, protocols_and_impls})
            :ok

          :noop ->
            res
        end
    end
  end

  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  defp read_manifest(manifest) do
    try do
      [@manifest_vsn | metadata] = manifest |> File.read!() |> :erlang.binary_to_term()
      metadata
    rescue
      _ ->
        # If there is no manifest or it is out of date, remove old files
        clean_consolidated()
        {0, {%{}, %{}}}
    end
  end

  defp write_manifest(manifest, metadata) do
    File.mkdir_p!(Path.dirname(manifest))
    manifest_data = :erlang.term_to_binary([@manifest_vsn | metadata], [:compressed])
    File.write!(manifest, manifest_data)
  end

  def clean do
    File.rm(manifest())
    clean_consolidated()
  end

  ## General handling

  def status(mtime_changed?, opts) do
    consolidation_path = Mix.Project.consolidation_path()

    cond do
      not Keyword.get(opts, :consolidate_protocols, true) ->
        clean_consolidated()
        :off

      # We need to reconsolidate all protocols whenever the dependency changes
      # because we only track protocols from the current app and from local deps.
      # We are only interested in the compile.lock from config_mtime (which is
      # a build artifact).
      not File.exists?(consolidation_path) or mtime_changed? ->
        :force

      true ->
        :on
    end
  end

  def compile(force?, old_protocols_and_impls, protocols_and_impls, opts) do
    output = Mix.Project.consolidation_path()

    res =
      if opts[:force] || force? do
        clean_consolidated()
        paths = consolidation_paths()

        paths
        |> Protocol.extract_protocols()
        |> consolidate(paths, output, opts)
      else
        protocols_and_impls
        |> diff_manifest(old_protocols_and_impls, output)
        |> consolidate(consolidation_paths(), output, opts)
      end

    Code.prepend_path(output)
    res
  end

  defp clean_consolidated do
    File.rm_rf(Mix.Project.consolidation_path())
  end

  defp consolidation_paths do
    filter_otp(:code.get_path(), :code.lib_dir())
  end

  defp filter_otp(paths, otp) do
    Enum.filter(paths, &(not :lists.prefix(otp, &1)))
  end

  defp consolidate([], _paths, output, _opts) do
    File.mkdir_p!(output)

    :noop
  end

  defp consolidate(protocols, paths, output, opts) do
    File.mkdir_p!(output)

    protocols
    |> Enum.uniq()
    |> Enum.map(&Task.async(fn -> consolidate_each(&1, paths, output, opts) end))
    |> Enum.map(&Task.await(&1, :infinity))

    :ok
  end

  defp consolidate_each(protocol, paths, output, opts) do
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
