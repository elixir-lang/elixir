defmodule Mix.Tasks.Compile.Protocols do
  use Mix.Task.Compiler

  @manifest "compile.protocols"
  @manifest_vsn 1

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
    protocols_and_impls = protocols_and_impls(config)

    cond do
      opts[:force] || Mix.Utils.stale?([Mix.Project.config_mtime()], [manifest]) ->
        clean()
        paths = consolidation_paths()

        paths
        |> Protocol.extract_protocols()
        |> consolidate(paths, output, manifest, protocols_and_impls, opts)

      protocols_and_impls ->
        manifest
        |> diff_manifest(protocols_and_impls, output)
        |> consolidate(consolidation_paths(), output, manifest, protocols_and_impls, opts)

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
    deps = for %{scm: scm, opts: opts} <- Mix.Dep.cached(), not scm.fetchable?, do: opts[:build]

    app =
      if Mix.Project.umbrella?(config) do
        []
      else
        [Mix.Project.app_path(config)]
      end

    protocols_and_impls =
      for path <- app ++ deps do
        manifest_path = Path.join(path, ".mix/compile.elixir")
        compile_path = Path.join(path, "ebin")
        Mix.Compilers.Elixir.protocols_and_impls(manifest_path, compile_path)
      end

    Enum.concat(protocols_and_impls)
  end

  defp consolidation_paths do
    filter_otp(:code.get_path(), :code.lib_dir())
  end

  defp filter_otp(paths, otp) do
    Enum.filter(paths, &(not :lists.prefix(&1, otp)))
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
    |> Enum.map(&Task.await(&1, 30000))

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
    Code.Identifier.inspect_as_atom(protocol)
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
        []
    end
  end

  defp write_manifest(manifest, metadata) do
    File.mkdir_p!(Path.dirname(manifest))
    manifest_data = :erlang.term_to_binary([@manifest_vsn | metadata], [:compressed])
    File.write!(manifest, manifest_data)
  end

  defp diff_manifest(manifest, new_metadata, output) do
    modified = Mix.Utils.last_modified(manifest)
    old_metadata = read_manifest(manifest, output)

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

    removed_metadata = old_metadata -- new_metadata

    removed_protocols =
      for {protocol, :protocol, _beam} <- removed_metadata,
          remove_consolidated(protocol, output),
          do: {protocol, true},
          into: %{}

    protocols =
      for {_, {:impl, protocol}, _beam} <- removed_metadata,
          not Map.has_key?(removed_protocols, protocol),
          do: {protocol, true},
          into: protocols

    Map.keys(protocols)
  end

  defp remove_consolidated(protocol, output) do
    File.rm(Path.join(output, "#{Atom.to_string(protocol)}.beam"))
  end
end
