defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn :v2

  @doc """
  Compiles stale Elixir files.

  It expects a manifest file, the source directories, the extensions
  to read in sources, the destination directory, a flag to know if
  compilation is being forced or not and a callback to be invoked
  once (and only if) compilation starts.

  The manifest is written down with information including dependencies
  between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  def compile(manifest, srcs, skip, exts, dest, force, on_start) do
    keep = srcs -- skip
    all  = Mix.Utils.extract_files(keep, exts)
    {all_entries, skip_entries} = parse_manifest(manifest, keep)

    removed =
      for {_b, _m, _k, source, _cd, _rd, _f, _bin} <- all_entries,
          not(source in all),
          do: source

    changed =
      if force do
        # A config, path dependency or manifest has
        # changed, let's just compile everything
        all
      else
        modified   = Mix.Utils.last_modified(manifest)
        all_mtimes = mtimes(all_entries)

        # Otherwise let's start with the new ones
        # plus the ones that have changed
        for(source <- all,
            not Enum.any?(all_entries, fn {_b, _m, _k, s, _cd, _rd, _f, _bin} -> s == source end),
            do: source)
          ++
        for({_b, _m, _k, source, _cd, _rd, files, _bin} <- all_entries,
            times = Enum.map([source|files], &Map.fetch!(all_mtimes, &1)),
            Mix.Utils.stale?(times, [modified]),
            do: source)
      end

    {entries, changed} = remove_stale_entries(all_entries, removed ++ changed)
    stale = changed -- removed

    cond do
      stale != [] ->
        compile_manifest(manifest, entries ++ skip_entries, stale, dest, on_start)
        :ok
      removed != [] ->
        write_manifest(manifest, entries ++ skip_entries)
        :ok
      true ->
        :noop
    end
  end

  defp mtimes(entries) do
    Enum.reduce(entries, %{}, fn {_b, _m, _k, source, _cd, _rd, files, _bin}, dict ->
      Enum.reduce([source|files], dict, fn file, dict ->
        if Map.has_key?(dict, file) do
          dict
        else
          Map.put(dict, file, Mix.Utils.last_modified(file))
        end
      end)
    end)
  end

  @doc """
  Removes compiled files.
  """
  def clean(manifest) do
    Enum.map read_manifest(manifest), fn {beam, _, _, _, _, _, _, _} ->
      File.rm(beam)
    end
    :ok
  end

  @doc """
  Returns protocols and implementations for the given manifest.
  """
  def protocols_and_impls(manifest) do
    for {_, module, kind, _, _, _, _, _} <- read_manifest(manifest),
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind}
  end

  defp compile_manifest(manifest, entries, stale, dest, on_start) do
    Mix.Project.ensure_structure()
    true = Code.prepend_path(dest)

    on_start.()
    cwd = File.cwd!

    # Starts a server responsible for keeping track which files
    # were compiled and the dependencies between them.
    {:ok, pid} = Agent.start_link(fn -> entries end)

    try do
      _ = Kernel.ParallelCompiler.files :lists.usort(stale),
        each_module: &each_module(pid, dest, cwd, &1, &2, &3),
        each_file: &each_file(&1),
        dest: dest
      Agent.cast pid, fn entries ->
        write_manifest(manifest, entries)
        entries
      end
    after
      Agent.stop(pid, :normal, :infinity)
    end

    :ok
  end

  defp each_module(pid, dest, cwd, source, module, binary) do
    beam =
      dest
      |> Path.join(Atom.to_string(module) <> ".beam")
      |> Path.relative_to(cwd)

    {compile, runtime} = Kernel.LexicalTracker.remotes(module)

    compile =
      compile
      |> List.delete(module)
      |> Enum.reject(&match?("elixir_" <> _, Atom.to_string(&1)))

    runtime =
      runtime
      |> List.delete(module)
      |> Enum.reject(&match?("elixir_" <> _, Atom.to_string(&1)))

    kind   = detect_kind(module)
    source = Path.relative_to(source, cwd)
    files  = get_external_resources(module, cwd)
    tuple  = {beam, module, kind, source, compile, runtime, files, binary}
    Agent.cast pid, &:lists.keystore(beam, 1, &1, tuple)
  end

  defp detect_kind(module) do
    cond do
      impl = Module.get_attribute(module, :impl) ->
        {:impl, impl[:protocol]}
      Module.get_attribute(module, :protocol) ->
        :protocol
      true ->
        :module
    end
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource),
        File.regular?(file),
        relative = Path.relative_to(file, cwd),
        Path.type(relative) == :relative,
        do: relative
  end

  defp each_file(file) do
    Mix.shell.info "Compiled #{file}"
  end

  ## Resolution

  # This function receives the manifest entries and some source
  # files that have changed. It then, recursively, figures out
  # all the files that changed (via the module dependencies) and
  # return the non-changed entries and the removed sources.
  defp remove_stale_entries(all, []) do
    {all, []}
  end

  defp remove_stale_entries(all, changed) do
    remove_stale_entries(all, %{}, Enum.into(changed, %{}, &{&1, true}))
  end

  defp remove_stale_entries(entries, old_stale, old_removed) do
    {rest, new_stale, new_removed} =
      Enum.reduce entries, {[], old_stale, old_removed}, &remove_stale_entry/2

    if map_size(new_stale) > map_size(old_stale) or
       map_size(new_removed) > map_size(old_removed) do
      remove_stale_entries(rest, new_stale, new_removed)
    else
      {rest, Map.keys(new_removed)}
    end
  end

  defp remove_stale_entry({beam, module, _kind, source, compile, runtime, _f, _bin} = entry,
                          {rest, stale, removed}) do
    cond do
      # If I changed in disk or have a compile time dependency
      # on something stale, I need to be recompiled.
      Map.has_key?(removed, source) or Enum.any?(compile, &Map.has_key?(stale, &1)) ->
        _ = File.rm(beam)
        _ = :code.purge(module)
        _ = :code.delete(module)
        {rest, Map.put(stale, module, true), Map.put(removed, source, true)}

      # If I have a runtime time dependency on something stale,
      # I am stale too.
      Enum.any?(runtime, &Map.has_key?(stale, &1)) ->
        {[entry|rest], Map.put(stale, module, true), removed}

      # Otherwise, we don't store it anywhere
      true ->
        {[entry|rest], stale, removed}
    end
  end

  ## Manifest handling

  defp read_manifest(manifest) do
    case :file.consult(manifest) do
      {:ok, [@manifest_vsn|t]} -> t
      _ -> []
    end
  end

  defp parse_manifest(manifest, keep_paths) do
    Enum.reduce read_manifest(manifest), {[], []}, fn
      {_, _, _, source, _, _, _, _} = entry, {keep, skip} ->
        if String.starts_with?(source, keep_paths) do
          {[entry|keep], skip}
        else
          {keep, [entry|skip]}
        end
    end
  end

  defp write_manifest(manifest, []) do
    File.rm(manifest)
    :ok
  end

  defp write_manifest(manifest, entries) do
    File.mkdir_p!(Path.dirname(manifest))

    File.open!(manifest, [:write], fn device ->
      :io.format(device, '~p.~n', [@manifest_vsn])

      Enum.map entries, fn {beam, _, _, _, _, _, _, binary} = entry ->
        if binary, do: File.write!(beam, binary)
        :io.format(device, '~p.~n', [put_elem(entry, 7, nil)])
      end

      :ok
    end)

    # Since Elixir is a dependency itself, we need to touch the lock
    # so the current Elixir version, used to compile the files above,
    # is properly stored.
    Mix.Dep.ElixirSCM.update
  end
end
