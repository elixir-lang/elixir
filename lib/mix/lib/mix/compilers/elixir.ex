defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @doc """
  Compiles stale Elixir files.

  It expects a manifest file, the source directories, the extensions
  to read in sources, the destination directory, a flag to know if
  compilation is being forced or not and a callback to be invoked
  once (and only if) compilation starts.

  The manifest is written down with information including dependencies
  in between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  def compile(manifest, srcs, skip, exts, dest, force, on_start) do
    keep = srcs -- skip
    all  = Mix.Utils.extract_files(keep, exts)
    {all_entries, skip_entries} = read_manifest(manifest, dest, keep)

    removed =
      for {_b, _m, source, _d, _f, _bin} <- all_entries, not(source in all), do: source

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
            not Enum.any?(all_entries, fn {_b, _m, s, _d, _f, _bin} -> s == source end),
            do: source)
          ++
        for({_b, _m, source, _d, files, _bin} <- all_entries,
            times = Enum.map([source|files], &HashDict.fetch!(all_mtimes, &1)),
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
      File.exists?(Mix.Project.app_path) ->
        # If we have an app path but a noop result, we rebuild
        # the structure because priv/include needs to be recopied when
        # symlink is not available. We do this in the Elixir compiler
        # instead of the main compiler one because other compilers may
        # return :ok, saying they did some work, but they may have not
        # built a structure at all.
        #
        # This implies the Elixir compiler is somewhat required, even
        # for Erlang projects, which is an ok assumption to make (we
        # ship with both Elixir and Erlang compilers by default).
        Mix.Project.build_structure()
        :noop
      true ->
        :noop
    end
  end

  defp mtimes(entries) do
    Enum.reduce(entries, HashDict.new, fn {_b, _m, source, _d, files, _bin}, dict ->
      Enum.reduce([source|files], dict, fn file, dict ->
        if HashDict.has_key?(dict, file) do
          dict
        else
          HashDict.put(dict, file, Mix.Utils.last_modified(file))
        end
      end)
    end)
  end

  @doc """
  Removes compiled files.
  """
  def clean(manifest) do
    case File.read(manifest) do
      {:ok, contents} ->
        contents
        |> String.split("\n")
        |> Enum.each &(&1 |> String.split("\t") |> hd |> File.rm)
        File.rm(manifest)
      {:error, _} ->
        :ok
    end
  end

  defp compile_manifest(manifest, entries, stale, dest, on_start) do
    Mix.Project.build_structure
    true = Code.prepend_path(dest)

    on_start.()
    cwd = File.cwd!

    # Starts a server responsible for keeping track which files
    # were compiled and the dependencies in between them.
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
      Agent.stop(pid, :infinity)
    end

    :ok
  end

  defp each_module(pid, dest, cwd, source, module, binary) do
    source = Path.relative_to(source, cwd)
    bin    = Atom.to_string(module)
    beam   = dest
             |> Path.join(bin <> ".beam")
             |> Path.relative_to(cwd)

    deps = Kernel.LexicalTracker.remotes(module)
           |> List.delete(module)
           |> :lists.usort
           |> Enum.map(&Atom.to_string(&1))
           |> Enum.reject(&match?("elixir_" <> _, &1))

    files = for file <- get_external_resources(module),
                File.regular?(file),
                relative = Path.relative_to(file, cwd),
                Path.type(relative) == :relative,
                do: relative

    Agent.cast pid, &:lists.keystore(beam, 1, &1, {beam, bin, source, deps, files, binary})
  end

  defp get_external_resources(module) do
    for {:external_resource, values} <- module.__info__(:attributes),
        value <- values,
        do: value
  end

  defp each_file(file) do
    Mix.shell.info "Compiled #{file}"
  end

  ## Resolution

  # This function receives the manifest entries and some source
  # files that have changed. It then, recursively, figures out
  # all the files that changed (via the module dependencies) and
  # return their sources as the remaining entries.
  defp remove_stale_entries(all, []) do
    {all, []}
  end

  defp remove_stale_entries(all, changed) do
    remove_stale_entries(all, :lists.usort(changed), [], [])
  end

  defp remove_stale_entries([{beam, module, source, _d, _f, _bin} = entry|t], changed, removed, acc) do
    if source in changed do
      atom = String.to_atom(module)
      _ = File.rm(beam)
      _ = :code.purge(atom)
      _ = :code.delete(atom)
      remove_stale_entries(t, changed, [module|removed], acc)
    else
      remove_stale_entries(t, changed, removed, [entry|acc])
    end
  end

  defp remove_stale_entries([], changed, removed, acc) do
    # If any of the dependencies for the remaining entries
    # were removed, get its source so we can remove them.
    next_changed = for {_b, _m, source, deps, _f, _bin} <- acc,
                    Enum.any?(deps, &(&1 in removed)),
                    do: source

    {acc, next} = remove_stale_entries(Enum.reverse(acc), next_changed)
    {acc, next ++ changed}
  end

  ## Manifest handling

  # Reads the manifest returning the results as tuples.
  # The beam files are read, removed and stored in memory.
  defp read_manifest(manifest, dest, keep_paths) do
    initial = {[], []}

    case File.read(manifest) do
      {:ok, contents} ->
        keep_paths = Enum.map(keep_paths, &(&1 <> "/"))
        Enum.reduce String.split(contents, "\n"), initial, fn x, acc ->
          read_manifest_entry(String.split(x, "\t"), acc, dest, keep_paths)
        end
      {:error, _} ->
        initial
    end
  end

  defp read_manifest_entry([_beam, module, source|deps], {keep, skip}, dest, keep_paths) do
    {deps, files} =
      case Enum.split_while(deps, &(&1 != "Elixir")) do
        {deps, ["Elixir"|files]} -> {deps, files}
        {deps, _} -> {deps, []}
      end

    # TODO: Notice we do not use beam from the file.
    # Once Elixir v1.1 is out, we can start writing "1"
    # instead of the beam file in write_manifest/2.
    entry = {Path.join(dest, module <> ".beam"), module, source,
             deps, files, nil}

    if String.starts_with?(source, keep_paths) do
      {[entry|keep], skip}
    else
      {keep, [entry|skip]}
    end
  end

  defp read_manifest_entry(_, acc, _dest, _skip_paths) do
    acc
  end

  # Writes the manifest separating entries by tabs.
  defp write_manifest(_manifest, []) do
    :ok
  end

  defp write_manifest(manifest, entries) do
    lines = Enum.map(entries, fn
      {beam, module, source, deps, files, binary} ->
        if binary, do: File.write!(beam, binary)
        tail = deps ++ ["Elixir"] ++ files
        [beam, module, source | tail] |> Enum.join("\t")
    end)

    # The Mix.Dep.Lock keeps all the project dependencies. Since Elixir
    # is a dependency itself, we need to touch the lock so the current
    # Elixir version, used to compile the files above, is properly stored.
    Mix.Dep.Lock.touch

    File.mkdir_p!(Path.dirname(manifest))
    File.write!(manifest, Enum.join(lines, "\n"))
  end
end
