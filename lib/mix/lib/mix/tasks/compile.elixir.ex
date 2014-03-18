defmodule Mix.Tasks.Compile.Elixir do
  # The ManifestCompiler is a convenience that tracks dependencies
  # in between files and recompiles them as they change recursively.
  defmodule ManifestCompiler do
    use GenServer.Behaviour
    @moduledoc false

    def files_to_path(manifest, changed, all, compile_path, on_start) do
      all_entries = read_manifest(manifest)

      removed =
        for { _b, _m, source, _d } <- all_entries, not(source in all), do: source

      changed =
        if Enum.any?(changed, &not(&1 in all)) do
          # Any changed file not in all triggers whole compilation
          # because it is an external file (like config)
          all
        else
          # Otherwise compile changed plus each entry in all that's
          # not in the manifest (i.e. new files)
          (changed ++ for i <- all,
                         not Enum.any?(all_entries, fn { _b, _m, s, _d } -> s == i end),
                         do: i)
        end

      { entries, changed } = remove_stale_entries(all_entries, removed ++ changed, [], [])

      # Remove files are not going to be compiled
      stale = changed -- removed

      cond do
        stale != [] ->
          on_start.()
          cwd = File.cwd!

          # Starts a server responsible for keeping track which files
          # were compiled and the dependencies in between them.
          { :ok, pid } = :gen_server.start_link(__MODULE__, entries, [])

          try do
            Kernel.ParallelCompiler.files :lists.usort(stale),
              each_module: &each_module(pid, compile_path, cwd, &1, &2, &3),
              each_file: &each_file(&1)
            :gen_server.cast(pid, { :write, manifest })
          after
            :gen_server.call(pid, :stop)
          end

          :ok
        removed != [] ->
          :ok
        true ->
          :noop
      end
    end

    defp each_module(pid, compile_path, cwd, source, module, binary) do
      bin  = atom_to_binary(module)
      beam = Path.join(compile_path, bin <> ".beam")

      deps = Kernel.LexicalTracker.remotes(module)
             |> :lists.usort |> Enum.map(&atom_to_binary(&1))

      relative = if cwd, do: Path.relative_to(source, cwd), else: source
      :gen_server.cast(pid, { :store, beam, bin, relative, deps, binary })
    end

    defp each_file(file) do
      Mix.shell.info "Compiled #{file}"
    end

    ## Resolution

    # This function receives the manifest entries and some source
    # files that have changed. It then, recursively, figures out
    # all the files that changed (thanks to the dependencies) and
    # return their sources as the remaining entries.
    defp remove_stale_entries(all, []) do
      { all, [] }
    end

    defp remove_stale_entries(all, changed) do
      remove_stale_entries(all, :lists.usort(changed), [], [])
    end

    defp remove_stale_entries([{ beam, module, source, _d } = entry|t], changed, removed, acc) do
      if source in changed do
        File.rm(beam)
        remove_stale_entries(t, changed, [module|removed], acc)
      else
        remove_stale_entries(t, changed, removed, [entry|acc])
      end
    end

    defp remove_stale_entries([], changed, removed, acc) do
      # If any of the dependencies for the remaining entries
      # were removed, get its source so we can removed them.
      next_changed = for { _b, _m, source, deps } <- acc,
                      Enum.any?(deps, &(&1 in removed)),
                      do: source

      { acc, next } = remove_stale_entries(Enum.reverse(acc), next_changed)
      { acc, next ++ changed }
    end

    ## Manifest handling

    # Reads the manifest returning the results as tuples.
    # The beam files are read, removed and stored in memory.
    defp read_manifest(manifest) do
      Enum.reduce Mix.Utils.read_manifest(manifest), [], fn x, acc ->
        case String.split(x, "\t") do
          [beam, module, source|deps] ->
            [{ beam, module, source, deps }|acc]
          _ ->
            acc
        end
      end
    end

    # Writes the manifest separating entries by tabs.
    defp write_manifest(_manifest, []) do
      :ok
    end

    defp write_manifest(manifest, entries) do
      lines = Enum.map(entries, fn
        { beam, module, source, deps, binary } ->
          if binary, do: File.write!(beam, binary)
          [beam, module, source | deps] |> Enum.join("\t")
      end)
      Mix.Utils.write_manifest(manifest, lines)
    end

    # Callbacks

    def init(entries) do
      { :ok, Enum.map(entries, &Tuple.insert_at(&1, 4, nil)) }
    end

    def handle_call(:stop, _from, entries) do
      { :stop, :normal, :ok, entries }
    end

    def handle_call(msg, from, state) do
      super(msg, from, state)
    end

    def handle_cast({ :write, manifest }, entries) do
      write_manifest(manifest, entries)
      { :noreply, entries }
    end

    def handle_cast({ :store, beam, module, source, deps, binary }, entries) do
      { :noreply, :lists.keystore(beam, 1, entries, { beam, module, source, deps, binary }) }
    end

    def handle_cast(msg, state) do
      super(msg ,state)
    end
  end

  use Mix.Task
  alias Mix.Tasks.Compile.Erlang

  @recursive true
  @manifest ".compile.elixir"

  @moduledoc """
  Compiles Elixir source files.

  Elixir is smart enough to recompile only files that changed
  and their dependencies. This means if `lib/a.ex` is invoking
  a function defined over `lib/b.ex`, whenever `lib/b.ex` changes,
  `lib/a.ex` is also recompiled.

  Note it is important to recompile a file dependencies because
  often there are compilation time dependencies in between them.

  ## Command line options

  * `--force` - forces compilation regardless of modification times;
  * `--no-docs` - Do not attach documentation to compiled modules;
  * `--no-debug-info` - Do not attach debug info to compiled modules;
  * `--ignore-module-conflict`
  * `--warnings-as-errors` - Treat warnings as errors and return a non-zero exit code

  ## Configuration

  * `:elixirc_paths` - directories to find source files.
    Defaults to `["lib"]`, can be configured as:

    ```
    [elixirc_paths: ["lib", "other"]]
    ```

  * `:elixirc_options` - compilation options that apply
     to Elixir's compiler, they are: `:ignore_module_conflict`,
     `:docs` and `:debug_info`. By default, uses the same
     behaviour as Elixir;

  * `:elixirc_exts` - extensions to compile whenever there
     is a change:

     ```
     [compile_exts: [:ex]]
     ```

  * `:elixirc_watch_exts` - extensions to watch in order to trigger
      a compilation:

      ```
      [elixirc_watch_exts: [:ex, :eex]]
      ```

  """

  @switches [ force: :boolean, docs: :boolean, warnings_as_errors: :boolean,
              ignore_module_conflict: :boolean, debug_info: :boolean ]

  @doc """
  Runs this task.
  """
  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, switches: @switches)

    project       = Mix.project
    compile_path  = Mix.Project.compile_path(project)
    compile_exts  = project[:elixirc_exts]
    watch_exts    = project[:elixirc_watch_exts]
    elixirc_paths = project[:elixirc_paths]

    manifest   = manifest()
    to_compile = Mix.Utils.extract_files(elixirc_paths, compile_exts)
    to_watch   = Mix.Utils.extract_files(elixirc_paths, watch_exts)
    to_watch   = Mix.Project.config_files ++ Erlang.manifests ++ to_watch

    stale = if opts[:force] || path_deps_changed?(manifest) do
      to_compile
    else
      Mix.Utils.extract_stale(to_watch, [manifest])
    end

    result = files_to_path(manifest, stale, to_compile, compile_path, fn ->
      Mix.Project.build_structure(project)
      Code.prepend_path(compile_path)
      set_compiler_opts(project, opts, [])
    end)

    # The Mix.Dep.Lock keeps all the project dependencies. Since Elixir
    # is a dependency itself, we need to touch the lock so the current
    # Elixir version, used to compile the files above, is properly stored.
    unless result == :noop, do: Mix.Dep.Lock.touch
    result
  end

  @doc """
  Returns Elixir manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Compiles stale Elixir files.

  It expects a manifest file, all stale files, all source files
  available (including the ones that are not stale) and a path
  where compiled files will be written to. All paths are required
  to be relative to the current working directory.

  The manifest is written down with information including dependencies
  in between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  defdelegate files_to_path(manifest, stale, all, path, on_start), to: ManifestCompiler

  defp set_compiler_opts(project, opts, extra) do
    opts = Dict.take(opts, [:docs, :debug_info, :ignore_module_conflict, :warnings_as_errors])
    opts = Keyword.merge(project[:elixirc_options] || [], opts)
    Code.compiler_options Keyword.merge(opts, extra)
  end

  defp path_deps_changed?(manifest) do
    manifest = Path.absname(manifest)

    deps = Enum.filter(Mix.Dep.children([]), fn dep ->
      dep.scm == Mix.SCM.Path
    end)

    Enum.any?(deps, fn(dep) ->
      Mix.Dep.in_dependency(dep, fn(_) ->
        Mix.Utils.stale?(Mix.Tasks.Compile.manifests, [manifest])
      end)
    end)
  end
end
