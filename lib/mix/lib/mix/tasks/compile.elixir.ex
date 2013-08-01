defmodule Mix.Tasks.Compile.Elixir do
  # The ManifestCompiler is a convenience that tracks dependencies
  # in between files and recompiles them as they change recursively.
  defmodule ManifestCompiler do
    use GenServer.Behaviour
    @moduledoc false

    def files_to_path(manifest, stale, all, compile_path, on_start) do
      all_entries = read_manifest(manifest)

      # Each entry that is not in all must be removed
      entries = lc { beam, _m, source, _d } = entry inlist all_entries,
                   is_in_list_or_remove(source, all, beam),
                   do: entry

      # Filter stale to be a subset of all
      stale = lc i inlist stale, i in all, do: i

      # Each entry in all that's not in the manifest is also stale
      stale = stale ++ lc i inlist all,
                          not Enum.any?(entries, fn { _b, _m, s, _d } -> s == i end),
                          do: i

      cond do
        stale != [] ->
          on_start.()
          { :ok, pid } = :gen_server.start_link(__MODULE__, entries, [])

          try do
            do_files_to_path(pid, entries, stale, compile_path, File.cwd)
            :gen_server.cast(pid, :merge)
          after
            :gen_server.call(pid, { :stop, manifest })
          end

          :ok
        all_entries != entries ->
          :ok
        true ->
          :noop
      end
    end

    defp is_in_list_or_remove(source, all, beam) do
      if source in all do
        true
      else
        File.rm(beam)
        false
      end
    end

    defp do_files_to_path(_pid, _entries, [], _compile_path, _cwd), do: :ok
    defp do_files_to_path(pid, entries, files, compile_path, cwd) do
      Kernel.ParallelCompiler.files :lists.usort(files),
        each_module: each_module(pid, compile_path, cwd, &1, &2, &3),
        each_file: each_file(&1),
        each_waiting: each_waiting(entries, &1)

      do_files_to_path(pid, entries, :gen_server.call(pid, :next), compile_path, cwd)
    end

    defp each_module(pid, compile_path, cwd, source, module, binary) do
      bin  = atom_to_binary(module)
      beam = Path.join(compile_path, bin <> ".beam")

      deps = Module.DispatchTracker.aliases(module) ++
             Module.DispatchTracker.remotes(module) ++
             Module.DispatchTracker.imports(module)
      deps = deps |> :lists.usort |> Enum.map(atom_to_binary(&1))

      :gen_server.cast(pid, { :store, beam, bin, Mix.Utils.relative_to_cwd(source, cwd), deps, binary })
    end

    defp each_file(file) do
      Mix.shell.info "Compiled #{file}"
    end

    defp each_waiting(entries, module) do
      module = atom_to_binary(module)
      Enum.find_value(entries, fn
        { _b, m, s, _d } when m == module -> s
        _ -> nil
      end)
    end

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
    defp write_manifest(manifest, entries, modules) do
      lines = Enum.map(entries, fn
        { beam, module, source, deps, binary } ->
          File.write!(beam, binary)
          deps = Enum.filter(deps, &1 in modules)
          [beam, module, source | deps] |> Enum.join("\t")
      end)

      manifest && Mix.Utils.write_manifest(manifest, lines)
    end

    # Callbacks

    def init(entries) do
      entries =
        Enum.reduce entries, [], fn
          { beam, module, source, deps }, acc ->
            case File.read(beam) do
              { :ok, binary } ->
                File.rm(beam)
                [{ beam, module, source, deps, binary }|acc]
              { :error, _ } ->
                acc
            end
        end

      { :ok, { entries, [] } }
    end

    def handle_call(:next, _from, { old, new }) do
      modules = lc { _b, module, _s, _d, _y } inlist new, do: module
      sources = lc { _b, _m, source, _d, _y } inlist new, do: source

      # For each previous entry in the manifest that
      # had its dependency changed and it was not yet
      # compiled, get its source as next
      next = lc { _b, module, source, deps, _y } inlist old,
                Enum.any?(modules, &1 in deps),
                not(module in modules),
                not(source in sources),
                do: source

      { :reply, next, { old, new } }
    end

    def handle_call({ :stop, manifest }, _from, { old, new }) do
      modules = lc { _b, m, _s, _d, _y } inlist old, do: m
      write_manifest(new == [] && manifest, old, modules)
      { :stop, :normal, :ok, { old, new } }
    end

    def handle_call(msg, from, state) do
      super(msg, from, state)
    end

    def handle_cast(:merge, { old, new }) do
      merged = :lists.ukeymerge(1, :lists.sort(new), :lists.sort(old))
      { :noreply, { merged, [] } }
    end

    def handle_cast({ :store, beam, module, source, deps, binary }, { old, new }) do
      { :noreply, { old, :lists.keystore(beam, 1, new, { beam, module, source, deps, binary }) } }
    end

    def handle_cast(msg, state) do
      super(msg ,state)
    end
  end

  use Mix.Task
  alias Mix.Tasks.Compile.Erlang

  @hidden true
  @shortdoc "Compile Elixir source files"
  @recursive true
  @manifest ".compile.elixir"

  @moduledoc """
  A task to compile Elixir source files.

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

  * `:watch_exts` - extensions to watch in order to trigger
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
    { opts, _ } = OptionParser.parse(args, switches: @switches)

    project       = Mix.project
    compile_path  = project[:compile_path]
    compile_exts  = project[:elixirc_exts]
    watch_exts    = project[:elixirc_watch_exts]
    elixirc_paths = project[:elixirc_paths]

    manifest   = manifest()
    to_compile = Mix.Utils.extract_files(elixirc_paths, compile_exts)
    to_watch   = Mix.Utils.extract_files(elixirc_paths, watch_exts)

    check_files = Mix.Project.config_files ++ [Erlang.manifest]

    all   = opts[:force] || Mix.Utils.stale?(check_files, [manifest]) || path_deps_changed?(manifest)
    stale = if all, do: to_watch, else: Mix.Utils.extract_stale(to_watch, [manifest])

    files_to_path(manifest, stale, to_compile, compile_path, fn ->
      File.mkdir_p!(compile_path)
      Code.prepend_path(compile_path)
      set_compiler_opts(project, opts, [])
    end)
  end

  @doc """
  The manifest for this compiler.
  """
  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end

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

    deps = Enum.filter(Mix.Deps.children, fn(Mix.Dep[] = dep) ->
      dep.scm == Mix.SCM.Path and dep.manager == :mix
    end)

    Enum.any?(deps, fn(dep) ->
      Mix.Deps.in_dependency(dep, fn(_) ->
        Mix.Utils.stale?(Mix.Tasks.Compile.manifests, [manifest])
      end)
    end)
  end
end
