defmodule Mix.Tasks.Compile.Elixir do
  use Mix.Task

  alias Mix.Tasks.Compile.Erlang

  @hidden true
  @shortdoc "Compile Elixir source files"
  @recursive true
  @manifest ".compile.elixir"

  @moduledoc """
  A task to compile Elixir source files.

  When this task runs, it will first check the modification times of
  all of the files to be compiled and if they haven't been
  changed since the last compilation, it will not compile
  them at all. If any one of them has changed, it compiles
  everything.

  For this reason, this task touches your `:compile_path`
  directory and sets the modification time to the current
  time and date at the end of each compilation. You can
  force compilation regardless of modification times by passing
  the `--force` option.

  Note it is important to recompile all files because
  often there are compilation time dependencies between
  the files (macros, etc). However, in some cases it
  is useful to compile just the changed files for quick
  development cycles, for such, a developer can pass
  the `--quick` option.

  ## Command line options

  * `--force` - forces compilation regardless of modification times;
  * `--quick`, `-q` - only compile files that changed;
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

  @switches [force: :boolean, quick: :boolean, docs: :boolean,
             ignore_module_conflict: :boolean, debug_info: :boolean,
             warnings_as_errors: :boolean]

  def run(args) do
    { opts, _ } = OptionParser.parse(args, aliases: [q: :quick], switches: @switches)

    project       = Mix.project
    compile_path  = project[:compile_path]
    compile_exts  = project[:elixirc_exts]
    watch_exts    = project[:elixirc_watch_exts]
    elixirc_paths = project[:elixirc_paths]

    manifest   = Path.join(compile_path, @manifest)
    to_compile = Mix.Utils.extract_files(elixirc_paths, compile_exts)
    to_watch   = Mix.Utils.extract_files(elixirc_paths, watch_exts)

    check_files = Mix.Project.config_files ++ [Erlang.manifest]

    stale = if Mix.Utils.stale?(check_files, [manifest]) or path_deps_changed?(manifest) do
      force = true
      to_watch
    else
      force = opts[:force]
      Mix.Utils.extract_stale(to_watch, [manifest])
    end

    if force or stale != [] do
      File.mkdir_p! compile_path
      Code.prepend_path compile_path
      compile_files(opts[:quick], project, compile_path, to_compile, stale, opts)
      :ok
    else
      :noop
    end
  end

  def manifest do
    @manifest
  end


  defp compile_files(true, project, compile_path, to_compile, stale, opts) do
    set_compiler_opts(project, opts, ignore_module_conflict: true)
    to_compile = lc f inlist stale, f in to_compile, do: f
    compile_files(to_compile, compile_path)
    File.touch! Path.join(compile_path, @manifest)
  end

  defmodule ManifestCompiler do
    @moduledoc false
    use GenServer.Behaviour

    def files_to_path(manifest, stale, all, compile_path) do
      entries = read_manifest(manifest)

      # Each entry that is not in all was removed and must be pruned
      entries = prune_entries(entries, fn(x) -> not(x in all) end)

      # Filter stale to be a subset of all
      stale = lc i inlist stale, i in all, do: i

      # Each entry in all that's not in the manifest is also stale
      stale  = stale ++ lc i inlist all,
                           not Enum.any?(entries, fn { _b, _m, s, _d } -> s == i end),
                           do: i

      { :ok, pid } = :gen_server.start_link(__MODULE__, entries, [])

      try do
        do_files_to_path(pid, stale, compile_path, File.cwd!)
        :gen_server.cast(pid, :merge)
      after
        case :gen_server.call(pid, :stop) do
          { :ok, entries }  -> write_manifest(manifest, entries)
          { :error, beams } -> Enum.each(beams, File.rm(&1))
        end
      end
    end

    defp do_files_to_path(_pid, [], _compile_path, _cwd), do: :ok
    defp do_files_to_path(pid, files, compile_path, cwd) do
      Kernel.ParallelCompiler.files_to_path :lists.usort(files), compile_path,
        each_module: each_module(pid, compile_path, cwd, &1, &2, &3),
        each_file: each_file(&1)

      do_files_to_path(pid, :gen_server.call(pid, :next), compile_path, cwd)
    end

    defp each_module(pid, compile_path, cwd, source, module, _binary) do
      bin  = atom_to_binary(module)
      beam = Path.join(compile_path, bin <> ".beam")

      deps = Module.DispatchTracker.remotes(module) ++
             Module.DispatchTracker.imports(module)
      deps = deps |> :lists.usort |> Enum.map(atom_to_binary(&1))

      :gen_server.cast(pid, { :store, beam, bin, Path.relative_to(source, cwd), deps })
    end

    defp each_file(file) do
      Mix.shell.info "Compiled #{file}"
    end

    # Reads the manifest returning the results as tuples.
    defp read_manifest(manifest) do
      Enum.reduce Mix.Utils.read_manifest(manifest), [], fn x, acc ->
        case String.split(x, "\t") do
          [beam, module, source | deps] ->  [{ beam, module, source, deps }|acc]
          _ -> acc
        end
      end
    end

    # Writes the manifest separating entries by tabs.
    defp write_manifest(manifest, entries) do
      lines = Enum.map(entries, fn
        { beam, module, source, deps } ->
          [beam, module, source | deps] |> Enum.join("\t")
      end)

      Mix.Utils.write_manifest(manifest, lines)
    end

    # If the source is no longer available OR the source is stale,
    # we remove its artifacts and prune the entry from the list.
    defp prune_entries([{ beam, _, source, _ } = entry|entries], callback) do
      if callback.(source) do
        File.rm(beam)
        prune_entries(entries, callback)
      else
        [entry|prune_entries(entries, callback)]
      end
    end

    defp prune_entries([], _callback), do: []

    # Callbacks

    def init(old) do
      { :ok, { old, [] } }
    end

    def handle_call(:stop, _from, { old, [] }) do
      modules  = lc { _b, module, _s, _d } inlist old, do: module

      filtered = lc { b, m, s, deps } inlist old do
        { b, m, s, Enum.filter(deps, &1 in modules) }
      end

      { :stop, :normal, { :ok, filtered }, { filtered, [] } }
    end

    def handle_call(:stop, _from, { _old, new } = state) do
      beams = lc { beam, _m, _s, _d } inlist new, do: beam
      { :stop, :normal, { :error, beams }, state }
    end

    def handle_call(:next, _from, { old, new }) do
      modules = lc { _b, module, _s, _d } inlist new, do: module

      # For each previous entry in the manifest that
      # had its dependency changed and it was not yet
      # compiled, get its source as next
      next = lc { beam, module, source, deps } inlist old,
                Enum.any?(modules, &1 in deps),
                not(module in modules),
                do: File.rm(beam) && source

      { :reply, next, { old, new } }
    end

    def handle_call(msg, from, state) do
      super(msg, from, state)
    end

    def handle_cast(:merge, { old, new }) do
      old = :lists.keymerge(1, :lists.sort(new), :lists.sort(old))
      { :noreply, { old, [] } }
    end

    def handle_cast({ :store, beam, module, source, deps }, { old, new }) do
      { :noreply, { old, [{ beam, module, source, deps }|new] } }
    end

    def handle_cast(msg, state) do
      super(msg ,state)
    end
  end

  defp compile_files(false, project, compile_path, to_compile, stale, opts) do
    manifest = Path.join(compile_path, @manifest)
    set_compiler_opts(project, opts, [])
    ManifestCompiler.files_to_path(manifest, stale, to_compile, compile_path)
  end

  defp set_compiler_opts(project, opts, extra) do
    opts = Dict.take(opts, [:docs, :debug_info, :ignore_module_conflict, :warnings_as_errors])
    opts = Keyword.merge(project[:elixirc_options] || [], opts)
    Code.compiler_options Keyword.merge(opts, extra)
  end

  defp compile_files(files, to) do
    Kernel.ParallelCompiler.files_to_path files, to,
      each_file: fn(file) ->
        Mix.shell.info "Compiled #{file}"
      end
  end

  defp path_deps_changed?(manifest) do
    manifest = Path.absname(manifest)
    deps = Enum.filter(Mix.Deps.children, fn(Mix.Dep[] = dep) ->
      dep.scm == Mix.SCM.Path and dep.manager == :mix
    end)

    Enum.any?(deps, fn(dep) ->
      Mix.Deps.in_dependency(dep, fn(_) ->
        Mix.Utils.stale?(collect_manifests, [manifest])
      end)
    end)
  end

  defp collect_manifests do
    manifests = Mix.Tasks.Compile.manifests
    compile_path = Mix.project[:compile_path]
    Enum.map(manifests, Path.join(compile_path, &1))
  end
end
