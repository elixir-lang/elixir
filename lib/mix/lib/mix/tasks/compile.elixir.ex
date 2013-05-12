defmodule Mix.Tasks.Compile.Elixir do
  use Mix.Task

  @hidden true
  @shortdoc "Compile Elixir source files"
  @recursive true
  @manifest ".compile.elixir"

  @moduledoc """
  A task to compile Elixir source files.

  When this task runs, it will first check the mod times of
  all of the files to be compiled and if they haven't been
  changed since the last compilation, it will not compile
  them at all. If any one of them has changed, it compiles
  everything.

  For this reason, this task touches your `:compile_path`
  directory and sets the modification time to the current
  time and date at the end of each compilation. You can
  force compilation regardless of mod times by passing
  the `--force` option.

  Note it is important to recompile all files because
  often there are compilation time dependencies between
  the files (macros and etc). However, in some cases it
  is useful to compile just the changed files for quick
  development cycles, for such, a developer can pass
  the `--quick` otion.

  ## Command line options

  * `--force` - forces compilation regardless of module times;
  * `--quick`, `-q` - only compile files that changed;

  ## Configuration

  * `:elixirc_paths` - directories to find source files.
    Defaults to `["lib"]`, can be configured as:

        [elixirc_paths: ["lib", "other"]]

  * `:elixirc_options` - compilation options that applies
     to Elixir's compiler, they are: `:ignore_module_conflict`,
     `:docs` and `:debug_info`. By default, uses the same
     behaviour as Elixir

   * `:elixirc_exts` - extensions to compile whenever there
     is a change:

         [compile_exts: [:ex]]

   * `:watch_exts` - extensions to watch in order to trigger
      a compilation:

         [elixirc_watch_exts: [:ex, :eex]]

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, aliases: [q: :quick],
                    switches: [force: :boolean, quick: :boolean])

    project       = Mix.project
    compile_path  = project[:compile_path]
    compile_exts  = project[:elixirc_exts]
    watch_exts    = project[:elixirc_watch_exts]
    elixirc_paths = project[:elixirc_paths]

    to_compile = Mix.Utils.extract_files(elixirc_paths, compile_exts)
    to_watch   = Mix.Project.config_files ++ Mix.Utils.extract_files(elixirc_paths, watch_exts)
    stale      = Mix.Utils.extract_stale(to_watch, [Path.join(compile_path, @manifest)])

    if opts[:force] or stale != [] do
      File.mkdir_p! compile_path
      compile_files opts[:quick], project, compile_path, to_compile, stale
      :ok
    else
      :noop
    end
  end

  defp compile_files(true, project, compile_path, to_compile, stale) do
    opts = project[:elixirc_options] || []
    opts = Keyword.put(opts, :ignore_module_conflict, true)
    Code.compiler_options(opts)
    to_compile = lc f inlist to_compile, f in stale, do: f
    compile_files(to_compile, compile_path)
  end

  defp compile_files(false, project, compile_path, to_compile, _stale) do
    Code.delete_path compile_path
    opts = project[:elixirc_options] || []
    Code.compiler_options(opts)

    { _current, to_remove } =
      Mix.Utils.manifest Path.join(compile_path, @manifest), fn ->
        compiled = compile_files to_compile, compile_path
        lc { mod, _ } inlist compiled, do: atom_to_binary(mod)
      end

    lc f inlist to_remove, do: File.rm(Path.join(compile_path, f) <> ".beam")
    Code.prepend_path compile_path
  end

  defp compile_files(files, to) do
    Kernel.ParallelCompiler.files_to_path files, to, fn(x) ->
      Mix.shell.info "Compiled #{x}"
      x
    end
  end
end
