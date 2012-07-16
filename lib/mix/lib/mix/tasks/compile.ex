defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile Elixir source files"

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
  the --force option.

  ## Configuration

  * `:source_paths` - directories to find source files.
    Defaults to `["lib"]`, can be configured as:

        [source_paths: ["lib", "other"]]

  * `:compile_path` - directory to output compiled files.
    Defaults to `"ebin"`, can be configured as:

        [compile_path: "ebin"]

  * `:compile_first` - which files need to be compiled first.
    Defaults to an empty list, can be configured as:

        [compile_first: ["lib/foo.ex" "lib/bar.ex"]]

  * `:elixirc_options` - compilation options that applies
     to Elixir's compiler, they are: `:ignore_module_conflict`,
     `:docs` and `:debug_info`. They all default to false.

  ## Command line options

  The compile task accepts the following options:

  * `--force` forces compilation regardless of mod times;

  """
  def run(args) do
    destructure [force], args

    project       = Mix.Project.config
    compile_path  = project[:compile_path]  || "ebin"
    compile_first = project[:compile_first] || []
    source_paths  = project[:source_paths]  || ["lib"]
    to_compile    = extract_files(source_paths)

    last_modified = last_modified(compile_path)
    File.mkdir_p!(compile_path)

    if force == "--force" || Enum.find(to_compile, stale?(&1, last_modified)) do
      if elixir_opts = project[:elixirc_options] do
        Code.compiler_options(elixir_opts)
      end

      ordered = List.uniq compile_first ++ to_compile
      compile_files ordered, compile_path
      File.touch(compile_path)
    else
      :noop
    end
  end

  defp extract_files(paths) do
    List.concat(lc path inlist paths do
      File.wildcard(File.join([path, "**/*.ex"]))
    end)
  end

  defp compile_files(files, to) do
    Elixir.ParallelCompiler.files_to_path files, to, fn(x) ->
      IO.puts "Compiled #{x}"
      x
    end
  end

  defp stale?(path, compare) do
    { :ok, stat } = File.stat(path)
    stat.mtime > compare
  end

  defp last_modified(path) do
    case File.stat(path) do
      { :ok, stat } -> stat.mtime
      { :error, _ } -> { { 1970, 1, 1 }, { 0, 0, 0 } }
    end
  end
end
