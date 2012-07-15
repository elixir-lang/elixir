defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile Elixir source files."

  @moduledoc """
  A task to compile Elixir source files. By default,
  output is compiled to `ebin/`. Sourcefiles are assumed
  to be in `lib/`. You can change this by setting `:source_paths`
  in your project. It is expected to be a list of paths like
  so:

  `[source_paths: ["src"]]`

  This list will be merged with the
  default one and the compile task will look for source files
  in both `lib/` and `src/`. Setting the compile path works similarly.

  `[compile_path: "exbin"]`

  This sets the compile path to `exbin` so that source files will
  be compiled to this directory instead of the default one.

  Sometimes files need to be compiled in a specific order. If you
  have this issue, you can use the `compile_first` key:

  `[compile_first: ["lib/foo.ex" "lib/bar.ex"]]`

  This will cause `foo.ex` and `bar.ex` to be compiled before any
  other files and in the order they appear in the list.

  If you need to pass compilation options, set them in your
  project under the `compile_options` key.

  NOTE: When this task runs, it touches your `:compile_path`
        directory and sets the modification time to the current
        time and date. The reason for this is so that when the
        task runs next time, it can check the mod times of all
        of the files to be compiled and if they haven't been
        changed since the last compilation, it will not compile
        them at all. If any one of them has changed, it compiles
        everything. You can force compilation regardless of mod
        times by passing the --force option.

  Arguments:
    --force: Force compilation regardless of modtimes
    none: Compile all source files if they have changed since the last
          compilation.
  """
  def run(args) do
    destructure([force], args)
    project = Mix.Mixfile.get_project
    compile_path = project[:compile_path]
    compile_first = project[:compile_first]
    Code.compiler_options(project[:compile_options])
    :file.make_dir compile_path
    to_compile = extract_files(project[:source_paths])
    if force == "--force" || Enum.find(to_compile, stale?(&1, compile_path <> "/__MAIN__")) do
      if !Enum.empty?(compile_first) do
        IO.puts "\nPerforming initial compilation (compile_first)...\n"
        Enum.each(compile_first, compile_file(&1, compile_path))
        IO.puts "\nCompiling everything else...\n"
      end
      Enum.each(project[:source_paths], fn(path) ->
        files = File.wildcard(File.join([path, "**/*.ex"]))
	compile_files(files, compile_path)
      end)
    end
    Mix.Utils.touch(compile_path)
  end

  defp extract_files(paths) do
    List.concat(lc path inlist paths, do: File.wildcard(File.join([path, "**/*.ex"])))
  end
  defp compile_files(files, to) do
    Elixir.ParallelCompiler.files_to_path(files, to, fn(x) ->
      IO.puts Enum.join(["Compiling ", x, " to ", to, "..."])
      x 
    end)
  end
  defp compile_file(file, to) do
    compile_files([file], to)
  end
  defp stale?(file, to) do
    {:ok, file_info} = File.read_info(file)
    case File.read_info(to) do
    {:ok, to_info} ->
      file_info.mtime > to_info.mtime
    {:error, _} ->
      true
    end
  end
end
