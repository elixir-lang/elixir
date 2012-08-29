defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile source files"

  @moduledoc """
  A meta task that compile source files. It simply runs the
  compilers registered in your project. At the end of compilation
  it ensures load paths are set.

  ## Configuration

  * `:compilers` - compilers to be run, defaults to:

      [:elixir, :app]

    It can be configured to handle custom compilers, for example:

      [compilers: [:elixir, :mycompiler, :app]]

  ## Command line options

  * `--list`     - List all enabled compilers.
  * `--no-check` - Skip dependencies check before compilation.

  """
  def run(["--list"]) do
    Mix.Task.load_all

    shell   = Mix.shell
    modules = Mix.Task.all_modules

    docs = lc module inlist modules,
             task = Mix.Task.task_name(module),
             match?("compile." <> _, task),
             doc = Mix.Task.shortdoc(module) do
      { task, doc }
    end

    max = Enum.reduce docs, 0, fn({ task, _ }, acc) ->
      max(size(task), acc)
    end

    sorted = Enum.qsort(docs)

    Enum.each sorted, fn({ task, doc }) ->
      shell.info format('mix ~-#{max}s # ~s', [task, doc])
    end

    shell.info "\nEnabled compilers: #{Enum.join get_compilers, ", "}"
  end

  def run(args) do
    remove_ebin

    changed = Enum.reduce get_compilers, false, fn(compiler, acc) ->
      res = Mix.Task.run("compile.#{compiler}", args)
      acc or res != :noop
    end

    if changed do
      compile_path = Mix.project[:compile_path]
      File.touch(compile_path)
    end

    readd_ebin
  end

  defp remove_ebin do
    ebin = Mix.project[:compile_path] || "ebin"
    :code.del_path(ebin /> File.expand_path /> binary_to_list)
  end

  defp readd_ebin do
    ebin = Mix.project[:compile_path] || "ebin"
    Code.prepend_path(ebin)
  end

  defp get_compilers do
    Mix.project[:compilers] || if Mix.Project.defined? do
      [:elixir, :app]
    else
      [:elixir]
    end
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) /> iolist_to_binary
  end
end
