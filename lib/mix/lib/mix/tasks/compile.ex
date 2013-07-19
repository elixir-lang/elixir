defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile source files"
  @recursive true

  @moduledoc """
  A meta task that compiles source files. It simply runs the
  compilers registered in your project. At the end of compilation
  it ensures load paths are set.

  ## Configuration

  * `:compilers` - compilers to be run, defaults to:

    ```
    [:elixir, :app]
    ```

    It can be configured to handle custom compilers, for example:

    ```
    [compilers: [:elixir, :mycompiler, :app]]
    ```

  ## Command line options

  * `--list` - List all enabled compilers.

  Remaining options are forwarded to underlying compilers.

  ## Common configuration

  The following options are usually shared by different compilers:

  * `:compile_path` - directory to output compiled files.
    Defaults to `"ebin"`, can be configured as:

    ```
    [compile_path: "ebin"]
    ```

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

    sorted = Enum.sort(docs)

    Enum.each sorted, fn({ task, doc }) ->
      shell.info format('mix ~-#{max}s # ~ts', [task, doc])
    end

    shell.info "\nEnabled compilers: #{Enum.join get_compilers, ", "}"
  end

  @doc """
  Runs this compile task by recursively calling all registered compilers.
  """
  def run(args) do
    Mix.Task.run("loadpaths", args)

    Enum.each(get_compilers, fn(compiler) ->
      Mix.Task.run("compile.#{compiler}", args)
    end)

    Code.prepend_path Mix.project[:compile_path]
  end

  @doc """
  Returns manifests for all compilers.
  """
  def manifests do
    Enum.reduce(get_compilers, [], fn(compiler, acc) ->
      module = Mix.Task.get("compile.#{compiler}")
      if function_exported?(module, :manifest, 0) do
        [module.manifest|acc]
      else
        acc
      end
    end)
  end

  defp get_compilers do
    Mix.project[:compilers] || if Mix.Project.get do
      [:yecc, :leex, :erlang, :elixir, :app]
    else
      [:yecc, :leex, :erlang, :elixir]
    end
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> iolist_to_binary
  end
end
