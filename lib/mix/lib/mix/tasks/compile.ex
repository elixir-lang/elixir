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

        [:leex, :yeec, :erlang, :elixir, :app]

  ## Command line options

  * `--list` - List all enabled compilers

  * `--no-deps-check` - Skips checking of dependencies

  Remaining options are forwarded to underlying compilers.

  """
  def run(["--list"]) do
    Mix.Task.load_all

    shell   = Mix.shell
    modules = Mix.Task.all_modules

    docs = for module <- modules,
               task = Mix.Task.task_name(module),
               match?("compile." <> _, task),
               doc = Mix.Task.moduledoc(module) do
      { task, first_line(doc) }
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
    # --no-deps is used only internally. It has not purpose
    # from Mix.CLI because the CLI itself already loads
    # dependencies.
    unless "--no-deps" in args do
      Mix.Task.run "deps.loadpaths", args
    end

    Mix.Task.run "loadpaths", args

    res =
      Enum.map(get_compilers, fn(compiler) ->
        List.wrap Mix.Task.run("compile.#{compiler}", args)
      end)

    Code.prepend_path Mix.Project.compile_path
    if Enum.any?(res, &(:ok in &1)), do: :ok, else: :noop
  end

  @doc """
  Returns manifests for all compilers.
  """
  def manifests do
    Enum.flat_map(get_compilers, fn(compiler) ->
      module = Mix.Task.get!("compile.#{compiler}")
      if function_exported?(module, :manifests, 0) do
        module.manifests
      else
        []
      end
    end)
  end

  defp get_compilers do
    Mix.project[:compilers] ||
      [:yecc, :leex, :erlang, :elixir, :app]
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> iolist_to_binary
  end

  defp first_line(doc) do
    String.split(doc, "\n", global: false) |> hd |> String.strip |> String.rstrip(?.)
  end
end
