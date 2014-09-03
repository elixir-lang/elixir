defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile source files"
  @recursive true

  @moduledoc """
  A meta task that compiles source files.

  It simply runs the compilers registered in your project. At
  the end of compilation it ensures load paths are set.

  ## Configuration

    * `:compilers` - compilers to run, defaults to:
      `[:leex, :yeec, :erlang, :elixir, :app]`

  ## Command line options

    * `--list`          - list all enabled compilers
    * `--no-deps-check` - skip checking of dependencies
    * `--force`         - force compilation

  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(["--list"]) do
    loadpaths!
    _ = Mix.Task.load_all

    shell   = Mix.shell
    modules = Mix.Task.all_modules

    docs = for module <- modules,
               task = Mix.Task.task_name(module),
               match?("compile." <> _, task),
               doc = Mix.Task.moduledoc(module) do
      {task, first_line(doc)}
    end

    max = Enum.reduce docs, 0, fn({task, _}, acc) ->
      max(byte_size(task), acc)
    end

    sorted = Enum.sort(docs)

    Enum.each sorted, fn({task, doc}) ->
      shell.info format('mix ~-#{max}s # ~ts', [task, doc])
    end

    shell.info "\nEnabled compilers: #{Enum.join compilers(), ", "}"
  end

  def run(args) do
    Mix.Project.get!
    Mix.Task.run "loadpaths", ["--no-readd"|args]

    res =
      Enum.map(compilers(), fn(compiler) ->
        List.wrap Mix.Task.run("compile.#{compiler}", args)
      end)

    true = Code.prepend_path(Mix.Project.compile_path)
    unless "--no-readd" in args, do: Code.readd_paths()
    if Enum.any?(res, &(:ok in &1)), do: :ok, else: :noop
  end

  # Loadpaths without checks because compilers may be defined in deps.
  defp loadpaths! do
    Mix.Task.run "loadpaths", ["--no-elixir-version-check", "--no-deps-check"]
    Mix.Task.reenable "loadpaths"
  end

  @doc """
  Returns all compilers.
  """
  def compilers do
    Mix.Project.config[:compilers] || Mix.compilers
  end

  @doc """
  Returns manifests for all compilers.
  """
  def manifests do
    Enum.flat_map(compilers(), fn(compiler) ->
      module = Mix.Task.get!("compile.#{compiler}")
      if function_exported?(module, :manifests, 0) do
        module.manifests
      else
        []
      end
    end)
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> IO.iodata_to_binary
  end

  defp first_line(doc) do
    String.split(doc, "\n", parts: 2) |> hd |> String.strip |> String.rstrip(?.)
  end
end
