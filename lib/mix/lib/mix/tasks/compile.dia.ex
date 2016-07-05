defmodule Mix.Tasks.Compile.Dia do
  use Mix.Task
  alias Mix.Compilers.Erlang
  alias :filelib, as: Filelib
  alias :diameter_dict_util, as: DiaDictUtil
  alias :diameter_codegen, as: DiaCodegen

  @recursive true
  @manifest ".compile.dia"

  @moduledoc """
  Compiles Diameter source files.

  ## Command line options

  There are no command line options.

  ## Configuration

    * `:erlc_paths` - directories to find source files. Defaults to `["src"]`.

    * `:dia_options` - compilation options that apply
      to Diameter's compiler.

      For a list of the many more available options,
      see [`:diameter_make`](http://erlang.org/doc/man/diameter_make.html).
      Note that the `:outdir` option is overridden by this compiler.
  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(_args) do
    project      = Mix.Project.config
    source_paths = project[:erlc_paths]
    mappings     = Enum.zip(["dia"], source_paths)
    options      = project[:dia_options] || []

    Erlang.compile(manifest(), mappings, :dia, :erl, [], fn
      input, output ->
        :ok = Filelib.ensure_dir(output)
        :ok = Path.join("include", "dummy.hrl") |> Filelib.ensure_dir
        case DiaDictUtil.parse({:path, input}, []) do
          {:ok, spec} ->
            filename = dia_filename(input, spec)
            include_path = to_charlist project[:erlc_include_path]
            _ = DiaCodegen.from_dict(filename, spec, [{:outdir, 'src'} | options], :erl)
            _ = DiaCodegen.from_dict(filename, spec, [{:outdir, include_path} | options], :hrl)
            file = to_charlist Path.join("src", filename)
            compile_path = to_charlist Mix.Project.compile_path(project)
            erlc_options = project[:erlc_options] || []
            erlc_options = erlc_options ++ [{:outdir, compile_path}, {:i, include_path}, :report]
            :compile.file(file, erlc_options)
          error -> Mix.raise "Diameter compiler error: #{inspect error}"
        end
    end)
  end

  @doc """
  Returns Dia manifests.
  """
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    Erlang.clean(manifest())
  end

  defp dia_filename(file, spec) do
    case spec[:name] do
      :undefined -> Path.basename(file) |> Path.rootname
      name -> name
    end
  end
end
