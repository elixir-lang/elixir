defmodule Mix.Tasks.Compile.Asn1 do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.asn1"

  @moduledoc """
  Compile ASN.1 source files.

  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the same source directory with a .erl extension.
  You can force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  ## Configuration

    * `:asn1_paths` - directories to find source files. Defaults to `["asn1"]`.

    * `:erlc_paths` - directories to store generated source files. Defaults to `["src"]`.

    * `:asn1_options` - compilation options that apply
      to ASN.1's compiler. There are many other available
      options here: http://erlang.org/doc/man/asn1ct.html#compile-2.

  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:asn1_paths] || ["asn1"]
    mappings     = Enum.zip(source_paths, project[:erlc_paths])
    options      = project[:asn1_options] || []

    Erlang.compile(manifest(), mappings, :'set.asn1', :erl, opts[:force], fn
      input, output ->
        options = options ++ [:noobj, outdir: Erlang.to_erl_file(Path.dirname(output))]
        { :asn1ct.compile(Erlang.to_erl_file(input), options),
          String.to_atom Path.basename(input, "set.asn1")}
    end)
  end

  @doc """
  Returns ASN.1 manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    Erlang.clean(manifest())
  end
end
