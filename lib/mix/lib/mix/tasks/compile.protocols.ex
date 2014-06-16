defmodule Mix.Tasks.Compile.Protocols do
  use Mix.Task

  @recursive true

  @shortdoc "Consolidates all protocols in all paths"

  @moduledoc ~S"""
  Consolidates all protocols in all paths.

  This module consolidates all protocols in the code path
  and output the new binary files to the given directory
  (defaults to "consolidated").

  A new directory will be created with the consolidated
  protocol versions in the build directory for the given
  environment. Simply add it to your codepath to make use
  of it:

      $ mix run -pa _build/dev/consolidated

  You can verify a protocol is consolidated by checking
  its attributes:

      $ iex -S mix run -pa _build/dev/consolidated
      iex> Protocol.consolidated?(Enumerable)
      true

  """

  def run(args) do
    Mix.Task.run "compile", args
    {opts, _, _} = OptionParser.parse(args, switches: [output: :string], aliases: [o: :output])

    paths = filter_otp(:code.get_path, :code.lib_dir)
    paths
    |> Protocol.extract_protocols
    |> consolidate(paths, opts[:output] || Path.join(Mix.Project.build_path, "consolidated"))

    :ok
  end

  defp filter_otp(paths, otp) do
    Enum.filter(paths, &(not :lists.prefix(&1, otp)))
  end

  defp consolidate(protocols, paths, output) do
    File.mkdir_p!(output)

    for protocol <- protocols do
      impls = Protocol.extract_impls(protocol, paths)
      maybe_reload(protocol)
      {:ok, binary} = Protocol.consolidate(protocol, impls)
      File.write!(Path.join(output, "#{protocol}.beam"), binary)
      Mix.shell.info "Consolidated #{inspect protocol}"
    end

    relative = Path.relative_to_cwd(output)
    Mix.shell.info "Consolidated protocols written to #{relative}"
  end

  defp maybe_reload(module) do
    case :code.which(module) do
      :non_existing ->
        module
      file ->
        unless Path.extname(file) == ".beam" do
          :code.purge(module)
          :code.delete(module)
        end
    end
  end
end
