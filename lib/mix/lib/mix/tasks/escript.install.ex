defmodule Mix.Tasks.Escript.Install do
  use Mix.Task

  @shortdoc "Install an escript locally"

  @moduledoc """
  Install an escript locally.

  If no argument is supplied but there is an escript in the project's root directory
  (created with `mix escript.build`), then the escript will be installed
  locally. For example:

      mix do escript.build, escript.install

  If an argument is provided, it should be a local path or a URL to a prebuilt escript.

      mix escript.install escript
      mix escript.install path/to/escript
      mix escript.install https://example.com/myescript

  After installation, the escript can be invoked as

      ~/.mix/escripts/foo

  For convenience, consider adding `~/.mix/escripts` directory to your `PATH` environment variable.

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make

  """

  def run(args) do
    {opts, args, _} = OptionParser.parse(args, switches: [force: :boolean])

    case args do
      [url_or_path] ->
        if local_path?(url_or_path) or file_url?(url_or_path) do
          install_escript(url_or_path, opts)
        else
          Mix.raise "Expected PATH to be a local file path or a file URL."
        end

      [] ->
        project = Mix.Project.config
        src = Mix.Escript.escript_name(project)
        if File.exists?(src) do
          install_escript(src, opts)
        else
          Mix.raise "Expected PATH to be given.\n#{usage}"
        end

      _ ->
          Mix.raise "Unexpected arguments.\n#{usage}"
    end
  end

  defp usage do
    "Usage: mix escript.install PATH"
  end

  @escript_file_mode 0o555 # only read and execute permissions

  defp install_escript(src, opts) do
    dst = Path.join([Mix.Local.escripts_path, basename(src)])
    if opts[:force] || should_install?(src, File.exists?(dst)) do
      File.rm!(dst)
      if Mix.Utils.copy_path!(src, dst, opts) do
        Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(dst)]
        File.chmod!(dst, @escript_file_mode)
      end
    end
  end

  defp local_path?(url_or_path) do
    File.regular?(url_or_path)
  end

  defp file_url?(url_or_path) do
    URI.parse(url_or_path).scheme in ["http", "https"]
  end

  defp should_install?(src, false) do
    Mix.shell.yes?("Are you sure you want to install escript #{src}?")
  end

  defp should_install?(src, true) do
    Mix.shell.yes?("Found existing escript: #{basename(src)}.\n" <>
                   "Are you sure you want to replace it?")
  end

  defp basename(path) do
    %URI{path: path} = URI.parse(path)
    Path.basename(path)
  end
end
