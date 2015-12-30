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

  @switches [force: :boolean]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    {opts, args, _} = OptionParser.parse(argv, switches: @switches)

    case args do
      [url_or_path] ->
        if local_path?(url_or_path) or file_url?(url_or_path) do
          install_escript(url_or_path, opts)
        else
          Mix.raise "Expected a local file path or a file URL.\n#{usage}"
        end

      [] ->
        project = Mix.Project.config
        src = Mix.Escript.escript_name(project)
        if File.exists?(src) do
          install_escript(src, opts)
        else
          Mix.raise "Expected an escript to exist in the current directory " <>
                    "or an argument to be given.\n#{usage}"
        end

      _ ->
        Mix.raise "Unexpected arguments.\n#{usage}"
    end
  end

  defp usage do
    "Usage: mix escript.install <path or url>"
  end

  @escript_file_mode 0o555 # only read and execute permissions

  defp install_escript(src, opts) do
    dirname = Mix.Local.escripts_path
    dst_path = Path.join(dirname, Mix.Local.Installer.basename(src))
    if opts[:force] || should_install?(src, File.exists?(dst_path)) do
      File.rm(dst_path)

      case Mix.Utils.read_path(src, opts) do
        {:ok, binary} ->
          File.mkdir_p!(dirname)
          File.write!(dst_path, binary)
        :badpath ->
          Mix.raise "Expected #{inspect src} to be a URL or a local file path"
        {:local, message} ->
          Mix.raise message
        {kind, message} when kind in [:remote, :checksum] ->
          Mix.raise """
          #{message}

          Could not fetch escript at:

              #{src}

          Please download the escript above manually to your current directory and run:

              mix escript.install ./#{Path.basename(src)}
          """
      end

      Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(dst_path)]
      File.chmod!(dst_path, @escript_file_mode)
      check_discoverability(dst_path)
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
    Mix.shell.yes?("Found existing escript: #{Mix.Local.Installer.basename(src)}.\n" <>
                   "Are you sure you want to replace it?")
  end

  defp check_discoverability(path) do
    executable = Path.basename(path)
    sys_path = System.find_executable(executable)
    if sys_path != path do
      # FIXME: come up with a better error message? If the user already has a utility with the
      # same name installed at a path that comes before escripts_path, the warning will seem
      # confusing
      Mix.shell.info "\nConsider adding #{Mix.Local.escripts_path} to your PATH\n"
                  <> "to be able to invoke escripts by name."
    end
  end
end
