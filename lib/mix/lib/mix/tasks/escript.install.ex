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

  @behaviour Mix.Local.Installer

  @escript_file_mode 0o555 # only read and execute permissions

  @switches [force: :boolean]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    Mix.Local.Installer.install({__MODULE__, :escript}, argv, @switches)
  end

  ### Mix.Local.Installer callbacks

  def check_path_or_url(_), do: :ok

  def find_previous_versions(_src, dst) do
    if File.exists?(dst), do: [dst], else: []
  end

  def before_install(_src, dst_path) do
    File.rm(dst_path)
    File.rm(dst_path <> ".bat")
    :ok
  end

  def after_install(dst, _previous) do
    File.chmod!(dst, @escript_file_mode)
    write_bat!(dst <> ".bat", :os.type)
    check_discoverability(dst)
  end

  ### Private helpers

  defp write_bat!(path, {:win32, _}) do
    File.write!(path, """
    @echo off
    @escript "%~dpn0" %*
    """)
    File.chmod!(path, @escript_file_mode)
  end
  defp write_bat!(_path, _type) do
    :ok
  end

  defp check_discoverability(path) do
    executable = Path.basename(path)
    sys_path = System.find_executable(executable)
    if sys_path != path do
      Mix.shell.info "\nConsider adding #{Mix.Local.path_for(:escript)} to your PATH\n"
                  <> "to be able to invoke escripts by name."
    end
  end
end
