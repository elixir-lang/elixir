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

  For convenience, consider adding `~/.mix/escripts` directory to your
  `PATH` environment variable. For more information, check the wikipedia
  article on PATH: https://en.wikipedia.org/wiki/PATH_(variable)

  ## Command line options

    * `--sha512` - checks the escript matches the given sha512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make

  """

  @behaviour Mix.Local.Installer

  @escript_file_mode 0o555 # only read and execute permissions

  @switches [force: :boolean, sha512: :string]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    Mix.Local.Installer.install({__MODULE__, :escript}, argv, @switches)
  end

  ### Mix.Local.Installer callbacks

  def check_path_or_url(_), do: :ok

  def find_previous_versions(_src, dst) do
    if File.exists?(dst), do: [dst], else: []
  end

  def install(dst, binary, _previous) do
    if escript?(binary) do
      _ = File.rm(dst)
      _ = File.rm(dst <> ".bat")

      executable = Path.basename(dst)
      previous_executable = System.find_executable(executable)

      File.mkdir_p!(Path.dirname(dst))
      File.write!(dst, binary)
      File.chmod!(dst, @escript_file_mode)
      write_bat!(dst <> ".bat", :os.type)

      Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(dst)]
      check_discoverability(dst, executable, previous_executable)
      :ok
    else
      Mix.raise "The given path does not point to an escript, installation aborted"
    end
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

  defp check_discoverability(dst, executable, previous_executable) do
    current_executable = System.find_executable(executable)

    cond do
      # If existing executable was changed,
      # it was overridden
      previous_executable && previous_executable != current_executable ->
        Mix.shell.error "\nwarning: escript #{inspect executable} overrides executable " <>
                        "#{inspect previous_executable} already in your PATH\n"

      # If existing executable didn't change but it is not the one we installed,
      # it is a conflict
      previous_executable && previous_executable != dst ->
        Mix.shell.error "\nwarning: escript #{inspect executable} conflicts with executable " <>
                        "#{inspect previous_executable} already in your PATH\n"

      # If current executable is nil or does not match the one we just installed,
      # PATH is misconfigured
      current_executable != dst ->
        Mix.shell.error "\nwarning: you must append #{inspect Mix.Local.path_for(:escript)} " <>
                        "to your PATH if you want to invoke escripts by name\n"

      true ->
        :ok
    end
  end

  defp escript?(binary) do
    parts = String.split(binary, "\n", parts: 4)
    match?(["#!" <> _, _, _, <<80, 75, 3, 4, _::binary>>], parts)
  end
end
