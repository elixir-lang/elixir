defmodule Mix.Tasks.Escript.Install do
  use Mix.Task

  @shortdoc "Installs an escript locally"

  @moduledoc """
  Installs an escript locally.

  If no argument is supplied but there is an escript in the project's root directory
  (created with `mix escript.build`), then the escript will be installed
  locally. For example:

      mix do escript.build, escript.install

  If an argument is provided, it should be a local path or a URL to a prebuilt escript,
  a Git repository, a GitHub repository, or a Hex package.

      mix escript.install escript
      mix escript.install path/to/escript
      mix escript.install https://example.com/my_escript
      mix escript.install git https://path/to/git/repo
      mix escript.install git https://path/to/git/repo branch git_branch
      mix escript.install git https://path/to/git/repo tag git_tag
      mix escript.install git https://path/to/git/repo ref git_ref
      mix escript.install github user/project
      mix escript.install github user/project branch git_branch
      mix escript.install github user/project tag git_tag
      mix escript.install github user/project ref git_ref
      mix escript.install hex hex_package
      mix escript.install hex hex_package 1.2.3

  After installation, the escript can be invoked as

      ~/.mix/escripts/foo

  For convenience, consider adding `~/.mix/escripts` directory to your
  `PATH` environment variable. For more information, check the wikipedia
  article on PATH: https://en.wikipedia.org/wiki/PATH_(variable)

  ## Command line options

    * `--sha512` - checks the escript matches the given SHA-512 checksum. Only
      applies to installations via URL or local path

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like Make

    * `--submodules` - fetches repository submodules before building escript from
      Git or GitHub

    * `--app` - specifies a custom app name to be used for building the escript
      from Git, GitHub, or Hex

  """

  @behaviour Mix.Local.Installer

  @escript_file_mode 0o555 # only read and execute permissions

  @switches [force: :boolean, sha512: :string, submodules: :boolean, app: :string]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    Mix.Local.Installer.install({__MODULE__, :escript}, argv, @switches)
  end

  ### Mix.Local.Installer callbacks

  def check_install_spec(_, _), do: :ok

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
      case :os.type() do
        {:win32, _} -> System.put_env("PATH", System.get_env("PATH") <> ";" <> String.replace(Path.dirname(dst), "/", "\\" ))
        _ -> :ok
      end
      
      check_discoverability(dst, executable, previous_executable)
      :ok
    else
      Mix.raise "The given path does not point to an escript, installation aborted"
    end
  end

  def build(_mixfile) do
    Mix.Task.run("escript.build", [])
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
      previous_executable && not equal_with_dot_bat previous_executable, dst  ->
        Mix.shell.error "\nwarning: escript #{inspect executable} conflicts with executable " <>
                        "#{inspect previous_executable} already in your PATH\n"

      # If current executable is nil or does not match the one we just installed,
      # PATH is misconfigured
      not equal_with_dot_bat current_executable, dst ->
        IO.warn(current_executable)
        IO.warn(dst)
        Mix.shell.error "\nwarning: you must append #{inspect Mix.Local.path_for(:escript)} " <>
                        "to your PATH if you want to invoke escripts by name\n"

      true ->
        :ok
    end
  end

  defp equal_with_dot_bat(program, program_stem) do
    program == program_stem or program == program_stem <> ".bat"
  end

  defp escript?(binary) do
    parts = String.split(binary, "\n", parts: 4)
    match?(["#!" <> _, _, _, <<80, 75, 3, 4, _::binary>>], parts)
  end
end
