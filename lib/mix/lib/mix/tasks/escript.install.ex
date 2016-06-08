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
  a git repository, a github repository, or a hex package.

      mix escript.install escript
      mix escript.install path/to/escript
      mix escript.install https://example.com/myescript
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
      applies to installations via URL or local path.

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make

    * `--submodules` - fetch repository submodules before building escript from
      git or github

    * `--app` - specify a custom app name to be used for building the escript
      from git, github, or hex
  """

  @behaviour Mix.Local.Installer

  @escript_file_mode 0o555 # only read and execute permissions

  @switches [force: :boolean, sha512: :string, submodules: :boolean, app: :string]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    {opts, args} = OptionParser.parse!(argv, strict: @switches)

    case args do
      ["git" | rest] when rest != [] ->
        raise_if_sha512("git", opts)
        install_from_git(rest, opts)
      ["github" | rest] when rest != [] ->
        raise_if_sha512("github", opts)
        install_from_github(rest, opts)
      ["hex" | rest] when rest != [] ->
        raise_if_sha512("hex", opts)
        install_from_hex(rest, opts)
      _ ->
        Mix.Local.Installer.install({__MODULE__, :escript}, argv, @switches)
    end
  end

  defp raise_if_sha512(name, opts) do
    if opts[:sha512] do
      Mix.raise "--sha512 is not supported for escript.install from #{name}"
    end
  end

  defp install_from_github([repo | rest], opts) do
    url = "https://github.com/#{repo}.git"
    install_from_git([url | rest], opts)
  end

  defp install_from_git([url], opts) do
    install_from_git([url, "branch", "master"], opts)
  end

  defp install_from_git([url, ref_type, ref], opts) do
    git_opts =
      ref_to_config(ref_type, ref) ++
      [git: url, submodules: opts[:submodules]]

    app_name =
      if opts[:app] do
        opts[:app]
      else
        "new escript"
      end

    fetch_build_and_install_escript(app_name, {String.to_atom(app_name), git_opts}, opts)
  end

  defp install_from_git([_url | rest], _opts) do
    Mix.raise "escript.install received invalid git checkout spec: #{Enum.join(rest, " ")}"
  end

  defp ref_to_config("branch", branch), do: [branch: branch]

  defp ref_to_config("tag", tag), do: [tag: tag]

  defp ref_to_config("ref", ref), do: [ref: ref]

  defp ref_to_config(ref_type, _) do
    Mix.raise "escript.install expected one of \"branch\", \"tag\", or \"ref\". Got: \"#{ref_type}\""
  end

  defp install_from_hex([package_name], opts) do
    install_from_hex([package_name, ">= 0.0.0"], opts)
  end

  defp install_from_hex([package_name, version], opts) do
    app_name =
      if opts[:app] do
        opts[:app]
      else
        package_name
      end

    dep_spec = {String.to_atom(app_name), version, hex: String.to_atom(package_name)}

    fetch_build_and_install_escript(app_name, dep_spec, opts)
  end

  defp install_from_hex([_package_name | rest], _opts) do
    Mix.raise "escript.install received invalid hex package spec: #{Enum.join(rest, " ")}"
  end

  defp fetch_build_and_install_escript(package_name, dep_spec, opts) do
    with_tmp_dir fn tmp_path ->
      File.mkdir_p!(tmp_path)

      File.write! Path.join(tmp_path, "mix.exs"), """
      defmodule EscriptInstaller.Mixfile do
        use Mix.Project

        def project do
          [app: :escript_installer,
           version: "0.0.1",
           deps: [#{inspect dep_spec}]]
        end
      end
      """

      with_mix_env_prod fn ->
        Mix.Project.in_project :escript_installer, tmp_path, fn _mixfile ->
          Mix.Task.run("deps.get", [])
        end

        package_path = Path.join([tmp_path, "deps", package_name])
        package_name = String.to_atom(package_name)
        post_config = [
          deps_path: Path.join(tmp_path, "deps"),
          lockfile: Path.join(tmp_path, "mix.lock")
        ]

        Mix.Project.in_project package_name, package_path, post_config, fn _mixfile ->
          Mix.Task.run("escript.build", [])
          Mix.Task.run("escript.install", install_opts(opts))
        end
      end
    end
  end

  defp with_tmp_dir(fun) do
    unique = :crypto.strong_rand_bytes(4) |> Base.url_encode64(padding: false)
    tmp_path = Path.join(System.tmp_dir!(), "mix-escript-install-" <> unique)

    try do
      fun.(tmp_path)
    after
      File.rm_rf!(tmp_path)
    end
  end

  defp with_mix_env_prod(fun) do
    previous_env = Mix.env()

    try do
      Mix.env(:prod)
      fun.()
    after
      Mix.env(previous_env)
    end
  end

  defp install_opts(opts) do
    if opts[:force], do: ["--force"], else: []
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
