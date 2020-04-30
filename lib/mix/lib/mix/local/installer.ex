defmodule Mix.Local.Installer do
  @moduledoc false

  # This module implements pieces of functionality shared
  # by the archive- and escript-related tasks.

  @typedoc """
  Installs types supported by `Mix.Local.Installer`.

    * `:project` - installs the current Mix project's artifact
    * `:local` - installs the artifact located at `path`
    * `:url` - installs the artifact retrievable at `url`
    * `:fetcher` - builds and install the artifact generated by the `dep_spec`

  """
  @type install_spec ::
          :project
          | {:local, path :: Path.t()}
          | {:url, url :: binary}
          | {:fetcher, dep_spec :: tuple}

  @doc """
  Checks that the `install_spec` and `opts` are supported by the respective module.
  """
  @callback check_install_spec(install_spec, opts :: keyword) :: :ok | {:error, String.t()}

  @doc """
  Returns a list of already installed version of the same artifact.
  """
  @callback find_previous_versions(basename :: String.t()) :: [Path.t()]

  @doc """
  Builds a local artifact either from a remote dependency or for
  the current project.
  """
  @callback build(install_spec, opts :: Keyword.t()) :: Path.t()

  @doc """
  The installation itself.
  """
  @callback install(basename :: String.t(), contents :: binary, previous :: [Path.t()]) :: :ok

  @doc """
  Common implementation of installation for archives and escripts.

  Relies on a few callbacks provided by respective callback modules
  for customizing certain steps in the installation process.
  """
  @spec install(module, OptionParser.argv(), keyword) :: boolean
  def install(module, argv, switches) do
    {opts, args} = OptionParser.parse!(argv, strict: switches)

    install_spec =
      case parse_args(args, opts) do
        {:error, message} -> Mix.raise(message <> "\n\n" <> usage(module))
        install_spec -> install_spec
      end

    case module.check_install_spec(install_spec, opts) do
      :ok -> :noop
      {:error, message} -> Mix.raise(message <> "\n\n" <> usage(module))
    end

    case install_spec do
      {:fetcher, dep_spec} ->
        if opts[:sha512] do
          Mix.raise(
            "--sha512 is not supported when installing from git/github/hex\n\n" <> usage(module)
          )
        end

        fetch(dep_spec, fn _ ->
          local_install(module, module.build(install_spec, opts), opts)
        end)

      {path_or_url, src} when path_or_url in [:local, :url] ->
        local_install(module, src, opts)

      :project ->
        local_install(module, module.build(install_spec, opts), opts)
    end
  end

  defp task(module) do
    Mix.Utils.module_name_to_command(module, 2)
  end

  defp usage(module) do
    "For more information run \"mix help #{task(module)}\""
  end

  defp local_path?(url_or_path) do
    File.regular?(url_or_path)
  end

  defp file_url?(url_or_path) do
    URI.parse(url_or_path).scheme in ["http", "https"]
  end

  defp local_install(module, src, opts) do
    basename = Path.basename(URI.parse(src).path)
    previous_files = module.find_previous_versions(basename)

    if opts[:force] || should_install?(src, previous_files) do
      case Mix.Utils.read_path(src, opts) do
        {:ok, binary} ->
          module.install(basename, binary, previous_files)

        :badpath ->
          Mix.raise("Expected #{inspect(src)} to be a local file path")

        {:local, message} ->
          Mix.raise(message)

        {kind, message} when kind in [:remote, :checksum] ->
          Mix.raise("""
          #{message}

          Could not run #{task(module)} for:

              #{src}

          Please download the file above to your current directory and run:

              mix #{task(module)} ./#{basename}

          You can download it either with your browser or via the command line.

          On Unix-like operating systems (Linux, macOS):

              wget #{src}

          or

              curl -o #{basename} #{src}

          On Windows / PowerShell (Windows 7 or later):

              powershell -Command "Invoke-WebRequest #{src} -OutFile #{basename}"

          or

              powershell -Command "(New-Object Net.WebClient).DownloadFile('#{src}', '#{basename}')"
          """)
      end

      true
    else
      false
    end
  end

  defp should_install?(src, previous_files) do
    message =
      case previous_files do
        [] ->
          "Are you sure you want to install #{inspect(src)}?"

        [file] ->
          "Found existing entry: #{file}\n" <>
            "Are you sure you want to replace it with #{inspect(src)}?"

        files ->
          "Found existing entries: #{Enum.map_join(files, ", ", &Path.basename/1)}\n" <>
            "Are you sure you want to replace them with #{inspect(src)}?"
      end

    Mix.shell().yes?(message)
  end

  @doc """
  Receives `argv` and `opts` from options parsing and returns an `install_spec`.
  """
  @spec parse_args([String.t()], keyword) :: install_spec | {:error, String.t()}
  def parse_args(argv, opts)

  def parse_args([], _opts) do
    :project
  end

  def parse_args([url_or_path], _opts) do
    cond do
      local_path?(url_or_path) -> {:local, url_or_path}
      file_url?(url_or_path) -> {:url, url_or_path}
      true -> {:error, "Expected #{inspect(url_or_path)} to be a local file path"}
    end
  end

  def parse_args(["github" | rest], opts) do
    [repo | rest] = rest
    url = "https://github.com/#{repo}.git"
    parse_args(["git", url] ++ rest, opts)
  end

  def parse_args(["git", url], opts) do
    parse_args(["git", url, "branch", "master"], opts)
  end

  def parse_args(["git", url, ref_type, ref], opts) do
    case ref_to_config(ref_type, ref) do
      {:error, error} ->
        {:error, error}

      git_config ->
        git_opts = git_config ++ [git: url, submodules: opts[:submodules]]

        app_name =
          if opts[:app] do
            opts[:app]
          else
            "new package"
          end

        {:fetcher, {String.to_atom(app_name), git_opts}}
    end
  end

  def parse_args(["git" | [_url | rest]], _opts) do
    {:error, "received invalid git checkout spec: #{Enum.join(rest, " ")}"}
  end

  def parse_args(["hex", package_name], opts) do
    parse_args(["hex", package_name, ">= 0.0.0"], opts)
  end

  def parse_args(["hex", package_name, version], opts) do
    app_name =
      if opts[:app] do
        opts[:app]
      else
        package_name
      end

    dep_opts =
      opts
      |> Keyword.take([:organization])
      |> Keyword.put(:hex, String.to_atom(package_name))

    {:fetcher, {String.to_atom(app_name), version, dep_opts}}
  end

  def parse_args(["hex" | [_package_name | rest]], _opts) do
    {:error, "received invalid Hex package spec: #{Enum.join(rest, " ")}"}
  end

  defp ref_to_config("branch", branch), do: [branch: branch]
  defp ref_to_config("tag", tag), do: [tag: tag]
  defp ref_to_config("ref", ref), do: [ref: ref]

  defp ref_to_config(ref_type, _) do
    {:error, "expected one of \"branch\", \"tag\", or \"ref\". Got: \"#{ref_type}\""}
  end

  @doc """
  A common implementation for uninstalling archives and scripts.
  """
  @spec uninstall(Path.t(), String.t(), OptionParser.argv(), keyword) :: Path.t() | nil
  def uninstall(root, listing, argv, switches) do
    {opts, argv} = OptionParser.parse!(argv, switches: switches)

    if name = List.first(argv) do
      found =
        if File.exists?(Path.join(root, name)) do
          Path.join(root, name)
        else
          matching_package(root, name)
        end

      cond do
        found && should_uninstall?(found, opts) ->
          File.rm_rf!(found)
          found

        found ->
          nil

        true ->
          Mix.shell().error("Could not find a local artifact named #{inspect(name)}. We found:")
          Mix.Task.rerun(listing)
          nil
      end
    else
      Mix.raise("No argument was given to uninstall command")
    end
  end

  defp matching_package(root, name) do
    root |> Path.join(name <> "-*") |> Path.wildcard() |> List.first()
  end

  defp should_uninstall?(path, opts) do
    opts[:force] || Mix.shell().yes?("Are you sure you want to uninstall #{path}?")
  end

  @doc """
  Fetches `dep_spec` with `in_fetcher` and then runs `in_package`.

  Generates a new Mix project in a temporary directory with the given `dep_spec`
  added to a mix.exs. Then, `in_fetcher` is executed in the fetcher project. By
  default, this fetches the dependency, but you can provide an `in_fetcher`
  during test or for other purposes. After the `in_fetcher` is executed,
  `in_package` is executed in the now (presumably) fetched package, with the
  package's config overridden with the deps_path and lockfile of the fetcher
  package. Also, the Mix env is set to :prod.
  """
  @spec fetch(tuple, (atom -> any), (atom -> any)) :: any
  def fetch(dep_spec, in_fetcher \\ &in_fetcher/1, in_package) do
    with_tmp_dir(fn tmp_path ->
      File.mkdir_p!(tmp_path)

      File.write!(Path.join(tmp_path, "mix.exs"), """
      defmodule Mix.Local.Installer.MixProject do
        use Mix.Project

        def project do
          [
            app: :mix_local_installer,
            version: "1.0.0",
            deps: [#{inspect(dep_spec)}]
          ]
        end
      end
      """)

      with_mix_env_prod(fn ->
        Mix.ProjectStack.on_clean_slate(fn ->
          Mix.Project.in_project(:mix_local_installer, tmp_path, in_fetcher)

          package_name = elem(dep_spec, 0)
          package_name_string = Atom.to_string(package_name)
          package_path = Path.join([tmp_path, "deps", package_name_string])

          post_config = [
            deps_path: Path.join(tmp_path, "deps"),
            lockfile: Path.join(tmp_path, "mix.lock")
          ]

          Mix.Project.in_project(package_name, package_path, post_config, fn mix_exs ->
            in_fetcher.(mix_exs)
            in_package.(mix_exs)
          end)
        end)
      end)
    end)
  after
    :code.purge(Mix.Local.Installer.Fetcher)
    :code.delete(Mix.Local.Installer.Fetcher)
  end

  defp in_fetcher(_mix_exs) do
    Mix.Task.run("deps.get", ["--only", Atom.to_string(Mix.env())])
  end

  defp with_tmp_dir(fun) do
    unique = :crypto.strong_rand_bytes(4) |> Base.url_encode64(padding: false)
    tmp_path = Path.join(System.tmp_dir!(), "mix-local-installer-fetcher-" <> unique)

    try do
      fun.(tmp_path)
    after
      File.rm_rf(tmp_path)
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
end
