defmodule Mix.SCM.Git do
  @behaviour Mix.SCM
  @moduledoc false

  def fetchable? do
    true
  end

  def format(opts) do
    opts[:git]
  end

  def format_lock(opts) do
    case opts[:lock] do
      {:git, _, lock_rev, lock_opts} ->
        lock = String.slice(lock_rev, 0, 7)

        case Enum.find_value([:branch, :ref, :tag], &List.keyfind(lock_opts, &1, 0)) do
          {:ref, _} -> lock <> " (ref)"
          {key, val} -> lock <> " (#{key}: #{val})"
          nil -> lock
        end

      _ ->
        nil
    end
  end

  def accepts_options(_app, opts) do
    opts =
      opts
      |> Keyword.put(:checkout, opts[:dest])
      |> sparse_opts()

    cond do
      gh = opts[:github] ->
        opts
        |> Keyword.delete(:github)
        |> Keyword.put(:git, "https://github.com/#{gh}.git")
        |> validate_git_options

      opts[:git] ->
        opts
        |> validate_git_options

      true ->
        nil
    end
  end

  def checked_out?(opts) do
    # Are we inside a Git repository?
    opts[:checkout]
    |> Path.join(".git/HEAD")
    |> File.regular?()
  end

  def lock_status(opts) do
    assert_git!()
    lock = opts[:lock]

    cond do
      lock_rev = get_lock_rev(lock, opts) ->
        File.cd!(opts[:checkout], fn ->
          %{origin: origin, rev: rev} = get_rev_info()

          if get_lock_repo(lock) == origin and lock_rev == rev do
            :ok
          else
            :mismatch
          end
        end)

      is_nil(lock) ->
        :mismatch

      true ->
        :outdated
    end
  end

  def equal?(opts1, opts2) do
    opts1[:git] == opts2[:git] and get_lock_opts(opts1) == get_lock_opts(opts2)
  end

  def managers(_opts) do
    []
  end

  def checkout(opts) do
    assert_git!()
    path = opts[:checkout]
    File.rm_rf!(path)
    File.mkdir_p!(path)

    File.cd!(path, fn ->
      git!(~w[-c core.hooksPath='' init --template='' --quiet])
      git!(["--git-dir=.git", "remote", "add", "origin", opts[:git]])
      checkout(path, opts)
    end)
  end

  def update(opts) do
    assert_git!()
    path = opts[:checkout]
    File.cd!(path, fn -> checkout(path, opts) end)
  end

  defp checkout(_path, opts) do
    Mix.shell().print_app()

    # Set configuration
    sparse_toggle(opts)
    update_origin(opts[:git])

    # Fetch external data
    ["--git-dir=.git", "fetch", "--force", "--quiet"]
    |> Kernel.++(progress_switch(git_version()))
    |> Kernel.++(tags_switch(opts[:tag]))
    |> git!()

    # Migrate the Git repo
    rev = get_lock_rev(opts[:lock], opts) || get_opts_rev(opts)
    git!(["--git-dir=.git", "checkout", "--quiet", rev])

    if opts[:submodules] do
      git!(
        ~w[-c core.hooksPath='' --git-dir=.git submodule update --init --recursive]
      )
    end

    # Get the new repo lock
    get_lock(opts)
  end

  defp sparse_opts(opts) do
    if opts[:sparse] do
      dest = Path.join(opts[:dest], opts[:sparse])
      Keyword.put(opts, :dest, dest)
    else
      opts
    end
  end

  defp sparse_toggle(opts) do
    cond do
      sparse = opts[:sparse] ->
        sparse_check(git_version())
        git!(["--git-dir=.git", "config", "core.sparsecheckout", "true"])
        File.mkdir_p!(".git/info")
        File.write!(".git/info/sparse-checkout", sparse)

      File.exists?(".git/info/sparse-checkout") ->
        File.write!(".git/info/sparse-checkout", "*")
        git!(["--git-dir=.git", "read-tree", "-mu", "HEAD"])
        git!(["--git-dir=.git", "config", "core.sparsecheckout", "false"])
        File.rm(".git/info/sparse-checkout")

      true ->
        :ok
    end
  end

  defp sparse_check(version) do
    unless {1, 7, 4} <= version do
      version = version |> Tuple.to_list() |> Enum.join(".")

      Mix.raise(
        "Git >= 1.7.4 is required to use sparse checkout. " <>
          "You are running version #{version}"
      )
    end
  end

  defp progress_switch(version) do
    if {1, 7, 1} <= version, do: ["--progress"], else: []
  end

  defp tags_switch(nil), do: []
  defp tags_switch(_), do: ["--tags"]

  ## Helpers

  defp validate_git_options(opts) do
    err =
      "You should specify only one of branch, ref or tag, and only once. " <>
        "Error on Git dependency: #{opts[:git]}"

    validate_single_uniq(opts, [:branch, :ref, :tag], err)
  end

  defp validate_single_uniq(opts, take, error) do
    case Keyword.take(opts, take) do
      [] -> opts
      [_] -> opts
      _ -> Mix.raise(error)
    end
  end

  defp get_lock(opts) do
    %{rev: rev} = get_rev_info()
    {:git, opts[:git], rev, get_lock_opts(opts)}
  end

  defp get_lock_repo({:git, repo, _, _}), do: repo

  defp get_lock_rev({:git, repo, lock, lock_opts}, opts) when is_binary(lock) do
    if repo == opts[:git] and lock_opts == get_lock_opts(opts) do
      lock
    end
  end

  defp get_lock_rev(_, _), do: nil

  defp get_lock_opts(opts) do
    lock_opts = Keyword.take(opts, [:branch, :ref, :tag, :sparse])

    if opts[:submodules] do
      lock_opts ++ [submodules: true]
    else
      lock_opts
    end
  end

  defp get_opts_rev(opts) do
    if branch = opts[:branch] do
      "origin/#{branch}"
    else
      opts[:ref] || opts[:tag] || "origin/master"
    end
  end

  defp get_rev_info do
    # These commands can fail and we don't want to raise.
    origin_command = ["--git-dir=.git", "config", "remote.origin.url"]
    rev_command = ["--git-dir=.git", "rev-parse", "--verify", "--quiet", "HEAD"]

    with {origin, 0} <- System.cmd("git", origin_command),
         {rev, 0} <- System.cmd("git", rev_command) do
      %{origin: String.trim(origin), rev: String.trim(rev)}
    else
      _ -> %{origin: nil, rev: nil}
    end
  end

  defp update_origin(location) do
    git!(["--git-dir=.git", "config", "remote.origin.url", location])
    :ok
  end

  defp git!(args, into \\ default_into()) do
    case System.cmd("git", args, into: into, stderr_to_stdout: true) do
      {response, 0} -> response
      {_, _} -> Mix.raise("Command \"git #{Enum.join(args, " ")}\" failed")
    end
  end

  defp default_into() do
    case Mix.shell() do
      Mix.Shell.IO -> IO.stream(:stdio, :line)
      _ -> ""
    end
  end

  defp assert_git! do
    case Mix.State.fetch(:git_available) do
      {:ok, true} ->
        :ok

      :error ->
        if System.find_executable("git") do
          Mix.State.put(:git_available, true)
        else
          Mix.raise(
            "Error fetching/updating Git repository: the \"git\" " <>
              "executable is not available in your PATH. Please install " <>
              "Git on this machine or pass --no-deps-check if you want to " <>
              "run a previously built application on a system without Git."
          )
        end
    end
  end

  def git_version do
    case Mix.State.fetch(:git_version) do
      {:ok, version} ->
        version

      :error ->
        version =
          ["--version"]
          |> git!("")
          |> parse_version

        Mix.State.put(:git_version, version)
        version
    end
  end

  defp parse_version("git version " <> version) do
    version
    |> String.split(".")
    |> Enum.take(3)
    |> Enum.map(&to_integer/1)
    |> List.to_tuple()
  end

  defp to_integer(string) do
    {int, _} = Integer.parse(string)
    int
  end
end
