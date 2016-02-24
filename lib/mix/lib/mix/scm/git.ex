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
        case Enum.find_value [:branch, :ref, :tag], &List.keyfind(lock_opts, &1, 0) do
          {:ref, _}  -> lock <> " (ref)"
          {key, val} -> lock <> " (#{key}: #{val})"
          nil        -> lock
        end
      _ ->
        nil
    end
  end

  def accepts_options(_app, opts) do
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
    # Are we inside a git repository?
    File.regular?(Path.join(opts[:dest], ".git/HEAD"))
  end

  def lock_status(opts) do
    assert_git

    case opts[:lock] do
      {:git, lock_repo, lock_rev, lock_opts} ->
        File.cd!(opts[:dest], fn ->
          rev_info = get_rev_info
          cond do
            not lock_up_to_date?(opts)     -> :outdated
            lock_rev  != rev_info[:rev]    -> :mismatch
            lock_repo != rev_info[:origin] -> :mismatch
            true                           -> :ok
          end
        end)
      nil ->
        :mismatch
      _ ->
        :outdated
    end
  end

  def equal?(opts1, opts2) do
    git_repos_match?(opts1[:git], opts2[:git]) &&
      get_lock_opts(opts1) == get_lock_opts(opts2)
  end

  def managers(_opts) do
    []
  end

  def checkout(opts) do
    assert_git

    path     = opts[:dest]
    location = opts[:git]

    _ = File.rm_rf!(path)
    git!(~s(clone --no-checkout --progress "#{location}" "#{path}"))

    File.cd! path, fn -> do_checkout(opts) end
  end

  def update(opts) do
    assert_git

    File.cd! opts[:dest], fn ->
      # Ensures origin is set the lock repo
      location = opts[:git]
      update_origin(location)

      command = IO.iodata_to_binary(["--git-dir=.git fetch --force",
                                     progress_switch(git_version()),
                                     tags_switch(opts[:tag])])

      git!(command)
      do_checkout(opts)
    end
  end

  defp progress_switch(version) when {1, 7, 1} <= version, do: " --progress"
  defp progress_switch(_),                                 do: ""

  defp tags_switch(nil), do: ""
  defp tags_switch(_), do: " --tags"

  ## Helpers

  defp validate_git_options(opts) do
    case Keyword.take(opts, [:branch, :ref, :tag]) do
      []  -> opts
      [_] -> opts
      _   ->
        Mix.raise "you should specify only one of branch, ref or tag, and only once. " <>
                  "Error on git dependency: #{opts[:git]}"
    end
  end

  defp do_checkout(opts) do
    ref = get_lock_rev_if_lock_up_to_date(opts) || get_opts_rev(opts)
    git!("--git-dir=.git checkout --quiet #{ref}")

    if opts[:submodules] do
      git!("--git-dir=.git submodule update --init --recursive")
    end

    get_lock(opts)
  end

  defp get_lock(opts) do
    rev_info = get_rev_info()
    {:git, opts[:git], rev_info[:rev], get_lock_opts(opts)}
  end

  defp get_lock_rev({:git, _repo, lock, _opts}) when is_binary(lock), do: lock
  defp get_lock_rev(_), do: nil

  defp get_lock_opts(opts) do
    lock_opts = Enum.find_value [:branch, :ref, :tag], &List.keyfind(opts, &1, 0)
    lock_opts = List.wrap(lock_opts)
    if opts[:submodules] do
      lock_opts ++ [submodules: true]
    else
      lock_opts
    end
  end

  defp get_lock_rev_if_lock_up_to_date(opts) do
    if lock_up_to_date?(opts) do
      get_lock_rev(opts[:lock])
    else
      nil
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
    destructure [origin, rev],
      :os.cmd('git --git-dir=.git config remote.origin.url && git --git-dir=.git rev-parse --verify --quiet HEAD')
      |> IO.iodata_to_binary
      |> String.split("\n", trim: true)
    [origin: origin, rev: rev]
  end

  defp update_origin(location) do
    git!(~s(--git-dir=.git config remote.origin.url "#{location}"))
    :ok
  end

  defp git!(command) do
    if Mix.shell.cmd("git " <> command) != 0 do
      Mix.raise "Command \"git #{command}\" failed"
    end
    :ok
  end

  defp assert_git do
    case Mix.State.fetch(:git_available) do
      {:ok, true} ->
        :ok
      :error ->
        if System.find_executable("git") do
          Mix.State.put(:git_available, true)
        else
          Mix.raise "Error fetching/updating Git repository: the \"git\" "  <>
            "executable is not available in your PATH. Please install "   <>
            "Git on this machine or pass --no-deps-check if you want to " <>
            "run a previously built application on a system without Git."
        end
    end
  end

  defp git_version do
    case Mix.State.fetch(:git_version) do
      {:ok, version} ->
        version
      :error ->
        version =
          :os.cmd('git --version')
          |> IO.iodata_to_binary
          |> parse_version

        Mix.State.put(:git_version, version)
        version
    end
  end

  defp git_repos_match?(repo_a, repo_b) do
    normalize_github_repo(repo_a) == normalize_github_repo(repo_b)
  end

  # TODO: Remove this on Elixir v2.0 to push everyone to https
  defp normalize_github_repo("git://github.com/" <> rest), do: "https://github.com/#{rest}"
  defp normalize_github_repo(repo), do: repo

  defp parse_version("git version " <> version) do
    String.split(version, ".")
    |> Enum.take(3)
    |> Enum.map(&to_integer/1)
    |> List.to_tuple
  end

  defp to_integer(string) do
    {int, _} = Integer.parse(string)
    int
  end

  defp lock_up_to_date?(opts) do
    case opts[:lock] do
      {:git, repo, _rev, lock_opts} ->
        git_repos_match?(repo, opts[:git]) and lock_opts == get_lock_opts(opts)
      _ ->
        false
    end
  end
end
