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
        opts |> Keyword.delete(:github) |> Keyword.put(:git, "git://github.com/#{gh}.git")
      opts[:git] ->
        opts
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
            lock_repo != opts[:git]          -> :outdated
            lock_opts != get_lock_opts(opts) -> :outdated
            lock_rev  != rev_info[:rev]      -> :mismatch
            lock_repo != rev_info[:origin]   -> :outdated
            true -> :ok
          end
        end)
      nil ->
        :mismatch
      _ ->
        :outdated
    end
  end

  def equal?(opts1, opts2) do
    opts1[:git] == opts2[:git] &&
      get_lock_opts(opts1) == get_lock_opts(opts2)
  end

  def checkout(opts) do
    assert_git

    path     = opts[:dest]
    location = opts[:git]

    _ = File.rm_rf!(path)
    cmd = "git"
    args = ["clone", "--no-checkout", "--progress", location, path]
  
    run_cmd_or_raise(cmd, args)
    File.cd! path, fn -> do_checkout(opts) end
  end

  def update(opts) do
    assert_git

    File.cd! opts[:dest], fn ->
      # Ensures origin is set the lock repo
      location = opts[:git]
      update_origin(location)
      cmd = "git"

      args = ["--git-dir=.git", "fetch" , "--force"]

      if {1, 7, 1} <= git_version() do
        args = args ++ ["--progress"]
      end

      if opts[:tag] do
        args = args ++ ["--tags"]
      end

      run_cmd_or_raise(cmd, args)
      do_checkout(opts)
    end
  end

  ## Helpers

  defp do_checkout(opts) do
    ref = get_lock_rev(opts[:lock]) || get_opts_rev(opts)
    run_cmd_or_raise("git", ["--git-dir=.git", "checkout", "--quiet", ref])

    if opts[:submodules] do
      run_cmd_or_raise("git", ["--git-dir=.git" , "submodule", "update", "--init", "--recursive"])
    end

    get_lock(opts)
  end

  defp get_lock(opts) do
    rev_info = get_rev_info
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

  defp get_opts_rev(opts) do
    if branch = opts[:branch] do
      "origin/#{branch}"
    else
      opts[:ref] || opts[:tag] || "origin/master"
    end
  end

  defp get_rev_info do
    origin = run_system("git", ["--git-dir=.git" , "config" , "remote.origin.url"])
    rev = run_system("git", ["--git-dir=.git" , "rev-parse" , "--verify", "--quiet", "HEAD"])
    [ origin: origin, rev: rev ]
  end

  defp update_origin(location) do
    _ = System.cmd("git", ["--git-dir=.git", "config" , "remote.origin.url", location])
    :ok
  end

  defp run_system(cmd, args) do
    System.cmd(cmd, args)
    |> elem(0)
    |> String.strip
  end 

  defp run_cmd_or_raise(cmd, args) do
    {_,status} = System.cmd(cmd, args, stderr_to_stdout: true )
    if status != 0 do
      Mix.raise "Command `#{cmd} #{Enum.join(args, " ")}` failed"
    end
    true
  end

  defp assert_git do
    case Mix.State.fetch(:git_available) do
      {:ok, true} ->
        :ok
      :error ->
        if :os.find_executable('git') do
          Mix.State.put(:git_available, true)
        else
          Mix.raise "Error fetching/updating Git repository: the `git` "  <>
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
          run_system("git", ["--version"])
          |> parse_version

        Mix.State.put(:git_version, version)
        version
    end
  end

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
end
