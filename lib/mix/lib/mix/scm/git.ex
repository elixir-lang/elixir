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
    opts = sparse_opts(opts)
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
    git_dest(opts)
    |> Path.join(".git/HEAD")
    |> File.regular?
  end

  def lock_status(opts) do
    assert_git!()
    lock = opts[:lock]

    cond do
      lock_rev = get_lock_rev(lock, opts) ->
        File.cd!(git_dest(opts), fn ->
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
    opts1[:git] == opts2[:git] and
      get_lock_opts(opts1) == get_lock_opts(opts2)
  end

  def managers(_opts) do
    []
  end

  def checkout(opts) do
    assert_git!()

    path     = git_dest(opts)
    location = opts[:git]

    _ = File.rm_rf!(path)

    fun =
      if opts[:sparse] do
        sparse_check(git_version())
        File.mkdir_p!(path)
        fn -> init_sparse(opts) end
      else
        git!(~s(clone --no-checkout --progress "#{location}" "#{path}"))
        fn -> do_checkout(opts) end
      end

    File.cd! path, fun
  end

  def update(opts) do
    assert_git!()

    File.cd! git_dest(opts), fn ->
      sparse_toggle(opts)

      location = opts[:git]
      update_origin(location)

      command = IO.iodata_to_binary(["--git-dir=.git fetch --force",
                                     progress_switch(git_version()),
                                     tags_switch(opts[:tag])])

      git!(command)
      do_checkout(opts)
    end
  end

  defp sparse_opts(opts) do
    if opts[:sparse] do
      dest = Path.join(opts[:dest], opts[:sparse])
      opts
      |> Keyword.put(:git_dest, opts[:dest])
      |> Keyword.put(:dest, dest)
    else
      opts
    end
  end

  defp sparse_check(version) do
    unless {1, 7, 0} <= version do
      version =
        version
        |> Tuple.to_list
        |> Enum.join(".")
      Mix.raise "Git >= 1.7.0 is required to use sparse checkout. " <>
                 "You are running version #{version}"
    end
  end

  defp sparse_toggle(opts) do
    git!("config core.sparsecheckout #{opts[:sparse] != nil}")
  end


  defp progress_switch(version) when {1, 7, 1} <= version, do: " --progress"
  defp progress_switch(_),                                 do: ""

  defp tags_switch(nil), do: ""
  defp tags_switch(_), do: " --tags"

  defp git_dest(opts) do
    if opts[:git_dest] do
      opts[:git_dest]
    else
      opts[:dest]
    end
  end

  ## Helpers

  defp validate_git_options(opts) do
    err = "You should specify only one of branch, ref or tag, and only once. " <>
          "Error on Git dependency: #{opts[:git]}"
    validate_single_uniq(opts, [:branch, :ref, :tag], err)

    err = "You should specify only one sparse path. " <>
          "Error on Git dependency: #{opts[:git]}"
    validate_single_uniq(opts, [:sparse], err)
  end

  defp validate_single_uniq(opts, take, error) do
    case Keyword.take(opts, take) do
      [] -> opts
      [_] -> opts
      _   -> Mix.raise error
    end
  end

  defp init_sparse(opts) do
    git!("init --quiet")
    git!("remote add origin #{opts[:git]} --fetch")
    sparse_toggle(opts)

    sparse_info =
      File.cwd!
      |> Path.join(".git/info/sparse-checkout")

    File.write(sparse_info, opts[:sparse])

    do_checkout(opts)
  end

  defp do_checkout(opts) do
    rev = get_lock_rev(opts[:lock], opts) || get_opts_rev(opts)
    git!("--git-dir=.git checkout --quiet #{rev}")

    if opts[:submodules] do
      git!("--git-dir=.git submodule update --init --recursive")
    end

    get_lock(opts)
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
    lock_opts = Keyword.take(opts, [:branch, :ref, :tag])
    lock_opts =
      if opts[:sparse] do
        lock_opts ++ [sparse: opts[:sparse]]
      else
        lock_opts
      end

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
    destructure [origin, rev],
      :os.cmd('git --git-dir=.git config remote.origin.url && git --git-dir=.git rev-parse --verify --quiet HEAD')
      |> IO.iodata_to_binary
      |> String.split("\n", trim: true)
    %{origin: origin, rev: rev}
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

  defp assert_git! do
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
