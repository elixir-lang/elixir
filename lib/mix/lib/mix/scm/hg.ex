defmodule Mix.SCM.Mercurial do
  @behaviour Mix.SCM
  @moduledoc false
  
  def fetchable? do
    true
  end
  
  def format(opts) do
    opts[:hg]
  end
  
  def format_lock(opts) do
    case opts[:lock] do
      {:hg, _, lock_rev, lock_opts} ->
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
      repo = opts[:bitbucket] ->
        opts |> Keyword.delete(:bitbucket) |> Keyword.put(:hg, "https://bitbucket.org/#{repo}")
      repo = opts[:hg_github] ->
        opts |> Keyword.delete(:hg_github) |> Keyword.put(:hg, "git://github.com/#{repo}.git")
      opts[:hg] ->
        opts
      true ->
        nil
    end
  end
  
  def checked_out?(opts) do
    # Are we inside a hg repository?
    File.regular?(Path.join(opts[:dest], ".hg/requires"))
  end
  
  def lock_status(opts) do
    assert_hg

    case opts[:lock] do
      {:hg, lock_repo, lock_rev, lock_opts} ->
        File.cd!(opts[:dest], fn ->
          rev_info = get_rev_info
          cond do
            lock_repo != opts[:hg]           -> :outdated
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
    opts1[:hg] == opts2[:hg] &&
      get_lock_opts(opts1) == get_lock_opts(opts2)
  end
  
  def checkout(opts) do
    assert_hg
    
    path     = opts[:dest]
    location = opts[:hg]
    
    _ = File.rm_rf!(path)
    hg!(["clone", location, path])
    
    File.cd! path, fn -> do_checkout(opts) end
  end
  
  def update(opts) do
    assert_hg
    
    File.cd! opts[:dest], fn ->
      # Ensures origin is set the lock repo
      location = opts[:hg]
      update_origin(location)
      
      hg!(["pull", "--force"])
      do_checkout(opts)
    end
  end
  
  ## Helpers
  
  defp do_checkout(opts) do
    ref = get_lock_rev(opts[:lock]) || get_opts_rev(opts)
    hg!(["update", ref])
    
    get_lock(opts)
  end
  
  defp get_lock(opts) do
    rev_info = get_rev_info()
    {:hg, opts[:hg], rev_info[:rev], get_lock_opts(opts)}
  end
  
  defp get_lock_rev({:hg, _repo, lock, _opts}) when is_binary(lock), do: lock
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
      branch
    else
      opts[:ref] || opts[:tag] || "default"
    end
  end
  
  defp get_rev_info do
    destructure [origin, rev],
      :os.cmd('hg paths default && hg head -T "{node}\n"')
      |> IO.iodata_to_binary
      |> String.split("\n", trim: true)
    [origin: origin, rev: rev]
  end
  
  defp update_origin(location) do
    hgrc_path = ".hg/hgrc"
    
    {:ok, str} = File.read(hgrc_path)
    str = String.replace(str, ~r/(default\s*=\s*)(.*)$/, "\\1#{location}")
    :ok = File.write(hgrc_path, str)
  end
  
  defp hg!(args) do
    {output, status} = System.cmd("hg", args, stderr_to_stdout: true)
    
    if status != 0 do
      Mix.raise "Command `hg #{Enum.join(args, " ")}` failed. Output:\n#{output}"
    end
    true
  end
  
  defp assert_hg do
    case Mix.State.fetch(:hg_available) do
      {:ok, true} ->
        :ok
      :error ->
        if System.find_executable("hg") do
          Mix.State.put(:hg_available, true)
        else
          Mix.raise "Error fetching/updating Mercurial repository: the `hg` "  <>
            "executable is not available in your PATH. Please install "   <>
            "Mercurial on this machine or pass --no-deps-check if you want to " <>
            "run a previously built application on a system without Mercurial."
        end
    end
  end
end
