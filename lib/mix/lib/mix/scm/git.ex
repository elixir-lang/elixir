defmodule Mix.SCM.Git do
  @behavior Mix.SCM
  @moduledoc false

  def format(opts) do
    [git: opts[:git]]
  end

  def accepts_options(opts) do
    cond do
      gh = opts[:github] ->
        opts /> Keyword.delete(:github) /> Keyword.put(:git, "https://github.com/#{gh}.git")
      opts[:git] ->
        opts
      true ->
        nil
    end
  end

  def checked_out?(opts) do
    File.dir?(File.join(opts[:path], ".git"))
  end

  def matches_lock?(opts) do
    opts[:lock] && File.cd!(opts[:path], fn -> opts[:lock] == get_rev end)
  end

  def equals?(opts1, opts2) do
    opts1[:git] == opts2[:git] and
      opts1[:branch] == opts2[:branch] and
      opts1[:tag] == opts2[:tag] and
      opts1[:ref] == opts2[:ref] and
      opts1[:submodules] == opts2[:submodules]
  end

  def checkout(opts) do
    path     = opts[:path]
    location = opts[:git]
    maybe_error System.cmd(%b[git clone --quiet --no-checkout "#{location}" "#{path}"])

    if checked_out?(opts) do
      File.cd! path, fn -> do_checkout(opts) end
    end
  end

  def update(opts) do
    File.cd! opts[:path], fn ->
      command = "git fetch --force --quiet"
      if opts[:tag] do
        command = command <> " --tags"
      end
      maybe_error System.cmd(command)
      do_checkout(opts)
    end
  end

  def clean(opts) do
    File.rm_rf opts[:path]
  end

  ## Helpers

  defp do_checkout(opts) do
    ref =
      if branch = opts[:branch] do
        "origin/#{branch}"
      else
        opts[:lock] || opts[:ref] || opts[:tag] || "origin/master"
      end

    maybe_error System.cmd("git checkout --quiet #{ref}")

    if opts[:submodules] do
      maybe_error System.cmd("git submodule update --init --recursive")
    end

    get_rev
  end

  defp get_rev do
    check_rev System.cmd('git rev-parse --verify --quiet HEAD')
  end

  defp check_rev([]),   do: nil
  defp check_rev(list), do: check_rev(list, [])

  defp check_rev([h|t], acc) when h in ?a..?f or h in ?0..?9 do
    check_rev(t, [h|acc])
  end

  defp check_rev(fin, acc) when fin == [?\n] or fin == [] do
    Enum.reverse(acc) /> list_to_binary
  end

  defp check_rev(_, _) do
    nil
  end

  defp maybe_error(""),    do: :ok
  defp maybe_error(other), do: Mix.shell.error(other)
end