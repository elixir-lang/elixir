defmodule Mix.SCM.Git do
  @behavior Mix.SCM
  @moduledoc false

  def key do
    :git
  end

  def consumes?(opts) do
    cond do
      gh = opts[:github] -> Keyword.put(opts, :git, "https://github.com/#{gh}.git")
      opts[:git]         -> opts
      true               -> nil
    end
  end

  def available?(path, _opts) do
    File.dir?(File.join(path, ".git"))
  end

  def check?(path, opts) do
    opts[:lock] && File.cd!(path, fn -> opts[:lock] == get_rev end)
  end

  def match?(opts1, opts2) do
    opts1[:git] == opts2[:git] and
      opts1[:branch] == opts2[:branch] and
      opts1[:tag] == opts2[:tag] and
      opts1[:ref] == opts2[:ref] and
      opts1[:submodules] == opts2[:submodules]
  end

  def checkout(path, opts) do
    location = opts[:git]
    maybe_error System.cmd("git clone --quiet --no-checkout #{location} #{path}")

    if available?(path, opts) do
      File.cd! path, fn -> checkout(opts) end
    end
  end

  def update(path, opts) do
    File.cd! path, fn ->
      command = "git fetch --force --quiet"
      if opts[:tag] do
        command = command <> " --tags"
      end
      maybe_error System.cmd(command)
      checkout(opts)
    end
  end

  def clean(path, _opts) do
    File.rm_rf path
  end

  ## Helpers

  defp checkout(opts) do
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