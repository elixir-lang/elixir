defmodule Mix.SCM.Git do
  @behavior Mix.SCM

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

  def available?(path) do
    File.dir?(File.join(path, ".git"))
  end

  def get(path, opts) do
    location = opts[:git]
    maybe_error System.cmd("git clone --quiet --no-checkout #{location} #{path}")

    if available?(path) do
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

  def clean(path) do
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

    check_rev System.cmd('git rev-parse --verify --quiet HEAD')
  end

  defp check_rev([]),   do: nil
  defp check_rev(list), do: check_rev(list, [])

  defp check_rev([h|t], acc) when h in ?a..?f or h in ?0..?9 do
    check_rev(t, [h|acc])
  end

  defp check_rev(fin, acc) when fin == [?\n] or fin == [] do
    List.reverse(acc) /> list_to_binary
  end

  defp check_rev(_, _) do
    nil
  end

  defp maybe_error(""),    do: :ok
  defp maybe_error(other), do: Mix.shell.error(other)
end