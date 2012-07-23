defmodule Mix.SCM.Git do
  @behavior Mix.SCM

  def key do
    :git
  end

  def available?(path) do
    File.dir?(File.join(path, ".git"))
  end

  def get(path, opts) do
    location = opts[:git]
    maybe_error System.cmd("git clone --quiet --no-checkout #{location} #{path}")

    if File.dir?(path) do
      update(path, opts, false)
    end
  end

  def update(path, opts, fetch // true) do
    File.cd! path, fn ->
      if fetch do
        maybe_error System.cmd("git fetch --force --quiet --tags")
      end

      ref = opts[:ref] || "master"
      maybe_error System.cmd("git reset --hard --quiet #{ref}")

      if opts[:submodules] do
        maybe_error System.cmd("git submodule update --init --recursive")
      end
    end
  end

  defp maybe_error(""),    do: :ok
  defp maybe_error(other), do: Mix.shell.error(other)
end