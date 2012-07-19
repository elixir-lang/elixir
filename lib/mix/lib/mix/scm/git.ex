defmodule Mix.SCM.Git do
  def available?(path) do
    File.dir?(File.join(path, ".git"))
  end

  def get(path, opts) do
    location = opts[:git]
    maybe_error System.cmd("git clone -q -n #{location} #{path}")

    if File.dir?(path) do
      File.cd! path, fn ->
        branch = opts[:branch] || "HEAD"
        maybe_error System.cmd("git checkout -q origin/#{branch}")

        if opts[:submodules] do
          maybe_error System.cmd("git submodule update --init --recursive")
        end
      end
    end
  end

  defp maybe_error(""),    do: :ok
  defp maybe_error(other), do: Mix.shell.error(other)
end