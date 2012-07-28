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

      if available?(".") do
        ref = opts[:ref] || "master"
        maybe_error System.cmd("git reset --hard --quiet #{ref}")

        if opts[:submodules] do
          maybe_error System.cmd("git submodule update --init --recursive")
        end

        check_rev System.cmd('git rev-parse --verify --quiet #{ref}')
      end
    end
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