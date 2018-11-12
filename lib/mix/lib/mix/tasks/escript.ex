defmodule Mix.Tasks.Escript do
  use Mix.Task

  @shortdoc "Lists installed escripts"

  @moduledoc ~S"""
  Lists all installed escripts.

  Escripts are installed at `~/.mix/escripts`. Add that path to your `PATH` environment variable
  to be able to run installed escripts from any directory.
  """

  use Bitwise

  @impl true
  def run(_) do
    escripts_path = Mix.Local.path_for(:escript)

    escripts_path
    |> list_dir()
    |> Enum.filter(fn filename -> executable?(Path.join(escripts_path, filename)) end)
    |> print()
  end

  defp list_dir(path) do
    case File.ls(path) do
      {:ok, list} -> list
      _ -> []
    end
  end

  defp executable?(path) do
    owner_exec_bit = 0o00100
    group_exec_bit = 0o00010
    other_exec_bit = 0o00001
    stat = File.stat!(path)

    case :os.type() do
      {:win32, _} ->
        # on win32, the script itself is not executable, but the bat is
        File.exists?(path <> ".bat") and stat.type == :regular

      _ ->
        executable_bit = stat.mode &&& (owner_exec_bit ||| group_exec_bit ||| other_exec_bit)
        executable_bit != 0 and stat.type == :regular and Path.extname(path) != ".bat"
    end
  end

  defp print([]) do
    Mix.shell().info("No escripts currently installed.")
  end

  defp print(items) do
    Enum.each(items, fn item -> Mix.shell().info(["* ", item]) end)
    Mix.shell().info("Escripts installed at: #{Mix.Local.path_for(:escript)}")
  end
end
