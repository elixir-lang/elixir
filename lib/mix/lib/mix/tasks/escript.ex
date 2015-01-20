defmodule Mix.Tasks.Escript do
  use Mix.Task

  @shortdoc "List installed escripts."

  @moduledoc ~S"""
  Lists all installed escripts.

  Escripts are installed at `~/.mix/escripts`. Add that path to your `PATH` environment variable
  to be able to run installed escripts from any directory.
  """

  use Bitwise

  @spec run(OptionParser.argv) :: :ok
  def run(_) do
    escripts_path = Mix.Local.escripts_path

    escripts =
      escripts_path
      |> list_dir
      |> Enum.filter(fn filename -> executable?(Path.join([escripts_path, filename])) end)

    if escripts == [] do
      Mix.shell.info "No escripts currently installed."
    else
      Enum.each escripts, fn filename ->
        Mix.shell.info "* #{filename}"
      end

      Mix.shell.info "Escripts installed at: #{escripts_path}"
    end
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

    (stat.mode &&& (owner_exec_bit ||| group_exec_bit ||| other_exec_bit)) != 0
    and stat.type == :regular
  end
end
