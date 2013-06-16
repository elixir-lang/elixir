defmodule Mix.Deps.Lock do
  @moduledoc false

  @doc """
  Touch the lockfile.
  """
  def touch do
    File.touch(lockfile)
  end

  @doc """
  Read the file, returns a keyword list containing
  the app name and its current lock information.
  """
  def read() do
    case File.read(lockfile) do
      { :ok, info } ->
        { value, _binding } = Code.eval_string(info)
        value || []
      { :error, _ } ->
        []
    end
  end

  @doc """
  Receives a keyword list and writes it to the disk.
  """
  def write(dict) do
    sorted = lc { app, rev } inlist Enum.sort(dict), rev != nil, do: { app, rev }

    unless read() == sorted do
      lines = Enum.map_join sorted, ",\n  ", fn { app, rev } ->
        %b("#{app}": #{inspect rev, raw: true})
      end

      File.write! lockfile, "[ " <> lines <> " ]\n"
    end
  end

  defp lockfile do
    Mix.project[:root_lockfile] || Mix.project[:lockfile]
  end
end
