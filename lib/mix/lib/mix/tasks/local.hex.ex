defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_s3           "https://s3.amazonaws.com/s3.hex.pm"
  @hex_list_url     @hex_s3 <> "/installs/list.csv"
  @hex_archive_url  @hex_s3 <> "/installs/[VERSION]/hex.ez"
  @fallback_elixir  "1.0.0"

  @shortdoc "Install hex locally"

  @moduledoc """
  Install Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make
  """
  @spec run(OptionParser.argv) :: boolean
  def run(args) do
    version = get_matching_version()
    url = String.replace(@hex_archive_url, "[VERSION]", version)
    Mix.Tasks.Archive.Install.run [url, "--system" | args]
  end

  defp get_matching_version do
    case Mix.Utils.read_path(@hex_list_url, [system: true]) do
      {:ok, csv} ->
        csv
        |> parse_csv
        |> all_eligibile_versions
        |> List.last
      {:remote, _} ->
        @fallback_elixir
    end
  end

  defp parse_csv(body) do
    :binary.split(body, "\n", [:global, :trim])
    |> Enum.flat_map(fn line ->
         [_hex|elixirs] = :binary.split(line, ",", [:global, :trim])
         elixirs
       end)
    |> Enum.uniq
  end

  defp all_eligibile_versions(versions) do
    {:ok, current_version} = Version.parse(System.version)
    Enum.filter(versions, &Version.compare(&1, current_version) != :gt)
  end
end
