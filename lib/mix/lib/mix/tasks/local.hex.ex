defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_s3           "https://s3.amazonaws.com/s3.hex.pm"
  @hex_list_url     @hex_s3 <> "/installs/list.csv"
  @hex_archive_url  @hex_s3 <> "/installs/[VERSION]/hex.ez"
  @hex_requirement  ">= 0.5.0"

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

  @doc false
  # Returns true if Hex is loaded or installed, otherwise returns false.
  @spec ensure_installed?(atom) :: boolean
  def ensure_installed?(app) do
    if Code.ensure_loaded?(Hex) do
      true
    else
      shell = Mix.shell
      shell.info "Could not find hex, which is needed to build dependency #{inspect app}"

      if shell.yes?("Shall I install hex?") do
        run ["--force"]
      else
        false
      end
    end
  end

  @doc false
  # Returns true if have required Hex, returns false if don't and don't update,
  # if update then exits.
  @spec ensure_updated?() :: boolean
  def ensure_updated?() do
    if Code.ensure_loaded?(Hex) do
      if Version.match?(Hex.version, @hex_requirement) do
        true
      else
        Mix.shell.info "Mix requires hex #{@hex_requirement} but you have #{Hex.version}"

        if Mix.shell.yes?("Shall I abort the current command and update hex?") do
          run ["--force"]
          exit({:shutdown, 0})
        end

        false
      end
    else
      false
    end
  end

  @doc false
  def start do
    try do
      Hex.start
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        Mix.shell.error "Could not start Hex. Try fetching a new version with " <>
                        "`mix local.hex` or uninstalling it with `mix archive.uninstall hex.ez`"
        :erlang.raise(kind, reason, stacktrace)
    end
  end

  defp get_matching_version do
    Mix.Utils.read_path!(@hex_list_url)
    |> parse_csv
    |> all_eligibile_versions
    |> List.last
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
