defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_url "https://hex.pm/installs/hex.ez"
  @hex_requirement ">= 0.6.0"

  @shortdoc "Install hex locally"

  @moduledoc """
  Install hex locally from #{@hex_url}.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make
  """
  @spec run(OptionParser.argv) :: boolean
  def run(args) do
    url = @hex_url <> "?elixir=" <> System.version
    Mix.Tasks.Archive.Install.run [url, "--shell" | args]
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
end
