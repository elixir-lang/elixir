defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_url "https://hex.pm/installs/hex.ez"
  @hex_requirement ">= 0.1.1-dev"

  @shortdoc "Install hex locally"

  @moduledoc """
  Install hex locally from #{@hex_url}.

      mix local.hex

  ## Command line options

  * `--force` - forces installation without a shell prompt. Primarily
    intended for automation in build systems like make.
  """
  def run(args) do
    Mix.Tasks.Local.Install.run [@hex_url|args]
  end

  @doc false
  def install_and_load(app) do
    shell = Mix.shell
    shell.info "Could not find hex, which is needed to build dependency #{inspect app}"

    if shell.yes?("Shall I install hex?") do
      run ["--force"]
      start_hex()
    end
  end

  @doc false
  def update_and_load do
    if Code.ensure_loaded?(Hex) do
      unless Version.match?(Hex.version, @hex_requirement) do
        Mix.shell.info "Mix requires hex #{@hex_requirement} but you have #{Hex.version}"

        if Mix.shell.yes?("Shall I abort the current command and update hex?") do
          run ["--force"]
          exit(0)
        end
      end

      start_hex()
    end
  end

  defp start_hex do
    try do
      Hex.start
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        Mix.shell.error "Could not start Hex. Try fetching a new version with " <>
                        "`mix local.hex` or uninstalling it with `mix local.uninstall hex`"
        :erlang.raise(kind, reason, stacktrace)
    end
  end
end
