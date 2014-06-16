defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_url "https://hex.pm/installs/hex.ez"
  @hex_requirement ">= 0.2.5"

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
  def maybe_install(app) do
    unless Code.ensure_loaded?(Hex) do
      shell = Mix.shell
      shell.info "Could not find hex, which is needed to build dependency #{inspect app}"

      if shell.yes?("Shall I install hex?") do
        run ["--force"]
      end
    end
  end

  @doc false
  def maybe_update do
    if Code.ensure_loaded?(Hex) do
      unless Version.match?(Hex.version, @hex_requirement) do
        Mix.shell.info "Mix requires hex #{@hex_requirement} but you have #{Hex.version}"

        if Mix.shell.yes?("Shall I abort the current command and update hex?") do
          run ["--force"]
          exit(0)
        end
      end
    end
  end

  @doc false
  def maybe_start do
    try do
      Code.ensure_loaded?(Hex) && Hex.start
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        Mix.shell.error "Could not start Hex. Try fetching a new version with " <>
                        "`mix local.hex` or uninstalling it with `mix local.uninstall hex`"
        :erlang.raise(kind, reason, stacktrace)
    end
  end
end
