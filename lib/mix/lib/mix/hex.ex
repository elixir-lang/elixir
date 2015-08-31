defmodule Mix.Hex do
  @moduledoc false
  @hex_requirement  ">= 0.5.0"

  @doc """
  Returns true if Hex is loaded or installed, otherwise returns false.
  """
  @spec ensure_installed?(atom) :: boolean
  def ensure_installed?(app) do
    if Code.ensure_loaded?(Hex) do
      true
    else
      shell = Mix.shell
      shell.info "Could not find hex, which is needed to build dependency #{inspect app}"

      if shell.yes?("Shall I install hex?") do
        Mix.Tasks.Local.Hex.run ["--force"]
      else
        false
      end
    end
  end

  @doc """
  Returns true if have required Hex, returns false if don't and don't update,
  if update then exits.
  """
  @spec ensure_updated?() :: boolean
  def ensure_updated?() do
    cond do
      !Code.ensure_loaded?(Hex) ->
        false
      !Version.match?(Hex.version, @hex_requirement) ->
        Mix.shell.info "Mix requires hex #{@hex_requirement} but you have #{Hex.version}"

        if Mix.shell.yes?("Shall I abort the current command and update hex?") do
          Mix.Tasks.Local.Hex.run ["--force"]
          exit({:shutdown, 0})
        end

        false
      true ->
        true
    end
  end

  @doc """
  Ensure Hex is started.
  """
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
