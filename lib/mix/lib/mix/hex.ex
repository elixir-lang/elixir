defmodule Mix.Hex do
  @moduledoc false
  @compile {:no_warn_undefined, Hex}
  @hex_requirement ">= 2.0.6"
  @hex_builds_url "https://builds.hex.pm"

  @doc """
  Returns `true` if `Hex` is loaded or installed.

  Otherwise returns `false`.
  """
  @spec ensure_installed?(boolean) :: boolean
  def ensure_installed?(force?) do
    cond do
      Code.ensure_loaded?(Hex) -> true
      force? or install_hex?() -> Mix.Tasks.Local.Hex.run(["--force"])
      true -> false
    end
  end

  defp install_hex? do
    shell = Mix.shell()
    shell.info("Mix requires the Hex package manager to fetch dependencies")

    shell.yes?(
      "Shall I install Hex? (if running non-interactively, use \"mix local.hex --force\")"
    )
  end

  @doc """
  Returns `true` if it has the required `Hex`. If an update is performed, it then exits.
  Otherwise returns `false` without updating anything.
  """
  @spec ensure_updated?() :: boolean
  def ensure_updated?() do
    cond do
      not Code.ensure_loaded?(Hex) ->
        false

      not Version.match?(Hex.version(), @hex_requirement) ->
        Mix.shell().info("Mix requires Hex #{@hex_requirement} but you have #{Hex.version()}")

        if Mix.shell().yes?("Shall I abort the current command and update Hex?") do
          Mix.Tasks.Local.Hex.run(["--force"])
          exit({:shutdown, 0})
        end

        false

      true ->
        true
    end
  end

  @doc """
  Ensures `Hex` is started.
  """
  def start do
    try do
      Hex.start()
    catch
      kind, reason ->
        Mix.shell().error(
          "Could not start Hex. Try fetching a new version with " <>
            "\"mix local.hex\" or uninstalling it with \"mix archive.uninstall hex.ez\""
        )

        :erlang.raise(kind, reason, __STACKTRACE__)
    end
  end

  @doc """
  Returns the URL to the Hex build assets.
  """
  def url do
    System.get_env("HEX_BUILDS_URL") || System.get_env("HEX_MIRROR") || @hex_builds_url
  end
end
