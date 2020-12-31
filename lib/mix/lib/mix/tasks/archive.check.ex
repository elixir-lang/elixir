defmodule Mix.Tasks.Archive.Check do
  use Mix.Task

  @moduledoc """
  Checks all archives are available.

  Mix projects can specify required archives using
  the `:archives` option:

      archives: [{:foo, "~> 1.0.0"}]

  This task guarantees this option is respected.
  """

  @impl true
  def run(_) do
    archives = Mix.Project.config()[:archives] || []

    Enum.each(archives, fn tuple ->
      {archive, req} = parse_archive(tuple)
      _ = Application.load(archive)
      vsn = Application.spec(archive, :vsn)

      cond do
        is_nil(vsn) ->
          Mix.raise(
            "Archive \"#{archive}\" could not be found. " <>
              "Please make sure the archive is installed locally."
          )

        not Version.match?(List.to_string(vsn), req) ->
          Mix.raise(
            "Archive \"#{archive}-#{vsn}\" does not match requirement #{req}. " <>
              "Please update your archive version accordingly."
          )

        true ->
          :ok
      end
    end)
  end

  defp parse_archive({archive, req}) when is_atom(archive) and is_binary(req) do
    case Version.parse_requirement(req) do
      {:ok, req} ->
        {archive, req}

      :error ->
        Mix.raise("Invalid requirement #{req} for archive \"#{archive}\"")
    end
  end

  defp parse_archive(other) do
    Mix.raise("""
    Expected archive to be in the format:

        {app :: atom, requirement :: binary}

    got:

        #{inspect(other)}

    """)
  end
end
