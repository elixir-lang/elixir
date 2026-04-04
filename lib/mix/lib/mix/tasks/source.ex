# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2026 The Elixir Team

defmodule Mix.Tasks.Source do
  use Mix.Task

  @shortdoc "Prints source location for modules and functions"

  @moduledoc """
  Prints source file location for modules and functions.

  ## Examples

      $ mix source MODULE           - prints the source location for the given module
      $ mix source MODULE.FUN       - prints the source location for the given module+function
      $ mix source MODULE.FUN/ARITY - prints the source location for the given module+function+arity

  ## Command line options

    * `--open`, `-o` - opens the source file in your editor instead of printing the location.
      Requires the `ELIXIR_EDITOR` or `EDITOR` environment variable to be set.

  """

  @compile {:no_warn_undefined, IEx.Introspection}

  @switches [open: :boolean]
  @aliases [o: :open]

  @impl true
  def run(argv) do
    {opts, args} = OptionParser.parse!(argv, strict: @switches, aliases: @aliases)

    case args do
      [module = <<first, _::binary>>] when first in ?A..?Z or first == ?: ->
        loadpaths!()

        decomposition =
          module
          |> Code.string_to_quoted!()
          |> IEx.Introspection.decompose(__ENV__)

        case decomposition do
          :error ->
            Mix.raise("Invalid expression: #{module}")

          _ ->
            case IEx.Introspection.source_location(decomposition) do
              {:ok, {file, line}} ->
                if opts[:open] do
                  case IEx.Introspection.open_location(file, line) do
                    {:ok, result} -> IO.write(result)
                    {:error, message} -> Mix.raise(message)
                  end
                else
                  Mix.shell().info("#{file}:#{line}")
                end

              {:error, reason} ->
                Mix.raise("Could not find source for #{module}, #{reason}")
            end
        end

      _ ->
        Mix.raise(
          "Unexpected arguments, expected \"mix source MODULE\" or \"mix source MODULE.FUN\""
        )
    end
  end

  # Loadpaths without checks because modules may be defined in deps.
  defp loadpaths! do
    args = [
      "--no-elixir-version-check",
      "--no-deps-check",
      "--no-archives-check",
      "--no-listeners"
    ]

    Mix.Task.run("loadpaths", args)
    Mix.Task.reenable("loadpaths")
    Mix.Task.reenable("deps.loadpaths")
  end
end
