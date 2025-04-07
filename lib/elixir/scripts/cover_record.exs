# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule CoverageRecorder do
  def maybe_record(suite_name) do
    if enabled?() do
      record(suite_name)

      true
    else
      false
    end
  end

  def enable_coverage do
    _ = :cover.stop()
    {:ok, pid} = :cover.start()

    cover_compile_ebins()

    pid
  end

  def cover_dir, do: Path.join(root_dir(), "cover")

  defp enabled? do
    case System.fetch_env("COVER") do
      {:ok, truthy} when truthy in ~w[1 true yes y] ->
        true

      _ ->
        false
    end
  end

  defp root_dir, do: Path.join(__DIR__, "../../..")
  defp ebins, do: root_dir() |> Path.join("lib/*/ebin") |> Path.wildcard()

  defp record(suite_name) do
    file = Path.join(cover_dir(), "ex_unit_#{suite_name}.coverdata")

    enable_coverage()

    System.at_exit(fn _status ->
      File.mkdir_p!(cover_dir())

      :ok = :cover.export(String.to_charlist(file))
    end)
  end

  defp cover_compile_ebins do
    relevant_beam_files()
    |> Enum.map(&String.to_charlist/1)
    |> :cover.compile_beam()
    |> Enum.each(fn
      {:ok, _module} ->
        :ok

      {:error, reason} ->
        raise "Failed to cover compile with reason: #{inspect(reason)}"
    end)
  end

  defp relevant_beam_files do
    ebins()
    |> Enum.flat_map(fn ebin ->
      ebin |> Path.join("*.beam") |> Path.wildcard()
    end)
    |> Enum.reject(&skip_from_coverage?/1)
  end

  @to_skip [
    # Tested via the CLI only
    :iex,
    Kernel.CLI,
    Mix.CLI,
    Mix.Compilers.Test,
    Mix.Tasks.Test,
    Mix.Tasks.Test.Coverage,

    # Documentation only
    Kernel.SpecialForms
  ]

  defp skip_from_coverage?(file) do
    mod = file |> Path.basename(".beam") |> String.to_atom()
    mod in @to_skip or match?({:docs_v1, _, _, _, _, %{deprecated: _}, _}, Code.fetch_docs(mod))
  end
end
