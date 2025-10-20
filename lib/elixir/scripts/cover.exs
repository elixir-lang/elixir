#!/bin/elixir

# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("cover_record.exs", __DIR__)
cover_pid = CoverageRecorder.enable_coverage()

coverdata_inputs =
  CoverageRecorder.cover_dir() |> Path.join("ex_unit_*.coverdata") |> Path.wildcard()

coverdata_output = Path.join(CoverageRecorder.cover_dir(), "combined.coverdata")

for file <- coverdata_inputs do
  :ok = :cover.import(String.to_charlist(file))
end

:ok = :cover.export(String.to_charlist(coverdata_output))

{:ok, _} = Application.ensure_all_started(:mix)

# Silence analyse import messages emitted by cover
{:ok, string_io} = StringIO.open("")
Process.group_leader(cover_pid, string_io)

:ok =
  Mix.Tasks.Test.Coverage.generate_cover_results(
    output: CoverageRecorder.cover_dir(),
    summary: [threshold: 0]
  )
