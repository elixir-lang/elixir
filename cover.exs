#!bin/elixir

# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

# TODO: Move this file in an acceptable location

cover_dir = __ENV__.file |> Path.dirname() |> Path.join("cover")
coverdata_inputs = cover_dir |> Path.join("{exunit,eunit}_*.coverdata") |> Path.wildcard()
coverdata_output = Path.join(cover_dir, "combined.coverdata")
ebins = __ENV__.file |> Path.dirname() |> Path.join("lib/*/ebin") |> Path.wildcard()

_ = :cover.stop()
{:ok, _pid} = :cover.start()

for ebin <- ebins do
  # TODO: Check result
  :cover.compile_beam_directory(String.to_charlist(ebin))
end

for file <- coverdata_inputs do
  :ok = :cover.import(String.to_charlist(file))
end

:ok = :cover.export(String.to_charlist(coverdata_output))

{:ok, _} = Application.ensure_all_started(:mix)
# TODO: How can I mute the "Analysis includes data from imported files" IO?
:ok = Mix.Tasks.Test.Coverage.generate_cover_results(output: cover_dir)
