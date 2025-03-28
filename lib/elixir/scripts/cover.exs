#!bin/elixir

# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

root_dir = __ENV__.file |> Path.dirname() |> Path.join("../../..")
cover_dir = Path.join(root_dir, "cover")
coverdata_inputs = cover_dir |> Path.join("ex_unit_*.coverdata") |> Path.wildcard()
coverdata_output = Path.join(cover_dir, "combined.coverdata")
ebins = root_dir |> Path.join("lib/*/ebin") |> Path.wildcard()

_ = :cover.stop()
{:ok, cover_pid} = :cover.start()

for ebin <- ebins,
    result <- :cover.compile_beam_directory(String.to_charlist(ebin)) do
  case result do
    {:ok, _module} ->
      :ok

    {:error, reason} ->
      raise "Failed to cover compile directory #{ebin} with reason: #{inspect(reason)}"
  end
end

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
    output: cover_dir,
    summary: [threshold: 0]
  )
