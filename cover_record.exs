# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

# TODO: Move this file in an acceptable location

case System.fetch_env("COVER_FILE") do
  {:ok, file} ->
    _ = :cover.stop()
    {:ok, _pid} = :cover.start()

    ebins = __ENV__.file |> Path.dirname() |> Path.join("lib/*/ebin") |> Path.wildcard()

    for ebin <- ebins do
      # TODO: Check result
      :cover.compile_beam_directory(String.to_charlist(ebin))
    end

    System.at_exit(fn _status ->
      :ok = :cover.export(String.to_charlist(file))
    end)

  :error ->
    :ok
end
