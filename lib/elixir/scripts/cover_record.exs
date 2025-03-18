# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

root_dir = __ENV__.file |> Path.dirname() |> Path.join("../../..")
ebins = root_dir |> Path.join("lib/*/ebin") |> Path.wildcard()

case System.fetch_env("COVER_FILE") do
  {:ok, file} ->
    _ = :cover.stop()
    {:ok, _pid} = :cover.start()

    for ebin <- ebins,
        result <- :cover.compile_beam_directory(String.to_charlist(ebin)) do
      case result do
        {:ok, _module} ->
          :ok

        {:error, reason} ->
          raise "Failed to cover compile directory #{ebin} with reason: #{inspect(reason)}"
      end
    end

    System.at_exit(fn _status ->
      :ok = :cover.export(String.to_charlist(file))
    end)

    true

  :error ->
    false
end
