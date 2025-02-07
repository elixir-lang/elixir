# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Iex do
  use Mix.Task

  @moduledoc """
  A task that simply instructs users to run `iex -S mix`.
  """

  @impl true
  def run(_) do
    Mix.raise("To use IEx with Mix, please run \"iex -S mix\"")
  end
end
