# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Deps.Precompile do
  use Mix.Task

  @moduledoc """
  Extension point for precompiling dependencies.

  This is a task that can be aliased by projects
  that need to execute certain tasks before
  compiling dependencies:

      aliases: ["deps.precompile": ["nerves.precompile", "deps.precompile"]]

  """

  @impl true
  def run(_) do
    :ok
  end
end
