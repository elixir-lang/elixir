# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Elixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir,
      version: System.version(),
      build_per_environment: false
    ]
  end
end
