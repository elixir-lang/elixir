# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule EEx.MixProject do
  use Mix.Project

  def project do
    [
      app: :eex,
      version: System.version(),
      build_per_environment: false
    ]
  end
end
