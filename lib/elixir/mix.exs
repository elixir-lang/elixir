defmodule Elixir.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir,
      version: System.version,
      build_per_environment: false
    ]
  end
end
