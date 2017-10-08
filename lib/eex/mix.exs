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
